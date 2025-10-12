
##################################################
package gdbgui;
##################################################

use v5.20;
use utf8;
use strict;

##################################################
# Perl dependencies
##################################################

use Data::Dumper;
use File::Slurp  qw(slurp write_file);
use File::Basename;
use FindBin 1.51 qw( $RealBin );

# IPC package share with debugger
use Devel::dipc;

# supress some GLib warnings
#local *STDERR;
#open( STDERR, '>', '/dev/null' ) or die $!;

##################################################
# glib and gtk dependencies via gir
##################################################

use Glib::Object::Introspection;

Glib::Object::Introspection->setup(
    basename => 'GLib',
    version  => '2.0',
    package  => 'GLib'
);

Glib::Object::Introspection->setup(
    basename => 'Gio',
    version  => '2.0',
    package  => 'Glib::IO'
);

Glib::Object::Introspection->setup(
    basename => 'Gdk',
    version  => '3.0',
    package  => 'Gdk'
);

Glib::Object::Introspection->setup(
    basename => 'Gtk',
    version  => '3.0',
    package  => 'Gtk3'
);

Glib::Object::Introspection->setup(
    basename => 'GtkSource',
    version  => '4',
    package  => 'Gtk::Source'
);

##################################################
# Debugger UI globals
##################################################

my $quit        = 0;                  # stop the UI
my $openFile    = "";                 # the currently shown file
my $currentFile = "";                 # the current file of debugging
my $currentLine = 0;                  # the current line under debug
my %files;    						  # files as seen by debugger, mapped to source
my $uixml = $RealBin . "/gdbg.ui";    # path to glade xml ui definition
my $pid   = 0;    					  # process ID of debugger process, to be signalled
my %breakpoints;    				  # remember breakpoint markers so we can delete 'em
my $fifo;           				  # IPC with debugger backend
my $uiDisabled = 1;					  # flag for UI disabled/enabled
my $searchDirection = 'forward';

##################################################
# global gtk widgets
##################################################

my %widgets;           # UI widgets indexed by widget id
my $langManager;       # gtsk source language manager
my $lang;              # gtk source view language (perl)
my $scheme;            # gtk source view theme to use
my $infoBuffer;        # the buffer displaying  call frame stack info
my $lexicalsBuffer;    # gtk source buffer used to display lexicals vars
my $subsBuffer;		   # buffer to display subs
my $filesBuffer;	   # buffer to display files
my $breakpointsBuffer; # buffer to display breakpoints
my %sourceBuffers;     # hash of source code buffers indexed by filename
my $ctx;               # the GTK main loop context
my $searchSettings;
my $searchCtx;


# INT signal handler
sub dbint_handler {

    $quit = 1;
}

##################################################
# main UI action handlers
##################################################

# run command - continue until next breakpoint
sub onRun {

    $fifo->write("continue");
    enableButtons(0);
}

# single step, recursing into functions
sub onStep {

    $fifo->write("step");
    enableButtons(0);
}

# single step, jumping over functions
sub onOver {

    $fifo->write("next");
    enableButtons(0);
}

# step out of current function, continue stepping afterwards
sub onOut {

    $fifo->write("return");
    enableButtons(0);
}

# stop button pressed - interrupt the debugger
sub onStop {

    #	print "KILL $pid\n";
	if(!$ENV{"GDBG_KILL_CMD"}) {

	    kill 'INT', $pid;
	}
	else {

		system($ENV{"GDBG_KILL_CMD"}." $pid");
	}
}

# user entered return in eval entry
sub onEval {

    my $e = $widgets{evalEntry}->get_text();

    $fifo->write("eval $e");
}


# user selected open-file from menu
sub onOpen {

    # show open file dialog
    my $dlg = Gtk3::FileChooserNative->new( 
		"Open File", 
		$widgets{mainWindow}, 
		'open',
        "OK", 
		"Cancel" 
	);

    my $r = $dlg->run();
    if ( $r == -3 ) {    # accept file

        my $fn = $dlg->get_filename();
        openFile($fn);
    }
}

# user selected 'Goto current line' from File Menu
sub onScroll {

    scroll($currentFile,$currentLine);
}

# set a breakpoint UI handler (click on marker of sourceView)
sub onMarker {
    my ( $self, $iter, $event ) = @_;

	# only allow setting breakpoints when interactive
	if($uiDisabled) {
		return;
	}

	# if we are not looking at a source file, no breaktpoints
	if( $widgets{sourceView}->get_buffer() eq $subsBuffer ||
		$widgets{sourceView}->get_buffer() eq $infoBuffer ||
		$widgets{sourceView}->get_buffer() eq $breakpointsBuffer ||
		$widgets{sourceView}->get_buffer() eq $filesBuffer) 
	{
		return;
	}

	# get line and filename
    my $line = $iter->get_line() + 1;
    my $filename = $openFile;

    # cannot set break points in eval code
    if ( $filename =~ /^\(eval/ ) {
        return;
    }

	# get line of text, skip over some obviously non-breakable lines
	my $text = getLine($iter,$sourceBuffers{$filename},$line);
	if( !$text || $text eq "" || 
	    $text =~ /^\s*((use)|(no)|(require)|(package)|#)/ ||
		$text =~ /^\s+$/ ) 
	{
		return;
	}

	# set breakpoints and notify debugger process
    my $bpn = $filename . ":" . $line;
    if ( $breakpoints{$bpn} ) {    # breakpoint already exists, remove it
        $sourceBuffers{$filename}->delete_mark( $breakpoints{$bpn} );
        delete $breakpoints{$bpn};
        $fifo->write("delbreakpoint $filename.",".$line");
    }
    else {
        $fifo->write("breakpoint $filename,$line");
    }
}

# user selected 'Show Lexicals' from file menu
sub onLexicals {

    my $e = $widgets{evalEntry}->get_text();

    $fifo->write("lexicals");
}

# show breakpoints window menu handler
sub onBreakpoints {

    $fifo->write("breakpoints");
}

# show subroutines window menu handler
sub onSubs {

    $fifo->write("functions");
}

# user clicks the call frame stack
sub onInfoPaneClick {

	my $widget = shift;
	my $event = shift;

	if($uiDisabled) {
		return;
	}

	my $text = getLineFromMouseClick($widget,$event);
	if( $text =~ /[^\[]+\[([^\[]+):([0-9]+)\]/ ) {

		my $file = $1;
		my $line = $2;
		scroll($file,$line);
	}
}

# mouse click on a line, if on breakpoints, files or subroutines view
sub onClick {

	my $widget = shift;
	my $event  = shift;

	if($uiDisabled) {
		return;
	}

	my $text = getLineFromMouseClick($widget,$event);
	if(!$text) { 
		return; 
	}

	if($widgets{sourceView}->get_buffer() eq $subsBuffer) {

		$fifo->write("functionbreak $text");
		return;
	}
	elsif($widgets{sourceView}->get_buffer() eq $breakpointsBuffer) {

		if ( $text =~ /^#/) { return; }
		if ($text =~ /([^:]+):([0-9]+)/ ) {
			my $file = $1;
			my $line = $2;
			scroll($file,$line);
		}
		return;
	}
	elsif($widgets{sourceView}->get_buffer() eq $filesBuffer) {

		openFile($text,1);
		return;
	}
}

# show files view
sub onFiles {

	my @files = keys %files;
	my @sorted = sort(@files);
	my $text = join("\n", @sorted);

	$filesBuffer->set_text($text,-1);

	$widgets{sourceView}->set_buffer($filesBuffer);
	$widgets{mainWindow}->set_title("Files loaded by the debugger:");
}

# user selected theme from Themes menu
sub onTheme {
    my ($menuItem) = @_;
    my $label = $menuItem->get_label();

    my $manager = Gtk::Source::StyleSchemeManager::get_default();
    $scheme = $manager->get_scheme($label);

    foreach my $key ( keys %sourceBuffers ) {

        $sourceBuffers{$key}->set_style_scheme($scheme);
    }
	
	$infoBuffer->set_style_scheme($scheme);;        
	$lexicalsBuffer->set_style_scheme($scheme);;    
	$subsBuffer->set_style_scheme($scheme);;		
	$filesBuffer->set_style_scheme($scheme);;	   
	$breakpointsBuffer->set_style_scheme($scheme);; 
}

# user selects source from Sources Menu
sub onWindow {
    my ($menuItem) = @_;
    my $label = $menuItem->get_label();

    $openFile = $label;

    $widgets{sourceView}->set_buffer( $sourceBuffers{$openFile} );
    $widgets{mainWindow}->set_title(basename($label));
	$widgets{statusBar}->set_text(basename($label));
	$widgets{statusBar}->set_tooltip_text($label);
}

# user closes main window
sub onDestroy {

    kill 'INT', $pid;
    $quit = 1;
}

##################################################
# UI search support
##################################################

sub onSearch {

	my $widget = shift;
	my $event = shift;

	my $key = $event->get_keyval();

	if($key != Gdk->KEY_Return) {
		# do default
		return 0;
	}

	# check for shift key being pressed
	my ($unused,$state) = $event->get_state();

	my @stateMask = @$state;

	my $isShift = 0;
	foreach my $s ( @stateMask ) {

		if( $s eq 'shift-mask') {
			$isShift = 1;
		}
	}

	my $query = $widget->get_text();

	$searchSettings->set_search_text($query);

print STDERR "SENSI: ".$searchSettings->get_case_sensitive()."\n";

	$searchCtx = Gtk::Source::SearchContext->new(
		$widgets{sourceView}->get_buffer(),
		$searchSettings,
	);

	$searchCtx->set_highlight(1);

	my ($hasSelection,$startIter,$endIter) = $widgets{sourceView}->get_buffer()->get_selection_bounds();
	my ($match,$matchStart, $matchEnd) ;

	my $direction = $isShift 
		? $searchDirection == 'forward' ? 'backward' : 'forward'   
		: $searchDirection;

print STDERR "SHIFT: $isShift $direction\n";		

	$searchDirection = $direction;

	if($direction eq 'forward' ) {

		my $searchIter = $endIter;	
		$searchIter->forward_char();

		($match,$matchStart, $matchEnd) = $searchCtx->forward($searchIter);
	}
	else {
		my $searchIter = $startIter;	
		($match,$matchStart, $matchEnd) = $searchCtx->backward($searchIter);
	}

	if($match) {

		my $buf = $widgets{sourceView}->get_buffer();
		$buf->select_range($matchStart,$matchEnd);

		$widgets{sourceView}->scroll_to_iter( $matchStart, 0, 1, 0.5, 0.5 );		
	}

	return 1;
}

sub onCancelSearch {

	my $widget = shift;
	my $event = shift;

	if($event->button->{type} eq 'button-press') {

		# left click
		if($event->button->{button} == 1) {

			if($searchCtx) {

				$searchCtx->set_highlight(0);
			}

			$widgets{search}->set_text("");
		}
		# right click
		if($event->button->{button} == 3) {

			my $dialog = $widgets{searchDialog};

			my $active = $searchDirection eq 'forward' ? 1 : 0;
			print STDERR "A: $active\n";
			$widgets{searchForward}->set_active($active);
			$widgets{searchBackward}->set_active(!$active);

			$dialog->set_transient_for($widgets{mainWindow});
			$dialog->set_modal(1);
			$dialog->show_all;
		}
	}
}

# search dialog events

sub onSearchDialogClose {

	my $widget = shift;
	my $event = shift;

	$widget->hide;
}

sub onSearchDialogDelete {

	my $widget = shift;
	my $event = shift;

	return 1;
}

sub onSearchBackward {

	my $widget = shift;
	my $event = shift;

	$searchDirection = 'backward';
}

sub onSearchForward {

	my $widget = shift;
	my $event = shift;

	$searchDirection = 'forward';
}

sub onSearchSensitive {

	my $widget = shift;
	my $event = shift;

	my $value = $widget->get_active;
print STDERR "SetSensi $value\n";	
	$searchSettings->set_case_sensitive($value);
}

sub onSearchWordBoundaries {

	my $widget = shift;
	my $event = shift;

	my $value = $widget->get_active;
	$searchSettings->set_at_word_boundaries ($value);
}

sub onSearchRegexEnabled {

	my $widget = shift;
	my $event = shift;

	my $value = $widget->get_active;
	$searchSettings->set_regex_enabled ($value);
}


##################################################
# process messages from debugger
##################################################

my @msg_handlers = (
	{
		# quit debugger
		regex   => qr/^quit$/s,
		handler => sub {
			$quit = 2;
		}
	},
	{
		# set current work directory
		regex   => qr/^cwd (,*)/s,
		handler => sub {

			$widgets{statusBar}->set_text($1);
			chdir $1;
		}
	},
	{
		# process id (pid) of dubugger process
		regex   => qr/^pid (.*)$/s,
		handler => sub {

			$pid = $1;

			foreach my $bp ( keys %breakpoints) {
				$fifo->write("breaktpoint $bp")
			}
		}
	},
	{
		# display file at line
		regex   => qr/^file ([^,]+),([0-9]*)/s,
		handler => sub {

			my $file = $1;
			my $line = $2;

			$currentLine = $line;
			$currentFile = $file;

			scroll($file,$line);
			enableButtons(1);		
		}
	},
	{
		# show file at line (like display above)
		# but do not set current file
		regex   => qr/^show ([^,]+),([0-9]*)/s,
		handler => sub {

			my $file = $1;
			my $line = $2;

			scroll($file,$line);
			enableButtons(1);		
		}
	},
	{
		# display call stack info
		regex   => qr/^info ([^,]+),([0-9]*),(.*)/s,
		handler => sub {

			my $file = $1;
			my $line = $2;
			my $info = $3;

			$currentLine = $line;
			$currentFile = $file;

			$infoBuffer->set_text( $info, -1 );		
		}
	},
	{
		# display lexicals
		regex   => qr/^lexicals ([^,]+),([0-9]*),(.*)/s,
		handler => sub {

			my $file = $1;
			my $line = $2;
			my $info = $3;

			$lexicalsBuffer->set_text( $info, -1 );
			$widgets{sourceView}->set_buffer($lexicalsBuffer);
			$widgets{mainWindow}->set_title("All current lexical variables:");
		}
	},
	{
		# display breakpoints
		regex   => qr/^breakpoints (.*)/s,
		handler => sub {

			my $info = $1;

			$breakpointsBuffer->set_text( $info, -1 );
			$widgets{sourceView}->set_buffer($breakpointsBuffer);
			$widgets{mainWindow}->set_title("All breakpoints currently set:");
		}
	},
	{
		# set a marker at file:line
		regex   => qr/^marker ([^,]+),([0-9]*)/s,
		handler => sub {

			my $file = $1;
			my $line = $2;

			my $bpn = $file . ":" . $line;

			my $buf = $sourceBuffers{$file};
			if (!$buf) {

				$buf = loadBuffer($file,$line);
			}

			my $iter = $buf->get_iter_at_line( $line - 1 );
			if ( !$breakpoints{$bpn} ) { # only if not exists
				my $mark = $sourceBuffers{$file}
					->create_source_mark( $bpn, "error", $iter );
				$breakpoints{$bpn} = $mark;
			}
		}
	},
	{
		# remove a marker at file:line
		regex   => qr/^rmarker ([^,]+),([0-9]*)/s,
		handler => sub {

			my $file = $1;
			my $line = $2;

			my $bpn = $file . ":" . $line;
			if ( $breakpoints{$bpn} ) {   
				
				# only if already exists
				my $buf = $sourceBuffers{$file};
				if ($buf) {

					my $iter = $buf->get_iter_at_line( $line - 1 );

					$sourceBuffers{$file}->delete_mark($breakpoints{$bpn});
					delete $breakpoints{$bpn};
				}
			}
		}
	},
	{
		# load file,line,source
		regex   => qr/^load ([^,]+),([0-9]+),(.*)/s,
		handler => sub {

			my $file = $1;
			my $line = $2;
			my $src  = $3;
			$widgets{statusBar}->set_text("$file");

			$files{$file} = $src;
			if ( $file !~ /^\/usr\// ) {
				scroll($file,$line);
			}
		}
	},
	{
		# eval results passed as string
		regex   => qr/^eval (.*)/s,
		handler => sub {

			my $evaled = $1;
			$lexicalsBuffer->set_text( $evaled, -1 );
			$widgets{sourceView}->set_buffer($lexicalsBuffer);
		}
	},
	{
		# all known subroutines for display
		regex   => qr/^subs (.*)/s,
		handler => sub {

			my $subs = $1;
			$subsBuffer->set_text( $subs, -1 );
			$widgets{sourceView}->set_buffer($subsBuffer);
			$widgets{mainWindow}->set_title("All subroutines loaded:");
		}
	},
);

sub process_msg {

    my $msg = shift;

    #print "MSG: ".substr($msg,0,60)."\n";

	foreach my $handler ( @msg_handlers) {
		if ( $msg =~ $handler->{regex} ) {
			$handler->{handler}->();
			return;
		}
	}
}

##################################################
# logic
##################################################


# open a new file in the visual debugger
sub openFile {
    my $filename = shift;
    my $line     = shift;

    $widgets{mainWindow}->set_title( basename($filename) . ":" . $line );	
	$widgets{statusBar}->set_text(basename($filename).":".$line);
	$widgets{statusBar}->set_tooltip_text($filename);

    while ( $ctx->pending() ) {
        $ctx->iteration(0);
    }

    $openFile = $filename;

	# if we already have a source buffer for this file
    if ( $sourceBuffers{$filename} ) {

        # file already exists!
        $widgets{sourceView}->set_buffer( $sourceBuffers{$filename} );

		# if we have the source
		if($files{$filename}) {

			# has it changed from display?
			my $txt = getSource($filename);
			if(  $txt ne $files{$filename} ) {

				$sourceBuffers{$filename}->set_text( $files{$filename}, -1 );
			}
		}
        return;
    }

	# create new source buffer
	my $buf = loadBuffer($filename,$line);

    while ( $ctx->pending() ) {
        $ctx->iteration(0);
    }

    # set the new buffer as current buffer for display
    $widgets{sourceView}->set_buffer($buf);

    while ( $ctx->pending() ) {
        $ctx->iteration(0);
    }	
}


##################################################
# UI helpers
##################################################

# load a file into a new source buffer
sub loadBuffer {

	my $file = shift;
	my $line = shift || 1;

	# get the content
	my $content = $files{$file};
	if( !$content ) {

		# if file exists on disk, read it in
		if(-e $file) {

			$content = slurp($file, binmoder => 'utf8' );
		}
		# otherwise ask the debugger process for the file
		else {
			# temp dummy content
			$content = '<unknown>'; # 
			# ask debugger to provide file
			$fifo->write("fetch $file,$line");			
		}
		$files{$file} = $content;
	}
	
	# create GTK source buffer with content
	my $buf = Gtk::Source::Buffer->new();
	$buf->set_language($lang);
	$buf->set_style_scheme($scheme);
	if ($content) {
		$buf->set_text( $content, -1 );
	}
	$sourceBuffers{$file} = $buf;

	# add a menu item to the windows menu
	my $item = Gtk3::MenuItem->new_with_label($file);
	$item->signal_connect( 'activate' => \&onWindow );

	$widgets{windowMenu}->add($item);
	$widgets{windowMenu}->show_all();

	return $buf;
}

# get full text source from a existing source buffer
sub getSource {

	my $filename = shift;

	my $startiter = $sourceBuffers{$filename}->get_start_iter();
	my $enditer   = $sourceBuffers{$filename}->get_end_iter();
	my $txt = $sourceBuffers{$filename}->get_text($startiter,$enditer,0);
	return $txt;
}

# get a line of text from buffer
sub getLine {
	my $iter = shift;
	my $buffer = shift;
	my $line = shift;

	my $count = $buffer->get_line_count();
	my $endIter = $buffer->get_iter_at_line($line);
	if($line-1 >= $count) {
		$endIter = $buffer->get_end_iter();
	}
	my $text = $buffer->get_text($iter,$endIter,1);
	chomp($text);

	return $text;
}

sub getLineFromMouseClick {

	my $widget = shift;
	my $event  = shift;

	# relative mouse position
	my ($r,$x,$y) = $event->get_coords();

	# get scroll adjustement
	my $pos = $widget->get_vadjustment()->get_value();
	$y += $pos; # reflect scrolling offset

	# now ask for iter
	my ($r,$iter) = $widget->get_iter_at_position($x,$y);
	$iter->forward_line();

	# get the line from the iter, (which is line+1)
	my $line = $iter->get_line();

	# adjust the iter again, now have correct line
	$iter->backward_line();

	# the the line source text
	my $text = getLine($iter,$widget->get_buffer(),$line);

	return $text;
}

# enable / disable buttons
sub enableButtons {

    my $state = shift;

	$uiDisabled = $state ? 0 : 1;

    $widgets{buttonRun}->set_sensitive($state);
    $widgets{buttonStep}->set_sensitive($state);
    $widgets{buttonOver}->set_sensitive($state);
    $widgets{buttonOut}->set_sensitive($state);
    $widgets{buttonLexicals}->set_sensitive($state);
    $widgets{buttonHome}->set_sensitive($state);
    $widgets{evalEntry}->set_sensitive($state);
    $widgets{lexicalsMenu}->set_sensitive($state);
    $widgets{breakpointsMenu}->set_sensitive($state);
    $widgets{openFileMenu}->set_sensitive($state);
    $widgets{showSubsMenu}->set_sensitive($state);
    $widgets{showFilesMenu}->set_sensitive($state);

    $widgets{buttonStop}->set_sensitive( $state ? 0 : 1 );
}

# update the info with current call frame stack
sub updateInfo {

    my ( $filename, $line, $info ) = @_;

    $widgets{mainWindow}->set_title( $filename . ":" . $line );

    $infoBuffer->set_text( $info, -1 );
}

# scroll to currently debugged line
sub scroll {

	my $file = shift;
    my $line = shift;

    openFile( $file, $line );

    while ( $ctx->pending() ) {
        $ctx->iteration(0);
    }

    my $iter = $sourceBuffers{$file}->get_iter_at_line( $line - 1 );
    $widgets{sourceView}->scroll_to_iter( $iter, 0, 1, 0, 0.5 );
    while ( $ctx->pending() ) {
        $ctx->iteration(0);
    }

    $sourceBuffers{$file}->place_cursor($iter);

    while ( $ctx->pending() ) {
        $ctx->iteration(0);
    }
}

##################################################
# build the ui once using gtk builder
##################################################

sub mapWidgets {

    my $builder = shift;

    my @widgetNames = qw(
      mainWindow statusBar
      windowMenu themesMenu
      lexicalsMenu breakpointsMenu
	  showSubsMenu showFilesMenu
      openFileMenu evalEntry
      sourceView infoPane
      openFile scrollMenu lexicalsMenu
      buttonRun buttonStep buttonOver
      buttonOut buttonStop buttonLexicals
      buttonHome search searchDialog
	  searchBackward searchForward
    );

    foreach my $wn (@widgetNames) {

        my $widget = $builder->get_object($wn);
        $widgets{$wn} = $widget;
    }
}

sub connect_signals {

    my ( $builder, $obj, $signal, $handler, $co, $flags, $data ) = @_;

    $obj->signal_connect( $signal => \&$handler );
}

sub build_ui {

	# load widgets from xml
    my $builder = Gtk3::Builder->new();
    $builder->add_from_file($uixml) or die 'file not found';

	# get refrences to widgets
    mapWidgets($builder);

	# connect UI signal handlers
    $builder->connect_signals_full( \&connect_signals, 0 );

	# Perl syntax highlighting support
    $langManager = Gtk::Source::LanguageManager->new();
    $lang        = $langManager->get_language("perl");

	# prepare support for breakpoint markers
    my $attrs = Gtk::Source::MarkAttributes->new();
    $attrs->set_icon_name("media-record");

	# setup main sourceView attributes
    $widgets{sourceView}->set_show_line_marks(1);
    $widgets{sourceView}->set_editable(0);
    $widgets{sourceView}->set_wrap_mode('none');
    $widgets{sourceView}->set_mark_attributes( "error", $attrs, 10 );

    # theme support for buffers
    my $manager = Gtk::Source::StyleSchemeManager::get_default();

	# populate schemes menu
    my $themes = $manager->get_property("scheme-ids");
    foreach my $theme (@$themes) {
        my $item = Gtk3::MenuItem->new_with_label($theme);
        $item->signal_connect( 'activate' => \&onTheme );
        $widgets{themesMenu}->add($item);
    }

	# default schema
    $scheme = $manager->get_scheme("solarized-dark");

	# preapre the info buffer to display call stack
    $infoBuffer = $widgets{infoPane}->get_buffer();
    $widgets{infoPane}->set_editable(0);
    $infoBuffer->set_style_scheme($scheme);

	# prepare the lexical vars display buffer
    $lexicalsBuffer = Gtk::Source::Buffer->new();
    $lexicalsBuffer->set_language($lang);
    $lexicalsBuffer->set_style_scheme($scheme);

	# buffer to show loaded subroutines
    $subsBuffer = Gtk::Source::Buffer->new();
    $subsBuffer->set_style_scheme($scheme);

	# buffer to show loaded files
    $filesBuffer = Gtk::Source::Buffer->new();
    $filesBuffer->set_style_scheme($scheme);

	# buffer to show loaded breakpoints
    $breakpointsBuffer = Gtk::Source::Buffer->new();
    $breakpointsBuffer->set_style_scheme($scheme);

	# prepare search support
	$searchSettings = Gtk::Source::SearchSettings->new();
	$searchSettings->set_wrap_around(1);
	$searchSettings->set_regex_enabled(1);
	$searchSettings->set_case_sensitive(0);
	
	# set everything to disabled on startup
    enableButtons(0);
    $widgets{buttonStop}->set_sensitive(0);

    # show the UI
    $widgets{mainWindow}->show_all();
}

##################################################
# initialize
##################################################

sub initialize {

    Gtk3::init();

    $ctx = GLib::MainContext::default();

    $SIG{'INT'} = "dbgui::dbint_handler";

    # the IPC named pipe (fifo) for comms with the debugger

    $fifo = Devel::dipc->new();

	my $fifo_dir = $ENV{"GDBG_FIFO_DIR"} || '/tmp/';

    # this will hang until the Debugger has connected
    $fifo->open_in("$fifo_dir/perl_debugger_fifo_out");
    $fifo->open_out("$fifo_dir/perl_debugger_fifo_in");
}

##################################################
# THE ui main loop
##################################################

initialize();
build_ui();

while ( !$quit ) {

    # pump GTK events
    while ( $ctx->pending() ) {
        $ctx->iteration(0);
    }

    # pump debugger IPC messages
    my @msgs = $fifo->read();
    foreach my $msg (@msgs) {
        process_msg($msg);
    }
}

##################################################
# over and out
##################################################

if ( $quit == 1 ) {
    $fifo->write("quit");
}

