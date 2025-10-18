
package App;

use v5.20;
use utf8;
use strict;

use File::Slurp  qw(slurp write_file);
use File::Basename;
use FindBin 1.51 qw( $RealBin );

use Devel::dipc;
use Devel::ui;

# supress some GLib warnings
#local *STDERR;
#open( STDERR, '>', '/dev/null' ) or die $!;

# Debugger UI globals

my $uixml = $RealBin . "/gdbg4.ui";    # path to glade xml ui definition

our $openFile    = "";                 # the currently shown file
our $currentFile = "";                 # the current file of debugging
our $currentLine = 0;                  # the current line under debug
our %files;    						   # files as seen by debugger, mapped to source
our %breakpoints;    				   # remember breakpoint markers so we can delete 'em
our $fifo;           				   # IPC with debugger backend
our $uiDisabled = 1;				   # flag for UI disabled/enabled
our $searchDirection = 'forward';

# global gtk widgets

our %widgets;           # UI widgets indexed by widget id
our %sourceBuffers;     # hash of source code buffers indexed by filename
our $searchCtx;         # current search context


# INT POSIX signal handler
sub dbint_handler {

    App::quit(1);
}

##################################################
# Gtk Actions
#
# these are the high level action events
# like choosing a command from the menu
# or pressing one of the toolbar buttons
##################################################

package App::Actions;
use base "App::ActionHandler";

use File::Basename;

sub onRun {

    $App::fifo->write("continue");
    App::enableButtons(0);
}

# single step, recursing into functions
sub onStep {

    $App::fifo->write("step");
    App::enableButtons(0);
}

# single step, jumping over functions
sub onOver {

    $App::fifo->write("next");
    App::enableButtons(0);
}

# step out of current function, continue stepping afterwards
sub onOut {

    $App::fifo->write("return");
    App::enableButtons(0);
}

# stop button pressed - interrupt the debugger
sub onStop {

	App::sendSignal('INT');
}

# user selected open-file from menu
sub onOpen {

	$App::widgets{fileChooserDialog}->show;
}

# user selected 'Goto current line' from File Menu
sub onScroll {

    App::scroll($App::currentFile,$App::currentLine);
}

# user selected 'Show Lexicals' from file menu
sub onLexicals {

    $App::fifo->write("lexicals");
}

# show breakpoints window menu handler
sub onBreakpoints {

    $App::fifo->write("breakpoints");
}

# show subroutines window menu handler
sub onSubs {

    $App::fifo->write("functions");
}

# show files view
sub onFiles {

	my @files = keys %App::files;
	my @sorted = sort(@files);
	my $text = join("\n", @sorted);

	$App::widgets{filesBuffer}->set_text($text,-1);

	$App::widgets{sourceView}->set_buffer($App::widgets{filesBuffer});
	$App::widgets{mainWindow}->set_title("Files loaded by the debugger:");
}

# user selected theme from Themes menu
sub onTheme :Detail {

	my ($action,$target) = @_;
    my $label = $target->get_string();

    my $manager = Gtk::Source::StyleSchemeManager::get_default();
    my $scheme = $manager->get_scheme($label);
	$App::widgets{scheme} = $scheme;

    foreach my $key ( keys %App::sourceBuffers ) {

        $App::sourceBuffers{$key}->set_style_scheme($scheme);
    }
	
	$App::widgets{infoBuffer}->set_style_scheme($scheme);;        
	$App::widgets{lexicalsBuffer}->set_style_scheme($scheme);;    
	$App::widgets{subsBuffer}->set_style_scheme($scheme);;		
	$App::widgets{filesBuffer}->set_style_scheme($scheme);;	   
	$App::widgets{breakpointsBuffer}->set_style_scheme($scheme);; 
}

# user selects source from Sources Menu
sub onWindow :Detail {

	my ($action,$target) = @_;
    my $path = $target->get_string();

    $App::openFile = $path;

    $App::widgets{sourceView}->set_buffer( $App::sourceBuffers{$App::openFile} );
    $App::widgets{mainWindow}->set_title(basename($path));
	$App::widgets{statusBar}->set_text(basename($path));
	$App::widgets{statusBar}->set_tooltip_text($path);
}

##################################################
# Gtk Signal Handlers
#
# lower level event handlers
##################################################
package App::Handlers;

use File::Basename;

# user has chosen file in FileChooser Dialog
sub onFileOpenResponse 
{
	my ($widget,$response) = @_;
	if ($response == -3 ) # accept
	{
		my $file = $widget->get_file;
		my $path = $file->get_path;

		App::openFile($path);
	}
}

# user entered return in eval entry
sub onEval {

    my $e = $App::widgets{evalEntry}->get_text();
    $App::fifo->write("eval $e");
}

# set a breakpoint UI handler (click on marker of sourceView)
sub onMarker {
    my ( $widget, $iter, $event ) = @_;

	# only allow setting breakpoints when interactive
	if($App::uiDisabled) {
		return;
	}

	# if we are not looking at a source file, no breaktpoints
	if( $App::widgets{sourceView}->get_buffer() == $App::widgets{subsBuffer} ||
		$App::widgets{sourceView}->get_buffer() == $App::widgets{infoBuffer} ||
		$App::widgets{sourceView}->get_buffer() == $App::widgets{breakpointsBuffer} ||
		$App::widgets{sourceView}->get_buffer() == $App::widgets{filesBuffer} ) 
	{
		return;
	}

	# get line and filename
    my $line = $iter->get_line() + 1;
    my $filename = $App::openFile;

    # cannot set break points in eval code
    if ( $filename =~ /^\(eval/ ) {
        return;
    }

	# get line of text, skip over some obviously non-breakable lines
	my $text = App::getLine($iter,$App::sourceBuffers{$filename},$line);
	if( !$text || $text eq "" || 
	    $text =~ /^\s*((use)|(no)|(require)|(package)|#)/ ||
		$text =~ /^\s+$/ ) 
	{
		return;
	}

	# set breakpoints and notify debugger process
    my $bpn = $filename . ":" . $line;
    if ( $App::breakpoints{$bpn} ) {    # breakpoint already exists, remove it
        $App::sourceBuffers{$filename}->delete_mark( $App::breakpoints{$bpn} );
        delete $App::breakpoints{$bpn};
        $App::fifo->write("delbreakpoint $filename.",".$line");
    }
    else {
        $App::fifo->write("breakpoint $filename,$line");
    }
}


# user clicks the call frame stack
sub onInfoPaneClick {

	my $controller = shift;
	my $nPress 	   = shift;
	my $x          = shift;
	my $y          = shift;

	if($App::uiDisabled) {
		return;
	}

	my $text = App::getLineFromMouseClick($App::widgets{infoPane},$x,$y);
	if( $text =~ /[^\[]+\[([^\[]+):([0-9]+)\]/ ) {

		my $file = $1;
		my $line = $2;
		App::scroll($file,$line);
	}
}

# mouse click on a line, if on breakpoints, files or subroutines view
sub onClick {

	my $controller = shift;
	my $nPress 	   = shift;
	my $x          = shift;
	my $y          = shift;

	if($App::uiDisabled) {
		return;
	}

	my $text = App::getLineFromMouseClick($App::widgets{sourceView},$x,$y);
	if(!$text) { 
		return; 
	}

	if($App::widgets{sourceView}->get_buffer()== $App::widgets{subsBuffer}) {

		$App::fifo->write("functionbreak $text");
		return;
	}
	elsif($App::widgets{sourceView}->get_buffer() == $App::widgets{breakpointsBuffer}) {

		if ( $text =~ /^#/) { return; }
		if ($text =~ /([^:]+):([0-9]+)/ ) {
			my $file = $1;
			my $line = $2;
			App::scroll($file,$line);
		}
		return;
	}
	elsif($App::widgets{sourceView}->get_buffer() == $App::widgets{filesBuffer}) {

		App::openFile($text,1);
		return;
	}
}

# user closes main window
sub onClose {

	App::sendSignal('KILL');
    App::quit(1);
}


# UI search support

sub onSearch {

	my $controller = shift;
	my $key        = shift;
	my $code       = shift;
	my $state      = shift;

	if($key != Gdk->KEY_Return) {
		# do default
		return 0;
	}

	# check for shift key being pressed

	my @stateMask = @$state;

	my $isShift = 0;
	foreach my $s ( @stateMask ) {

		if( $s eq 'shift-mask') {
			$isShift = 1;
		}
	}

	my $query = $App::widgets{search}->get_text();

	$App::widgets{searchSettings}->set_search_text($query);

	$App::searchCtx = Gtk::Source::SearchContext->new(
		$App::widgets{sourceView}->get_buffer(),
		$App::widgets{searchSettings},
	);

	$App::searchCtx->set_highlight(1);

	my ($hasSelection,$startIter,$endIter) = $App::widgets{sourceView}->get_buffer()->get_selection_bounds();
	my ($match,$matchStart, $matchEnd) ;

	my $direction = $isShift 
		? $App::searchDirection eq 'forward' ? 'backward' : 'forward'   
		: $App::searchDirection;

	if($direction eq 'forward' ) {

		my $searchIter = $endIter;	
		$searchIter->forward_char();

		($match,$matchStart, $matchEnd) = $App::searchCtx->forward($searchIter);
	}
	else {
		my $searchIter = $startIter;	
		($match,$matchStart, $matchEnd) = $App::searchCtx->backward($searchIter);
	}

	if($match) {

		my $buf = $App::widgets{sourceView}->get_buffer();
		$buf->select_range($matchStart,$matchEnd);

 		$App::widgets{sourceView}->get_buffer()->place_cursor($matchStart);

		$App::widgets{sourceView}->scroll_to_iter( $matchStart, 0, 1, 0.5, 0.5 );		

		my $line = $matchStart->get_line() +1;
		$App::widgets{statusBar}->set_text(basename($App::openFile).":".$line);

	}

	return 1;
}

sub onCancelSearch {

	my $controller = shift;

	my $button = $controller->get_current_button();	

	# left click
	if($button == 1) {

		if($App::searchCtx) {

			$App::searchCtx->set_highlight(0);
		}

		$App::widgets{search}->set_text("");
	}

	# right click
	if($button == 3) {

		my $dialog = $App::widgets{searchDialog};

		my $active = $App::searchDirection eq 'forward' ? 1 : 0;
		$App::widgets{searchForward}->set_active($active);
		$App::widgets{searchBackward}->set_active(!$active);

		$dialog->set_transient_for($App::widgets{mainWindow});
		$dialog->set_modal(1);
		$dialog->show;
	}
}

# search settings dialog

sub onSearchBackward {

	$App::searchDirection = 'backward';
}

sub onSearchForward {

	$App::searchDirection = 'forward';
}

sub onSearchSensitive {

	my $widget = shift;
	my $event  = shift;

	my $active = $widget->get_active;
	$App::widgets{searchSettings}->set_case_sensitive($active);
}

sub onSearchWordBoundaries  {

	my $widget = shift;
	my $event  = shift;

	my $active = $widget->get_active;
	$App::widgets{searchSettings}->set_at_word_boundaries ($active);
}

sub onSearchRegexEnabled  {

	my $widget = shift;
	my $event  = shift;

	my $active = $widget->get_active;
	$App::widgets{searchSettings}->set_regex_enabled ($active);
}

sub onSearchDialogDelete {

	my $widget = shift;
	my $event = shift;

	$widget->hide;
	return 1;
}

##################################################
# FIFO Message Handlers
#
# these are command received from
# the debugger in the debugged process
##################################################

package App::Messages;
use base 'App::MsgHandler';

sub quit :Regex() {

	App::quit(2);
}

sub cwd :Regex((.*)) {

	my $path = shift;

	$App::widgets{statusBar}->set_text($path);
	chdir $1;
}

sub pid :Regex((.*)) {

	my $pid = shift;
	App::setPID($pid);

	foreach my $bp ( keys %App::breakpoints) {
		$App::fifo->write("breaktpoint $bp")
	}
}

sub file :Regex(([^,]+),([0-9]*)) {

	my $file = shift;
	my $line = shift;

	$App::currentLine = $line;
	$App::currentFile = $file;

	App::scroll($file,$line);
	App::enableButtons(1);		
}

sub show :Regex(([^,]+),([0-9]*)) {

	my $file = shift;
	my $line = shift;

	App::scroll($file,$line);
	App::enableButtons(1);		
}

sub info :Regex(([^,]+),([0-9]*),(.*)) {

	my $file = shift;
	my $line = shift;
	my $info = shift;

	$App::currentLine = $line;
	$App::currentFile = $file;

	$App::widgets{infoBuffer}->set_text( $info, -1 );			
}

sub lexicals :Regex(([^,]+),([0-9]*),(.*)) {

	my $file = shift;
	my $line = shift;
	my $info = shift;

	$App::widgets{lexicalsBuffer}->set_text( $info, -1 );
	$App::widgets{sourceView}->set_buffer($App::widgets{lexicalsBuffer});
	$App::widgets{mainWindow}->set_title("All current lexical variables:");
}

sub breakpoints :Regex((.*)) {

	my $info = shift;

	$App::widgets{breakpointsBuffer}->set_text( $info, -1 );
	$App::widgets{sourceView}->set_buffer($App::widgets{breakpointsBuffer});
	$App::widgets{mainWindow}->set_title("All breakpoints currently set:");
}

sub marker :Regex(([^,]+),([0-9]*)) {

	my $file = shift;
	my $line = shift;

	my $bpn = $file . ":" . $line;

	my $buf = $App::sourceBuffers{$file};
	if (!$buf) {

		$buf = App::loadBuffer($file,$line);
	}

	my $iter = $buf->get_iter_at_line( $line - 1 );
	if ( !$App::breakpoints{$bpn} ) { # only if not exists
		my $mark = $App::sourceBuffers{$file}
			->create_source_mark( $bpn, "error", $iter );
		$App::breakpoints{$bpn} = $mark;
	}
}

sub rmarker :Regex(([^,]+),([0-9]*)) {

	my $file = shift;
	my $line = shift;

	my $bpn = $file . ":" . $line;
	if ( $App::breakpoints{$bpn} ) {   
		
		# only if already exists
		my $buf = $App::sourceBuffers{$file};
		if ($buf) {

			my $iter = $buf->get_iter_at_line( $line - 1 );

			$App::sourceBuffers{$file}->delete_mark($App::breakpoints{$bpn});
			delete $App::breakpoints{$bpn};
		}
	}
}

sub load :Regex(([^,]+),([0-9]*),(.*)) {

	my $file = shift;
	my $line = shift;
	my $src  = shift;

	$App::widgets{statusBar}->set_text("$file");
	$App::files{$file} = $src;
	App::scroll($file,$line);	
}

sub evaluate :Regex((.*)) {

	my $evaled = shift;

	$App::widgets{lexicalsBuffer}->set_text( $evaled, -1 );
	$App::widgets{sourceView}->set_buffer($App::widgets{lexicalsBuffer});
}

sub subs :Regex((.*)) {

	my $subs = shift;

	$App::widgets{subsBuffer}->set_text( $subs, -1 );
	$App::widgets{sourceView}->set_buffer($App::widgets{subsBuffer});
	$App::widgets{mainWindow}->set_title("All subroutines loaded:");
}

##################################################
# The Debugger UI Application
##################################################

package App;

my $langManager;       # gtsk source language manager
my $lang;              # gtk source view language (perl)
my $ctx;               # the GTK main loop context
my $pid   = 0;    	   # process ID of debugger process, to be signalled
my $quit  = 0;         # stop the UI

sub quit {
	my $value = shift;
	$quit = $value;
}

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
	$buf->set_style_scheme($App::widgets{scheme});
	if ($content) {
		$buf->set_text( $content, -1 );
	}
	$sourceBuffers{$file} = $buf;

	# add a menu item to the windows menu
	my $menuItem = Glib::IO::MenuItem->new(basename($file),"win.onWindow('$file')");#::/tmp/file.txt");
	$widgets{sourceMenu}->append_item($menuItem);  		

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
	my $x  = shift;
	my $y  = shift;

	# get scroll adjustement
	my $pos = $widget->get_vadjustment()->get_value();
	$y += $pos; # reflect scrolling offset

	# now ask for iter
	my ($r2,$iter) = $widget->get_iter_at_position($x,$y);
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

	$App::Builder::actions{onLexicals}->set_enabled($state);	
	$App::Builder::actions{onBreakpoints}->set_enabled($state);	
	$App::Builder::actions{onOpen}->set_enabled($state);	
	$App::Builder::actions{onSubs}->set_enabled($state);	
	$App::Builder::actions{onFiles}->set_enabled($state);	

    $widgets{buttonStop}->set_sensitive( $state ? 0 : 1 );
}

# update the info with current call frame stack
sub updateInfo {

    my ( $filename, $line, $info ) = @_;

    $widgets{mainWindow}->set_title( $filename . ":" . $line );

    $widgets{infoBuffer}->set_text( $info, -1 );
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

sub setPID {
	
	my $id = shift;
	$pid = $id;
}

sub sendSignal {

	my $signal = shift;

	if(!$ENV{"GDBG_KILL_CMD"}) {

	    kill $signal, $pid;
	}
	elsif ($signal ne 'KILL') { 

		system($ENV{"GDBG_KILL_CMD"}."$signal $pid");
	}
}

# build the ui once using gtk builder

my @widgetNames = qw(
	mainWindow statusBar windowMenu themesMenu
	lexicalsMenu breakpointsMenu showSubsMenu showFilesMenu
	openFileMenu evalEntry sourceView infoPane
	openFile scrollMenu lexicalsMenu buttonRun buttonStep buttonOver
	buttonOut buttonStop buttonLexicals buttonHome search searchDialog
	searchBackward searchForward themeMenu sourceMenu cancelSearch
);

sub build_ui {

	%widgets = App::Builder::build_ui( $uixml, \@widgetNames);

	my $mainWindow = $widgets{mainWindow};

	App::Builder::add_actions( $mainWindow );

	App::Builder::add_key_controller( $widgets{search}, "key-pressed", \&App::Handlers::onSearch );
	App::Builder::add_mouse_controller( $widgets{infoPane}, "pressed", \&App::Handlers::onInfoPaneClick );
	App::Builder::add_mouse_controller( $widgets{cancelSearch}, "pressed", \&App::Handlers::onCancelSearch );
	App::Builder::add_mouse_controller( $widgets{sourceView}, "pressed", \&App::Handlers::onClick );

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

	    my $menuItem = Glib::IO::MenuItem->new("$theme","win.onTheme('$theme')");#::/tmp/file.txt");
  		$widgets{themeMenu}->append_item($menuItem);  		
    }

	# default schema
    my $scheme = $manager->get_scheme("solarized-dark");
	$widgets{scheme} = $scheme;

	# prepare the info buffer to display call stack
    my $infoBuffer = $widgets{infoPane}->get_buffer();
    $widgets{infoPane}->set_editable(0);
    $infoBuffer->set_style_scheme($scheme);
	$widgets{infoBuffer} = $infoBuffer;

	# prepare the lexical vars display buffer
    my $lexicalsBuffer = Gtk::Source::Buffer->new();
    $lexicalsBuffer->set_language($lang);
    $lexicalsBuffer->set_style_scheme($scheme);
	$widgets{lexicalsBuffer} = $lexicalsBuffer;

	# buffer to show loaded subroutines
    my $subsBuffer = Gtk::Source::Buffer->new();
    $subsBuffer->set_style_scheme($scheme);
	$widgets{subsBuffer} = $subsBuffer;

	# buffer to show loaded files
    my $filesBuffer = Gtk::Source::Buffer->new();
    $filesBuffer->set_style_scheme($scheme);
	$widgets{filesBuffer} = $filesBuffer;

	# buffer to show loaded breakpoints
    my $breakpointsBuffer = Gtk::Source::Buffer->new();
    $breakpointsBuffer->set_style_scheme($scheme);
	$widgets{breakpointsBuffer} = $breakpointsBuffer;

	# prepare search support
	my $searchSettings = Gtk::Source::SearchSettings->new();
	$searchSettings->set_wrap_around(1);
	$searchSettings->set_regex_enabled(1);
	$searchSettings->set_case_sensitive(0);
	$widgets{searchSettings} = $searchSettings;
	
	# set everything to disabled on startup
    enableButtons(0);
    $widgets{buttonStop}->set_sensitive(0);

	# preload filechooser
    my $fileChooserDialog = Gtk::FileChooserNative->new (
		"Open File",
		$mainWindow,
		"open",
		"_Open",
		"_Cancel"
	);

	$fileChooserDialog->signal_connect( "response", \&App::Handlers::onFileOpenResponse);
	$widgets{fileChooserDialog} = $fileChooserDialog;

    # show the UI
    $widgets{mainWindow}->show();
}

# initialize IPC with debugger process

sub initialize {

    $SIG{'INT'} = "App::dbint_handler";

    # the IPC named pipe (fifo) for comms with the debugger

    $fifo = Devel::dipc->new();

	my $fifo_dir = $ENV{"GDBG_FIFO_DIR"} || '/tmp/';

    # this will hang until the Debugger has connected
    $fifo->open_in("$fifo_dir/perl_debugger_fifo_out");
    $fifo->open_out("$fifo_dir/perl_debugger_fifo_in");
}

# startup

initialize();
build_ui();

# run the ui loop

$ctx = GLib::MainContext::default();

while ( !$quit ) {

    # pump GTK events
    while ( $ctx->pending() ) {
        $ctx->iteration(0);
    }

    # pump debugger IPC messages
    my @msgs = $fifo->read();
    foreach my $msg (@msgs) {
	
		App::MsgHandler::handle($msg);
    }
}

if ( $quit == 1 ) {
    $fifo->write("quit");
}
