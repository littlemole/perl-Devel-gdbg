
##################################################
package gdbgui;
##################################################

use v5.20;
use utf8;
use strict;

##################################################
# Perl dependencies
##################################################

use JSON;
use Data::Dumper;
use File::Slurp  qw(slurp write_file);
use File::Basename;
use FindBin 1.51 qw( $RealBin );
use Params::Util qw<_HASH _HASH0 _HASHLIKE _ARRAYLIKE>;
use Sub::Util qw(subname);

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
# Widgets Collection Helper
# access all the Gtk Widget objects by name
# the 'name' is the id attribute from
# the glade xml description
##################################################

##################################################
package Widgets;
##################################################

sub new {
	my $class   = shift;
	my $widgets = shift; # hash name => widget

	my $self = {
		widgets => $widgets
	};

	return bless $self, $class;
}

sub add {
	my $self   = shift;
	my $name   = shift;
	my $widget = shift;

	$self->{widgets}->{$name} = $widget; 
}

# fake $widget accessors: calling
#     $widgets->widget_name()
# is equivalent to
#     $widgets->{widgets}->{widget_name}

sub AUTOLOAD {

	no strict;
	my $self = shift;
	my $widgets = $self->{widgets};

	(my $method = $AUTOLOAD) =~ s{.*::}{};

	use strict;
	if(! exists $widgets->{$method} ) {
		die ("Widget $method does not exist.");
	}
	return $widgets->{$method};
}

##################################################
package gdbgui;
##################################################

##################################################
# Debugger UI globals
##################################################

my $quit        = 0;                  # stop the UI
my $openFile    = "";                 # the currently shown file
my $currentFile = "";                 # the current file of debugging
my $currentLine = 0;                  # the current line under debug
my $pid         = 0;    			  # process ID of debugger process, to be signalled
my $uiDisabled  = 1;				  # flag for UI disabled/enabled
my %files;    						  # files loaded, mapped to source
my $fifo;           				  # IPC with debugger backend
my $rpc;                              # RPC abstraction on top of $fifo
my $searchDirection = 'forward';      # search direction for text search
my $selectedVar = "/";                # last expanded item in var inspection tree
my $uixml = $RealBin . "/gdbg.ui";    # path to glade xml ui definition

##################################################
# global gtk widgets
##################################################

my $widgets;           				  # UI widgets indexed by widget id
my $scheme;            				  # gtk source view theme to use
my %sourceBuffers;     				  # hash of source code buffers indexed by filename
my $ctx;               				  # the GTK main loop context
my $searchCtx;         				  # search ctx


##################################################
# msghandlers, actions and accelerators
# these will be auto-populated from
# sub metadata attributes
##################################################

my %msgHandlers;
my %simpleActions;
my %detailedActions;
my @accels;

##################################################
# support for sub metadata attributes
##################################################

sub MODIFY_CODE_ATTRIBUTES {
	my ($package,$coderef,@attrs) = @_;
	my $fun = subname($coderef);

	if ( $fun =~ /::([^:]+)$/ ) {
		$fun = $1;
	}

	for my $attr ( @attrs ) {

		if( $attr =~ /^Action$/ ) {

			$simpleActions{$fun} = $coderef;
		}

		if( $attr =~ /^DetailedAction$/ ) {

			$detailedActions{$fun} = $coderef;
		}

		if( $attr =~ /^Accel\(([^\)]+)\)$/ ) {

			push @accels, {
				key     => $1,
				handler => $coderef,
			};
		}

		if( $attr =~ /^RPC$/ ) {

			$msgHandlers{$fun} = $coderef;
		}
	}

	return ();
}


##################################################
# main UI action handlers
##################################################

#-------------------------------------------------
# high level Gtk Action handlers
#-------------------------------------------------

# run command - continue until next breakpoint
sub onRun :Action {

	$rpc->continue();
    enableButtons(0);
}

# single step, recursing into functions
sub onStep :Action :Accel(<ctrl>Right) {

	$rpc->step();
    enableButtons(0);
}

# single step, jumping over functions
sub onOver :Action :Accel(<ctrl>Down) {

	$rpc->next();
    enableButtons(0);
}

# step out of current function, continue stepping afterwards
sub onOut :Action :Accel(<ctrl>Left) {

	$rpc->return();	
    enableButtons(0);
}

# stop button pressed - interrupt the debugger
sub onStop :Action {

    #	print "KILL $pid\n";
	if(!$ENV{"GDBG_KILL_CMD"}) {

	    kill 'INT', $pid;
	}
	else {
		my $cmd = $ENV{"GDBG_KILL_CMD"};

		# if $cmd contains the string '{{PID}}',
		# replace with current $pid
		$cmd =~ s/\{\{PID\}\}/$pid/;

		system("bash -c '$cmd'");
	}
}

# user entered return in eval entry
sub onEval :Action {

    my $e = $widgets->evalEntry->get_text();
	$fifo->rpc( "eval", $e );
}


# user selected open-file from menu
sub onOpen :Action {

    # show open file dialog
    my $dlg = Gtk3::FileChooserNative->new( 
		"Open File", 
		$widgets->mainWindow, 
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
sub onScroll :Action {

    scroll($currentFile,$currentLine);
}

# user selected 'Show Lexicals' from file menu
sub onLexicals :Action {

	$rpc->lexicals();
}

# show breakpoints window menu handler
sub onBreakpoints :Action {

	$rpc->breakpoints();
}

sub onStoreBreakpoints :Action {

	$rpc->storebreakpoints();
}

# show subroutines window menu handler
sub onSubs :Action {

	$rpc->functions();	
}

# reload the current, active file, if any
sub onReload :Action {

	my $widget = shift;
	my $event = shift;

	$rpc->fetch($currentFile,$currentLine);	
}

# show files view
sub onFiles :Action {

	my @files = keys %files;
	my @sorted = sort(@files);
	my $text = join("\n", @sorted);

	$widgets->filesBuffer->set_text($text,-1);

	$widgets->sourceView->set_buffer($widgets->filesBuffer);
	$widgets->mainWindow->set_title("Files loaded by the debugger:");
}

#-------------------------------------------------
# low level Gtk signal handlers
#-------------------------------------------------

# set a breakpoint UI handler (click on marker of sourceView)
sub onMarker {
    my ( $self, $iter, $event ) = @_;

	# only allow setting breakpoints when interactive
	if($uiDisabled) {
		return;
	}

	my $buf = $widgets->sourceView->get_buffer();

	# if we are not looking at a source file, no breaktpoints
	if( $buf == $widgets->subsBuffer ||
		$buf == $widgets->infoBuffer ||
		$buf == $widgets->breakpointsBuffer ||
		$buf == $widgets->filesBuffer) 
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

	$rpc->breakpoint($filename,$line);
}

# toggle the breakpoint on current line
sub onToggleBreakpoint :Accel(<ctrl>BackSpace) {

	if($uiDisabled) {
		return;
	}

	my $buf = $widgets->sourceView->get_buffer();

	# if we are not looking at a source file, no breaktpoints
	if( $buf == $widgets->subsBuffer ||
		$buf == $widgets->infoBuffer ||
		$buf == $widgets->breakpointsBuffer ||
		$buf == $widgets->filesBuffer) 
	{
		return;
	}

    # cannot set break points in eval code
    if ( $currentFile =~ /^\(eval/ ) {
        return;
    }

	my $mark = $widgets->sourceView->get_buffer()->get_insert();
	my $iter = $widgets->sourceView->get_buffer()->get_iter_at_mark($mark);
	my $line = $iter->get_line()+1;

	# get line of text, skip over some obviously non-breakable lines
	my $text = getLine($iter,$sourceBuffers{$currentFile},$line);
	if( !$text || $text eq "" || 
	    $text =~ /^\s*((use)|(no)|(require)|(package)|#)/ ||
		$text =~ /^\s+$/ ) 
	{
		return;
	}

	$rpc->breakpoint($currentFile,$line);
}

# toggle running state (Stop/Run)
sub onToggleRunning :Accel(<ctrl>space) {

	if($uiDisabled) {

		onStop();
	}
	else {

		onRun();
	}
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

	my $buf = $widgets->sourceView->get_buffer();

	if($buf == $widgets->subsBuffer) {

		$rpc->functionbreak($text);
		return;
	}
	elsif($buf == $widgets->breakpointsBuffer) {

		if ( $text =~ /^#/) { return; }
		if ($text =~ /([^:]+):([0-9]+)/ ) {
			my $file = $1;
			my $line = $2;
			scroll($file,$line);
		}
		return;
	}
	elsif($buf == $widgets->filesBuffer) {

		openFile($text,1);
		return;
	}
}

# lazily load var inspection tree content
sub onRowExpanded {

	my $widget = shift;
	my $iter   = shift;

	my $model = $widgets->lexicalTreeView->get_model();
	my $gval = $model->get_value($iter,2); 
	$selectedVar = $gval;

	my $first = $model->iter_children($iter);
	$gval = $model->get_value($first,0); 
	if( $gval eq '' ) {

		$gval = $model->get_value($first,1); 
		if( $gval eq '' ) {

			$gval = $model->get_value($iter,2); 
			$model->remove($first);
			$rpc->jsonlexicals( $gval );
		}
	}
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
	
	$widgets->infoBuffer->set_style_scheme($scheme);;        
	$widgets->lexicalsBuffer->set_style_scheme($scheme);;    
	$widgets->subsBuffer->set_style_scheme($scheme);;		
	$widgets->filesBuffer->set_style_scheme($scheme);;	   
	$widgets->breakpointsBuffer->set_style_scheme($scheme);; 
}

# user selects source from Sources Menu
sub onWindow {
    my ($menuItem) = @_;
    my $label = $menuItem->get_label();

    $openFile = $label;

    $widgets->sourceView->set_buffer( $sourceBuffers{$openFile} );
    $widgets->mainWindow->set_title(basename($openFile));
	$widgets->statusBar->set_text(basename($openFile));
	$widgets->statusBar->set_tooltip_text($openFile);

	$widgets->sourcesCombo->set_active_id($openFile);
}

# user selects source from combo box
sub onFileChoose {

	my $file = $widgets->sourcesCombo->get_active_text();

    $openFile = $file;

    $widgets->sourceView->set_buffer( $sourceBuffers{$openFile} );
    $widgets->mainWindow->set_title(basename($openFile));
	$widgets->statusBar->set_text(basename($openFile));
	$widgets->statusBar->set_tooltip_text($openFile);

}

# user closes main window
sub onDestroy {

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

	$widgets->searchSettings->set_search_text($query);

	$searchCtx = Gtk::Source::SearchContext->new(
		$widgets->sourceView->get_buffer(),
		$widgets->searchSettings,
	);

	$searchCtx->set_highlight(1);

	my ($hasSelection,$startIter,$endIter) = $widgets->sourceView->get_buffer()->get_selection_bounds();
	my ($match,$matchStart, $matchEnd) ;

	my $direction = $isShift 
		? $searchDirection == 'forward' ? 'backward' : 'forward'   
		: $searchDirection;

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

		my $buf = $widgets->sourceView->get_buffer();
		$buf->select_range($matchStart,$matchEnd);

		$widgets->sourceView->scroll_to_iter( $matchStart, 0, 1, 0.5, 0.5 );		
	}

	return 1;
}

sub onCancelSearch {

	my $widget = shift;
	my $event = shift;

	my $img = $widget->get_property("image");

	if($event->button->{type} eq 'button-press') {

		# left click
		if($event->button->{button} == 1) {

			if($searchCtx) {

				$searchCtx->set_highlight(0);
			}

			$widgets->search->set_text("");
		}
		# right click
		if($event->button->{button} == 3) {

			my $dialog = $widgets->searchDialog;

			my $active = $searchDirection eq 'forward' ? 1 : 0;
			$widgets->searchForward->set_active($active);
			$widgets->searchBackward->set_active(!$active);

			$dialog->set_transient_for($widgets->mainWindow);
			$dialog->set_modal(1);
			$dialog->show_all;
		}
	}
}

sub onFocusSearch :Accel(<ctrl>f) {

	$widgets->search->grab_focus();
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
	$widgets->searchSettings->set_case_sensitive($value);
}

sub onSearchWordBoundaries {

	my $widget = shift;
	my $event = shift;

	my $value = $widget->get_active;
	$widgets->searchSettings->set_at_word_boundaries ($value);
}

sub onSearchRegexEnabled {

	my $widget = shift;
	my $event = shift;

	my $value = $widget->get_active;
	$widgets->searchSettings->set_regex_enabled ($value);
}


##################################################
# process rpc messages from debugger
##################################################

sub	quit :RPC {

	$quit = 2;
}

sub	cwd :RPC {
	
	my $cwd = shift;
	$widgets->statusBar->set_text($cwd);
	chdir $cwd;
}

sub	pid :RPC {
	
	$pid = shift;
}

sub	file :RPC {

	my $file = shift;
	my $line = shift;

	$currentLine = $line;
	$currentFile = $file;

	scroll($file,$line);
	enableButtons(1);		
}

sub show :RPC {
	# show file at line (like display above)
	# but do not set current file
	my $file = shift;
	my $line = shift;

	scroll($file,$line);
	enableButtons(1);		
}

sub	info :RPC {
	# display call stack info

	my $file = shift;
	my $line = shift;
	my $info = shift;

	$currentLine = $line;
	$currentFile = $file;

	$widgets->infoBuffer->set_text( $info, -1 );		
	$rpc->jsonlexicals("/");
}

sub	lexicals :RPC {
	# display lexicals

	my $file = shift;
	my $line = shift;
	my $info = shift;

	$widgets->lexicalsBuffer->set_text( $info, -1 );
	$widgets->sourceView->set_buffer($widgets->lexicalsBuffer);
	$widgets->mainWindow->set_title("All current lexical variables:");
}

sub	jsonlexicals :RPC {

	my $file = shift;
	my $target = shift;
	my $info = decode_json(shift);

	my $model = $widgets->lexicalTreeView->get_model;
	if($target eq '/') {
		$model->clear;
	}

	my $root = undef;
	my $result = find_root($target,$root);
	if($result->{found}) {
		my $iter = $result->{result};
		my $path = '';
		if($iter) {
			$path = $model->get_value($iter,2);
		}
		populate_lexicals($info,$model,$iter,$path);
	}
	else {
		populate_lexicals($info,$model,$root);
	}
}

sub	breakpoints :RPC {
	# display breakpoints
	my $info = shift;

	$widgets->breakpointsBuffer->set_text( $info, -1 );
	$widgets->sourceView->set_buffer($widgets->breakpointsBuffer);
	$widgets->mainWindow->set_title("All breakpoints currently set:");
}

sub	setbreakpoints :RPC {
	# set breakpoints for file

	my $file  = shift;
	my $lines = shift;
	my @lines = split ',' , $lines;

print STDERR "BR: $file ".Dumper($lines);	

	my $buf = $sourceBuffers{$file};
	if(!$buf) {
		return;
	}

	my $start = $buf->get_start_iter;
	my $end   = $buf->get_end_iter;

	$buf->remove_source_marks($start,$end,"error");

	foreach my $line ( @lines ) {

		my $bpn = $file . ":" . $line;
		my $iter = $buf->get_iter_at_line( $line - 1 );
		$sourceBuffers{$file}->create_source_mark( $bpn, "error", $iter );
	}
}

sub	load :RPC {
	# load file,line,source

	my $file = shift;
	my $line = shift;
	my $src  = shift;
	$widgets->statusBar->set_text("$file");

	$files{$file} = $src;
	# discuss whether this if is a good idea?
	if ( $file !~ /^\/usr\// ) {
		scroll($file,$line);
	}
}

sub	eval :RPC {
	# eval results passed as string

	my $evaled = shift // '<undef>';
	$widgets->lexicalsBuffer->set_text( $evaled, -1 );
	$widgets->sourceView->set_buffer($widgets->lexicalsBuffer);
}

sub	subs :RPC {
	# all known subroutines for display

	my $subs = shift;
	$widgets->subsBuffer->set_text( $subs, -1 );
	$widgets->sourceView->set_buffer($widgets->subsBuffer);
	$widgets->mainWindow->set_title("All subroutines loaded:");
}

#-------------------------------------------------
# process a single RPC message by dispatching
# it to the appropriate :RPC handler sub
#-------------------------------------------------

sub process_msg {

    my $msg = shift;

	my $cmd = $msg->{cmd};
	my $params = $msg->{params} // [];

	if ( $cmd && exists $msgHandlers{$cmd}) {
		$msgHandlers{$cmd}->( $params->@* );
	}
	else {
		print STDERR "unknown $cmd. ".Dumper($msg);
	}
}


##################################################
# SIGINT UNIX signal handler
##################################################

# deprecated (unused)
sub dbint_handler {

    $quit = 1;
}

##################################################
# UI helpers
##################################################

# open a new file in the visual debugger
sub openFile {
    my $filename = shift;
    my $line     = shift;

    $widgets->mainWindow->set_title( basename($filename) . ":" . $line );	
	$widgets->statusBar->set_text(basename($filename).":".$line);
	$widgets->statusBar->set_tooltip_text($filename);

    while ( $ctx->pending() ) {
        $ctx->iteration(0);
    }

    $openFile = $filename;

	# if we already have a source buffer for this file
    if ( $sourceBuffers{$filename} ) {

        # file already exists!
        $widgets->sourceView->set_buffer( $sourceBuffers{$filename} );

		# if we have the source
		if($files{$filename}) {

			# has it changed from display?
			my $txt = getSource($filename);
			if(  $txt ne $files{$filename} ) {

				$sourceBuffers{$filename}->set_text( $files{$filename}, -1 );
			}
		}
		$widgets->sourcesCombo->set_active_id($filename);
        return;
    }

	# create new source buffer
	my $buf = loadBuffer($filename,$line);

    while ( $ctx->pending() ) {
        $ctx->iteration(0);
    }

    # set the new buffer as current buffer for display
    $widgets->sourceView->set_buffer($buf);
	$widgets->sourcesCombo->set_active_id($filename);

    while ( $ctx->pending() ) {
        $ctx->iteration(0);
    }	

	$rpc->getbreakpoints($filename);
}

# load a file into a new source buffer
sub loadBuffer {

	my $file = shift;
	my $line = shift || 1;

	# get the content
	my $content = $files{$file};
	if( !$content ) {

		# temp dummy content
		$content = '<unknown>'; # 
		# ask debugger to provide file
		$rpc->fetch($file,$line);
		$files{$file} = $content;
	}
	
	# create GTK source buffer with content
	my $buf = Gtk::Source::Buffer->new();
	$buf->set_language($widgets->lang);
	$buf->set_style_scheme($scheme);
	if ($content) {
		$buf->set_text( $content, -1 );
	}
	$sourceBuffers{$file} = $buf;

	# add a menu item to the windows menu
	my $item = Gtk3::MenuItem->new_with_label($file);
	$item->signal_connect( 'activate' => \&onWindow );

	$widgets->windowMenu->add($item);
	$widgets->windowMenu->show_all();

	# also add to combo box
	$widgets->sourcesCombo->append($file,$file);
	$widgets->sourcesCombo->set_active_id($file);

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
	my ($r2,$iter) = $widget->get_iter_at_position($x,$y);
	$iter->forward_line();

	# get the line from the iter, (which is line+1)
	my $line = $iter->get_line();

	# adjust the iter again, now have correct line
	$iter->backward_line();

	# the line source text
	my $text = getLine($iter,$widget->get_buffer(),$line);

	return $text;
}

# enable / disable buttons
sub enableButtons {

    my $state = shift;

	$uiDisabled = $state ? 0 : 1;

    $widgets->buttonRun->set_sensitive($state);
    $widgets->buttonStep->set_sensitive($state);
    $widgets->buttonOver->set_sensitive($state);
    $widgets->buttonOut->set_sensitive($state);
    $widgets->buttonLexicals->set_sensitive($state);
    $widgets->buttonHome->set_sensitive($state);
    $widgets->evalEntry->set_sensitive($state);
    $widgets->lexicalsMenu->set_sensitive($state);
    $widgets->breakpointsMenu->set_sensitive($state);
    $widgets->openFileMenu->set_sensitive($state);
    $widgets->showSubsMenu->set_sensitive($state);
    $widgets->showFilesMenu->set_sensitive($state);
    $widgets->lexicalTreeView->set_sensitive($state);

    $widgets->buttonStop->set_sensitive( $state ? 0 : 1 );
}

# update the info with current call frame stack
sub updateInfo {

    my ( $filename, $line, $info ) = @_;

    $widgets->mainWindow->set_title( $filename . ":" . $line );

    $widgets->infoBuffer->set_text( $info, -1 );
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
    $widgets->sourceView->scroll_to_iter( $iter, 0, 1, 0, 0.5 );
    while ( $ctx->pending() ) {
        $ctx->iteration(0);
    }

    $sourceBuffers{$file}->place_cursor($iter);

    while ( $ctx->pending() ) {
        $ctx->iteration(0);
    }
}

##################################################
# variable inspector support
##################################################

# find a node '$target' in the var inspection tree
# returning a Gtk TreeWidget iterator

sub find_root {
	my ($target,$root) = @_;

	if($target eq '' || $target eq '/') {
		return {
			result => $root,
			found => 1,
		};
	}

	my $model = $widgets->lexicalTreeView->get_model;

	my $iter = $model->iter_children($root);
	while( $iter) {

		my $gv;
		eval {
			$gv = $model->get_value($iter,0);
			$gv = $model->get_value($iter,2);
		};
		if($@) {

			return {
				found => 0,
				result => undef,
			};
		}

		$gv .= '';

		my $i = index $target, $gv;

		if($i == 0) {

			if($target eq $gv) {
				return {
					result => $iter,
					found => 1,
				};
			}
			else {
				return find_root($target,$iter);
			}
		}
		$model->iter_next($iter);
	}
	return {
		found => 0,
		result => undef,
	};
}

# simple helper to produce a GLib GValue
sub GValue {

	my $value = shift;
	my $type  = shift || "Glib::String";

	# https://metacpan.org/pod/Glib
	# G_TYPE_STRING     Glib::String
	# G_TYPE_INT        Glib::Int
	# G_TYPE_UINT       Glib::UInt
	# G_TYPE_DOUBLE     Glib::Double
	# G_TYPE_BOOLEAN    Glib::Boolean

	# create a GValue
	my $gv = Glib::Object::Introspection::GValueWrapper->new ( $type, $value);
	return $gv;
}

# populate an Gtk TreeView node

sub populate_item {

	my ($model,$iter,$key,$item,$path) = @_;

	$model->set_value($iter,0,GValue($key));
	$model->set_value($iter,2,GValue($path));

	if($item->{placeholder}) {

		$model->set_value($iter,1,GValue($item->{type}));

		my $it = $model->append($iter);
		my $gv = GValue('');
		$model->set_value($it,0,$gv);
		$model->set_value($it,1,$gv);
		$model->set_value($it,2,$gv);
		return;
	}

	if( $item->{type} eq 'REF' || $item->{type} eq 'SCALAR') {
		my $gv = GValue($item->{value});
		$model->set_value($iter,1,$gv);
	}
	elsif( $item->{type} eq 'CODE' || $item->{type} eq 'IO' || $item->{type} eq 'GLOB'  ) {
		my $gv = GValue($item->{type});
		$model->set_value($iter,1,$gv);
	}
	else {
		my $gv = GValue($item->{type});
		$model->set_value($iter,1,$gv);
		if( defined $item->{value} ) {
			populate_lexicals($item,$model,$iter,$path);
		}				

		if( index( $selectedVar, $path) == 0 ) {
			if($iter) {
				my $p = $model->get_path($iter);
				if($p) {
					$widgets->lexicalTreeView->expand_to_path($p);
					$widgets->lexicalTreeView->scroll_to_cell($p,undef,0,0,0);					
				}
			}
		}
	}
}

# populate the tree view (or parts of it)

sub populate_lexicals {

	my ($data,$model,$root,$path) = @_;

	if(!$path) {
		$path = '';
	}

	my $type = $data->{type};
	my $value = $data->{value};
	my $placeholder = $data->{placeholder};

	if($type ne 'ARRAY' && $value ne '<unk>' && $value) {

		if(!_HASHLIKE($value)) {
			$value = {};
		}
		foreach my $key ( sort keys $value->%* ) {

			my $item = $value->{$key};

			my $iter = $model->append($root);

			populate_item($model,$iter,$key,$item,"$path/$key");			
		}
	}
	else {

		my $i = 0;
		foreach my $item ( $value->@* ) {

			my $iter = $model->append($root);
			populate_item($model,$iter,"",$item,"$path/$i");			
			$i++;
		}
	}

	if( $path && index( $selectedVar, $path) == 0 ) {
		my $iter = $model->iter_children($root);
		if($iter) {
			my $p = $model->get_path($iter);
			if($p) {
				$widgets->lexicalTreeView->expand_to_path($p);
				$widgets->lexicalTreeView->scroll_to_cell($p,undef,0,0,0);	
			}
		}
	}

}

##################################################
# build the ui once using gtk builder
##################################################

# make $widgets available to UI controller
sub mapWidgets {

    my $builder = shift;

	my %widgets;
	my $list = $builder->get_objects();
	for my $widget ( @$list ) {

		my $id = Gtk3::Buildable::get_name($widget);
		$widgets{$id} = $widget;
	}

	$widgets = Widgets->new( \%widgets );
}

# connect signals specified in XML
sub connect_signals {

    my ( $builder, $obj, $signal, $handler, $co, $flags, $data ) = @_;

    $obj->signal_connect( $signal => \&$handler );
}

# prepare action specified with :Action and :DetailedAction 
# sub attributes
sub add_actions {

  	my $mainWindow = shift;

	for my $key ( keys %simpleActions ) {

		my $handler = $simpleActions{$key};

		my $action = Glib::IO::SimpleAction->new($key);
		$action->signal_connect("activate", $handler);

		$mainWindow->add_action($action);    
	}

	for my $key ( keys %detailedActions ) {

		my $handler = $detailedActions{$key};

		my $gvt = Glib::VariantType->new("s");
		my $gv  = Glib::Variant->new_string("");

		my $action = Glib::IO::SimpleAction->new_stateful($key,$gvt,$gv);
		$action->signal_connect("activate", $handler);

		$mainWindow->add_action($action);    
	}
}

# add accelerators specifiec with :Accel attributes
sub add_accels {

	$widgets->add( accel => Gtk3::AccelGroup->new() );
	$widgets->mainWindow->add_accel_group($widgets->accel);

	for my $accel ( @accels ) {

		my $key     = $accel->{key};
		my $handler = $accel->{handler};

		my ($key,$mod) = Gtk3::accelerator_parse($key);
		$widgets->accel->connect( $key, $mod, [], $handler );
	}
}

# Gtk Builder - load UI from XML
sub build_ui {

	# load widgets from xml
    my $builder = Gtk3::Builder->new();
    $builder->add_from_file($uixml) or die 'file not found';

	# get references to widgets
    mapWidgets($builder);

	# connect UI signal handlers
    $builder->connect_signals_full( \&connect_signals, 0 );

	# keyboard shortcut accelerators
	add_accels();

	# attach actions
	add_actions( $widgets->mainWindow );

	# Perl syntax highlighting support
    my $langManager = Gtk::Source::LanguageManager->new();
    my $lang        = $langManager->get_language("perl");

	$widgets->add( langManager => $langManager );
	$widgets->add( lang        => $lang );

	# prepare support for breakpoint markers
    my $attrs = Gtk::Source::MarkAttributes->new();
    $attrs->set_icon_name("media-record");

	# setup main sourceView attributes
    $widgets->sourceView->set_show_line_marks(1);
    $widgets->sourceView->set_editable(0);
    $widgets->sourceView->set_wrap_mode('none');
    $widgets->sourceView->set_mark_attributes( "error", $attrs, 10 );

    # theme support for buffers
    my $manager = Gtk::Source::StyleSchemeManager::get_default();

	# populate schemes menu
    my $themes = $manager->get_property("scheme-ids");
    foreach my $theme (@$themes) {
        my $item = Gtk3::MenuItem->new_with_label($theme);
        $item->signal_connect( 'activate' => \&onTheme );
        $widgets->themesMenu->add($item);
    }

	# default schema
    $scheme = $manager->get_scheme("solarized-dark");

	# prepare the info buffer to display call stack
    my $infoBuffer = $widgets->infoPane->get_buffer();
    $widgets->infoPane->set_editable(0);
    $infoBuffer->set_style_scheme($scheme);
	$widgets->add( infoBuffer => $infoBuffer );

	# prepare the lexical vars display buffer
    my $lexicalsBuffer = Gtk::Source::Buffer->new();
    $lexicalsBuffer->set_language($lang);
    $lexicalsBuffer->set_style_scheme($scheme);
	$widgets->add( lexicalsBuffer => $lexicalsBuffer );

	# buffer to show loaded subroutines
    my $subsBuffer = Gtk::Source::Buffer->new();
    $subsBuffer->set_style_scheme($scheme);
	$widgets->add( subsBuffer => $subsBuffer);

	# buffer to show loaded files
    my $filesBuffer = Gtk::Source::Buffer->new();
    $filesBuffer->set_style_scheme($scheme);
	$widgets->add( filesBuffer => $filesBuffer );

	# buffer to show loaded breakpoints
    my $breakpointsBuffer = Gtk::Source::Buffer->new();
    $breakpointsBuffer->set_style_scheme($scheme);
	$widgets->add( breakpointsBuffer => $breakpointsBuffer );

	# prepare the var inspection tree view
	my $treeModel = $widgets->LexicalTreeStore();
	$widgets->lexicalTreeView->set_model($treeModel);

	my $renderer1 = Gtk3::CellRendererText->new ();
	my $renderer2 = Gtk3::CellRendererText->new ();

	# enable if you want to see the hidden 'path' data
	# value of the the tree view in the third column

	# my $renderer3 = Gtk3::CellRendererText->new ();

	my $column1 = Gtk3::TreeViewColumn->new();
	$column1->set_resizable(1);
	$column1->set_sizing('fixed');
	$column1->set_fixed_width(100);
	$column1->set_title("Var");
  	$column1->pack_start($renderer1, 0);	
	$column1->add_attribute($renderer1,"text",0);
	$widgets->lexicalTreeView->append_column( $column1);

	my $column2 = Gtk3::TreeViewColumn->new();
	$column2->set_resizable(1);
	$column2->set_sizing('fixed');
	$column2->set_fixed_width(100);
	$column2->set_title("Info");
  	$column2->pack_start($renderer2, 0);	
	$column2->add_attribute($renderer2,"text",1);
	$widgets->lexicalTreeView->append_column( $column2 );

	# enable if you want to see the hidden 'path' data
	# value of the the tree view in the third column

	# my $column3 = Gtk3::TreeViewColumn->new();
	# $column3->set_resizable(1);
	# $column3->set_sizing('fixed');
	# $column3->set_fixed_width(100);
	# $column3->set_title("Path");
  	# $column3->pack_start($renderer3, 0);	
	# $column3->add_attribute($renderer3,"text",2);
	# $widgets{lexicalTreeView}->append_column( $column3 );

	$widgets->lexicalTreeView->signal_connect( 'row-expanded' => \&onRowExpanded );
	$widgets->lexicalTreeView->set_enable_tree_lines(1);

	# prepare search support
	my $searchSettings = Gtk::Source::SearchSettings->new();
	$searchSettings->set_wrap_around(1);
	$searchSettings->set_regex_enabled(1);
	$searchSettings->set_case_sensitive(0);
	$widgets->add( searchSettings => $searchSettings );
	
	# set everything to disabled on startup
    enableButtons(0);

	# enable button stop, just to be sure
    $widgets->buttonStop->set_sensitive(1);

    # show the UI
    $widgets->mainWindow->show_all();
}

##################################################
# initialize
##################################################

sub initialize {

    Gtk3::init();

    $ctx = GLib::MainContext::default();

	#deprecated:  
	#$SIG{'INT'} = "dbgui::dbint_handler";

    # the IPC named pipe (fifo) for comms with the debugger

	my $fifo_dir = $ENV{"GDBG_FIFO_DIR"} || '/tmp/';

    $fifo = Devel::dipc->new();
    $fifo->open_in( "$fifo_dir/perl_debugger_fifo_out");
    $fifo->open_out("$fifo_dir/perl_debugger_fifo_in");

	# high level RPC wrapper on top of $fifo
	$rpc = Devel::dipc::RPC->new($fifo);

	if ( $ENV{"GDBG_NO_FORK"} ) {

		# possibly re-attaching the debugger UI to
		# the process being debugged. 
		onStop();		
		$rpc->next();
	}

	# show the UI
	build_ui();
}

##################################################
# THE ui MAIN loop
##################################################

# initialize the app
initialize();

# run the main event loop, handling both
# Gtk and IPC fifo events
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

if ( $quit == 1 ) 
{
	if(!$ENV{"GDBG_KILL_CMD"}) {
    	kill 'INT', $pid;
	}
	$rpc->quit();
}

