
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
local *STDERR;
open( STDERR, '>', '/dev/null' ) or die $!;

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
my %files;    # files as seen by debugger, mapped to abs_path($file)
my $uixml = $RealBin . "/gdbg.ui";    # path to glade xml ui definition
my $pid   = 0;    # process ID of debugger process, to be signalled
my %breakpoints;    # remember breakpoint markers so we can delete 'em
my $fifo;           # IPC with debugger backend

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

##################################################
# UI action handlers
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
    kill 'INT', $pid;
}

# user entered return in eval entry
sub onEval {

    my $e = $widgets{evalEntry}->get_text();

    $fifo->write("eval $e");
}

# user selected open-file from menu
sub onOpen {

    # show open file dialog
    my $dlg =
      Gtk3::FileChooserNative->new( "Open File", $widgets{mainWindow}, 'open',
        "OK", "Cancel" );

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

# user selected theme from Themes menu
sub onTheme {
    my ($menuItem) = @_;
    my $label = $menuItem->get_label();

    my $manager = Gtk::Source::StyleSchemeManager::get_default();
    $scheme = $manager->get_scheme($label);

    foreach my $key ( keys %sourceBuffers ) {

        $sourceBuffers{$key}->set_style_scheme($scheme);
    }
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

# set a breakpoint UI handler (click on marker of sourceView)
sub onMarker {
    my ( $self, $iter, $event ) = @_;
    my $line = $iter->get_line() + 1;
    my $filename = $openFile;

	if( $widgets{sourceView}->get_buffer() eq $subsBuffer ||
		$widgets{sourceView}->get_buffer() eq $infoBuffer ||
		$widgets{sourceView}->get_buffer() eq $breakpointsBuffer ||
		$widgets{sourceView}->get_buffer() eq $filesBuffer) 
	{
		return;
	}

    # cannot set break points in eval code
    if ( $filename =~ /^\(eval/ ) {
        return;
    }

	my $text = getLine($iter,$sourceBuffers{$filename},$line);
	if( !$text || $text eq "" || 
	    $text =~ /^\s*((use)|(no)|(require)|(package)|#)/ ||
		$text =~ /^\s+$/ ) 
	{
		return;
	}

#    my $iter = $sourceBuffers{$filename}->get_iter_at_line( $line - 1 ); #unused!

    my $bpn = $filename . ":" . $line;
    if ( $breakpoints{$bpn} ) {    # breakpoint already exists, remove it
        $sourceBuffers{$filename}->delete_mark( $breakpoints{$bpn} );
        delete $breakpoints{$bpn};
        $fifo->write("delbreakpoint $filename,$line");
    }
    else {
        $fifo->write("breakpoint $filename,$line");
    }
}

# mouse click on a line, if on breakpoints, files or subroutines view
sub onClick {
	my $widget = shift;
	my $event = shift;

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

	if($widgets{sourceView}->get_buffer() eq $subsBuffer) {

		$fifo->write("functionbreak $text");
		return;
	}
	elsif($widgets{sourceView}->get_buffer() eq $breakpointsBuffer) {

		if(!$text) { return; }
		if ( $text =~ /^#/) { return; }
		if ($text =~ /([^:]+):([0-9]+)/ ) {
			my $file = $1;
			my $line = $2;
			scroll($file,$line);
		}
		return;
	}
	elsif($widgets{sourceView}->get_buffer() eq $filesBuffer) {
		if(!$text) { return; }
		if ( -e $text ) { 
			openFile($text,1);
		}
		return;
	}
}

sub onFiles {

	my @files = keys %files;
	my @sorted = sort(@files);
	my $text = join("\n", @sorted);
	$filesBuffer->set_text($text,-1);
	$widgets{sourceView}->set_buffer($filesBuffer);

	$widgets{mainWindow}->set_title("Files loaded by the perl interpreter:");
}

# user closes main window
sub onDestroy {

    kill 'INT', $pid;
    $quit = 1;
}

# INT signal handler
sub dbint_handler {

    $quit = 1;
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
		regex   => qr/^cwd$/s,
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
		}
	},
	{
		# display file at line
		regex   => qr/^file ([^,]+),([0-9]*)/s,
		handler => sub {

			my $file = $1;
			my $line = $2;

			#print "File $file:$line\n";

			$currentLine = $line;
			$currentFile = $file;

			scroll($file,$line);
			enableButtons(1);		
		}
	},
	{
		# show file at line (like ddisplay above)
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
			$widgets{mainWindow}->set_title("All current lexical (my) variables:");
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

				my $content = $files{$file};
				if( !$content ) {

					$content = slurp($file, binmoder => 'utf8' );
					$files{$file} = $content;
				}
				
				$buf = Gtk::Source::Buffer->new();
				$buf->set_language($lang);
				$buf->set_style_scheme($scheme);
				if ($content) {
					$buf->set_text( $content, -1 );
				}
				$sourceBuffers{$file} = $buf;
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

			my $buf = $sourceBuffers{$file};
			if ($buf) {
				my $iter = $buf->get_iter_at_line( $line - 1 );
				if ( $breakpoints{$bpn} ) {    # only if already exists

					$sourceBuffers{$file}->delete_mark($breakpoints{$bpn});
					delete $breakpoints{$bpn};
				}
			}
		}
	},
	{
		# load file,source
		regex   => qr/^load ([^,]+),(.*)/s,
		handler => sub {

			my $file = $1;
			my $src  = $2;
			$widgets{statusBar}->set_text("load $file");

			$files{$file} = $src;
			if ( $file !~ /^\/usr\// ) {
				openFile($file);
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

    #	print "MSG: $msg\n";

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

    $openFile = $filename;
    if ( $sourceBuffers{$filename} ) {

        # file already exists!
        $widgets{sourceView}->set_buffer( $sourceBuffers{$filename} );
        return;
    }

    # load file content
    my $content = $files{$filename};
    if ( !$content && $filename !~ /^\(eval / ) {
        $content = slurp( $filename, binmoder => 'utf8' );
		$files{$filename} = $content;
    }

    # prepare source buffer for display
    my $buffer = Gtk::Source::Buffer->new();
    $buffer->set_language($lang);
    $buffer->set_style_scheme($scheme);
    if ($content) {
        $buffer->set_text( $content, -1 );
    }
    $sourceBuffers{$filename} = $buffer;

    # set the new buffer as current buffer for display
    $widgets{sourceView}->set_buffer($buffer);

    # add a menu item to the windows menu
    my $item = Gtk3::MenuItem->new_with_label($filename);
    $item->signal_connect( 'activate' => \&onWindow );
    $widgets{windowMenu}->add($item);
    $widgets{windowMenu}->show_all();
}


##################################################
# UI helpers
##################################################

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

sub enableButtons {

    my $state = shift;
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

sub scroll {
	my $file = shift;
    my $line = shift;

    openFile( $file, $line );

    while ( $ctx->pending() ) {
        $ctx->iteration(0);
    }

    # scroll to currently debugged line
    my $iter = $sourceBuffers{$file}->get_iter_at_line( $line - 1 );
    $sourceBuffers{$file}->place_cursor($iter);
    $widgets{sourceView}->scroll_to_iter( $iter, 0, 1, 0, 0.5 );

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
      buttonHome
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

    my $builder = Gtk3::Builder->new();
    $builder->add_from_file($uixml) or die 'file not found';

    mapWidgets($builder);

    $builder->connect_signals_full( \&connect_signals, 0 );

    $langManager = Gtk::Source::LanguageManager->new();
    $lang        = $langManager->get_language("perl");

    $widgets{sourceView}->set_show_line_marks(1);
    $widgets{sourceView}->set_editable(0);
    $widgets{sourceView}->set_wrap_mode('none');

    my $attrs = Gtk::Source::MarkAttributes->new();
    $attrs->set_icon_name("media-record");
    $widgets{sourceView}->set_mark_attributes( "error", $attrs, 10 );

    # theme support
    my $manager = Gtk::Source::StyleSchemeManager::get_default();

    my $themes = $manager->get_property("scheme-ids");
    foreach my $theme (@$themes) {
        my $item = Gtk3::MenuItem->new_with_label($theme);
        $item->signal_connect( 'activate' => \&onTheme );
        $widgets{themesMenu}->add($item);
    }
    $scheme = $manager->get_scheme("solarized-dark");

    $infoBuffer = $widgets{infoPane}->get_buffer();
    $widgets{infoPane}->set_editable(0);
    $infoBuffer->set_style_scheme($scheme);

    $lexicalsBuffer = Gtk::Source::Buffer->new();
    $lexicalsBuffer->set_language($lang);
    $lexicalsBuffer->set_style_scheme($scheme);

    $subsBuffer = Gtk::Source::Buffer->new();
    $subsBuffer->set_style_scheme($scheme);

    $filesBuffer = Gtk::Source::Buffer->new();
    $filesBuffer->set_style_scheme($scheme);

    $breakpointsBuffer = Gtk::Source::Buffer->new();
    $breakpointsBuffer->set_style_scheme($scheme);

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

    # this will hang until the Debugger has connected
    $fifo->open_in("/tmp/perl_debugger_finfo_out");
    $fifo->open_out("/tmp/perl_debugger_finfo_in");

#    $fifo->write("s");
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

# print "QUIT $quit\n";

if ( $quit == 1 ) {
    $fifo->write("quit");
}

