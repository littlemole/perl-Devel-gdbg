
##################################################
package gdbgui;
##################################################

use v5.20;
use utf8;
use strict;

##################################################
# Perl dependencies
##################################################

# IPC package share with debugger
use Devel::gdbg::dipc;

##################################################
# supress some GLib warnings
##################################################

#local *STDERR;
#open( STDERR, '>', '/dev/null' ) or die $!;


##################################################
# MVC - Debugger UI Application Model
##################################################

package model;
use parent 'Devel::gdbg::model';
use strict;

sub new {

	my $class = shift;
	my $view  = shift;

	my $self = {
		quit        => 0,                  # stop the UI
		openFile    => "",                 # the currently shown file
		currentFile => "",                 # the current file of debugging
		currentLine => 0,                  # the current line under debug
		pid         => 0,    			   # process ID of debugger process, to be signalled
		uiDisabled  => 1,				   # flag for UI disabled/enabled
		files       => {},                 # files loaded, mapped to source
		fifo        => undef,              # IPC with debugger backend
		rpc         => undef,              # RPC abstraction on top of $fifo
		selectedVar => "/",                # last expanded item in var inspection tree
		searchDirection => 'forward',      # search direction for text search

		view        => $view,
	};

	bless $self, $class;

	# prepare IPC over FIRO with debugger process
	my $fifo_dir = $ENV{"GDBG_FIFO_DIR"} || '/tmp/';

    my $fifo = Devel::gdbg::dipc->new();
    $fifo->open_in( "$fifo_dir/perl_debugger_fifo_out");
    $fifo->open_out("$fifo_dir/perl_debugger_fifo_in");

	$self->{fifo} = $fifo;

	# high level RPC wrapper on top of $fifo
	$self->{rpc} = Devel::gdbg::dipc::RPC->new($fifo);

	return $self;
}

#----------------------------------
# RPC event handler send by
# debugger process
#----------------------------------

sub	quit :RPC {

	my $self = shift;

	$self->{quit} = 2;
}

sub	cwd :RPC {
	
	my $self = shift;
	my $cwd  = shift;

	$self->view->statusBar->set_text($cwd);
	chdir $cwd;
}

sub	pid :RPC {
	
	my $self = shift;

	$self->{pid} = shift;
}

sub	file :RPC {

	my $self = shift;
	my $file = shift;
	my $line = shift;

	$self->{currentLine} = $line;
	$self->{currentFile} = $file;

	$self->scroll($file,$line);
	$self->enableButtons(1);		
}

sub show :RPC {
	# show file at line (like file above)
	# but do not set current file
	my $self = shift;
	my $file = shift;
	my $line = shift;

	$self->scroll($file,$line);
	$self->enableButtons(1);		
}

sub	info :RPC {
	# display call stack info

	my $self = shift;
	my $file = shift;
	my $line = shift;
	my $info = shift;

	$self->{currentLine} = $line;
	$self->{currentFile} = $file;

	$self->view->infoBuffer->set_text( $info, -1 );		
	$self->rpc->jsonlexicals("/");
}

sub	lexicals :RPC {
	# display lexicals

	my $self = shift;
	my $file = shift;
	my $line = shift;
	my $info = shift;

	my $view = $self->view;

	$view->lexicalsBuffer->set_text( $info, -1 );
	$view->sourceView->set_buffer($view->lexicalsBuffer);
	$view->status("All current lexical variables:");
}

sub	jsonlexicals :RPC {

	my $self   = shift;
	my $file   = shift;
	my $target = shift;
	my $info   = shift;

	my $view = $self->view;

	my $treemodel = $view->lexicalTreeView->get_model;
	if($target eq '/') {
		$treemodel->clear;
	}

	my $root = undef;
	my $result = $view->find_root($target,$root);
	if($result->{found}) {
		my $iter = $result->{result};
		my $path = '';
		if($iter) {
			$path = $treemodel->get_value($iter,2);
		}
		$view->populate_lexicals($info,$treemodel,$self->selectedVar,$iter,$path);
	}
	else {
		$view->populate_lexicals($info,$treemodel,$self->selectedVar,$root);
	}
}

sub	breakpoints :RPC {
	# display breakpoints
	my $self = shift;
	my $info = shift;

	my $view = $self->view;

	$view->breakpointsBuffer->set_text( $info, -1 );
	$view->sourceView->set_buffer($view->breakpointsBuffer);
	$view->status("All breakpoints currently set:");
}

sub	setbreakpoints :RPC {
	# set breakpoints for file

	my $self  = shift;
	my $file  = shift;
	my $lines = shift;

	my $view = $self->view;

	my @lines = split ',' , $lines;

	my $buf = $view->sourceBuffers->{$file};
	if(!$buf) {
		return;
	}

	my $start = $buf->get_start_iter;
	my $end   = $buf->get_end_iter;

	$buf->remove_source_marks($start,$end,"error");

	foreach my $line ( @lines ) {

		my $bpn = $file . ":" . $line;
		my $iter = $buf->get_iter_at_line( $line - 1 );
		$buf->create_source_mark( $bpn, "error", $iter );
	}
}

sub	load :RPC {
	# load file,line,source

	my $self = shift;
	my $file = shift;
	my $line = shift;
	my $src  = shift;

	$self->view->statusBar->set_text("$file");

	$self->{files}->{$file} = $src;
	$self->scroll($file,$line);
}

sub	eval :RPC {
	# eval results passed as string

	my $self = shift;
	my $evaled = shift // '<undef>';

	my $view = $self->view;

	$view->lexicalsBuffer->set_text( $evaled, -1 );
	$view->sourceView->set_buffer($view->lexicalsBuffer);
}

sub	subs :RPC {
	# all known subroutines for display

	my $self = shift;
	my $subs = shift;

	my $view = $self->view;
	
	$view->subsBuffer->set_text( $subs, -1 );
	$view->sourceView->set_buffer($view->subsBuffer);
	$view->status("All subroutines loaded:");
}

#----------------------------------
# Debugger UI logic
#----------------------------------

# open a new file in the visual debugger
sub openFile {

	my $self     = shift;
    my $filename = shift;
    my $line     = shift;

	my $view = $self->view;

    $view->status( $filename, $line );	

	$view->pump_msgs();

    $self->{openFile} = $filename;

	# if we already have a source buffer for this file
    if ( $view->sourceBuffers->{$filename} ) {

        # file already exists!
        $view->sourceView->set_buffer( 
			$view->sourceBuffers->{$filename} 
		);

		# if we have the source
		if($self->files->{$filename}) {

			# has it changed from display?
			my $txt = $view->getSource($filename);
			if(  $txt ne $self->files->{$filename} ) {

				$view->sourceBuffers->{$filename}->set_text( 
					$self->files->{$filename}, -1 
				);
			}
		}
		$view->sourcesCombo->set_active_id($filename);
        return;
    }

	# create new source buffer
	my $buf = $self->loadBuffer($filename,$line);

	$view->pump_msgs();

    # set the new buffer as current buffer for display
    $view->sourceView->set_buffer($buf);
	$view->sourcesCombo->set_active_id($filename);

	$view->pump_msgs();

	$self->rpc->getbreakpoints($filename);
}

# load a file into a new source buffer
sub loadBuffer {

	my $self = shift;
	my $file = shift;
	my $line = shift || 1;

	my $view = $self->view;

	# get the content
	my $content = $self->files->{$file};
	if( !$content ) {

		# temp dummy content
		$content = '<unknown>'; # 
		# ask debugger to provide file
		$self->rpc->fetch($file,$line);
		$self->{files}->{$file} = $content;
	}
	
	# create GTK source buffer with content
	my $buf = Gtk::Source::Buffer->new();
	$buf->set_language($self->view->lang);
	$buf->set_style_scheme($self->view->scheme);
	if ($content) {
		$buf->set_text( $content, -1 );
	}
	$view->sourceBuffers->{$file} = $buf;

	# add file to combo box
	$view->sourcesCombo->remove_all();
	my @files = sort keys $self->{files}->%*; 
	for my $f( @files ) {
		$view->sourcesCombo->append($f,$f);
	}
	$view->sourcesCombo->set_active_id($file);

	return $buf;
}

# scroll to currently debugged line
sub scroll {

	my $self = shift;
	my $file = shift;
    my $line = shift;

	my $view = $self->view;

    $self->openFile( $file, $line );

	$view->pump_msgs();

	my $buf = $view->sourceBuffers->{$file};

    my $iter = $buf->get_iter_at_line( $line - 1 );
    $view->sourceView->scroll_to_iter( $iter, 0, 1, 0, 0.5 );

	$view->pump_msgs();

    $buf->place_cursor($iter);

	$view->pump_msgs();
}


# enable / disable buttons
sub enableButtons {

	my $self  = shift;
    my $state = shift;

	my $view = $self->view;

	$self->{uiDisabled} = $state ? 0 : 1;

    $view->buttonRun->set_sensitive($state);
    $view->buttonStep->set_sensitive($state);
    $view->buttonOver->set_sensitive($state);
    $view->buttonOut->set_sensitive($state);
    $view->buttonLexicals->set_sensitive($state);
#   $view->buttonHome->set_sensitive($state);
    $view->evalEntry->set_sensitive($state);
    $view->lexicalsMenu->set_sensitive($state);
    $view->breakpointsMenu->set_sensitive($state);
	$view->saveBreakpointsMenu->set_sensitive($state);
    $view->openFileMenu->set_sensitive($state);
    $view->showSubsMenu->set_sensitive($state);
    $view->showFilesMenu->set_sensitive($state);
    $view->lexicalTreeView->set_sensitive($state);
	$view->buttonReloadFile->set_sensitive($state);
	$view->reloadMenu->set_sensitive($state);

    $view->buttonStop->set_sensitive( $state ? 0 : 1 );
}


##########################################
# MVC- Debugger UI View
##########################################

package view;
use parent 'Devel::gdbg::view';
use strict;

use File::Basename;
use Params::Util qw<_HASH _HASH0 _HASHLIKE _ARRAYLIKE>;
use FindBin qw( $RealBin );

sub new {

	my $class = shift;

	my $self = {
		uixml         => $RealBin."/gdbg.ui", # path to glade xml ui definition
		scheme        => undef,
		zoom          => 10,

		widgets       => {
			sourceBuffers => {},
			searchCtx     => undef,
		}
	};

	bless $self, $class;

	$self->init();
	return $self;
}


# get full text source from a existing source buffer
sub getSource {

	my $self     = shift;
	my $filename = shift;

	my $buf       = $self->sourceBuffers->{$filename};
	my $startiter = $buf->get_start_iter();
	my $enditer   = $buf->get_end_iter();
	my $txt       = $buf->get_text($startiter,$enditer,0);
	return $txt;
}


# get a line of text from buffer
sub getLine {

	my $self   = shift;
	my $iter   = shift;
	my $buffer = shift;
	my $line   = shift;

	my $count = $buffer->get_line_count();
	my $endIter = $buffer->get_iter_at_line($line);
	if($line-1 >= $count) {
		$endIter = $buffer->get_end_iter();
	}
	my $text = $buffer->get_text($iter,$endIter,1);
	chomp($text);

	return $text;
}

# get a line from mouseclick
sub getLineFromMouseClick {

	my $self   = shift;
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
	my $text = $self->getLine($iter,$widget->get_buffer(),$line);

	return $text;
}

# update the info with current call frame stack
sub updateInfo {

    my ( $self, $filename, $line, $info ) = @_;

    $self->mainWindow->set_title( $filename . ":" . $line );

    $self->infoBuffer->set_text( $info, -1 );
}

#-------------------------------------------------
# variable inspector tree widget support
#-------------------------------------------------

# find a node '$target' in the var inspection tree
# returning a Gtk TreeWidget iterator

sub find_root {

	my ($self,$target,$root) = @_;

	if($target eq '' || $target eq '/') {
		return {
			result => $root,
			found => 1,
		};
	}

	my $treemodel = $self->lexicalTreeView->get_model;

	my $iter = $treemodel->iter_children($root);
	while( $iter) {

		my $gv;
		eval {
			$gv = $treemodel->get_value($iter,0);
			$gv = $treemodel->get_value($iter,2);
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
				return $self->find_root($target,$iter);
			}
		}
		$treemodel->iter_next($iter);
	}
	return {
		found => 0,
		result => undef,
	};
}


# populate an Gtk TreeView node

sub populate_item {

	my ($self,$treemodel,$selectedVar,$iter,$key,$item,$path) = @_;

	$treemodel->set_value($iter,0,$self->GValue($key));
	$treemodel->set_value($iter,2,$self->GValue($path));

	if($item->{placeholder}) {

		$treemodel->set_value($iter,1,$self->GValue($item->{type}));

		my $it = $treemodel->append($iter);
		my $gv = $self->GValue('');
		$treemodel->set_value($it,0,$gv);
		$treemodel->set_value($it,1,$gv);
		$treemodel->set_value($it,2,$gv);
		return;
	}

	if( $item->{type} eq 'REF' || $item->{type} eq 'SCALAR') {
		my $gv = $self->GValue($item->{value});
		$treemodel->set_value($iter,1,$gv);
	}
	elsif( $item->{type} eq 'CODE' || $item->{type} eq 'IO' || $item->{type} eq 'GLOB'  ) {
		my $gv = $self->GValue($item->{type});
		$treemodel->set_value($iter,1,$gv);
	}
	else {
		my $gv = $self->GValue($item->{type});
		$treemodel->set_value($iter,1,$gv);
		if( defined $item->{value} ) {
			$self->populate_lexicals($item,$treemodel,$selectedVar,$iter,$path);
		}				

		if( index( $selectedVar, $path) == 0 ) {
			if($iter) {
				my $p = $treemodel->get_path($iter);
				if($p) {
					$self->lexicalTreeView->expand_to_path($p);
					$self->lexicalTreeView->scroll_to_cell($p,undef,0,0,0);					
				}
			}
		}
	}
}

# populate the tree view (or parts of it)

sub populate_lexicals {

	my ($self,$data,$treemodel,$selectedVar,$root,$path) = @_;

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

			my $iter = $treemodel->append($root);

			$self->populate_item($treemodel,$selectedVar,$iter,$key,$item,"$path/$key");			
		}
	}
	else {

		my $i = 0;
		foreach my $item ( $value->@* ) {

			my $iter = $treemodel->append($root);
			$self->populate_item($treemodel,$selectedVar,$iter,"",$item,"$path/$i");			
			$i++;
		}
	}

	if( $path && index( $selectedVar, $path) == 0 ) {
		my $iter = $treemodel->iter_children($root);
		if($iter) {
			my $p = $treemodel->get_path($iter);
			if($p) {
				$self->lexicalTreeView->expand_to_path($p);
				$self->lexicalTreeView->scroll_to_cell($p,undef,0,0,0);	
			}
		}
	}
}

#-------------------------------------------------
# set status helper
#-------------------------------------------------

sub status {

	my $self     = shift;
	my $filename = shift;
	my $line     = shift;

	if(!$filename) {
		return;
	}

	if(defined $line) {

		$self->mainWindow->set_title( basename($filename));	
		$self->statusBar->set_text(basename($filename).":".$line);
		$self->statusBar->set_tooltip_text($filename);
	}
	else {

		$self->mainWindow->set_title( basename($filename) );	
	}
}

#-------------------------------------------------
# initialize the UI from xml
# and add our widget customizations
#-------------------------------------------------

sub build_ui {

	my $self       = shift;
	my $controller = shift;
	my $uixml      = $self->{uixml};

	$self->SUPER::build_ui($uixml,$controller);

    #css
    my $provider = Gtk3::CssProvider->new();
    print STDERR $RealBin."/gdbg.css\n";
    my $css = Glib::IO::File::new_for_path($RealBin."/gdbg.css");
    $provider->load_from_file($css);
    my $screen = Gdk::Screen::get_default();
    Gtk3::StyleContext::add_provider_for_screen($screen, $provider, 400); 

	# Perl syntax highlighting support
    my $langManager = Gtk::Source::LanguageManager->new();
    my $lang        = $langManager->get_language("perl");

	$self->add( langManager => $langManager );
	$self->add( lang        => $lang );

	# prepare support for breakpoint markers
    my $attrs = Gtk::Source::MarkAttributes->new();
    $attrs->set_icon_name("media-record");

	# setup main sourceView attributes
    $self->sourceView->set_show_line_marks(1);
    $self->sourceView->set_editable(0);
    $self->sourceView->set_wrap_mode('none');
    $self->sourceView->set_mark_attributes( "error", $attrs, 10 );

    # theme support for buffers
    my $manager = Gtk::Source::StyleSchemeManager::get_default();

	# populate schemes menu
    my $themes = $manager->get_property("scheme-ids");
    foreach my $theme (@$themes) {
        my $item = Gtk3::MenuItem->new_with_label($theme);
        $item->signal_connect( 
			'activate' => sub {

				return $controller->onTheme( @_ );
			}
		);
			
        $self->themesMenu->add($item);
    }

	# default schema
    $self->add(scheme => $manager->get_scheme("solarized-dark") );

	# prepare the info buffer to display call stack
    my $infoBuffer = $self->infoPane->get_buffer();
    $self->infoPane->set_editable(0);
    $infoBuffer->set_style_scheme($self->scheme);
	$self->add( infoBuffer => $infoBuffer );

	# prepare the lexical vars display buffer
    my $lexicalsBuffer = Gtk::Source::Buffer->new();
    $lexicalsBuffer->set_language($lang);
    $lexicalsBuffer->set_style_scheme($self->scheme);
	$self->add( lexicalsBuffer => $lexicalsBuffer );

	# buffer to show loaded subroutines
    my $subsBuffer = Gtk::Source::Buffer->new();
    $subsBuffer->set_style_scheme($self->scheme);
	$self->add( subsBuffer => $subsBuffer);

	# buffer to show loaded files
    my $filesBuffer = Gtk::Source::Buffer->new();
    $filesBuffer->set_style_scheme($self->scheme);
	$self->add( filesBuffer => $filesBuffer );

	# buffer to show loaded breakpoints
    my $breakpointsBuffer = Gtk::Source::Buffer->new();
    $breakpointsBuffer->set_style_scheme($self->scheme);
	$self->add( breakpointsBuffer => $breakpointsBuffer );

	# prepare the var inspection tree view
	my $treeModel = $self->LexicalTreeStore();
	$self->lexicalTreeView->set_model($treeModel);

	my $renderer1 = Gtk3::CellRendererText->new ();
	my $renderer2 = Gtk3::CellRendererText->new ();

	# enable if you want to see the hidden 'path' data
	# value of the the tree view in the third column

	# my $renderer3 = Gtk3::CellRendererText->new ();

	# name column
	my $column1 = Gtk3::TreeViewColumn->new();
	$column1->set_resizable(1);
	$column1->set_sizing('fixed');
	$column1->set_fixed_width(100);
	$column1->set_title("Var");
  	$column1->pack_start($renderer1, 0);	
	$column1->add_attribute($renderer1,"text",0);
	$self->lexicalTreeView->append_column( $column1);

	# value column
	my $column2 = Gtk3::TreeViewColumn->new();
	$column2->set_resizable(1);
	$column2->set_sizing('fixed');
	$column2->set_fixed_width(100);
	$column2->set_title("Info");
  	$column2->pack_start($renderer2, 0);	
	$column2->add_attribute($renderer2,"text",1);
	$self->lexicalTreeView->append_column( $column2 );

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

	$self->lexicalTreeView->signal_connect( 
		'row-expanded' => sub {

			return $controller->onRowExpanded(@_);
		}
	);

	$self->lexicalTreeView->set_enable_tree_lines(1);

	# prepare search support
	my $searchSettings = Gtk::Source::SearchSettings->new();
	$searchSettings->set_wrap_around(1);
	$searchSettings->set_regex_enabled(1);
	$searchSettings->set_case_sensitive(0);
	$self->add( searchSettings => $searchSettings );
	
	# enable button stop, just to be sure
    #$self->buttonStop->set_sensitive(1);

	if( $ENV{"GDBG_RESTART_CMD"} ) {

		my $menuItem = Gtk3::MenuItem->new_with_label("Restart Docker Container");
		$self->fileMenu->append($menuItem);
		$menuItem->signal_connect( 
			'activate' => sub {

				return $controller->onRestartDocker( @_ );
			}
		);
	}

    # show the UI
    $self->mainWindow->show_all();
}


##################################################
# MVC - Debugger UI Controller
##################################################

package controller;
use parent 'Devel::gdbg::controller';
use Encode;
use strict;

sub new {
	my $class = shift;
	my $model = shift;
	my $view  = shift;

	my $self = {
		model => $model,
		view  => $view,
	};

	bless $self, $class;

	$view->build_ui($self);
	$model->enableButtons(0);

	return $self;
}

sub view {
	my $self = shift;
	return $self->{view};
}

sub model {
	my $self = shift;
	return $self->{model};
}

#-------------------------------------------------
# high level Gtk Action handlers
#-------------------------------------------------

# run command - continue until next breakpoint
sub onRun :Action {

	my $self = shift;

	my $model = $self->model;

	$model->rpc->continue();
    $model->enableButtons(0);
}

# single step, recursing into functions
sub onStep :Action :Accel(<ctrl>Right) {

	my $self = shift;

	my $model = $self->model;

	$model->rpc->step();
    $model->enableButtons(0);
}

# single step, jumping over functions
sub onOver :Action :Accel(<ctrl>Down) {

	my $self = shift;

	my $model = $self->model;

	$model->rpc->next();
    $model->enableButtons(0);
}

# step out of current function, continue stepping afterwards
sub onOut :Action :Accel(<ctrl>Left) {

	my $self = shift;

	my $model = $self->model;

	$model->rpc->return();	
    $model->enableButtons(0);
}

# stop button pressed - interrupt the debugger
sub onStop :Action {

	my $self = shift;

	my $model = $self->model;

    #	print "KILL $pid\n";
	if(!$ENV{"GDBG_KILL_CMD"}) {

	    kill 'INT', $model->{pid};
	}
	else {
		my $cmd = $ENV{"GDBG_KILL_CMD"};

		# if $cmd contains the string '{{PID}}',
		# replace with current $pid
		my $pid = $model->{pid};
		$cmd =~ s/\{\{PID\}\}/$pid/;

		system("bash -c '$cmd'");
	}
}

# user entered return in eval entry
sub onEval :Action {

	my $self = shift;

	my $model = $self->model;
	my $view  = $self->view;

    my $e = $view->evalEntry->get_text();
	$model->rpc->eval( $e );
}


# user selected open-file from menu
sub onOpen :Action {

	my $self   = shift;
	my $action = shift;
	my $param  = shift;

	my $model = $self->model;
	my $view  = $self->view;

    # show open file dialog
    my $dlg = Gtk3::FileChooserNative->new( 
		"Open File", 
		$view->mainWindow, 
		'open',
        "OK", 
		"Cancel" 
	);

    my $r = $dlg->run();
    if ( $r == -3 ) {    # accept file

        my $fn = $dlg->get_filename();
        $model->openFile($fn);
    }
}

# user selected 'Goto current line' from File Menu
sub onScroll :Action {

	my $self = shift;

	my $model = $self->model;

    $model->scroll(
		$model->currentFile,
		$model->currentLine
	);
}

# user selected 'Show Lexicals' from file menu
sub onLexicals :Action {

	my $self = shift;

	my $model = $self->model;

	$model->rpc->lexicals();
}

# show breakpoints window menu handler
sub onBreakpoints :Action {

	my $self = shift;

	my $model = $self->model;

	$model->rpc->breakpoints();
}

sub onStoreBreakpoints :Action {

	my $self = shift;

	my $model = $self->model;

	$model->rpc->storebreakpoints();
}

# show subroutines window menu handler
sub onSubs :Action {

	my $self = shift;

	my $model = $self->model;

	$model->rpc->functions();	
}

# reload the current, active file, if any
sub onReload :Action {

	my $self   = shift;
	my $widget = shift;
	my $event  = shift;

print STDERR "RELOAD!\n";

	my $model = $self->model;

	$model->rpc->fetch(
		$model->{openFile},
		$model->{openFile} eq $model->currentFile 
			? $model->currentLine
			: 1
	);	
}

# show files view
sub onFiles :Action {

	my $self = shift;

	my $model = $self->model;
	my $view  = $self->view;

	my @files = keys $model->files->%*;
	my @sorted = sort(@files);
	my $text = join("\n", @sorted);

	$view->filesBuffer->set_text($text,-1);

	$view->sourceView->set_buffer($view->filesBuffer);
	$view->status("Files loaded by the debugger:");
}

sub onZoomIn :Accel(<ctrl>plus) {

	my $self = shift;

	my $model = $self->model;
	my $view  = $self->view;

	my $styleCtx = $view->sourceView->get_style_context();

	my $oldProvider = $view->{provider};
	if( $oldProvider ) {
		$styleCtx->remove_provider($oldProvider);
	}

	my $zoom = $view->{zoom};

	if($zoom < 100) {
		$zoom += 2;
		$view->{zoom} = $zoom;
	}
	my $css = "textview { font-family: Monospace; font-size: ".$zoom."pt; } ";

	my $provider = Gtk3::CssProvider->new();
	$provider->load_from_data(Encode::encode_utf8($css));
	$styleCtx->add_provider( $provider, 600 ); # 'STYLE_PROVIDER_PRIORITY_APPLICATION');
	$view->{provider} = $provider;
}

sub onZoomOut :Accel(<ctrl>minus) {

	my $self = shift;

	my $model = $self->model;
	my $view  = $self->view;

	my $styleCtx = $view->sourceView->get_style_context();

	my $oldProvider = $view->{provider};
	if( $oldProvider ) {
		$styleCtx->remove_provider($oldProvider);
	}

	my $zoom = $view->{zoom};

	if($zoom > 8) {
		$zoom -= 2;
		$view->{zoom} = $zoom;
	}
	my $css = "textview { font-family: Monospace; font-size: ".$zoom."pt; } ";

	my $provider = Gtk3::CssProvider->new();
	$provider->load_from_data(Encode::encode_utf8($css));
	$styleCtx->add_provider( $provider, 600 ); # 'STYLE_PROVIDER_PRIORITY_APPLICATION');
	$view->{provider} = $provider;
}

#-------------------------------------------------
# low level Gtk signal handlers
#-------------------------------------------------

# set a breakpoint UI handler (click on marker of sourceView)
sub onMarker {

    my ( $self, $widget, $iter, $event ) = @_;

	my $model = $self->model;
	my $view  = $self->view;

	# only allow setting breakpoints when interactive
	if($model->uiDisabled) {
		return;
	}

	my $buf = $self->view->sourceView->get_buffer();

	# if we are not looking at a source file, no breaktpoints
	if( $buf == $view->subsBuffer ||
		$buf == $view->infoBuffer ||
		$buf == $view->breakpointsBuffer ||
		$buf == $view->filesBuffer) 
	{
		return;
	}

	# get line and filename
    my $line = $iter->get_line() + 1;

    $model->{filename} = $model->{openFile};

    # cannot set break points in eval code
    if ( $model->filename =~ /^\(eval/ ) {
        return;
    }

	# get line of text, skip over some obviously non-breakable lines
	my $text = $view->getLine(
		$iter,
		$view->sourceBuffers->{$model->filename},
		$line
	);
	if( !$text || $text eq "" || 
	    $text =~ /^\s*((use)|(no)|(require)|(package)|#)/ ||
		$text =~ /^\s+$/ ) 
	{
		return;
	}

	$model->rpc->breakpoint(
		$model->filename,
		$line
	);
}

# toggle the breakpoint on current line
sub onToggleBreakpoint :Accel(<ctrl>BackSpace) {

	my $self = shift;

	my $model = $self->model;
	my $view  = $self->view;

	if($model->uiDisabled) {
		return;
	}

	my $buf = $view->sourceView->get_buffer();

	# if we are not looking at a source file, no breaktpoints
	if( $buf == $view->subsBuffer ||
		$buf == $view->infoBuffer ||
		$buf == $view->breakpointsBuffer ||
		$buf == $view->filesBuffer) 
	{
		return;
	}

    # cannot set break points in eval code
    if ( $model->currentFile =~ /^\(eval/ ) {
        return;
    }

	my $mark = $view->sourceView->get_buffer()->get_insert();
	my $iter = $view->sourceView->get_buffer()->get_iter_at_mark($mark);
	my $line = $iter->get_line()+1;

	# get line of text, skip over some obviously non-breakable lines
	my $text = $view->getLine(
		$iter,
		$view->sourceBuffers->{$model->currentFile},
		$line
	);
	if( !$text || $text eq "" || 
	    $text =~ /^\s*((use)|(no)|(require)|(package)|#)/ ||
		$text =~ /^\s+$/ ) 
	{
		return;
	}

	$model->rpc->breakpoint(
		$model->currentFile,
		$line
	);
}

# toggle running state (Stop/Run)
sub onToggleRunning :Accel(<ctrl>space) {

	my $self = shift;

	my $model = $self->model;

	if($model->uiDisabled) {

		onStop();
	}
	else {

		onRun();
	}
}

# user clicks the call frame stack
sub onInfoPaneClick {

	my $self   = shift;
	my $widget = shift;
	my $event  = shift;

	my $model = $self->model;
	my $view  = $self->view;

	if($model->uiDisabled) {
		return;
	}

	my $text = $view->getLineFromMouseClick($widget,$event);
	if( $text =~ /[^\[]+\[([^\[]+):([0-9]+)\]/ ) {

		my $file = $1;
		my $line = $2;
		$model->scroll($file,$line);
	}
}

# mouse click on a line, if on breakpoints, files or subroutines view
sub onClick {

	my $self   = shift;
	my $widget = shift;
	my $event  = shift;

	my $model = $self->model;
	my $view  = $self->view;

	if($model->uiDisabled) {
		return;
	}

	my $text = $view->getLineFromMouseClick($widget,$event);
	if(!$text) { 
		return; 
	}

	my $buf = $view->sourceView->get_buffer();

	if($buf == $view->subsBuffer) {

		$model->rpc->functionbreak($text);
		return;
	}
	elsif($buf == $view->breakpointsBuffer) {

		if ( $text =~ /^#/) { return; }
		if ($text =~ /([^:]+):([0-9]+)/ ) {
			my $file = $1;
			my $line = $2;
			$model->scroll($file,$line);
		}
		return;
	}
	elsif($buf == $view->filesBuffer) {

		$model->openFile($text,1);
		return;
	}
}

# lazily load var inspection tree content
sub onRowExpanded {

	my $self   = shift;
	my $widget = shift;
	my $iter   = shift;

	my $model = $self->model;
	my $view  = $self->view;

	my $treemodel = $view->lexicalTreeView->get_model();
	my $gval = $treemodel->get_value($iter,2); 
	$model->{selectedVar} = $gval;

	my $first = $treemodel->iter_children($iter);
	$gval = $treemodel->get_value($first,0); 
	if( $gval eq '' ) {

		$gval = $treemodel->get_value($first,1); 
		if( $gval eq '' ) {

			$gval = $treemodel->get_value($iter,2); 
			$treemodel->remove($first);
			$model->rpc->jsonlexicals( $gval );
		}
	}
}

# user selected theme from Themes menu
sub onTheme {
	
    my ($self,$menuItem) = @_;

	my $model = $self->model;
	my $view  = $self->view;
    my $label = $menuItem->get_label();

    my $manager = Gtk::Source::StyleSchemeManager::get_default();
    $view->{widgets}->{scheme} = $manager->get_scheme($label);

    foreach my $key ( keys $view->sourceBuffers()->%* ) {

        $view->sourceBuffers->{$key}->set_style_scheme(
			$view->scheme
		);
    }
	
	$view->infoBuffer->set_style_scheme($view->scheme);        
	$view->lexicalsBuffer->set_style_scheme($view->scheme);    
	$view->subsBuffer->set_style_scheme($view->scheme);		
	$view->filesBuffer->set_style_scheme($view->scheme);	   
	$view->breakpointsBuffer->set_style_scheme($view->scheme); 
}

# user selects source from combo box
sub onFileChoose {

	my $self = shift;

	my $model = $self->model;
	my $view  = $self->view;

	my $file = $view->sourcesCombo->get_active_text();

    $model->{openFile} = $file;

    $view->sourceView->set_buffer( 
		$view->sourceBuffers->{$model->{openFile}} 
	);

    $view->status( 
		$model->{openFile}
	);
}

# user closes main window
sub onDestroy {

	my $self = shift;

	my $model = $self->model;

    $model->{quit} = 1;
}

sub onRestartDocker {

	my $self = shift;

	my $model = $self->model;

	my $cmd = $ENV{"GDBG_RESTART_CMD"};

print STDERR "RESTART: $cmd\n";

	return if !$cmd;

	# if $cmd contains the string '{{PID}}',
	# replace with current $pid
	my $pid = $model->{pid};
	$cmd =~ s/\{\{PID\}\}/$pid/;

print STDERR "RESTART: $cmd\n";

	system("bash -c '$cmd &'");	

	$self->view->status("RESTARTING ...............");	
}

#-------------------------------------------------
# UI search support
#-------------------------------------------------

sub onSearch {

	my $self   = shift;
	my $widget = shift;
	my $event   = shift;

	my $key = $event->get_keyval();

	if($key != Gdk->KEY_Return) {
		# do default
		return 0;
	}

	return $self->doSearch($widget,$event);
}

sub doSearch {

	my $self   = shift;
	my $widget = shift;
	my $event  = shift;

	my $model = $self->model;
	my $view  = $self->view;

	# check for shift key being pressed
	my ($unused,$state) = $event->get_state();

	my @stateMask = @$state;

	my $isShift = 0;
	foreach my $s ( @stateMask ) {

		if( $s eq 'shift-mask') {
			$isShift = 1;
		}
	}

	# right click
	if($event->button->{button} == 3) {

		my $dialog = $view->searchDialog;

		my $active = $model->searchDirection() eq 'forward' ? 1 : 0;
		$view->searchForward->set_active($active);
		$view->searchBackward->set_active(!$active);

		$dialog->set_transient_for($view->mainWindow);
		$dialog->set_modal(1);
		$dialog->show_all;

		return 0;
	}

	my $query = $view->search->get_text();

	$view->searchSettings->set_search_text($query);

	$view->{widgets}->{searchCtx} = Gtk::Source::SearchContext->new(
		$view->sourceView->get_buffer(),
		$view->searchSettings,
	);

	$view->searchCtx->set_highlight(1);

	my ($hasSelection,$startIter,$endIter) = $view->sourceView->get_buffer()->get_selection_bounds();
	my ($match,$matchStart, $matchEnd);

	my $direction = $isShift 
		? $model->searchDirection() == 'forward' ? 'backward' : 'forward'   
		: $model->searchDirection();

	$model->{searchDirection} = $direction;

	if($direction eq 'forward' ) {

		my $searchIter = $endIter;	
		$searchIter->forward_char();

		($match,$matchStart, $matchEnd) = $view->searchCtx->forward($searchIter);
	}
	else {
		my $searchIter = $startIter;	
		($match,$matchStart, $matchEnd) = $view->searchCtx->backward($searchIter);
	}

	if($match) {

		my $buf = $view->sourceView->get_buffer();
		$buf->select_range($matchStart,$matchEnd);

		$view->sourceView->scroll_to_iter( $matchStart, 0, 1, 0.5, 0.5 );		
	}

	return 1;
}

sub onCancelSearch {

	my $self   = shift;
	my $widget = shift;
	my $event  = shift;

	my $model = $self->model;
	my $view  = $self->view;

	my $img = $widget->get_property("image");

	if($event->button->{type} eq 'button-press') {

		# left click
		if($event->button->{button} == 1) {

			if($view->searchCtx) {

				$view->searchCtx->set_highlight(0);
			}

			$view->search->set_text("");
		}
		# right click
		if($event->button->{button} == 3) {

			my $dialog = $view->searchDialog;

			my $active = $model->searchDirection() eq 'forward' ? 1 : 0;
			$view->searchForward->set_active($active);
			$view->searchBackward->set_active(!$active);

			$dialog->set_transient_for($view->mainWindow);
			$dialog->set_modal(1);
			$dialog->show_all;
		}
	}
}

sub onFocusSearch :Accel(<ctrl>f) {

	my $self = shift;

	my $view = $self->view;

	$view->search->grab_focus();
}

#-------------------------------------------------
# search settings dialog events
#-------------------------------------------------

sub onSearchDialogClose {

	my $self   = shift;
	my $widget = shift;
	my $event  = shift;

	$widget->hide;
}

sub onSearchDialogDelete {

	my $self   = shift;
	my $widget = shift;
	my $event  = shift;

	return 1;
}

sub onSearchBackward {

	my $self   = shift;
	my $widget = shift;
	my $event   = shift;

	my $model = $self->model;

	$model->{searchDirection} = 'backward';
}

sub onSearchForward {

	my $self   = shift;
	my $widget = shift;
	my $event  = shift;

	my $model = $self->model;

	$model->{searchDirection} = 'forward';
}

sub onSearchSensitive {

	my $self   = shift;
	my $widget = shift;
	my $event  = shift;

	my $view  = $self->view;

	my $value = $widget->get_active;
	$view->searchSettings->set_case_sensitive($value);
}

sub onSearchWordBoundaries {

	my $self   = shift;
	my $widget = shift;
	my $event  = shift;

	my $view  = $self->view;

	my $value = $widget->get_active;
	$view->searchSettings->set_at_word_boundaries($value);
}

sub onSearchRegexEnabled {

	my $self   = shift;
	my $widget = shift;
	my $event  = shift;

	my $view  = $self->view;

	my $value = $widget->get_active;
	$view->searchSettings->set_regex_enabled($value);
}


##################################################
# GDBG UI - main
##################################################

package gdbgui;

# initialize the app

my $view       = view->new();
my $model      = model->new($view);
my $controller = controller->new($model,$view);

if ( $ENV{"GDBG_NO_FORK"} ) {

	# possibly re-attaching the debugger UI to
	# the process being debugged. 
	$controller->onStop();		
	$model->rpc->next();
}

#-------------------------------------------------
# THE ui MAIN loop
#-------------------------------------------------

# run the main event loop, handling both
# Gtk and IPC fifo events

$controller->run();


#-------------------------------------------------
# over and out
#-------------------------------------------------

if ( $model->{quit} == 1 ) 
{
	if(!$ENV{"GDBG_KILL_CMD"}) {
    	kill 'INT', $model->{pid};
	}
	$model->rpc->quit();
}

$model->fifo->close();

