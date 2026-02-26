package Devel::gdbg::view;

use v5.20;
use utf8;
use strict;

##################################################
# Perl dependencies
##################################################

use File::Basename;
use Glib::Object::Introspection;

##################################################
# glib and gtk dependencies via gir
##################################################

BEGIN {

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
}


sub add {
	my $self   = shift;
	my $name   = shift;
	my $widget = shift;

	$self->{widgets}->{$name} = $widget; 
}

sub init {

	my $self = shift;
    
	Gtk3::init();

    $self->{ctx} = GLib::MainContext::default();	
}

sub run {

	my $self  = shift;
	my $model = shift;

	while ( !$model->{quit} ) {

		# pump GTK events
		$self->pump_msgs();

		# pump debugger IPC messages
		my @msgs = $model->fifo->read();
		foreach my $msg (@msgs) {

			$model->process_msg($msg);
		}
	}
}

sub pump_msgs {
	my $self  = shift;
	my $ctx = $self->{ctx};

    while ( $ctx->pending() ) {
        $ctx->iteration(0);
    }	
}


# simple helper to produce a GLib GValue
sub GValue {

	my $self  = shift;
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

# Gtk Builder - load UI from XML
sub build_ui {

	my $self       = shift;
	my $uixml      = shift;
	my $controller = shift;

	my $widgets = $self->{widgets};

	# load widgets from xml
    my $builder = Gtk3::Builder->new();
    $builder->add_from_file($uixml) or die 'file not found';

	# map widgets
	my $list = $builder->get_objects();
	for my $widget ( @$list ) {

		my $id = Gtk3::Buildable::get_name($widget);
		$widgets->{$id} = $widget;
	}

	# connect UI signal handlers
    $builder->connect_signals_full(  sub {

	    my ( $builder, $obj, $signal, $handler, $co, $flags, $data ) = @_;

		my $class = ref $controller;
		my $signalHandler = \&{"$class"."::"."$handler"};
    	$obj->signal_connect( $signal => sub {

			return $signalHandler->( $controller, @_ );
		});

	}, 0 );

	# keyboard shortcut accelerators
	$self->add( accel => Gtk3::AccelGroup->new() );
	$self->mainWindow->add_accel_group($self->accel);

	for my $accel ( @Devel::gdbg::controller::accels ) {

		my $key     = $accel->{key};
		my $handler = $accel->{handler};

		my ($key,$mod) = Gtk3::accelerator_parse($key);
		$self->accel->connect( $key, $mod, [], sub {

			return $handler->( $controller, @_ );
		});
	}

	# attach actions

	my %simpleActions = %Devel::gdbg::controller::simpleActions;
	for my $key ( keys %simpleActions ) {

		my $handler = $simpleActions{$key};

		my $action = Glib::IO::SimpleAction->new($key);
		$action->signal_connect("activate", sub {

			return $handler->( $controller, @_ );
		});

		$self->mainWindow->add_action($action);    
	}

	my %detailedActions = %Devel::gdbg::controller::detailedActions;
	for my $key ( keys %detailedActions ) {

		my $handler = $detailedActions{$key};

		my $gvt = Glib::VariantType->new("s");
		my $gv  = Glib::Variant->new_string("");

		my $action = Glib::IO::SimpleAction->new_stateful($key,$gvt,$gv);
		$action->signal_connect("activate", sub {

			return $handler->( $controller, @_ );
		});

		$self->mainWindow->add_action($action);    
	}
}

# prevent AUTOLOAD from treating 
# DESTROY as widget

sub DESTROY {
	
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

	# my ($package, $filename, $line) = caller;
	# print STDERR "$package, $filename $line\n";

	use strict;
	if(! exists $widgets->{$method} ) {
		die ("Widget $method does not exist.");
	}
	return $widgets->{$method};
}


1;
