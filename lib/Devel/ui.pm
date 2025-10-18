
use v5.20;
use utf8;
use strict;

##################################################
# Perl dependencies
##################################################

use File::Basename;

##################################################
# glib and gtk dependencies via gir
##################################################
BEGIN {

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
    version  => '4.0',
    package  => 'Gdk'
);

Glib::Object::Introspection->setup(
    basename => 'Gtk',
    version  => '4.0',
    package  => 'Gtk'
);

Glib::Object::Introspection->setup(
    basename => 'GtkSource',
    version  => '5',
    package  => 'Gtk::Source'
);

}

# connect signals to handlers in App::Handlers
# via GtkBuilder
package App::Scope;

use Glib::Object::Subclass
   Glib::Object::,   # parent class, derived from Glib::Object
   signals => {},
   properties => [],
   interfaces => [ "Gtk::BuilderScope" ]
;
   
sub CREATE_CLOSURE {

  my ($widget,$builder,$fun) = @_;

  return sub {
    no strict;
      &{ "$fun" }( @_ );
  };
}

sub GET_TYPE_FROM_NAME {
  
  my ($widget,$builder,$name) = @_;
  
  if( $name =~ /^GtkSource/ ) {
  
    $name =~ s/^GtkSource/Gtk::Source::/ ;
    return $name;
  }
  if( $name =~ /^Gtk/ ) {
  
    $name =~ s/^Gtk/Gtk::/ ;
    return $name;
  }
  if( $name =~ /^Gdk/ ) {
  
    $name =~ s/^Gdk/Gdk::/ ;
    return $name;
  }  
}

# helper as base class for App::Messages
# adds suport for :regex(...) function
# attributes

package App::MsgHandler;
use Sub::Util qw(subname);

our %regex;

sub MODIFY_CODE_ATTRIBUTES {
  my ($package,$coderef,$attr) = @_;
  my $fun = subname($coderef);

  if ( $fun =~ /::([^:]+)$/ ) {
    $fun = $1;
  }

  my $rgx = $attr;
  $rgx =~ s/^Regex\(//;
  $rgx =~ s/\)$//;

  my $whitespace = ' ';
  if ( $rgx eq '' ) {
	$whitespace = '';
  }
  my $full_regex = qr(^$fun$whitespace$rgx$)s;
  $regex{$full_regex} = $coderef;

  return ();
}

# handle a string message, the format is
# cmd, followed by whitespace, followed b0 or more
# comma separated arguments
# will invoke the handler matching the handler regex
# and pass the (first four) regex groups as @_
sub handle {

  my $msg = shift;
  foreach my $r ( keys %regex ) {
  
    if( $msg =~ /$r/s ) {
      $regex{$r}->($1,$2,$3,$4);
      return;
    }
  }
}

package App::ActionHandler;
use Sub::Util qw(subname);

our %detailed;

sub MODIFY_CODE_ATTRIBUTES {
  my ($package,$coderef,$attr) = @_;
  my $fun = subname($coderef);

  if ( $fun =~ /::([^:]+)$/ ) {
    $fun = $1;
  }

  if( $attr =~ /Detail/ ) {
    $detailed{$fun} = 1;
  }

  return ();
}

# boilerplate package to init a Gtk App
package App::Builder;

our $builder;
our %actions;

# adds all actions from App::Actions package
# to the main window so they can be invoked
# by widgets defined in xml
sub add_actions {

  my $mainWindow = shift;

  foreach my $name( keys %App::Actions:: ) {
  
    my @parts = split( '::', $name);
    
    my $n = $parts[-1];

    my $handler = $App::Actions::{$n};

	my $action;
    if( $App::ActionHandler::detailed{$n} ) {
		my $gvt = Glib::VariantType->new("s");
		my $gv = Glib::Variant->new_string("");
		$action = Glib::IO::SimpleAction->new_stateful($n,$gvt,$gv);
	}
	else {
		$action = Glib::IO::SimpleAction->new($n);
	}

	$actions{$n} = $action;

    $action->signal_connect("activate", $handler);
    $mainWindow->add_action($action);    
  }
}

# manually add a key_controller for low level key handling
sub add_key_controller {

  my $target  = shift;
  my $event   = shift;
  my $handler = shift;
    
  my $event_controller = Gtk::EventControllerKey->new();
 
  $event_controller->signal_connect( $event, $handler );
    
  $event_controller->set_propagation_phase('capture');

  $target->add_controller($event_controller);
}

sub add_mouse_controller {

  my $target  = shift;
  my $event   = shift;
  my $handler = shift;
    
  my $event_controller = Gtk::GestureClick->new();
  $event_controller->set_button(0);

  $event_controller->signal_connect( $event, $handler );
    
  $event_controller->set_propagation_phase('capture');

  $target->add_controller($event_controller);
}

# get a xml defined GtkWidget by name (id)
sub get_object {

  my $name = shift;
  return $builder->get_object($name);
}

# wrapper around GtkBuilder->add_from_file
# will also call Gtk::init, install the
# AppScope for automatic signal connection
# for xml defined signals to App::Handlers,

sub build_ui {

    my $xml = shift;
    my $widgetNames = shift;

    # initialize Gtk
    Gtk::init;
    
    # prepare GtkBuilder
    $builder = Gtk::Builder->new();
    
    my $scope = App::Scope->new();
    $builder->set_scope( $scope );
    
    # load widgets from file
    $builder->add_from_file($xml) or die 'file not found';

    # map and return widgets
    my %widgets;
    for my $widgetName ( @$widgetNames ) {
    
      $widgets{$widgetName} = $builder->get_object($widgetName);
    }    
    
    return (%widgets);
}


1;
