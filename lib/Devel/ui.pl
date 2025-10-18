
##################################################
package App;
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
use lib $RealBin;

use ui;

#################################
# event handling
#################################

# Action handlers for any GtkActions defined in the xml
package App::Actions;

sub about {
      print STDERR "ABOUT\n";
}



# package to hold the raw Gtk Signal handlers setup
# manually or via GtkBuilder from xml
package App::Handlers;

use Data::Dumper;

sub on_response 
{
  my ($widget,$response) = @_;
  if ($response == -3 ) # accept
    {
      my $file = $widget->get_file;

      print STDERR "File: $file\n";

    }
}

sub onButton1 {

      print STDERR "clicked button1\n";

      my $native = Gtk::FileChooserNative->new ("Open File",
                                        $App::mainWindow,
                                        $Gtk::FileChooser::action_open,
                                        "_Open",
                                        "_Cancel");

     $native->signal_conect( "response", \&on_response);                                     
     $native->show;
}
    
sub onKeyPressed {

      print STDERR Dumper(\@_);
}
    
sub onClose {
      $App::quit = 1;
}

# package to hold the FIFO msg handlers
# send via named pipe from the actual
# debugger process
package App::Messages;
use base 'App::MsgHandler';

use Data::Dumper;

sub load :regex(([^,]+),(.*)) {

  print Dumper(\@_);
}

#################################
# the App
#################################

package App;

my $msg = "load filename,112";
App::MsgHandler::handle($msg);

our $quit = 0;
our $mainWindow;
our $widgets;

# init

sub init {

  my @widgetNames = qw(mainWindow edit);
  
  $widgets = App::Builder::build_ui("ui.xml", \@widgetNames);

  $mainWindow = $widgets->{mainWindow};

  App::Builder::add_actions( $mainWindow );
  App::Builder::add_key_controller( $mainWindow, 'key-pressed', 'onKeyPressed' );
  App::Builder::add_key_controller( $widgets->{edit}, "key-pressed", "onKeyPressed" );
    
  $mainWindow->show;
}

sub run {

  # the loop
  my $ctx = GLib::MainContext::default();

  while ( !$quit ) {

    # pump GTK events
    while ( $ctx->pending() ) {
        $ctx->iteration(0);
    }
  }
}

# intialize and run the app
init();
run();

    # pump debugger IPC messages
 #   my @msgs = $fifo->read();
  #  foreach my $msg (@msgs) {
   #     process_msg($msg);
    #}

