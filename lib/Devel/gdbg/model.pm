package Devel::gdbg::model;

use v5.20;
use utf8;
use strict;

use File::Basename;
use Sub::Util qw(subname);
use File::Basename;

my %msgHandlers;

sub process_msg {

	my $self  = shift;
    my $msg = shift;

	my $cmd = $msg->{cmd};
	my $params = $msg->{params} // [];

	if ( $cmd && exists $msgHandlers{$cmd}) {
		$msgHandlers{$cmd}->( $self, $params->@* );
	}
	else {
		print STDERR "unknown $cmd. ".Dumper($msg);
	}
}

sub MODIFY_CODE_ATTRIBUTES {
	my ($package,$coderef,@attrs) = @_;
	my $fun = subname($coderef);

	if ( $fun =~ /::([^:]+)$/ ) {
		$fun = $1;
	}

	for my $attr ( @attrs ) {

		if( $attr =~ /^RPC$/ ) {

			$msgHandlers{$fun} = $coderef;
		}
	}

	return ();
}

# prevent AUTLOAD to treat
# DESTROY as property
sub DESTROY {
	
}

sub AUTOLOAD {

	no strict;
	my $self = shift;

	(my $method = $AUTOLOAD) =~ s{.*::}{};

	# print STDERR "$method\n";
	# my ($package, $filename, $line) = caller;
	# print STDERR "$package, $filename $line\n";

	use strict;
	if(! exists $self->{$method} ) {
		die ("property $method does not exist.");
	}
	return $self->{$method};
}

1;
