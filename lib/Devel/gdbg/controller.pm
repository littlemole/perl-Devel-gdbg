
##################################################
package Devel::gdbg::controller;
##################################################

use v5.20;
use utf8;
use strict;

##################################################
# Perl dependencies
##################################################

use Data::Dumper;
use Sub::Util qw(subname);

our %simpleActions;
our %detailedActions;
our @accels;

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
	}

	return ();
}

1;
