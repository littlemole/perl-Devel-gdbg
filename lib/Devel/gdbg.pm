package Devel::gdbg;

use v5.20;
use utf8;
use strict;
use warnings;
use File::Basename;
use Data::Dumper;

our @ISA = qw();

our $VERSION = '0.01';

##################################################
# first things first: fork the UI process
##################################################

if ( !$ENV{"GDBG_NO_FORK"} ) {
    my $ui = dirname( $INC{"Devel/gdbg.pm"} ) . "/gdbgui.pl";

    my $pid = fork();
    if ( $pid == 0 ) {

        # child displays UI
        exec("perl $ui");
    }
}

##################################################
# all actual Debugger code is within package DB
##################################################
##################################################
package DB;
##################################################

use v5.20;
use utf8;
use strict;

##################################################
# Perl dependencies
##################################################

use Data::Dumper;
use PadWalker   qw(peek_my peek_our peek_sub closed_over);
use Cwd         qw(getcwd);
use File::Slurp qw(slurp write_file);
use File::Basename;

# shared lib for IPC between debugger and UI
use Devel::dipc;

##################################################
# Debugger globals
##################################################

my $skip        = 1;     # skip tracing functions
my $stepout     = 0;     # step out of function and resume debugger
my $stepover    = 0;     # step over flag
my $depth       = 0;     # depth of call chain
my $breakout    = 0;     # leave gtk loop to return to debugger
my $currentFile = "";    # the current file of debugging
my $currentLine = 0;     # the current line being debugged
my $started     = 0;     # debugging has started

##################################################
# caches
##################################################

my %breakpoints;         # remember breakpoint markers so we can delete 'em
my $lexicals;            # caches current lexicals (for eval)
my %files;               # map abs path -> filename as seen by debugger
my %postpone;            # postponed break points

##################################################
##################################################

# INT signal handler
sub dbint_handler {
    $DB::single = 1;

    # print "signalled\n" ;
}

$SIG{'INT'} = "DB::dbint_handler";

##################################################
# initialize
##################################################

my $fifo = Devel::dipc->new();

$fifo->open_out("/tmp/perl_debugger_finfo_out");

# send current working dir to UI
$fifo->write( "cwd " . getcwd() );

# send PID of current process to UI
$fifo->write( "pid " . $$ );

$fifo->open_in("/tmp/perl_debugger_finfo_in");

restoreBreakpoints();

# start tracing
$DB::trace = 1;

##################################################
# helpers
##################################################

sub find_file {

    my $file = shift;
    my $inc  = shift;
    if ( !$inc ) {
        $inc = \@INC;
    }

    if ( $file =~ /^\// ) {
        return $file;
    }

    foreach my $i (@$inc) {
        if ( -e "$i/$file" ) {
            return "$i/$file";
        }
    }

    if ( -e getcwd() . "/$file" ) {
        return getcwd() . "/$file";
    }
    return $file;
}

sub find_module {

    my $fun = shift;
    my $inc  = shift;

    if ( !$inc ) {
        $inc = \@INC;
    }

	$fun =~ s/::/\//g;
	my $module = dirname($fun);

	if($module eq "main") {
		$module = $0;
		if( $module =~ /^\// ) {
			return $module;
		}
		return getcwd()."/".$module;
	}

    foreach my $i (@$inc) {
        if ( -e "$i/$module.pm" ) {
            return "$i/$module.pm";
        }
    }
    foreach my $i (@$inc) {
        if ( -e "$i/$module.pl" ) {
            return "$i/$module.pl";
        }
    }

    if ( -e getcwd() . "/$module.pm" ) {
        return getcwd() . "/$module.pm";
    }

    if ( -e getcwd() . "/$module.pl" ) {
        return getcwd() . "/$module.pl";
    }


    return $fun;
}

sub restoreBreakpoints {

    if ( -e ".pgdbbrkpts" ) {
        my @lines = slurp(".pgdbbrkpts");
        foreach my $line (@lines) {
            if ( $line =~ /^([^:]+):([0-9]+)/ ) {
                my $file = $1;
                my $line = $2;
                if ( !$postpone{$file} ) {
                    $postpone{$file} = [];
                }
                push @{ $postpone{$file} }, $line;
            }
        }
    }
}

sub dumpBreakpoints {

    my @a;
    foreach my $key ( keys %breakpoints ) {
        push @a, $key . "\n";
    }
    write_file( ".pgdbbrkpts", @a );
}

##################################################
# logic
##################################################

# set a breakpoint
sub setBreakpoint {
    my $abspath = shift;
    my $line    = shift;

    my $filename = $files{$abspath};
    if ( !$filename ) {
        $filename = $abspath;
    }

    my $bpn = $abspath . ":" . $line;
    if ( !$breakpoints{$bpn} ) { 

        # check if file is already loaded by perl
        if ( hasdblines($filename) ) {
            # set a new breakpoint on this resp. the next breakable line, if any
            my $l = $line;
            while ( !checkdbline( $filename, $l ) ) { $l++; }
            $breakpoints{$bpn} = 1;
            setdbline( $filename, $l, 1 );
            $fifo->write("marker $abspath,$l");
        }
        else {
            # check if this is an existing postponed breakpoint
            if ( !$postpone{$abspath} ) {
                $postpone{$abspath} = [];
            }
            my $exists = 0;
            foreach my $p ( @{ $postpone{$abspath} } ) {
                if ( $p == $line ) {
                    $exists = 1;
                }
            }
            if (!$exists) {

                # set a new postponed breakpoint
                push @{ $postpone{$abspath} }, $line;
                $fifo->write("marker $abspath,$line");
            }
        }
    }
}

sub deleteBreakpoint {

    my $abspath = shift;
    my $line    = shift;

    my $filename = $files{$abspath};
    if ( !$filename ) {
        $filename = $abspath;
    }

    my $bpn = $abspath . ":" . $line;
    if ( $breakpoints{$bpn} ) {    

        if ( hasdblines($filename) ) {

            setdbline( $filename, $line, 0 );
        }
		delete $breakpoints{$bpn};
	}

	# remove any postponed breakpoints as well
	my $exists = 0;
	my @ps;    # all items from postponed array but $line
	foreach my $p ( @{ $postpone{$abspath} } ) {
		if ( $p == $line ) {
			$exists = 1;
		}
		else {
			push @ps, $p;
		}
	}
	if ($exists) {

		# postponed breakpoint exists, remove
		$postpone{$abspath} = \@ps;
	}
}

sub setPotponedBreakpoints {

	my $file = shift;

	my $pp = $postpone{$file};
	foreach my $p (@$pp) {

		# find breakpoint and update the UI
		my $l = $p;
		while ( !checkdbline( $file, $l ) ) { $l++; }
		my $bpn = $file . ":" . $l;
		$breakpoints{$bpn} = 1;
		setdbline( $file, $l, 1 );
		$fifo->write("marker $file,$l");
	}
	$postpone{$file} = [];
}

# update the info with current lexicals and call frame stack
sub updateInfo {

    my ( $package, $filename, $line ) = @_;

    my $abspath = find_file($filename);

    my $info = "# callstack:\n";
    for ( my $i = 1 ; $i < 25 ; $i++ ) {
        my ( $p2, $fn2, $ln2, $fun2 ) = caller $i + 1;
        if ( !$p2 ) { last; }
        $info .= "$fun2() [$p2]\n";
    }

    my $msg = $abspath . "," . $line . "," . $info;

    $fifo->write("info $msg");
}

# show lexicals
sub showLexicals {

    my ( $filename, $line ) = @_;

    my $abspath = $filename;

    my $h = peek_my(3);
    $lexicals = $h;
    my $info = "# lexicals:\n" . Dumper($h);
    $info =~ s/    / /gm;

    my $msg = $abspath . "," . $line . "," . $info;

    $fifo->write("lexicals $msg");
}

##################################################
# debug helpers to set perl magic vars
##################################################

# set a breakpoint
sub setdbline {
    my ( $fname, $lineno, $value ) = @_;

    # print "# set break at $fname:$lineno = $value\n";
    local (*dbline) = $main::{ '_<' . $fname };

    no strict;
    $dbline{$lineno} = $value;
}

# get a breakpoint
sub getdbline {
    my ( $fname, $lineno ) = @_;
    local (*dbline) = $main::{ '_<' . $fname };
    no strict;
    return $dbline{$lineno};
}

# get source line
sub getdbsrc {
    my ( $fname, $lineno ) = @_;
    local (*dbline) = $main::{ '_<' . $fname };
    no strict;
    return $dbline[$lineno];
}

# get full sources for a debugged file
sub dbdumpsrc {
    my ($fname) = @_;
    my $r = "";
    local (*dbline) = $main::{ '_<' . $fname };

    no strict;
    my $first = 1;
    foreach my $line (@dbline) {
        if ($line) {
            $r .= $line;
            if ($first) {

                $r =~ s/use Devel::gdbg;\n//gm;
            }
            $first = 0;
        }
    }
    return $r;
}

# check if file is already loaded by perl
sub hasdblines {
    my ($fname) = @_;
    if ( exists $main::{ '_<' . $fname } ) { return 1; }
    return 0;
}

# check if a line is breakable
sub checkdbline($$) {
    my ( $fname, $lineno ) = @_;

    return 0 unless $fname;    # we're getting an undef here on 'Restart...'

    local ($^W)     = 0;                          # spares us warnings under -w
    local (*dbline) = $main::{ '_<' . $fname };

    no strict;
    my $flag = $dbline[$lineno] != 0;

    return $flag;

}

# helper to set a breakpoint for a subroutine
sub brkonsub {

    my ($name) = shift;

    if ( !exists $DB::sub{$name} ) {
        print "No subroutine $name.  Try main::$name\n";
        return;
    }

    # file name will be in $1, start line $2, end line $3
    $DB::sub{$name} =~ /(.*):([0-9]+)-([0-9]+)$/o;
    for ( $2 .. $3 ) {
        next unless &checkdbline( $1, $_ );
        setdbline( $1, $_, 1 );
		return $_;
        last;
    }
	return -1;
}

sub getSubs {

	my @subs = keys %DB::sub;
	my @sorted = sort @subs;
	my $result = join("\n", @sorted);
	return $result;
}

##################################################
# messages from ui
##################################################

my @msg_handlers = (
	{
		regex => qr/^quit$/s,
		handler => \&msg_quit
	},
	{
		regex => qr/^step$/s,
		handler => \&msg_step
	},
	{
		regex => qr/^lexicals$/s,
		handler => \&msg_lexicals
	},
	{
		regex => qr/^continue$/s,
		handler => \&msg_continue
	},
	{
		regex => qr/^next$/s,
		handler => \&msg_next
	},
	{
		regex => qr/^return$/s,
		handler => \&msg_return
	},
	{
		regex => qr/^eval (.*)$/ms,
		handler => \&msg_eval
	},
	{
		regex => qr/^breakpoint ([^,]+),([0-9]+)$/s,
		handler => \&msg_breakpoint
	},
	{
		regex => qr/^delbreakpoint ([^,]+),([0-9]+)$/s,
		handler => \&msg_delbreakpoint
	},
	{
		regex => qr/^functionbreak (.*)/s,
		handler => \&msg_functionbreak
	},
	{
		regex => qr/^breakpoints$/s,
		handler => \&msg_breakpoints
	},
	{
		regex => qr/^functions$/s,
		handler => \&msg_functions
	},
);

sub msg_quit {

	dumpBreakpoints();
	$fifo->close();
	POSIX::_exit(0);
}

sub msg_step {

	$breakout   = 1;
	$DB::single = 1;
}

sub msg_lexicals {

#	showLexicals( $currentFile, $currentLine );
#    my $abspath = $filename;

    my $h = peek_my(3);
    $lexicals = $h;
    my $info = "# lexicals:\n" . Dumper($h);
    $info =~ s/    / /gm;

    my $msg = $currentFile . "," . $currentLine . "," . $info;

    $fifo->write("lexicals $msg");

}

sub msg_continue {

	$breakout   = 1;
	$DB::single = 0;
}

sub msg_next {

	$breakout   = 1;
	$DB::single = 0;
	$stepover   = $depth + 1;
}

sub msg_return {

	if ( $depth > 0 ) {
		$breakout   = 1;
		$stepout    = 1;
		$DB::single = 0;
	}
	else {
		$breakout   = 1;
		$DB::single = 0;
		$stepover   = $depth + 1;
	}
}

sub msg_eval {

	my $r = eval($1);
	if ($@) {
		$r = $@;
	}
	$fifo->write("eval $r");
}

sub msg_breakpoint {

	my $file = $1;
	my $line = $2;
	setBreakpoint( $file, $line );
}

sub msg_delbreakpoint {

	my $file = $1;
	my $line = $2;
	deleteBreakpoint( $file, $line );
}

sub msg_functionbreak {

	my $fun = $1;
	my $file = find_module($fun);
	my $line = brkonsub($fun);
	$fifo->write("file $file,$line");
	$fifo->write("marker $file,$line");
	$breakpoints{"$file:$line"} = 1;
}

sub msg_breakpoints {

	my $data = "# set breakpoints:\n";
	foreach my $bp ( keys %breakpoints ) {
		$data .= $bp . "\n";
	}
	$data .= "\n# postponed breakpoints\n";
	foreach my $key ( keys %postpone ) {
		my $p = $postpone{$key};
		foreach my $line (@$p) {
			$data .= $key . ":" . $line . "\n";
		}
	}
	$fifo->write( "breakpoints " . $data ."\n");
}

sub msg_functions {

	my $subs = getSubs();
	$fifo->write("subs $subs");
}

sub process_msg {

    my $msg = shift;

    #print "MSG: $msg\n";

	foreach my $handler ( @msg_handlers) {
		if ( $msg =~ $handler->{regex} ) {
			$handler->{handler}->();
			return;
		}
	}


    # if ( $msg eq "step" ) {    # single step

    #     $breakout   = 1;
    #     $DB::single = 1;
    # }
    # elsif ( $msg eq "quit" ) {    # quit
    #     dumpBreakpoints();
    #     $fifo->close();
    #     POSIX::_exit(0);
    # }
    # elsif ( $msg eq "lexicals" ) {    # show lexicals
    #     showLexicals( $currentFile, $currentLine );
    # }
    # elsif ( $msg eq "continue" ) {    # continue

    #     $breakout   = 1;
    #     $DB::single = 0;
    # }
    # elsif ( $msg eq "next" ) {    # step oover

    #     $breakout   = 1;
    #     $DB::single = 0;
    #     $stepover   = $depth + 1;
    # }
    # elsif ( $msg eq "return" ) {    # step out of current function

    #     if ( $depth > 0 ) {
    #         $breakout   = 1;
    #         $stepout    = 1;
    #         $DB::single = 0;
    #     }
    #     else {
    #         $breakout   = 1;
    #         $DB::single = 0;
    #         $stepover   = $depth + 1;
    #     }
    # }
    # elsif ( $msg =~ /^eval (.*)/ ) {    # eval-uate code at current pos

    #     my $r = eval($1);
    #     if ($@) {
    #         $r = $@;
    #     }
    #     $fifo->write("eval $r");
    # }
    # elsif ( $msg =~ /^breakpoint ([^,]+),([0-9]+)/ ) {    # set breakpoint

    #     my $file = $1;
    #     my $line = $2;
    #     setBreakpoint( $file, $line );
    # }
    # elsif ( $msg =~ /^functionbreak (.*)/ ) {    # set breakpoint at fun

    #     my $fun = $1;
	# 	my $file = find_module($fun);
	# 	my $line = brkonsub($fun);
	# 	$fifo->write("file $file,$line");
	# 	$fifo->write("marker $file,$line");
	# 	$breakpoints{"$file:$line"} = 1;
    # }
    # elsif ( $msg eq "breakpoints" ) {                     # dump brakpoint info

    #     my $data = "# set breakpoints:\n";
    #     foreach my $bp ( keys %breakpoints ) {
    #         $data .= $bp . "\n";
    #     }
    #     $data .= "\n# postponed breakpoints\n";
    #     foreach my $key ( keys %postpone ) {
    #         my $p = $postpone{$key};
    #         foreach my $line (@$p) {
    #             $data .= $key . ":" . $line . "\n";
    #         }
    #     }
    #     $fifo->write( "breakpoints " . $data ."\n");
    # }
	# elsif ( $msg eq "functions" ) {
	# 	my $subs = getSubs();
	# 	$fifo->write("subs $subs");
	# }

}

##################################################
# THE actual debugger
##################################################

# called from Perl for every breakable line

sub DB {


	$skip = 1;
    my ( $package, $filename, $line ) = caller;
	my ( $p, $fn, $ln, $fun ) = caller 1;

# print "SINGLE ".$DB::single." $filename:$line $fun\n";
    my $abspath    = find_file($filename);
    my $isBrkPoint = getdbline( $filename, $line );

    $currentLine = $line;

    # allow function tracing. see DB::sub below
    $skip = 0;

    # always skip over ourselves
    if ( $fun && $fun =~ /^Devel::/ ) {
      return;
    }
    if ( $filename =~ /Devel\/gdbg.pm$/ ) {
		return;
    }


    # check if we are done stepping over a line
    if ( $depth < $stepover ) {
        $stepover   = 0;
        $DB::single = 1;
    }

    if ($isBrkPoint) {
        $DB::single = 1;    # set debugger to single step
    }

    # if we are single stepping, update the UI
    if ($DB::single) {

        $started = 1;

        # special handling for eval "" code
        if ( $filename =~ /\(eval / ) {

            my $src = dbdumpsrc($filename);
            $files{$filename} = $filename;
            $fifo->write("load $filename,$src");
            $abspath = $filename;
        }

        # move UI to current file:line
        $fifo->write("file $abspath,$line");

        if ( $currentFile ne $filename ) {

            # file being debugged has changed
            # update the displayed file
            $currentFile = $filename;
        }

        # update the info pane call frame stack
        updateInfo( $package, $filename, $line );

        # run the message loop now until users
        # invokes an action that breaks the debugger

        # check if we have postponed breakpoints
		foreach my $key ( keys %postpone) {

			# if we now! have source data for this file
			if( hasdblines($key) ) {

				setPotponedBreakpoints($key);
			}
        }

        $skip = 1;    # diable function tracing
        # pump messages from UI
        while ( !$breakout ) {

            my @msgs = $fifo->read( \&process_msg );
            foreach my $msg (@msgs) {
                process_msg($msg);
            }
        }
        $breakout = 0;

        $skip = 0;    # re-enable function tracing

    }
}

# called from Perl when entering a function to trace function calls

sub sub {

    if ($skip) {    # if skip flag is set skip any tracing
        no strict;
        return &$sub;
    }

    no strict;
    if ( $sub =~ /^Devel::/ ) {    # never trace the Devel:: stuff
        return &$sub;
    }
    use strict;

    # increase depth counter
    $depth++;

    # following depends on function context (list,scalar,void)

    if (wantarray) {               # array ctx
        no strict;

        # actually call the function
        my @r = &$sub;
        use strict;
        if ($stepout) {

            # if stepout flag was set,
            # switch back to single stepping
            # right after the function call
            $stepout    = 0;
            $DB::single = 1;
        }
        $depth--;
        return @r;
    }
    elsif ( defined wantarray ) {    # scalar ctx

        no strict;

        # actually call the function
        my $r = &$sub;
        use strict;
        if ($stepout) {

            # if stepout flag was set,
            # switch back to single stepping
            # right after the function call
            $stepout    = 0;
            $DB::single = 1;
        }
        $depth--;
        return $r;
    }
    else {    # void ctx
        no strict;

        # actually call the function
        &$sub;
        use strict;
        if ($stepout) {

            # if stepout flag was set,
            # switch back to single stepping
            # right after the function call
            $stepout    = 0;
            $DB::single = 1;
        }
        $depth--;
    }
}

# called from Perl in debug mode, once files have been loaded
sub postponed {

    $skip = 1;    # disable function tracing

    my $id = shift;
    $id =~ /::_<(.*)/;
    my $file = $1;

    my $abspath = find_file($file);
    $files{$abspath} = $file;

    # print "LOAD: $file -> $abspath\n";

    if ( $file !~ /Devel\/gdbg.pm$/ ) {    # do not trace ourselves

        # send source for file to UI
        my $src = dbdumpsrc($file);
        $fifo->write("load $abspath,$src");

        # check if we have postponed breakpoints
        if ( $postpone{$abspath} ) {

			setPotponedBreakpoints($abspath);
        }
    }

    $skip = 0;    # re-enable function tracing
}

##################################################
# over and out
##################################################

END {

    $skip       = 1;
    $DB::single = 0;
    $DB::trace  = 0;

    dumpBreakpoints();

    $fifo->write("quit");

    POSIX::_exit(0);
    $skip = 0;
}

1;

__END__
# Below is stub documentation for your module. You'd better edit it!

=head1 NAME

Devel::gdbg - Perl Debugger using Gtk on Linux

=head1 SYNOPSIS

> perl -d:gdbg yourscript.pl

=head1 DESCRIPTION

Implements a simple visual GTK based debugger for perl scripts.

By default invoking the debugger will fork a UI process.
Prevent this with setting ENV variable GDBG_NO_FORK, in which
case it is needed to start the UI backend manualle beforehand
using:

> /usr/local/share/perl/5.34.0/Devel/gbdgui.pl

(or wherever you installed Devel::gbgb)

and then start the to be debugged program like so:

> GDBG_NO_FORK=1 perl -d:gdbg yourscript.pl



=head1 SEE ALSO


=head1 AUTHOR

littlemole@oha7.org

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2024 by littlemole

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.34.0 or,
at your option, any later version of Perl 5 you may have available.


=cut
