package Devel::gdbg;

use v5.20;
use utf8;
use strict;
use warnings;
use File::Basename;
use Data::Dumper;
use JSON;

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
		# $^X is the path to current perl executable
        exec("$^X $ui");
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
use Cwd         qw(getcwd abs_path);
use File::Slurp qw(slurp write_file);
use File::Basename;
use Storable qw(dclone);
use JSON;
use Params::Util qw<_HASH _HASH0 _HASHLIKE _ARRAYLIKE>;

# shared lib for IPC between debugger and UI
use Devel::dipc;

##################################################
# Debugger globals
##################################################

my $stepout     = 0;     # step out of function and resume debugger
my $stepover    = 0;     # step over flag
my $depth       = 0;     # depth of call chain
my $breakout    = 0;     # leave gtk loop to return to debugger
my $currentFile = "";    # the current file of debugging
my $currentLine = 0;     # the current line being debugged

##################################################
# caches
##################################################

my %breakpoints;         # remember breakpoint markers so we can delete 'em
my $lexicals;            # caches current lexicals (for eval)
my %files;               # map abs path -> filename as seen by debugger
my %postpone;            # postponed break points
my $lastSelection = '';

##################################################
##################################################

# INT signal handler
sub dbint_handler {
    $DB::single = 1;
}

$SIG{'INT'} = "DB::dbint_handler";

##################################################
# initialize IPC
##################################################

my $fifo_dir = $ENV{"GDBG_FIFO_DIR"} || '/tmp/';

my $fifo = Devel::dipc->new();

$fifo->open_out("$fifo_dir/perl_debugger_fifo_out");
$fifo->open_in("$fifo_dir/perl_debugger_fifo_in");

my $rpc = Devel::dipc::RPC->new($fifo);
$rpc->cwd( getcwd() );
$rpc->pid( $$ );

restoreBreakpoints();

# start non tracing
$DB::trace = 0;

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
        my $r = $file;
		return $r;
    }

    foreach my $i (@$inc) {
        if ( -e "$i/$file" ) {
        	my $r = "$i/$file";
			return $r;
        }
    }

    if ( -e getcwd() . "/$file" ) {
        my $r = getcwd() . "/$file";
		return $r;
    }
    my $r = $file;
	return $r;
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


##################################################
# breakpoint handling
##################################################

sub setBreakpointOnNextBreakableLine {

	my $abspath = shift;
	my $line = shift;

	# get short filename, if any
    my $filename = $files{$abspath};
    if ( !$filename ) {
        $filename = $abspath;
    }

	# starting at the desired line
	my $l = $line;
	while(1) {
		# check if this line is breakable
		my $r = checkdbline( $filename, $l );
		if($r == -1) {
			# error, no lines
			return -1;
		}
		elsif ( $r == 1 ) {
			# found suitable line to break
			last;
		}
		# try next line
		$l++;
	}

	# set and remember the breakpoint
    my $bpn = $abspath . ":" . $l;
	$breakpoints{$bpn} = 1;
	setdbline( $filename, $l, 1 );
	return $l;
}

# set a breaktpoint
sub setBreakpoint {
    my $abspath = shift;
    my $line    = shift;

    my $filename = $files{$abspath};
    if ( !$filename ) {
        $filename = $abspath;
    }

	# only if we do not have a breakpoint yet
    my $bpn = $abspath . ":" . $line;
    if ( !$breakpoints{$bpn} ) { 

        # check if file is already loaded by perl
        if ( hasdblines($filename) ) {

            # set a new breakpoint on this resp. the next breakable line, if any
            my $l = setBreakpointOnNextBreakableLine($abspath,$line);
			if($l != -1 ) {

				getBreakpointsForFile($abspath);
			}
        }
        else {

            if ( !exists $postpone{$abspath} ) {
				$postpone{$abspath} = [];
			}
            # check if this is an existing postponed breakpoint
			my $exists = 0;
			foreach my $p ( @{ $postpone{$abspath} } ) {
				if ( $p == $line ) {
					$exists = 1;
				}
			}
			if (!$exists) {

				# set a new postponed breakpoint
				push @{ $postpone{$abspath} }, $line;
				getBreakpointsForFile($abspath);
			}
			else {
				deleteBreakpoint($abspath,$line);
			}
        }
    }
	else {
		deleteBreakpoint($abspath,$line);
	}
}

# delete a breaktpoint
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

		$postpone{$abspath} = \@ps;
	}
	getBreakpointsForFile($abspath);
}

# helper to set a postponed breakpoint
# after source file was loaded by perl
sub setPotponedBreakpoints {

	my $file = shift;

	my $pp = $postpone{$file};	
	if(!$pp) {
		$pp = $postpone{abs_path($file)};
		if(!$pp) {
			return;
		}
	}

	my $update = 0;

	foreach my $p (@$pp) {

		# find breakpoint and update the UI
		my $l = setBreakpointOnNextBreakableLine($file,$p);
		if($l != -1 ) {

			$update = 1;
		}
	}
	delete $postpone{$file};
	
	if($update) {

		getBreakpointsForFile($file);	
	}
}

# helper to send current breakpoints
# for a file to UI client

sub getBreakpointsForFile {
	my $file = shift;
	
	my @lines;
	foreach my $key ( %breakpoints ) {
		if( $key =~ /^$file:(.*)$/ ) {
			my $line = $1;
			push @lines, $line;
		}
	}

	foreach my $line ( $postpone{$file}->@* ) {
		
		push @lines, $line;
	}

	my $lines = join ',' , @lines;
	$lines = $lines // '';

	$rpc->setbreakpoints($file,$lines);
}

# helper to load persited breakpoints from disk
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

# helper to persist breakpoints on disk
sub dumpBreakpoints {

    my @a;
    foreach my $key ( keys %breakpoints ) {
        push @a, $key . "\n";
    }
    write_file( ".pgdbbrkpts", @a );
}

# oh boy, that was a lot of breakpoint handling

##############################################
# update the call frame stack info
##############################################

sub updateInfo {

    my ( $package, $filename, $line ) = @_;

	# extract urrent function
	my ( $p, $f, $lx, $fun ) = caller 2;

	$fun //= '';

	# callstack with initial entry (current fun)
	my @stack = (
		 "# callstack:",
		"$fun() [$filename:$line]"
	);

	# walk a bit of the call stack
    for ( my $i = 1 ; $i < 25 ; $i++ ) {

		# the callers frame
        my ( $p1, $fn1, $ln1, $fun1 ) = caller $i + 1;
		# and the caller callers frame
        my ( $p2, $fn2, $ln2, $fun2 ) = caller $i + 2;

		# we are probably done
        if ( !$p1 ) { 
			last; 
		}

		# might be empty (like in main)
		$fun1 //= '';
		$fun2 //= '';
		
		# add one line to the call stack
		# note the function is fun2!
		my $f = abs_path($fn1) || $fn1;
        push @stack, "$fun2() [$f:$ln1]";
    }

	my $info = join "\r\n", @stack;

	# update the UI
    my $abspath = abs_path(find_file($filename));

	$rpc->info( $abspath,$line,$info );
}

##################################################
# UI var inspector support
##################################################

# helper to truncate values displayed in UI tree
sub truncate_lex {
	my $value = shift;
	
	$value = defined $value ? $value : 'undef';
	$value = "$value";

	return substr($value,0,255);
}

# helper to de-reference a Perl data structure
# suitable to be serialized as JSON and then
# displayed as a UI tree
sub deref {

	my ($source,$start,$target) = shift;

	my $ref_type = ref $source;
	if(!$ref_type) {
		if( ref \$source eq 'GLOB') {
			return {
				type  => "GLOB",
				value => '*',
			};
		}
		return {
			type  => "SCALAR",
			value => truncate_lex( $source ),
		};
	}
	elsif(_ARRAYLIKE($source)) {
		my @result;
		foreach my $item ( $source->@* ) {
			push @result, deref($item,$target);
		}
		return {
			type  => $ref_type,
			value => \@result,
		};
	}
	elsif(_HASHLIKE($source)) {
		my %result;
		foreach my $item ( keys $source->%* ) {
			$result{$item} = deref($source->{$item},$target);
		}
		return {
			type  => $ref_type,
			value => \%result,
		};
	}
	elsif($ref_type eq 'CODE') {
		return {
			type  => "CODE",
			value => '<>',
		};

	}
	elsif($ref_type eq 'FORMAT') {
		return {
			type  => "FORMAT",
			value => truncate_lex( $source )
		};
		
	}
	elsif($ref_type eq 'IO') {
		return {
			type  => "IO",
			value => '<>',
		};
		
	}
	elsif($ref_type eq 'SCALAR') {
		return {
			type  => "REF",
			value => truncate_lex( ${$source} ),
		};
		
	}
	elsif($ref_type eq 'VSTRING') {
		return {
			type  => "VSTRING",
			value => truncate_lex( $source ),
		};
		
	}
	elsif($ref_type eq 'GLOB') {
		return {
			type  => "GLOB",
			value => '<>',
		};
		
	}
	elsif($ref_type eq 'LVALUE') {
		return {
			type  => "LVALUE",
			value => truncate_lex( $source ),
		};
		
	}
	elsif($ref_type eq 'REGEXP') {
		return {
			type  => "REGEXP",
			value => truncate_lex( $source ),
		};
		
	}
	elsif($ref_type eq 'REF') {
		return deref( ${$source});
	}
	return {
		type  => $ref_type,
		value => undef,
	};
}

# helper to find a chlld node,
# used if expanding a tree node in the UI
sub find_lex_src {

	my ($source,$target) = @_;

	if (ref $source eq 'REF') {
		$source = ${$source};
	}
	
	if( (scalar @$target) == 0 ) {
		return $source;
	}

	my $p = shift $target->@*;

	if(_HASHLIKE($source)) {
		my $i = $source->{$p};
		return find_lex_src( $i, $target );
	}
	elsif(_ARRAYLIKE($source)) {
		my $i = $source->[$p];
		return find_lex_src( $i, $target );
	}
	else {
		return;
	}

	return;
}

# expand a lexical variable
sub expand_lex {
	my ($source,$isRoot,$level) = @_;

	$level = $level // 0;

	my $ref_type = ref $source;
	if(!$ref_type) {
		if( ref \$source eq 'GLOB') {
			return {
				type  => "GLOB",
				value => '*',
			};
		}
		return {
			type  => "SCALAR",
			value => truncate_lex( $source ),
		};
	}
	elsif(_ARRAYLIKE($source)) {
		if($level == 0 || $isRoot) {
			my @result;
			my $c = 0;
			foreach my $item ( $source->@* ) {
				my $expand = 0;
				if(index($lastSelection,"$c") == 0) {
					$expand = 1;
				}
				push @result, expand_lex($item,$isRoot,$level+1);
				$c++;
			}
			return {
				type  => $ref_type,
				value => \@result,
			};
		}
		return {
			type  => $ref_type,
			placeholder => 1,
		};
	}
	elsif(_HASHLIKE($source)) {
		if($level == 0 || $isRoot ) {
			my %result;
			foreach my $item ( keys $source->%* ) {
				my $expand = '';
				if(index("/$lastSelection","$isRoot/$item") == 0) {
					$expand = "$isRoot/$item";
				}
				$result{$item} = expand_lex($source->{$item},$expand,$level+1);
			}
			return {
				type  => $ref_type,
				value => \%result,
			};
		}
		return {
			type  => $ref_type,
			placeholder => 1,
		};
	}
	elsif($ref_type eq 'CODE') {
		return {
			type  => "CODE",
			value => '<>',
		};

	}
	elsif($ref_type eq 'FORMAT') {
		return {
			type  => "FORMAT",
			value => truncate_lex( $source ),
		};
		
	}
	elsif($ref_type eq 'IO') {
		return {
			type  => "IO",
			value => '<>',
		};
		
	}
	elsif($ref_type eq 'SCALAR') {
		return {
			type  => "REF",
			value => truncate_lex( ${$source} ),
		};
		
	}
	elsif($ref_type eq 'VSTRING') {
		return {
			type  => "VSTRING",
			value => truncate_lex( $source )
		};
		
	}
	elsif($ref_type eq 'GLOB') {
		return {
			type  => "GLOB",
			value => '<>',
		};
		
	}
	elsif($ref_type eq 'LVALUE') {
		return {
			type  => "LVALUE",
			value => truncate_lex( $source )
		};
		
	}
	elsif($ref_type eq 'REGEXP') {
		return {
			type  => "REGEXP",
			value => truncate_lex( $source )
		};
		
	}
	elsif($ref_type eq 'REF') {
		return expand_lex( ${$source}, $isRoot, $level);
	}
	return {
		type  => $ref_type,
		value => undef,
	};

}

##################################################
# debug helpers to set perl debugger magic vars
##################################################

# set a breakpoint
sub setdbline {
    my ( $fname, $lineno, $value ) = @_;

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

	if( !defined *dbline) {
		return -1;
	}

    no strict;
    my $flag = $dbline[$lineno] != 0;

    return 1;
}

# get the start line of a FQ sub name from perl
sub getSubLine {

    my ($name) = shift;

    if ( !exists $DB::sub{$name} ) {
        # print "No subroutine $name.  Try main::$name\n";
        return;
    }

    # file name will be in $1, start line $2, end line $3
    $DB::sub{$name} =~ /(.*):([0-9]+)-([0-9]+)$/o;
	return $2;
}

# fetch all known subs known to perl and output FQ sub names
sub getSubs {

	my @subs = keys %DB::sub;
	my @sorted = sort @subs;
	my $result = join("\n", @sorted);
	return $result;
}


##################################################
# process messages from ui
##################################################

my %msg_handlers = (
	init => sub {
		# send current working dir to UI
		$rpc->cwd( getcwd() );

		# send PID of current process to UI
		$rpc->pid( $$ );
	},
	quit => sub {

		dumpBreakpoints();
		if(!$ENV{"GDBG_NO_FORK"}) {			
			$fifo->close();
			POSIX::_exit(0);
		}
	},
	step => sub {
		# single step (into)
		$breakout   = 1;
		$DB::single = 1;
		$DB::trace  = 1;
	},
	continue => sub {
		# continue to next breakpoint

		$breakout   = 1;
		$DB::single = 0;
		$DB::trace  = 0;
	},
	next => sub {
		# single step (over)
		$breakout   = 1;
		$DB::single = 0;
		$DB::trace  = 1;
		$stepover   = $depth + 1;
	},
	return => sub {
		# (single) step out of function
		$DB::trace = 1;
		$breakout   = 1;
		$DB::single = 0;
		$stepover   = $depth;
	},
	eval => sub {
		# eval in current context
		my $r = eval(shift);
		if ($@) {
			$r = $@;
		}
		$rpc->eval( $r );
	},
	lexicals => sub {
		# show lexicals
		my $h = peek_my(3);
		my $info = Dumper($h);
		$info =~ s/    / /gm;

		$rpc->lexicals( $currentFile,$currentLine,$info );
	},
	jsonlexicals => sub {
		# show lexicals JSON

		my $target = shift;
		my $source = peek_my(3);

		$target =~ s/^\///;
		$target =~ s/\/$//;

		my @t = split( /\//, $target );

		if( scalar @t != 0) {
			$lastSelection = $target;
		}

		my $result = find_lex_src($source,\@t);
		my $data = {};

		if($result) {

			$data = expand_lex($result,'');
		}

		my $json = JSON->new()->allow_blessed()->allow_unknown();
		my $info = $json->utf8->encode($data);
		my $msg = $currentFile . ",/" . $target . "," . $info;

		$rpc->jsonlexicals( $currentFile, "/$target", $info );
	},
	breakpoint => sub {
		my $file = shift;
		my $line = shift;
		setBreakpoint( $file, $line );
	},
	functionbreak => sub {
		# lookup source position of fq function

		my $fun = shift;
		my $file = find_module($fun);
		my $line = getSubLine($fun);

		$rpc->show( $file, $line );
		getBreakpointsForFile($file);
	},
	storebreakpoints => sub {
		# dump breakpoints for display
		dumpBreakpoints();
	},
	breakpoints => sub {
		# dump breakpoints for display

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
		$rpc->breakpoints( $data );
	},
	functions => sub {
		# dump list of fq function names

		my $subs = getSubs();
		$rpc->subs( $subs );
	},
	fetch => sub {
		# fetch line,file

		my $file = shift;
		my $line = shift;

		if($files{$file}) {

			my $src = dbdumpsrc($files{$file});
			$rpc->load($file,$line,$src);
		}
		elsif( -e $file ) {
				
			my $abspath = abs_path(find_file($file));
			my$src = slurp($abspath);
			$rpc->load($file,$line,$src);
		}
	},
);

# process a single msg from UI via its handler
sub process_msg {

    my $msg = shift;

	my $cmd    = $msg->{cmd};
	my $params = $msg->{params};
	
	if($cmd && exists $msg_handlers{$cmd} ) {

		$msg_handlers{$cmd}->( $params->@* )
	}
	else {
		print STDERR "unknown $cmd. ".Dumper($msg);
	}
}

##################################################
# the actual debugger
##################################################

# called from Perl for every breakable line

sub DB {

    my ( $package, $filename, $line ) = caller;

    my $abspath = abs_path(find_file($filename));

    if ( $depth < $stepover ) {
        $stepover   = 0;
        $DB::single = 1;
        $DB::trace  = 0;
    }

	# check if we have breakpoint
    my $isBrkPoint = getdbline( $filename, $line );
    if ($isBrkPoint) {
        $DB::single = 1;    # set debugger to single step
    }

	# process a bunch of msgs, if any
	my @msgs = $fifo->read( \&process_msg );
	foreach my $msg (@msgs) {
		process_msg($msg);
	}

    # if we are single stepping, update the UI
    if ($DB::single) {

		# file being debugged has changed
		# update the file to display
		$currentFile = $filename;
		$currentLine = $line;

        # special handling for eval "" code
        if ( $filename =~ /\(eval / ) {

            $files{$filename} = $filename;
            $abspath = $filename;
        }

        # move UI to current file:line
		$rpc->file( $abspath, $line );
		getBreakpointsForFile($abspath);

        # update the info pane call frame stack
        updateInfo( $package, $filename, $line );

        # check if we have postponed breakpoints
		foreach my $key ( keys %postpone) {

			# if we now! have source data for this file
			if( hasdblines($key) ) {

				setPotponedBreakpoints($key);
			}
        }

        # run the message loop now until users
        # invokes an action that advances the debugger

		# pump messages from UI
        do {

            my @msgs = $fifo->read( \&process_msg );
            foreach my $msg (@msgs) {
                process_msg($msg);
            }
        } while ( !$breakout );
		
        $breakout = 0;
    }
}

# called from Perl when entering a function to trace function calls

sub sub {

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

    my $id = shift;
    $id =~ /::_<(.*)/;
    my $file = $1;

	my $path = abs_path(find_file($file));
	if(!$path) {
		return;
	}

    $files{$path} = $file;

    if ( $file !~ /Devel\/gdbg.pm$/ ) {    # do not trace ourselves

        if ( $postpone{$path} ) {

			setPotponedBreakpoints($path);
        }
    }
}

##################################################
# over and out
##################################################

END {

    $DB::single = 0;
    $DB::trace  = 0;

    dumpBreakpoints();

	$rpc->quit();

    POSIX::_exit(0);
}

1;

__END__

=head1 NAME

Devel::gdbg - Perl Debugger using Gtk

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

Copyright (C) 2024-2026 by littlemole

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.34.0 or,
at your option, any later version of Perl 5 you may have available.


=cut
