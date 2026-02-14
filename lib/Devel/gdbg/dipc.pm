package Devel::gdbg::dipc;

use v5.20;
use strict;
use utf8;
use POSIX qw(mkfifo);
use IO::Select;
use Encode;
use Fcntl;
use JSON;

##############################n###########
# bi-direktional IPC via a pair of
# named pipes
#
# both processed create a new dipc like
#
#     my $fifo = Devel::dipc->new();
#
# then create an input and output stream
# over a named pipe. in the parent process do:
#
#     $fifo->open_in("/tmp/my_read_pipe");
#     $fifo->open_out("/tmp/my_write_pipe");
#
# in the child process inverse the pipe
# names:
#
#     $fifo->open_in("/tmp/my_write_pipe");
#     $fifo->open_out("/tmp/my_read_pipe");
#
# note opening read will hang until the other
# side connected.
#
# one now has a bidrectional IPC pipe.
#
# writing is easy and uses blocking IO:
#
# 	  $fifo->write("my message");
#
# reading however is async and non-blocking.
# read clients are supposed to repeatingly
# call $fifo->read() , e.g. in an event or
# UI msg loop.
#
#     @msgs = $fifo->read();
#	  foreach my $msg ( @msgs) {
# 		#do something
#	  }
#
# read will not block and return an empty
# list if there are no msgs.
##########################################

sub new {

    my $class = shift;

    my $self = {
        state   => 0,
        buf     => undef,
        datalen => 0,
    };

    bless $self, $class;

    return $self;
}

sub write {

    my $self = shift;
    my $msg  = shift;

	my $json = JSON->new()->allow_blessed()->allow_unknown();

	my $jsondata = $json->utf8->encode($msg);
	my $data = $self->_encode_msg($jsondata);

    my $fh = $self->{fout};
    print $fh $data;
    $fh->flush();
}

sub read {

    my $self = shift;
    my $cb   = shift;

    $self->{msgs} = [];

    if ( $self->{select}->can_read(0) ) {
        if ( $self->{state} ) {
            $self->_read_data();
        }
        else {
            $self->_read_size();
        }
    }
    my @result = @{ $self->{msgs} };
    $self->{msgs} = [];
    return @result;
}

sub _read_size {
    my $self = shift;

    my $want = 4 - length( $self->{buf} );

    my $n = read( $self->{fin}, $self->{buf}, $want, length( $self->{buf} ) );
    if ( $n == 0 ) {

        return;
    }

    if ( length( $self->{buf} ) == 4 ) {

        $self->{datalen} = unpack( "N", $self->{buf} );
        $self->{buf}     = undef;
        $self->{state}   = 1;
        $self->_read_data();
    }
}

sub _read_data {
    my $self = shift;

    my $want = $self->{datalen} - length( $self->{buf} );

    my $n = read( $self->{fin}, $self->{buf}, $want, length( $self->{buf} ) );
    if ( $n == 0 ) {

        return;
    }

    if ( length( $self->{buf} ) == $self->{datalen} ) {

		my $msg = decode_json( $self->{buf} );

        $self->{buf}     = undef;
        $self->{datalen} = 0;
        $self->{state}   = 0;

        push @{ $self->{msgs} }, $msg;

        $self->_read_size();
    }
}

sub close {

    my $self = shift;
    if ( $self->{fout} ) {
        close( ( $self->{fout} ) );
    }
    if ( $self->{fin} ) {
        close( ( $self->{fin} ) );
    }
}

sub _encode_msg {
    my $self = shift;
    my $msg  = shift;
    my $str  = Encode::encode_utf8($msg);
    my $l    = length($str);
    my $r    = pack( "N", $l );
    $r .= $str;
    return $r;
}

sub open_out {

    my $self = shift;
    my $out  = shift;

    $self->_makemyfifo($out);

    open my $fh, ">>:raw", $out || die "can't open " . $out . ": $!";

    $self->{fout} = $fh;
}

sub open_in {

    my $self = shift;
    my $in   = shift;

    $self->_makemyfifo($in);

    my $fh;

	# note: O_RDWR and not O_RDONLY,
	# although there will be no writing ever
	# this is to keep the fifo alive
	# by having at least on 'writer'
    sysopen( $fh, $in, O_RDWR | O_NONBLOCK )
      || die "open " . $in . " failed: $!";

    $self->{fin} = $fh;

    my $select = IO::Select->new();
    $select->add($fh);

    $self->{select} = $select;
}

sub _makemyfifo {
    my $self = shift;
    my $path = shift;

	if( -e $path ) { return }

    mkfifo( $path, 0777 ) || die "mkfifo $path already exists: $!"; 
}

###############################################
package Devel::gdbg::dipc::RPC;
###############################################

# high level rpc abstraction to hide
# the fifo write complexitiy

sub new {

    my $class = shift;
	my $fifo  = shift;

    my $self = {
		fifo => $fifo,
    };

    bless $self, $class;

    return $self;	
}

sub DESTROY {
	
}

sub AUTOLOAD {

	no strict;
	my $self = shift;
	(my $method = $AUTOLOAD) =~ s{.*::}{};

	use strict;
	$self->{fifo}->write( { cmd => $method, params => [ @_ ] });
}


1;
