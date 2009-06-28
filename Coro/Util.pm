=head1 NAME

Coro::Util - various utility functions.

=head1 SYNOPSIS

 use Coro::Util;

=head1 DESCRIPTION

This module implements various utility functions, mostly replacing perl
functions by non-blocking counterparts.

This module is an AnyEvent user. Refer to the L<AnyEvent>
documentation to see how to integrate it into your own programs.

=over 4

=cut

package Coro::Util;

no warnings;
use strict;

use Socket ();

use AnyEvent ();
use AnyEvent::Socket ();

use Coro::State;
use Coro::Handle;
use Coro::Storable ();
use Coro::Semaphore;

use base 'Exporter';

our @EXPORT = qw(gethostbyname gethostbyaddr);
our @EXPORT_OK = qw(inet_aton fork_eval);

our $VERSION = 5.14;

our $MAXPARALLEL = 16; # max. number of parallel jobs

my $jobs = new Coro::Semaphore $MAXPARALLEL;

sub _do_asy(&;@) {
   my $sub = shift;
   $jobs->down;
   my $fh;

   my $pid = open $fh, "-|";

   if (!defined $pid) {
      die "fork: $!";
   } elsif (!$pid) {
      syswrite STDOUT, join "\0", map { unpack "H*", $_ } &$sub;
      Coro::State::_exit 0;
   }

   my $buf;
   my $wakeup = Coro::rouse_cb;
   my $w; $w = AnyEvent->io (fh => $fh, poll => 'r', cb => sub {
      sysread $fh, $buf, 16384, length $buf
         and return;

      undef $w;
      $wakeup->();
   });

   Coro::rouse_wait;

   $jobs->up;
   my @r = map { pack "H*", $_ } split /\0/, $buf;
   wantarray ? @r : $r[0];
}

=item $ipn = Coro::Util::inet_aton $hostname || $ip

Works almost exactly like its AnyEvent::Socket counterpart, except that it
does not block other coroutines and returns the results.

=cut

sub inet_aton {
   AnyEvent::Socket::inet_aton $_[0], Coro::rouse_cb;
   my @res = Coro::rouse_wait;
   wantarray ? @res : $res[0]
}

=item gethostbyname, gethostbyaddr

Work similarly to their perl counterparts, but do not block. Uses
C<Anyevent::Util::inet_aton> internally.

=cut

sub gethostbyname($) {
   my @res = inet_aton $_[0];

   ($_[0], $_[0], &Socket::AF_INET, 4, map +(format_ip $_), grep length == 4, @res)
}

sub gethostbyaddr($$) {
   _do_asy { gethostbyaddr $_[0], $_[1] } @_
}

=item @result = Coro::Util::fork_eval { ... }, @args

Executes the given code block or code reference with the given arguments
in a separate process, returning the results. The return values must be
serialisable with Coro::Storable. It may, of course, block.

Note that using event handling in the sub is not usually a good idea as
you will inherit a mixed set of watchers from the parent.

Exceptions will be correctly forwarded to the caller.

This function is useful for pushing cpu-intensive computations into a
different process, for example to take advantage of multiple CPU's. Its
also useful if you want to simply run some blocking functions (such as
C<system()>) and do not care about the overhead enough to code your own
pid watcher etc.

This function might keep a pool of processes in some future version, as
fork can be rather slow in large processes.

Example: execute some external program (convert image to rgba raw form)
and add a long computation (extract the alpha channel) in a separate
process, making sure that never more then $NUMCPUS processes are being
run.

   my $cpulock = new Coro::Semaphore $NUMCPUS;

   sub do_it {
      my ($path) = @_;

      my $guard = $cpulock->guard;

      Coro::Util::fork_eval {
         open my $fh, "convert -depth 8 \Q$path\E rgba:"
            or die "$path: $!";

         local $/;
         # make my eyes hurt
         pack "C*", unpack "(xxxC)*", <$fh>
      }
   }

   my $alphachannel = do_it "/tmp/img.png";

=cut

sub fork_eval(&@) {
   my ($cb, @args) = @_;

   pipe my $fh1, my $fh2
      or die "pipe: $!";

   my $pid = fork;

   if ($pid) {
      undef $fh2;

      my $res = Coro::Storable::thaw +(Coro::Handle::unblock $fh1)->readline (undef);
      waitpid $pid, 0; # should not block, we expect the child to simply behave

      die $$res unless "ARRAY" eq ref $res;

      return wantarray ? @$res : $res->[-1];

   } elsif (defined $pid) {
      delete $SIG{__WARN__};
      delete $SIG{__DIE__};
      # just in case, this hack effectively disables event processing
      # in the child. cleaner and slower would be to canceling all
      # event watchers, but we are event-model agnostic.
      undef $Coro::idle;
      $Coro::current->prio (Coro::PRIO_MAX);

      eval {
         undef $fh1;

         my @res = eval { $cb->(@args) };

         open my $fh, ">", \my $buf
            or die "fork_eval: cannot open fh-to-buf in child: $!";
         Storable::store_fd $@ ? \"$@" : \@res, $fh;
         close $fh;

         syswrite $fh2, $buf;
         close $fh2;
      };

      warn $@ if $@;
      Coro::State::_exit 0;

   } else {
      die "fork_eval: $!";
   }
}

# make sure store_fd is preloaded
eval { Storable::store_fd undef, undef };

1;

=back

=head1 AUTHOR

 Marc Lehmann <schmorp@schmorp.de>
 http://home.schmorp.de/

=cut

