=head1 NAME

Coro::Timer - timers and timeouts, independent of any event loop

=head1 SYNOPSIS

 use Coro::Timer qw(sleep timeout);
 # nothing exported by default

 sleep 10;

=head1 DESCRIPTION

This package implements a simple timer callback system which works
independent of the event loop mechanism used.

=over 4

=cut

package Coro::Timer;

no warnings;

use Carp ();
use Exporter;

use AnyEvent ();

use Coro ();
use Coro::AnyEvent ();

$VERSION = 5.15;
@EXPORT_OK = qw(timeout sleep);

=item $flag = timeout $seconds;

This function will wake up the current coroutine after $seconds
seconds and sets $flag to true (it is false initially).  If $flag goes
out of scope earlier nothing happens. This is used to implement the
C<timed_down>, C<timed_wait> etc. primitives. It is used like this:

   sub timed_wait {
      my $timeout = Coro::Timer::timeout 60;

      while (condition false) {
         Coro::schedule; # wait until woken up or timeout
         return 0 if $timeout; # timed out
      }

      return 1; # condition satisfied
   }

=cut

# deep magic, expecially the double indirection :(:(
sub timeout($) {
   my $current = $Coro::current;
   my $timeout;
   bless {
      timer => AnyEvent->timer (after => $_[0], cb => sub {
                  $timeout = 1;
                  $current->ready;
               }),
      timeout => \$timeout,
   }, "Coro::Timer::Timeout";
}

package Coro::Timer::Timeout;

sub bool { ${$_[0]{timeout}} }

use overload 'bool' => \&bool, '0+' => \&bool;

package Coro::Timer;

=item sleep $seconds

This function works like the built-in sleep, except maybe more precise
and, most important, without blocking other coroutines.

=cut

sub sleep {
   my $timer = AnyEvent->timer (after => $_[0], cb => Coro::rouse_cb);
   Coro::rouse_wait;
}

1;

=back

=head1 AUTHOR

 Marc Lehmann <schmorp@schmorp.de>
 http://home.schmorp.de/

=cut

