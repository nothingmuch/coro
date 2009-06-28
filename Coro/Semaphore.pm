=head1 NAME

Coro::Semaphore - counting semaphores

=head1 SYNOPSIS

 use Coro::Semaphore;

 $sig = new Coro::Semaphore [initial value];

 $sig->down; # wait for signal

 # ... some other "thread"

 $sig->up;

=head1 DESCRIPTION

This module implements counting semaphores. You can initialize a mutex
with any level of parallel users, that is, you can intialize a sempahore
that can be C<down>ed more than once until it blocks. There is no owner
associated with semaphores, so one coroutine can C<down> it while another
can C<up> it.

Counting semaphores are typically used to coordinate access to
resources, with the semaphore count initialized to the number of free
resources. Coroutines then increment the count when resources are added
and decrement the count when resources are removed.

=over 4

=cut

package Coro::Semaphore;

no warnings;

use Coro ();

$VERSION = 5.14;

=item new [inital count]

Creates a new sempahore object with the given initial lock count. The
default lock count is 1, which means it is unlocked by default. Zero (or
negative values) are also allowed, in which case the semaphore is locked
by default.

=item $sem->count

Returns the current semaphore count.

=item $sem->adjust ($diff)

Atomically adds the amount given to the current semaphore count. If the
count becomes positive, wakes up any waiters. Does not block if the count
becomes negative, however.

=item $sem->down

Decrement the counter, therefore "locking" the semaphore. This method
waits until the semaphore is available if the counter is zero.

=item $sem->wait

Similar to C<down>, but does not actually decrement the counter. Instead,
when this function returns, a following call to C<down> or C<try> is
guaranteed to succeed without blocking, until the next coroutine switch
(C<cede> etc.).

Note that using C<wait> is much less efficient than using C<down>, so try
to prefer C<down> whenever possible.

=item $sem->wait ($callback)

If you pass a callback argument to C<wait>, it will not wait, but
immediately return. The callback will be called as soon as the semaphore
becomes available (which might be instantly), and gets passed the
semaphore as first argument.

The callback might C<down> the semaphore exactly once, might wake up other
coroutines, but is I<NOT> allowed to block (switch to other coroutines).

=cut

#=item $status = $sem->timed_down ($timeout)
#
#Like C<down>, but returns false if semaphore couldn't be acquired within
#$timeout seconds, otherwise true.

#sub timed_down {
#   require Coro::Timer;
#   my $timeout = Coro::Timer::timeout ($_[1]);
# 
#   while ($_[0][0] <= 0) {
#      push @{$_[0][1]}, $Coro::current;
#      &Coro::schedule;
#      if ($timeout) {
#         # ugly as hell. slow, too, btw!
#         for (0..$#{$_[0][1]}) {
#            if ($_[0][1][$_] == $Coro::current) {
#               splice @{$_[0][1]}, $_, 1;
#               return;
#            }
#         }
#         die;
#      }
#   }
# 
#   --$_[0][0];
#   return 1;
#}

=item $sem->up

Unlock the semaphore again.

=item $sem->try

Try to C<down> the semaphore. Returns true when this was possible,
otherwise return false and leave the semaphore unchanged.

=item $sem->waiters

In scalar context, returns the number of coroutines waiting for this
semaphore.

=item $guard = $sem->guard

This method calls C<down> and then creates a guard object. When the guard
object is destroyed it automatically calls C<up>.

=cut

sub guard {
   &down;
   bless [$_[0]], Coro::Semaphore::guard::
}

#=item $guard = $sem->timed_guard ($timeout)
#
#Like C<guard>, but returns undef if semaphore couldn't be acquired within
#$timeout seconds, otherwise the guard object.

#sub timed_guard {
#   &timed_down
#      ? bless \\$_[0], Coro::Semaphore::guard::
#      : ();
#}

sub Coro::Semaphore::guard::DESTROY {
   &up($_[0][0]);
}

=back

=head1 AUTHOR

 Marc Lehmann <schmorp@schmorp.de>
 http://home.schmorp.de/

=cut

1

