=head1 NAME

Coro::Channel - message queues

=head1 SYNOPSIS

 use Coro::Channel;

 $q1 = new Coro::Channel <maxsize>;

 $q1->put ("xxx");
 print $q1->get;

 die unless $q1->size;

=head1 DESCRIPTION

A Coro::Channel is the equivalent of a unix pipe (and similar to amiga
message ports): you can put things into it on one end and read things out
of it from the other end. If the capacity of the Channel is maxed out
writers will block. Both ends of a Channel can be read/written from by as
many coroutines as you want concurrently.

=over 4

=cut

package Coro::Channel;

use strict qw(vars subs);
no warnings;

use Coro ();
use Coro::Semaphore ();

our $VERSION = 5.15;

sub DATA (){ 0 }
sub SGET (){ 1 }
sub SPUT (){ 2 }
sub CEOS (){ 3 }

=item $q = new Coro:Channel $maxsize

Create a new channel with the given maximum size (practically unlimited
if C<maxsize> is omitted). Giving a size of one gives you a traditional
channel, i.e. a queue that can store only a single element (which means
there will be no buffering, and C<put> will wait until there is a
corresponding C<get> call). To buffer one element you have to specify
C<2>, and so on.

=cut

sub new {
   # we cheat and set infinity == 2*10**9
   bless [
      [],
      (Coro::Semaphore::_alloc 0),
      (Coro::Semaphore::_alloc +($_[1] || 2_000_000_000) - 1),
   ]
}

=item $q->put ($scalar)

Put the given scalar into the queue.

=cut

sub put {
   push @{$_[0][DATA]}, $_[1];
   Coro::Semaphore::up   $_[0][SGET];
   Coro::Semaphore::down $_[0][SPUT];
}

=item $q->get

Return the next element from the queue, waiting if necessary.

=cut

sub get {
   Coro::Semaphore::down $_[0][SGET];
   Coro::Semaphore::up   $_[0][SPUT];
   shift @{$_[0][DATA]}
}

=item $q->shutdown

Shuts down the Channel by pushing a virtual end marker onto it: This
changes the behaviour of the Channel when it becomes or is empty to return
C<undef>, almost as if infinitely many C<undef> elements have been put
into the queue.

Specifically, this function wakes up any pending C<get> calls and lets
them return C<undef>, the same on future C<get> calls. C<size> will return
the real number of stored elements, though.

Another way to describe the behaviour is that C<get> calls will not block
when the queue becomes empty but immediately return C<undef>. This means
that calls to C<put> will work normally and the data will be returned on
subsequent C<get> calls.

This method is useful to signal the end of data to any consumers, quite
similar to an end of stream on e.g. a tcp socket: You have one or more
producers that C<put> data into the Channel and one or more consumers who
C<get> them. When all producers have finished producing data, a call to
C<shutdown> signals this fact to any consumers.

=cut

sub shutdown {
   Coro::Semaphore::adjust $_[0][SGET], 2_000_000_000;
}

=item $q->size

Return the number of elements waiting to be consumed. Please note that:

  if ($q->size) {
     my $data = $q->get;
     ...
  }

is I<not> a race condition but instead works just fine. Note that the
number of elements that wait can be larger than C<$maxsize>, as it
includes any coroutines waiting to put data into the channel (but not any
shutdown condition).

This means that the number returned is I<precisely> the number of calls to
C<get> that will succeed instantly and returning some data.

=cut

sub size {
   scalar @{$_[0][DATA]}
}

1;

=back

=head1 AUTHOR

 Marc Lehmann <schmorp@schmorp.de>
 http://home.schmorp.de/

=cut

