=head1 NAME

Coro::AnyEvent - integrate threads into AnyEvent

=head1 SYNOPSIS

 use Coro;
 use Coro::AnyEvent;

 # use coro within an AnyEvent environment

=head1 DESCRIPTION

When one naively starts to use threads in Perl, one will quickly run
into the problem that threads that block on a syscall (sleeping,
reading from a socket etc.) will block all threads.

If one then uses an event loop, the problem is that the event loop has
no knowledge of threads and will not run them before it polls for new
events, again blocking the whole process.

This module integrates threads into any event loop supported by
AnyEvent, combining event-based programming with coroutine-based
programming in a natural way.

All you have to do is C<use Coro::AnyEvent>, run the event loop of your
choice in some thread and then you can run threads freely.

=head1 USAGE

This module autodetects the event loop used (by relying on L<AnyEvent>)
and will either automatically defer to the high-performance L<Coro::EV> or
L<Coro::Event> modules, or will use a generic integration into any event
loop supported by L<AnyEvent>.

Note that if you need to wait for a single event, the rouse functions will
come in handy (see the Coro manpage for details):

   # wait for single SIGINT
   {
      my $int_w = AnyEvent->signal (signal => "INT", cb => Coro::rouse_cb);
      Coro::rouse_wait;
   }

=head1 FUNCTIONS

Coro::AnyEvent offers a few functions that might be useful for
"background" threads:

=over 4

=cut

package Coro::AnyEvent;

no warnings;
use strict;

use Coro;
use AnyEvent ();

our $VERSION = 5.15;

#############################################################################
# idle handler

our $IDLE;

#############################################################################
# 0-timeout idle emulation watcher

our $ACTIVITY;

sub _activity {
   $ACTIVITY ||= AnyEvent->timer (after => 0, cb => \&_schedule);
}

Coro::_set_readyhook (\&AnyEvent::detect);

AnyEvent::post_detect {
   unshift @AnyEvent::CondVar::ISA, "Coro::AnyEvent::CondVar";

   Coro::_set_readyhook undef;

   my $model = $AnyEvent::MODEL;

   if ($model eq "AnyEvent::Impl::EV" and eval { require Coro::EV }) {
      # provider faster versions of some functions

      eval '
         *sleep = \&Coro::EV::timer_once;
         *poll  = \&Coro::EV::timer_once;
         *idle  = sub {
            my $w = EV::idle Coro::rouse_cb;
            Coro::rouse_wait;
         };
         *idle_upto = sub {
            my $cb = Coro::rouse_cb;
            my $t = EV::timer $_[0], 0, $cb;
            my $w = EV::idle $cb;
            Coro::rouse_wait;
         };
         *readable = sub {
            EV::READ  & Coro::EV::timed_io_once $_[0], EV::READ , $_[1]
         };
         *writable = sub {
            EV::WRITE & Coro::EV::timed_io_once $_[0], EV::WRITE, $_[1]
         };
      ';
      die if $@;

   } elsif ($model eq "AnyEvent::Impl::Event" and eval { require Coro::Event }) {
      # let Coro::Event do its thing
   } else {
      # do the inefficient thing ourselves
      Coro::_set_readyhook \&_activity;

      $IDLE = new Coro sub {
         my $one_event = AnyEvent->can ("one_event");

         while () {
            $one_event->();
            Coro::schedule;
         }
      };
      $IDLE->{desc} = "[AnyEvent idle process]";

      $Coro::idle = $IDLE;
   }
};

=item Coro::AnyEvent::poll

This call will block the current thread until the event loop has polled
for new events and instructs the event loop to poll for new events once,
without blocking.

Note that this call will not actually execute the poll, just block until
new events have been polled, so other threads will have a chance to run.

This is useful when you have a thread that does some computations, but you
still want to poll for new events from time to time. Simply call C<poll>
from time to time:

   my $long_calc = async {
      for (1..10000) {
         Coro::AnyEvent::poll:
         # do some stuff, make sure it takes at least 0.001s or so
      }
   }

Although you should also consider C<idle> or C<idle_upto> in such cases.

=item Coro::AnyEvent::sleep $seconds

This blocks the current thread for at least the given number of seconds.

=item Coro::AnyEvent::idle

This call is similar to C<poll> in that it will also poll for
events. Unlike C<poll>, it will only resume the thread once there are no
events to handle anymore, i.e. when the process is otherwise idle.

=item Coro::AnyEvent::idle_upto $seconds

Like C<idle>, but with a maximum waiting time.

If your process is busy handling events, calling C<idle> can mean that
your thread will never be resumed. To avoid this, you can use C<idle_upto>
and specify a timeout, after which your thread will be resumed even if the
process is completely busy.

=item Coro::AnyEvent::readable $fh[, $timeout]

=item Coro::AnyEvent::writable $fh[, $timeout]

Blocks the current thread until the given file handle (not file
descriptor) becomes readable (or writable), or the given timeout has
elapsed, whichever happens first. No timeout counts as infinite timeout.

Returns true when the file handle became ready, false when a timeout
occured.

Note that these functions are quite inefficient as compared to using a
single watcher (they recreate watchers on every invocation) or compared to
using Coro::Handle.

Note also that they only work for sources that have reasonable
non-blocking behaviour (e.g. not files).

Example: wait until STDIN becomes readable, then quit the program.

   use Coro::AnyEvent;
   print "press enter to quit...\n";
   Coro::AnyEvent::readable *STDIN;
   exit 0;

=cut

sub poll() {
   my $w = AnyEvent->timer (after => 0, cb => Coro::rouse_cb);
   Coro::rouse_wait;
}

sub sleep($) {
   my $w = AnyEvent->timer (after => $_[0], cb => Coro::rouse_cb);
   Coro::rouse_wait;
}

sub idle() {
   my $w = AnyEvent->idle (cb => Coro::rouse_cb);
   Coro::rouse_wait;
}

sub idle_upto($) {
   my $cb = Coro::rouse_cb;
   my $t = AnyEvent->timer (after => shift, cb => $cb);
   my $w = AnyEvent->idle (cb => $cb);
   Coro::rouse_wait;
}

sub readable($;$) {
   my $cb = Coro::rouse_cb;
   my $w = AnyEvent->io (fh => $_[0], poll => "r", cb => sub { $cb->(1) });
   my $t = defined $_[1] && AnyEvent->timer (after => $_[1], cb => sub { $cb->(0) });
   Coro::rouse_wait
}

sub writable($;$) {
   my $cb = Coro::rouse_cb;
   my $w = AnyEvent->io (fh => $_[0], poll => "w", cb => sub { $cb->(1) });
   my $t = defined $_[1] && AnyEvent->timer (after => $_[1], cb => sub { $cb->(0) });
   Coro::rouse_wait
}

#############################################################################
# override condvars

package Coro::AnyEvent::CondVar;

sub _send {
   (delete $_[0]{_ae_coro})->ready if $_[0]{_ae_coro};
}

sub _wait {
   while (!$_[0]{_ae_sent}) {
      local $_[0]{_ae_coro} = $Coro::current;
      Coro::schedule;
   }
}

1;

=back

=head1 IMPLEMENTATION DETAILS

Unfortunately, few event loops (basically only L<EV> and L<Event>)
support the kind of integration required for smooth operations well, and
consequently, AnyEvent cannot completely offer the functionality required
by this module, so we need to improvise.

Here is what this module does when it has to work with other event loops:

=over 4

=item * run ready threads before blocking the process

Each time a thread is put into the ready queue (and there are no other
threads in the ready queue), a timer with an C<after> value of C<0> is
registered with AnyEvent.

This creates something similar to an I<idle> watcher, i.e. a watcher
that keeps the event loop from blocking but still polls for new
events. (Unfortunately, some badly designed event loops (e.g. Event::Lib)
don't support a timeout of C<0> and will always block for a bit).

The callback for that timer will C<cede> to other threads of the same or
higher priority for as long as such threads exists. This has the effect of
running all threads that have work to do until all threads block to wait
for external events.

If no threads of equal or higher priority are ready, it will cede to any
thread, but only once. This has the effect of running lower-priority
threads as well, but it will not keep higher priority threads from
receiving new events.

The priority used is simply the priority of the thread that runs the event
loop, usually the main program, which usually has a priority of C<0>. Note
that Coro::AnyEvent does I<not> run an event loop for you, so unless the
main program runs one, there will simply be no event loop to C<cede> to
(event handling will still work, somewhat inefficiently, but any thread
will have a higher priority than event handling in that case).

=item * provide a suitable idle callback.

In addition to hooking into C<ready>, this module will also provide a
C<$Coro::idle> handler that runs the event loop. It is best not to take
advantage of this too often, as this is rather inefficient, but it should
work perfectly fine.

=item * provide overrides for AnyEvent's condvars

This module installs overrides for AnyEvent's condvars. That is, when
the module is loaded it will provide its own condition variables. This
makes them coroutine-safe, i.e. you can safely block on them from within a
coroutine.

=item * lead to data corruption or worse

As C<unblock_sub> cannot be used by this module (as it is the module
that implements it, basically), you must not call into the event
loop recursively from any coroutine. This is not usually a difficult
restriction to live with, just use condvars, C<unblock_sub> or other means
of inter-coroutine-communications.

If you use a module that supports AnyEvent (or uses the same event loop
as AnyEvent, making the compatible), and it offers callbacks of any kind,
then you must not block in them, either (or use e.g. C<unblock_sub>), see
the description of C<unblock_sub> in the L<Coro> module.

This also means that you should load the module as early as possible,
as only condvars created after this module has been loaded will work
correctly.

=back

=head1 SEE ALSO

L<AnyEvent>, to see which event loops are supported, L<Coro::EV> and
L<Coro::Event> for more efficient and more correct solutions (they will be
used automatically if applicable).

=head1 AUTHOR

 Marc Lehmann <schmorp@schmorp.de>
 http://home.schmorp.de/

=cut

