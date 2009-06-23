=head1 NAME

Coro::EV - do events the coro-way, with EV

=head1 SYNOPSIS

 use Coro;
 use Coro::EV;

 EV::loop;

=head1 DESCRIPTION

This module does two things: First, it offers some utility functions that
might be useful for threads, and secondly, it integrates Coro into the
EV main loop:

Before the process blocks (in EV::loop) to wait for events, this module
will schedule and run all ready (= runnable) threads of the same or
higher priority. After that, it will cede once to a threads of lower
priority, then continue in the event loop.

That means that threads with the same or higher pripority as the threads
running the main loop will inhibit event processing, while threads of
lower priority will get the CPU, but cannot completeley inhibit event
processing. Note that for that to work you actually have to run the EV
event loop in some thread.

=head1 RUNNING WITH OR WITHOUT A MAINLOOP

In general, you should always run EV::loop, either in your main program,
or in a separate coroutine. If you don't do that and all coroutines
start waiting for some events, this module will run the event loop once,
but this is very inefficient and will also not make it possible to run
background threads.

To run the EV event loop in a separate thread, you can simply do this:

  async { EV::loop };

=head1 FUNCTIONS

=over 4

=cut

package Coro::EV;

no warnings;
use strict;

use Carp;
no warnings;

use Coro;

use EV ();
use XSLoader;

BEGIN {
   our $VERSION = 5.14;

   local $^W = 0; # avoid redefine warning for Coro::ready;
   XSLoader::load __PACKAGE__, $VERSION;
}

our $IDLE = new Coro sub {
   while () {
      &_loop_oneshot;
      &Coro::schedule;
   }
};
$IDLE->{desc} = "[EV idle process]";

$Coro::idle = $IDLE;

=item $revents = Coro::EV::timed_io_once $fileno_or_fh, $events[, $timeout]

Blocks the coroutine until either the given event set has occured on the
fd, or the timeout has been reached (if timeout is missing or C<undef>
then there will be no timeout). Returns the received flags.

=item Coro::EV::timer_once $after

Blocks the coroutine for at least C<$after> seconds.

=cut

1;

=back

=head1 AUTHOR

 Marc Lehmann <schmorp@schmorp.de>
 http://home.schmorp.de/

=cut

