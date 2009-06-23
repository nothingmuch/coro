=head1 NAME

Coro::State - first class continuations

=head1 SYNOPSIS

 use Coro::State;

 $new = new Coro::State sub {
    print "in coro (called with @_), switching back\n";
    $new->transfer ($main);
    print "in coro again, switching back\n";
    $new->transfer ($main);
 }, 5;

 $main = new Coro::State;

 print "in main, switching to coro\n";
 $main->transfer ($new);
 print "back in main, switch to coro again\n";
 $main->transfer ($new);
 print "back in main\n";

=head1 DESCRIPTION

This module implements coro. Coros, similar to threads and continuations,
allow you to run more than one "thread of execution" in parallel. Unlike
so-called "kernel" threads, there is no parallelism and only voluntary
switching is used so locking problems are greatly reduced. The latter is
called "cooperative" threading as opposed to "preemptive" threading.

This can be used to implement non-local jumps, exception handling,
continuation objects and more.

This module provides only low-level functionality. See L<Coro> and related
modules for a higher level threads abstraction including a scheduler.

=head2 MODEL

Coro::State implements two different thread models: Perl and C. The C
threads (called cctx's) are basically simplified perl interpreters
running/interpreting the Perl threads. A single interpreter can run any
number of Perl threads, so usually there are very few C threads.

When Perl code calls a C function (e.g. in an extension module) and that C
function then calls back into Perl or transfers control to another thread,
the C thread can no longer execute other Perl threads, so it stays tied to
the specific thread until it returns to the original Perl caller, after
which it is again available to run other Perl threads.

The main program always has its own "C thread" (which really is
*the* Perl interpreter running the whole program), so there will always
be at least one additional C thread. You can use the debugger (see
L<Coro::Debug>) to find out which threads are tied to their cctx and
which aren't.

=head2 MEMORY CONSUMPTION

A newly created Coro::State that has not been used only allocates a
relatively small (a hundred bytes) structure. Only on the first
C<transfer> will perl allocate stacks (a few kb, 64 bit architetcures
use twice as much, i.e. a few kb :) and optionally a C stack/thread
(cctx) for threads that recurse through C functions. All this is very
system-dependent. On my x86-pc-linux-gnu system this amounts to about 2k
per (non-trivial but simple) Coro::State.

You can view the actual memory consumption using Coro::Debug. Keep in mind
that a for loop or other block constructs can easily consume 100-200 bytes
per nesting level.

=cut

package Coro::State;

use strict;
no warnings "uninitialized";

use Carp;

use Time::HiRes (); # currently only used for PerlIO::cede

our $DIEHOOK;
our $WARNHOOK;

BEGIN {
   $DIEHOOK  = sub { };
   $WARNHOOK = sub { warn $_[0] };
}

sub diehook  { &$DIEHOOK  }
sub warnhook { &$WARNHOOK }

use XSLoader;

BEGIN {
   our $VERSION = 5.14;

   # must be done here because the xs part expects it to exist
   # it might exist already because Coro::Specific created it.
   $Coro::current ||= { };

   {
      # save/restore the handlers before/after overwriting %SIG magic
      local $SIG{__DIE__};
      local $SIG{__WARN__};

      XSLoader::load __PACKAGE__, $VERSION;
   }

   # need to do it after overwriting the %SIG magic
   $SIG{__DIE__}  ||= \&diehook;
   $SIG{__WARN__} ||= \&warnhook;
}

use Exporter;
use base Exporter::;

=head2 GLOBAL VARIABLES

=over 4

=item $Coro::State::DIEHOOK

This works similarly to C<$SIG{__DIE__}> and is used as the default die
hook for newly created Coro::States. This is useful if you want some generic
logging function that works for all threads that don't set their own
hook.

When Coro::State is first loaded it will install these handlers for the
main program, too, unless they have been overwritten already.

The default handlers provided will behave like the built-in ones (as if
they weren't there).

If you don't want to exit your program on uncaught exceptions, you can
must not return from your die hook - terminate instead.

Note 1: You I<must> store a valid code reference in these variables,
C<undef> will I<not> do.

Note 2: The value of this variable will be shared among all threads, so
changing its value will change it in all threads that don't have their
own die handler.

=item $Coro::State::WARNHOOK

Similar to above die hook, but augments C<$SIG{__WARN__}>.

=back

=head2 FUNCTIONS

=over 4

=item $coro = new Coro::State [$coderef[, @args...]]

Create a new Coro::State thread object and return it. The first
C<transfer> call to this thread will start execution at the given
coderef, with the given arguments.

Note that the arguments will not be copied. Instead, as with normal
function calls, the thread receives passed arguments by reference, so
make sure you don't change them in unexpected ways.

Returning from such a thread is I<NOT> supported. Neither is calling
C<exit> or throwing an uncaught exception. The following paragraphs
describe what happens in current versions of Coro.

If the subroutine returns the program will be terminated as if execution
of the main program ended.

If it throws an exception the program will terminate unless the exception
is caught, exactly like in the main program.

Calling C<exit> in a thread does the same as calling it in the main
program, but due to libc bugs on many BSDs, this doesn't work reliable
everywhere.

If the coderef is omitted this function will create a new "empty"
thread, i.e. a thread that cannot be transfered to but can be used
to save the current thread state in (note that this is dangerous, as no
reference is taken to ensure that the "current thread state" survives,
the caller is responsible to ensure that the cloned state does not go
away).

The returned object is an empty hash which can be used for any purpose
whatsoever, for example when subclassing Coro::State.

Certain variables are "localised" to each thread, that is, certain
"global" variables are actually per thread. Not everything that would
sensibly be localised currently is, and not everything that is localised
makes sense for every application, and the future might bring changes.

The following global variables can have different values per thread,
and have the stated initial values:

   Variable       Initial Value
   @_             whatever arguments were passed to the Coro
   $_             undef
   $@             undef
   $/             "\n"
   $SIG{__DIE__}  aliased to $Coro::State::DIEHOOK(*)
   $SIG{__WARN__} aliased to $Coro::State::WARNHOOK(*)
   (default fh)   *STDOUT
   $^H, %^H       zero/empty.
   $1, $2...      all regex results are initially undefined

   (*) reading the value from %SIG is not supported, but local'ising is.

If you feel that something important is missing then tell me. Also
remember that every function call that might call C<transfer> (such
as C<Coro::Channel::put>) might clobber any global and/or special
variables. Yes, this is by design ;) You can always create your own
process abstraction model that saves these variables.

The easiest way to do this is to create your own scheduling primitive like
in the code below, and use it in your threads:

  sub my_cede {
     local ($;, ...);
     Coro::cede;
  }

Another way is to use dynamic winders, see C<Coro::on_enter> and
C<Coro::on_leave> for this.

=item $prev->transfer ($next)

Save the state of the current subroutine in C<$prev> and switch to the
thread saved in C<$next>.

The "state" of a subroutine includes the scope, i.e. lexical variables and
the current execution state (subroutine, stack).

=item $state->is_new

Returns true iff this Coro::State object is "new", i.e. has never been run
yet. Those states basically consist of only the code reference to call and
the arguments, but consumes very little other resources. New states will
automatically get assigned a perl interpreter when they are transfered to.

=item $state->is_destroyed

Returns true iff the Coro::State object has been destroyed (by
C<cancel>), i.e. it's resources freed because they were C<cancel>'d (or
C<terminate>'d).

=item $state->cancel

Forcefully destructs the given Coro::State. While you can keep the
reference, and some memory is still allocated, the Coro::State object is
effecticely dead, destructors have been freed, it cannot be transfered to
anymore.

=item $state->throw ([$scalar])

See L<< Coro->throw >>.

=item $state->call ($coderef)

Try to call the given C<$coderef> in the context of the given state. This
works even when the state is currently within an XS function, and can
be very dangerous. You can use it to acquire stack traces etc. (see the
Coro::Debug module for more details). The coderef MUST NOT EVER transfer
to another state.

=item $state->eval ($string)

Like C<call>, but eval's the string. Dangerous.

=item $state->swap_defsv

=item $state->swap_defav

Swap the current C<$_> (swap_defsv) or C<@_> (swap_defav) with the
equivalent in the saved state of C<$state>. This can be used to give the
coro a defined content for C<@_> and C<$_> before transfer'ing to it.

=item $state->trace ($flags)

Internal function to control tracing. I just mention this so you can stay
away from abusing it.

=item $bytes = $state->rss

Returns the memory allocated by the coro (which includes static
structures, various perl stacks but NOT local variables, arguments or any
C context data). This is a rough indication of how much memory it might
use.

=item $state->has_cctx

Returns whether the state currently uses a cctx/C context. An active
state always has a cctx, as well as the main program. Other states only
use a cctxts when needed.

=item Coro::State::force_cctx

Forces the allocation of a C context for the currently running coro
(if not already done). Apart from benchmarking there is little point
in doing so, however.

=item $ncctx = Coro::State::cctx_count

Returns the number of C-level coro allocated. If this number is
very high (more than a dozen) it might help to identify points of C-level
recursion in your code and moving this into a separate coro.

=item $nidle = Coro::State::cctx_idle

Returns the number of allocated but idle (free for reuse) C level
coro. Currently, Coro will limit the number of idle/unused cctxs to
8.

=item $old = Coro::State::cctx_stacksize [$new_stacksize]

Returns the current C stack size and optionally sets the new I<minimum>
stack size to C<$new_stacksize> I<long>s. Existing stacks will not
be changed, but Coro will try to replace smaller stacks as soon as
possible. Any Coro::State that starts to use a stack after this call is
guaranteed this minimum stack size.

Please note that coros will only need to use a C-level stack if the
interpreter recurses or calls a function in a module that calls back into
the interpreter, so use of this feature is usually never needed.

=item $old = Coro::State::cctx_max_idle [$new_count]

Coro caches C contexts that are not in use currently, as creating them
from scratch has some overhead.

This function returns the current maximum number of idle C contexts and
optionally sets the new amount. The count must be at least C<1>, with the
default being C<4>.

=item @states = Coro::State::list

Returns a list of all states currently allocated.

=item $clone = $state->clone

This exciting method takes a Coro::State object and clones it, i.e., it
creates a copy. This makes it possible to restore a state more than once,
and even return to states that have returned or have been terminated.

Since its only known purpose is for intellectual self-gratification, and
because it is a difficult piece of code, it is not enabled by default, and
not supported.

Here are a few little-known facts: First, coros *are* full/true/real
continuations. Secondly Coro::State objects (without clone) *are* first
class continuations. Thirdly, nobody has ever found a use for the full
power of call/cc that isn't better (faster, easier, more efficiently)
implemented differently, and nobody has yet found a useful control
construct that can't be implemented without it already, just much faster
and with fewer resources. And lastly, Scheme's call/cc doesn't support
using call/cc to implement threads.

Among the games you can play with this is implementing a scheme-like
call-with-current-continuation, as the following code does (well, with
small differences).

   # perl disassociates from local lexicals on frame exit,
   # so use a global variable for return values.
   my @ret;

   sub callcc($@) {
      my ($func, @arg) = @_;

      my $continuation = new Coro::State;
      $continuation->transfer (new Coro::State sub {
         my $escape = sub {
            @ret = @_;
            Coro::State->new->transfer ($continuation->clone);
         };
         $escape->($func->($escape, @arg));
      });

      my @ret_ = @ret; @ret = ();
      wantarray ? @ret_ : pop @ret_
   }

Which could be used to implement a loop like this:

   async {
      my $n; 
      my $l = callcc sub { $_[0] };
     
      $n++; 
      print "iteration $n\n";

      $l->($l) unless $n == 10;
   };  

If you find this confusing, then you already understand the coolness of
call/cc: It can turn anything into spaghetti code real fast.

Besides, call/cc is much less useful in a Perl-like dynamic language (with
references, and its scoping rules) then in, say, scheme.

Now, the known limitations of C<clone>:

It probably only works on perl 5.10; it cannot clone a coro inside
the substition operator (but windows perl can't fork from there either)
and some other contexts, and C<abort ()> is the preferred mechanism to
signal errors. It cannot clone a state that has a c context attached
(implementing clone on the C level is too hard for me to even try),
which rules out calling call/cc from the main coro. It cannot
clone a context that hasn't even been started yet. It doesn't work with
C<-DDEBUGGING> (but what does). It probably also leaks, and sometimes
triggers a few assertions inside Coro. Most of these limitations *are*
fixable with some effort, but that's pointless just to make a point that
it could be done.

The current implementation could without doubt be optimised to be a
constant-time operation by doing lazy stack copying, if somebody were
insane enough to invest the time.

=cut

# used by Coro::Debug only atm.
sub debug_desc {
   $_[0]{desc}
}

# for very deep reasons, we must initialise $Coro::main here.

{
   package Coro;

   our $main;    # main coro
   our $current; # current coro

   $main = Coro::State::new Coro::;

   $main->{desc} = "[main::]";

   # maybe some other module used Coro::Specific before...
   $main->{_specific} = $current->{_specific}
      if $current;

   _set_current $main;
}

1;

=back

=head1 BUGS

This module is not thread-safe. You must only ever use this module from
the same thread (this requirement might be removed in the future).

=head1 SEE ALSO

L<Coro>.

=head1 AUTHOR

 Marc Lehmann <schmorp@schmorp.de>
 http://home.schmorp.de/

=cut

