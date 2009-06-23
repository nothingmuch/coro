=head1 NAME

Coro::Select - a (slow but coro-aware) replacement for CORE::select

=head1 SYNOPSIS

 use Coro::Select;          # replace select globally (be careful, see below)
 use Core::Select 'select'; # only in this module
 use Coro::Select ();       # use Coro::Select::select

=head1 DESCRIPTION

This module tries to create a fully working replacement for perl's
C<select> built-in, using C<AnyEvent> watchers to do the job, so other
coroutines can run in parallel to any select user. As many libraries that
only have a blocking API do not use global variables and often use select
(or IO::Select), this effectively makes most such libraries "somewhat"
non-blocking w.r.t. other coroutines.

To be effective globally, this module must be C<use>'d before any other
module that uses C<select>, so it should generally be the first module
C<use>'d in the main program. Note that overriding C<select> globally
might actually cause problems, as some C<AnyEvent> backends use C<select>
themselves, and asking AnyEvent to use Coro::Select, which in turn asks
AnyEvent will not quite work.

You can also invoke it from the commandline as C<perl -MCoro::Select>.

To override select only for a single module (e.g. C<Net::DBus::Reactor>),
use a code fragment like this to load it:

   {
      package Net::DBus::Reactor;
      use Coro::Select qw(select);
      use Net::DBus::Reactor;
   }

Performance naturally isn't great (every file descriptor must be dup'ed),
but unless you need very high select performance you normally won't notice
the difference.

This implementation works fastest when only very few bits are set in the
fd set(s).

=over 4

=cut

package Coro::Select;

use strict;

use Errno;

use Coro ();
use AnyEvent ();
use Coro::AnyEvent ();

use base Exporter::;

our $VERSION = 5.14;
our @EXPORT_OK = "select";

sub import {
   my $pkg = shift;
   if (@_) {
      $pkg->export (scalar caller 0, @_);
   } else {
      $pkg->export ("CORE::GLOBAL", "select");
   }
}

sub select(;*$$$) { # not the correct prototype, but well... :()
   if (@_ == 0) {
      return CORE::select
   } elsif (@_ == 1) {
      return CORE::select $_[0]
   } elsif (defined $_[3] && !$_[3]) {
      return CORE::select $_[0], $_[1], $_[2], $_[3]
   } else {
      my $nfound = 0;
      my @w;
      my $wakeup = Coro::rouse_cb;

      # AnyEvent does not do 'e', so replace it by 'r'
      for ([0, 'r', '<'], [1, 'w', '>'], [2, 'r', '<']) {
         my ($i, $poll, $mode) = @$_;
         if (defined $_[$i]) {
            my $rvec = \$_[$i];

            # we parse the bitmask by first expanding it into
            # a string of bits
            for (unpack "b*", $$rvec) {
               # and then repeatedly matching a regex against it
               while (/1/g) {
                  my $fd = (pos) - 1;

                  (vec $$rvec, $fd, 1) = 0;

                  # we need to dup(), unfortunately
                  open my $fh, "$mode&$fd"
                     or do { $! = Errno::EBADF; return -1 };

                  push @w,
                     $fh,
                     AnyEvent->io (fh => $fh, poll => $poll, cb => sub {
                        (vec $$rvec, $fd, 1) = 1;
                        ++$nfound;
                        $wakeup->();
                     });
               }
            }
         }
      }

      push @w,
         AnyEvent->timer (after => $_[3], cb => $wakeup)
            if defined $_[3];

      Coro::rouse_wait;

      return $nfound
   }
}

1;

=back

=head1 SEE ALSO

L<Coro::LWP>.

=head1 AUTHOR

 Marc Lehmann <schmorp@schmorp.de>
 http://home.schmorp.de/

=cut


