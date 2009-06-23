=head1 NAME

Coro::Specific - manage coroutine-specific variables.

=head1 SYNOPSIS

 use Coro::Specific;

 my $ref = new Coro::Specific;

 $$ref = 5;
 print $$ref;

=head1 DESCRIPTION

This module can be used to create variables (or better: references to
them) that are specific to the currently executing coroutine. This module
does not automatically load the Coro module (so the overhead will be small
when no coroutines are used).

=over 4

=cut

package Coro::Specific;

BEGIN { eval { require warnings } && warnings->unimport ("uninitialized") }

$VERSION = 5.14;

=item new

Create a new coroutine-specific scalar and return a reference to it. The
scalar is guarenteed to be "undef". Once such a scalar has been allocated
you cannot deallocate it (yet), so allocate only when you must.

=cut

my $idx;

sub new {
   my $var;
   tie $var, Coro::Specific::;
   \$var;
}

sub TIESCALAR {
   my $idx = $idx++;
   bless \$idx, $_[0];
}

sub FETCH {
   $Coro::current->{_specific}[${$_[0]}];
}

sub STORE {
   $Coro::current->{_specific}[${$_[0]}] = $_[1];
}

#sub DESTROY {
#   push @idx, $$_[0];
#}

1;

=back

=head1 BUGS

The actual coroutine specific values do not automatically get destroyed
when the Coro::Specific object gets destroyed.

=head1 AUTHOR

 Marc Lehmann <schmorp@schmorp.de>
 http://home.schmorp.de/

=cut

