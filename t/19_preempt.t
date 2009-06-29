$| = 1;
print "1..25\n";

use Coro qw(:prio cede async current);
use Time::HiRes qw(time);

print "ok\n";

my @log;

sub trace {
   push @log, [ $_[0] => 0 + current ];
}

my @bg_coros;

my $t = time;

sub waste_time {
   my ( $i, $loop );
   my $preemptable;

   print "ok - started\n";

   while ( $t + 1.5 > time ) {
      if ($preemptable) {
         current->no_preempt;
         trace("no_preempt");
      }
      else {
         trace("preempt");
         current->preempt;
      }

      $preemptable = !$preemptable;

      for ( 1 .. 100 ) {
         for ( 1 .. 10 ) {

            # waste some time
            # i think just $i++ is not sufficient for a random
            # distribution of opcode durations
            my $x = $i++ ** 4;
            my $foo = ( ( "x" x 5000 ) . "zxx" );
            $foo =~ /x+x/;
            sub { index( shift, "z" ) }->($foo);
         }

         trace("loop");
      }
   }

   trace("done");

   print "ok - wasted CPU time\n";
}

@bg_coros = map {
   async { waste_time }
} 1 .. 10;

cede;

$_->join for @bg_coros;

my $prev_loop;
my $switches;
my $preempted_critical;

# analyzes the trace log
# $switches counts the number of times loops interlaces (meaning that the coros
# preempted each other at least once)
# $preempted_critical critical counts such intersections that have happened
# between calling no_preempt and preempt (meaning that a critical section was
# ceded even though it shouldn't have)
while (@log) {
   my ( $type, $id ) = @{ shift @log };

   if ( $type eq 'loop' ) {
      $switches++ if defined $prev_loop and $prev_loop != $id;
      $prev_loop = $id;
   }
   elsif ( $type eq 'no_preempt' ) {
      while (@log) {
         my ( $type, $inner_id ) = @{ shift @log };
         last if $type eq 'preempt' or $type eq 'done';
         $preempted_critical++ if $id != $inner_id;
      }
   }
}

# check that there were at least 2 more cedes than would have happened using
# just a join
print "not " if $switches < 7;
print "ok - ceded implicitly\n";

print "not " if $switches < 30;
print "ok - many times times ($switches)\n";

print "not " if $preempted_critical;
print "ok - did not cede while inside critical sections\n";

print "ok - end\n";

