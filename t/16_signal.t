$|=1;
print "1..10\n";

no warnings;
use Coro;
use Coro::Signal;

{
   my $sig = new Coro::Signal;

   $as1 = async {
      my $g = $sig->wait;
      print "ok 3\n";
   };    

   $as2 = async {
      my $g = $sig->wait;
      print "ok 4\n";
   };    

   cede; # put 1, 2 in wait q

   $as3 = async {
      my $g = $sig->wait;
      print "ok 2\n";
   };    

   $as4 = async {
      my $g = $sig->wait;
      print "ok 6\n";
   };    

   $as5 = async {
      my $g = $sig->wait;
      print "ok 9\n";
   };    

   $sig->send; # ready 1
   $sig->send; # ready 2
   $sig->send; # remember

   print +(Coro::Semaphore::count $sig) == 1 ? "" : "not ", "ok 1\n";

   cede; # execute 3 (already ready, no contention), 1, 2

   print +(Coro::Semaphore::count $sig) == 0 ? "" : "not ", "ok 5\n";

   $sig->send;
   cede;

   print +(Coro::Semaphore::count $sig) == 0 ? "" : "not ", "ok 7\n";

   $sig->broadcast;
   print +(Coro::Semaphore::count $sig) == 0 ? "" : "not ", "ok 8\n";
   cede;

   print +(Coro::Semaphore::count $sig) == 0 ? "" : "not ", "ok 10\n";
}

