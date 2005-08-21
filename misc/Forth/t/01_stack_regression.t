#!/usr/bin/perl

use lib "t/lib";
use Forth::Interp::Test;

__END__

=== dup
--- forth: 2 1 dup . . .
--- result: 1 \s+ 1 \s+ 2

=== pick (dup)
--- forth: 5 3 2 0 pick . . .
--- result: 2 \s+ 2 \s+ 3

=== mydup
--- forth: : mydup 0 pick ; 2 1 mydup . . .
--- result: 1 \s+ 1 \s+ 2

=== ?dup true
--- forth: 2 1 ?dup . . .
--- result: 1 \s+ 1 \s+ 2

=== ?dup false
--- forth: 2 0 ?dup . .
--- result: 0 \s+ 2

=== swap
--- forth: 2 1 swap . .
--- result: 2 \s+ 1

=== roll (swap)
--- forth: 2 3 1 roll . .
--- result: 2 \s+ 3

=== over
--- forth: 5 3 2 over . . .
--- result: 3 \s+ 2 \s+ 3

=== pick (over)
--- forth: 5 3 2 1 pick . . .
--- result: 3 \s+ 2 \s+ 3

=== pick (arbitrary)
--- forth: 5 3 2 2 pick . . .
--- result: 5 \s+ 2 \s+ 3

=== rot
--- forth: 5 3 2 rot . . .
--- result: 5 \s+ 2 \s+ 3

=== roll (rot)
--- forth: 5 3 2 2 roll . . .
--- result: 5 \s+ 2 \s+ 3

=== myrot
--- forth: : myrot 2 roll ; 5 3 2 myrot . . .
--- result: 5 \s+ 2 \s+ 3


