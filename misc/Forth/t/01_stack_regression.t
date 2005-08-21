#!/usr/bin/perl

use lib "t/lib";
use Forth::Interp::Test;

__END__

=== dup
--- forth: 2 1 dup . . .
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

=== over
--- forth: 5 3 2 over . . .
--- result: 3 \s+ 2 \s+ 3

=== pick
--- forth: 5 3 2 1 pick . . .
--- result: 3 \s+ 2 \s+ 3

=== pick
--- forth: 5 3 2 2 pick . . .
--- result: 5 \s+ 2 \s+ 3

=== rot
--- forth: 5 3 2 rot . . .
--- result: 5 \s+ 2 \s+ 3

