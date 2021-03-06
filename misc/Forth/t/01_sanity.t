#!/usr/bin/perl

use lib "t/lib";
use Forth::Interp::Test;

__END__

=== no op
--- forth:
--- result:

=== literal interactive
--- forth: 571 .
--- result: 571


=== simple math
--- forth: 2 2 + .
--- result: 4


=== swap
--- forth: 1 2 swap . .
--- result: 1 \s+ 2


=== over
--- forth: 1 2 over . . .
--- result: 1 \s+ 2 \s+ 1


=== dup
--- forth: 1 2 dup . . .
--- result: 2 \s+ 2 \s+ 1


=== simple user word
--- forth: : foo 7 ; foo .
--- result: 7


=== word composition
--- forth
: foo swap - ;
: bar 5 foo ;
3 bar .
--- result: 2


=== if true
--- forth
: foo if 5 then ;
2 ( against stack underflow )
1 foo .
--- result: 5


=== if false
--- forth
: foo if 10 then ;
2 ( against stack underflow )
0 foo .
--- result: 2


=== if and else true
--- forth
: foo if 5 else 10 then ;
1 foo .
--- result: 5


=== if and else false
--- forth
: foo if 5 else 10 then ;
0 foo .
--- result: 10

=== ?dup on true
--- forth: 0 5 ?dup . .
--- result: 5\s*5

=== ?dup on false
--- forth: 5 0 ?dup . .
--- result: 0\s*5

=== rot
--- forth: 10 11 1 rot rot . . .
--- result: 11\s*10\s*1

=== exit
--- forth
: foo 3 exit 4 ;
: bar 2 foo 5 ;
1 bar 6 . . . . .
--- result: 6 \s* 5 \s* 3 \s* 2 \s* 1

