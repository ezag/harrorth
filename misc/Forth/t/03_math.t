#!/usr/bin/perl

use lib "t/lib";
use Forth::Interp::Test;

__END__

=== addition
--- forth: 4 5 + 3 + .
--- result: 12


=== subtraction
--- forth: 10 6 - .
--- result: 4


=== subtraction
--- forth: 8 3 - 3 - .
--- result: 2


=== multiplication
--- forth: 10 10 * 2 * .
--- result: 200


=== division
--- forth: 100 10 2 / / .
--- result: 20


=== integer division
--- forth: 10 3 / .
--- result: 3


=== less than
--- forth: 10 3 < .
--- result: [0]


=== less than
--- forth: : true? if 1 else 0 then . ; 5 6 < true?
--- result: 1


=== greater than
--- forth: 5 10 > .
--- result: [0]


=== less or equal
--- forth: : true? if 1 else 0 then . ; 5 5 <= true?
--- result: 1

