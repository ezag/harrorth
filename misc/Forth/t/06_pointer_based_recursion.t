#!/usr/bin/perl

use lib "t/lib";
use Forth::Interp::Test;

__END__

=== :noname
--- forth
:noname 10 + ;
.
--- result: \d+

=== applying a :noname
--- forth
5
:noname 10 + ;
execute
.
--- result: 15

=== recursion
--- forth
variable ub
:noname
?dup if
	1 .  1 - ub @ execute
then ;
ub !

3 ub @ execute
--- result: 1\s*1\s*1

=== mutual recursion
--- forth
variable even
: odd
?dup if
	1 - even @ execute
else
	false
then ;

:noname
?dup if
	1 - odd
else
	true
then ;
even !

: true? if 5 else 10 then . ;

3 odd true?
4 odd true?

3 even @ execute true?
4 even @ execute true?
--- result: 5\s*10\s*10\s*5
