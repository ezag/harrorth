#!/usr/bin/perl

use lib "t/lib";
use Forth::Interp::Test;

__END__

=== the recurse word
--- forth
: ub ?dup if 1 . 1 - recurse then ;
7 ub
--- result: 1\s*1\s*1\s*1\s*1\s*1\s*1

=== the recursive word
--- forth
: ub recursive ?dup if 1 . 1 - ub then ;
4 ub
--- result: 1\s*1\s*1\s*1

=== defer and is
--- forth

defer even
: odd   ?dup if 1 - even else false then ;
:noname ?dup if 1 - odd  else true  then ; is even

: true? if 5 else 10 then . ;

3 odd true?
4 odd true?

3 even true?
4 even true?
--- result: 5\s*10\s*10\s*5


=== :deferred
--- skip: gforth
--- forth
defer even
:         odd  ?dup if 1 - even else false then ;
:deferred even ?dup if 1 - odd  else true  then ;

: true? if 5 else 10 then . ;

3 odd true?
4 odd true?

3 even true?
4 even true?
--- result: 5\s*10\s*10\s*5

