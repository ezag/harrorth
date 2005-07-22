#!/usr/bin/perl

use lib "t/lib";
use Forth::Interp::Test;

__END__

=== addresses on the stack
--- forth
: something create does> ;
something moose

moose .
--- result: \d+


=== make a constant
--- forth
: const_moose create , does> @ ;
5 const_moose foo
foo .
--- result: 5

=== N-ary incrementor generator
--- forth
variable x
10 x !

: 1+! dup @ 1 + swap ! ;
x 1+!
x ?

: mk-n+! create , does> @ 1 pick @ + swap ! ;

1 mk-n+! one+!
x one+!
x ?

5 mk-n+! 5+!
x 5+!
x ?
--- result: 11 \s+ 12 \s+ 17

