#!/usr/bin/perl

use lib "t/lib";
use Forth::Interp::Test;

__END__

=== variables are words that put numbers on the stack
--- forth
variable x
x .
--- result: \d+

=== initial value
--- forth
variable x
x ?
--- result: [0]


=== writing to a variable
--- forth
variable x
10 x !
x ?
--- result: 10


=== the 'create' word
--- forth
: var create 5 , ;
var x
x ?
3 x !
x ?
--- result: 5\s+3


