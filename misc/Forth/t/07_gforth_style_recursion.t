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
