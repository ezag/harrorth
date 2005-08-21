#!/usr/bin/perl

use lib "t/lib";
use Forth::Interp::Test;

__END__

=== emitting chars
--- forth: 65 emit
--- result: A

=== strings on the stack
--- forth: : hello S" Hello World!" ; hello . .
--- result: \d+ \s+ \d+

=== typing strings
--- forth: : hello S" Hello World!" type ; hello
--- result: Hello\ World!

=== second number is length
--- forth: : hello S" 1234567890" ; hello 3 - type
--- result: 1234567

=== first number is offset
--- forth: : hello S" 1234567890" ; hello 2 - swap 2 + swap type
--- result: 34567890


