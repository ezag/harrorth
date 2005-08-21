#!/usr/bin/perl

use lib "t/lib";
use Forth::Interp::Test;

__END__

=== begin/until
--- forth

: COUNTDOWN
	BEGIN
		DUP .
		1 - DUP 1 <
	UNTIL
;

3 COUNTDOWN

--- result: 3 \s+ 2 \s+ 1



=== do/loop
--- forth

: REP
	4 0 DO
		DUP .
	LOOP
;

3 REP

--- result: 3 \s+ 3 \s+ 3 \s+ 3



=== do/loop end case
--- forth

: REP
	4 3 DO
		DUP .
	LOOP
;

3 REP
--- result: 3



=== begin/while/repeat
--- forth

: SUM
	0
	BEGIN
		OVER 0 >
	WHILE
		OVER +
		SWAP 1 - SWAP
	REPEAT
	SWAP DROP
;

4 SUM .

--- result: 10

