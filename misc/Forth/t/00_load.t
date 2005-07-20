#!/usr/bin/perl

use Test::More tests => 2;

my $m; BEGIN { use_ok($m = "Forth::Interp") }

isa_ok($m->new, $m);

