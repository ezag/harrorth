#!/usr/bin/perl

use lib "t/lib";
use Forth::Interp::Test;

__END__

=== state
--- forth: state ?
--- result: [0]


=== brackets
--- forth: : foo [ 6 . ] ;
--- result: 6


=== immediate words and state during compilation
--- forth
: istrue if 1 else 0 then . ;
: foo state @ istrue ; immediate
state on foo
--- result: 1

