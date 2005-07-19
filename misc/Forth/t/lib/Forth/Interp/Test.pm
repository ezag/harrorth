#!/usr/bin/perl

package Forth::Interp::Test;
use Test::Base -Base;

filters {
	forth => [qw/run_forth/],
	result => [qw/result_regex/],
};

package Forth::Interp::Test::Filter;
use Test::Base::Filter -Base;

use IPC::Run ();

sub run_forth {
	my $in = shift;
	my $out = `gforth -e "$in bye"`;
	$out;
}

sub result_regex {
	my $value = shift;
	qr/
		^\s*
		$value
		\s*$
	/xs;
}


__END__

=pod

=head1 NAME

Forth::Interp::Test - 

=head1 SYNOPSIS

	use Forth::Interp::Test;

=head1 DESCRIPTION

=cut


