#!/usr/bin/perl

package Forth::Interp::Test;
use Test::Base -Base;

filters {
	forth => [qw/run_forth/],
	result => [qw/result_regex/],
};

package Forth::Interp::Test::Filter;
use Test::Base::Filter -Base;


sub run_forth {
	my $in = shift;

	my $backend = $ENV{FORTH_TEST_BACKEND} || "p5orth";

	if ($backend eq "p5orth"){
		require Forth::Interp;

		local *STDOUT;
		open STDOUT, ">", \(my $out = "");

		my $i = Forth::Interp->new;
		$i->set_buffer(uc($in));
		$i->run_buffer;
		
		return $out;
	} elsif ($backend eq "gforth"){
		require IPC::Run;
		
		IPC::Run::run([ qw/gforth -e/, $in . " bye" ], ">", \(my $out)) or die "$!";

		$out;
	}
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


