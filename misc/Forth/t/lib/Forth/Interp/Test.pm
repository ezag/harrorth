#!/usr/bin/perl

package Forth::Interp::Test;
use Test::Base -Base;

filters {
	skip => [qw/fudge_skip_list/],
	todo => [qw/fudge_skip_list/],
	forth => [qw/run_forth/],
	result => [qw/result_regex/],
};

package Forth::Interp::Test::Filter;
use Test::Base::Filter -Base;

sub fudge_skip_list {
	my @skip_backends = map { split /\s+/ } @_;
	my $backend = $ENV{FORTH_TEST_BACKEND} || "p5orth";
	if (grep { $_ eq $backend } @skip_backends){
		"$backend is in skip list";
	} else { return undef }
}

sub run_forth {
	my @skip_backends = map { split /\s+/ } grep { defined } @{$self->current_block->{skip}};

	my $in = shift;

	my $backend = $ENV{FORTH_TEST_BACKEND} || "p5orth";

	if (grep { $_ eq "$backend" } @skip_backends){
		return undef;
	} elsif ($backend eq "p5orth"){
		require Forth::Interp;

		local *STDOUT;
		open STDOUT, ">", \(my $out = "");
		local *STDERR;
		open STDERR, ">", \$out;

		my $i = Forth::Interp->new;
		$i->set_buffer($in);
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


