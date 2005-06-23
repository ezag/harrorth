package Interp;

use strict;
use warnings;

sub STATE () { 0 }
sub IP () { 1 }
sub DICT_HEAD () { 2 }

sub new {
	my $pkg = shift;

	my $self = bless {
		heap	=> [
			0,
			undef,
			0,
		],
		rstack	=> [],
		dstack	=> [],
	}, $pkg;

	my %regPrims = (
		"GET-LINE"	=> sub { $self->{buffer} = <> },
		"GET-WORD"	=> sub {
			(my($word), $_) = split(/\s+/, $_, 2) for $self->{buffer};
			my @word = split //, $word;
			push @{$self->{dstack}}, scalar(@{$self->{heap}}), scalar(@word);
			push @{$self->{heap}}, @word;
		},
		"HERE"		=> sub { push @{$self->{dstack}}, scalar @{$self->{heap}} },
		"ALLOT"		=> sub { push @{$self->{heap}}, undef },
		"CELL"		=> sub { push @{$self->{dstack}}, 1 },
		"STATE"		=> sub { push @{$self->{dstack}}, STATE },
		"IP"		=> sub { push @{$self->{dstack}}, IP },
		"DICT-HEAD"	=> sub { push @{$self->{dhstack}}, 2 },
		"!"			=> sub {
			my $address = pop @{$self->{dstack}};
			my $value = pop @{$self->{dstack}};
			$self->{heap}[$address] = $value;
		},
		"@"			=> sub { push @{$self->{dstack}}, $self->{heap}[pop @{$self->{dstack}}] },
		"THROW"		=> sub { @{$self->{rstack}} = (); $self->{heap}[IP] = 0 },
		"PUSH"		=> sub { push @{$self->{dstack}}, $self->{heap}[$self->{heap}[IP]++] },
		"SWAP"		=> sub { push @{$self->{dstack}}, pop @{$self->{dstack}}, pop @{$self->{dstack}} },
		"PICK"		=> sub { my $howmany = pop @{$self->{dstack}}; push @{$self->{dstack}}, ${$self->{dstack}}[-$howmany] },
		"DROP"		=> sub { pop @{$self->{dstack}} },
		"."			=> sub { print pop(@{$self->{dstack}}) . "\n" },
		".S"		=> sub { print "[ @{$self->{dstack}} ]\n" },
		"BSR"		=> sub {
			push @{$self->{rstack}}, 1 + $self->{heap}[IP];
			$self->{heap}[IP] = $self->{heap}[ $self->{heap}[IP] ];
		},
		"EXIT"		=> sub {
			if (@{$self->{rstack}}) {
				$self->{heap}[IP] = pop @{$self->{rstack}};
			} else {
				no warnings;
				last EXEC;
			}
		},
		"JNZ"		=> sub {
			my $branch = $self->{heap}[ $self->{heap}[IP]++ ];
			$self->{heap}[IP] = $branch if (pop @{$self->{dstack}});
		},
		"R>"		=> sub { push @{$self->{dstack}}, pop @{$self->{rstack}} },
		"SEE"		=> sub {
			(my($word), $self->{buffer}) = split /\s+/, $self->{buffer}, 2;
			print ": $word ";
			if (my $def = $self->{word_dict}{$word}){
				while (my $cell = $self->{heap}[$def++]){
					if ($cell == $self->{prim_dict}{EXIT}){
						print ";\n";
						return;
					} elsif($cell == $self->{prim_dict}{BSR}){
						my $word = $self->{heap}[$def++];
						foreach my $key (keys %{ $self->{word_dict} }){
							if ($self->{word_dict}{$key} == $word){
								print "$key ";
								last;
							}
						}
					} elsif ($cell == $self->{prim_dict}{PUSH}){
						print "$self->{heap}[$def++] ";	
					} else {
						print "$self->{inverted_prims}{$cell} ";
					}
				}
			} else {
				print "<prim> ;\n";
			}
		},
		"'"		=> sub {
				(my($word), $self->{buffer}) = split /\s+/, $self->{buffer}, 2;
				push @{$self->{dstack}}, $self->{word_dict}{$word} || $self->{prim_dict}{$word};
		},
		"COMPILE-LITERAL-AT" => sub {
				my $address = pop @{$self->{dstack}};
				my $value = pop @{$self->{dstack}};
				@{$self->{heap}}[$address, $address+1] = ($self->{prim_dict}{PUSH}, $value);
		},
		map { $_ => eval "sub { push \@{$self->{dstack}}, (pop \@{$self->{dstack}}) $_ (pop \@{$self->{dstack}}) }" } qw(+ - * /),
	);
	$_ = [ 0, $_ ] for values %regPrims;
	my %immediatePrims = (
		";"		=> sub { $self->{heap}[STATE] = 0 }
	);

	my $hash = \%regPrims;
	for (keys %$hash) {
		$self->{prim_dict}{$_} = @{$self->{prims} ||= []};
		push @{$self->{prims}}, @{ $hash->{$_} };
		$self->{inverted_prims}{$self->{prim_dict}{$_}} = $_;
	}

	$self->{heap}[DICT_HEAD] = scalar @{ $self->{heap} };

	$self->mkprelude;

	$self;
}

sub loop {
	my $self = shift;
	REDO: {
		&{ $self->{prims}[$self->{prim_dict}{"GET-LINE"} + 1] };
		$_ = uc for $self->{buffer};

		while ($self->{buffer} =~ /\S/) {
			(my ($word), $self->{buffer}) = split /\s+/, $self->{buffer}, 2;
			if (my $def = $self->{word_dict}{$word}){
				$self->{heap}[IP] = $def;
				$self->execute;
			} elsif (my $prim = $self->{prim_dict}{$word}) {
				&{ $self->{prims}[$prim + 1] };
			} elsif ($word =~ /(\d+)/){
				push @{$self->{dstack}}, 0+$1;
			} else {
				warn "blah! $word is bullshit";
			}
		}
		redo;
	}
}

sub execute {
	my $self = shift;
	EXEC: while ($self->{heap}[IP] < @{ $self->{heap} }){
		my $instr = $self->{heap}[$self->{heap}[IP]++];
		my $fun = $self->{prims}[$instr + 1]
			or die "AAAH! invalid opcode";
		&{ $fun };
	}
}

my $i = Interp->new;

push @{$i->{heap}}, (
	$i->{prim_dict}{PUSH}, 0,
	$i->{prim_dict}{JNZ}, (6 + @{$i->{heap}}),
	$i->{prim_dict}{PUSH}, 2,
	$i->{prim_dict}{".S"},
);

$i->loop;

sub mkprelude {
	my $self = shift;
	use Algorithm::Dependency::Objects::Ordered;
	my $prelude = <<PRELUDE;
: REVEAL ; ( FIXME )
: ' ; ( FIXME )

: COMPILE-LITERAL
	HERE
	COMPILE-LITERAL-AT
;

: EXIT
	R> (take the pointer to caller from the return stack)
	IP ! (write the value on the data stack into the instruction pointer)
;
: EXECUTE IP ! ; (EXECUTE is sort of like go to this continuation)
: JMP
	R> DROP (throw away our caller)
	EXECUTE (go to the the code on the stack)
;
: ; [ ' EXIT ] LITERAL COMPILE STATE OFF ; IMMEDIATE
: :
	GET-WORD
	HEADER
	STATE ON
;
: [
	STATE OFF
; IMMEDIATE
: ]
	STATE ON
;
: CREATE
	GET-WORD
	HEADER
	REVEAL
	COMPILE-VAR
;
: LITERAL-SIZE CELL 2 * ;
: LITERAL COMPILE-LITERAL ; IMMEDIATE
: , HERE CELL ALLOT ! ;
: CALL-SIZE CELL ;
: COMPILE , ; ( a call to some code is just the intruction pointer to use )
: COMPILE-VAR
	HERE
	LITERAL-SIZE +
	CALL-SIZE +
	COMPILE-LITERAL (compile the literal for `here` after the compiled exit)
	[ ' EXIT ] LITERAL COMPILE (COMPILE the a call to EXIT)
;
: DOES>
	HERE
	DUP CALL-SIZE -
	COMPILE-LITERAL-AT (overwrite the calll to 'exit' as created by 'COMPILE-VAR')
	[ ' DOES' ] LITERAL COMPILE
	[ ' EXIT ] LITERAL COMPILE (don't execute code appearing after DOES>)
; IMMEDIATE
: DOES'
	( in the word being written to, compile a literal containing )
	LAST-BODY COMPILE-LITERAL
	[ ' JMP ] LITERAL COMPILE
;
: LAST-BODY DICT-HEAD @ HEADER-SIZE + ;
: OPEN-DICT-ENTRY
	DICT-HEAD DUP
	@ , (point the current entry to the old dict head)
	HERE OVER ! (set dictionary head to 'here')
;
: DUP 0 PICK ;
: OVER 1 PICK ;
: HEADER-SIZE
	[ 0
	CELL + (linked list pointer)
	CELL + (immediate)
	CELL + (compile only)
	CELL + (string ptr)
	CELL + (string length)
	] LITERAL
;
: HEADER
	OPEN-DICT-ENTRY
	0 , (immediate?)
	0 , (compile only?)
	, , (string position & length)
;
: IMMEDIATE-FLAG-OF-LAST-WORD
	DICT-HEAD @ CELL +
;
: IMMEDIATE
	IMMEDIATE-FLAG-OF-LAST-WORD ON
;
: IMMEDIATE?
	IMMEDIATE-FLAG-OF-LAST-WORD @
;
: ON TRUE SWAP ! ;
: OFF FALSE SWAP ! ;
: TRUE 1 ;
: FALSE 0 ;
: ? @ . ;
PRELUDE

	my %bootstrap_words;
	
	foreach my $def ($prelude =~ /:\s+(\S+.*?)\s+;\s+(IMMEDIATE)?/sg) {
		$def || next;
		$def =~ s/\(.*?\)//gs;
		my ($name, @def) = split /\s+/, $def;
		$bootstrap_words{$name} = \@def;
	}

	{
		package WordDep;
		use overload '""' => "name";
		
		sub new {
			my $pkg = shift;
			bless {
				name => shift,
				depends => shift || [],
			}, $pkg;
		}
		sub name {
			$_[0]{name}
		}
		sub depends {
			@{$_[0]{depends}}
		}
	}

	my %objs = map { $_ => WordDep->new($_) } keys %{$self->{prim_dict}}, keys %bootstrap_words;
	foreach my $word (keys %bootstrap_words){
		$objs{$word}{depends} = [ map { exists $objs{$_} ? $objs{$_} : () } @{$bootstrap_words{$word}} ];
	}

	my $prims = Set::Object->new(map { $objs{$_} } keys %{$self->{prim_dict}});
	my $boot = Set::Object->new(map { $objs{$_} } keys %bootstrap_words);

	my $d = Algorithm::Dependency::Objects::Ordered->new(
		objects => $prims + $boot,
		selected => $prims,
	);
	my @ordered = map { $_->name } $d->schedule_all;

	my $bsr = $self->{prim_dict}{BSR};
	foreach my $word (@ordered) {
		my @def = @{$bootstrap_words{$word}};
		$self->{word_dict}{$word} = scalar @{$self->{heap}};
		push @{$self->{heap}}, my @compiled = (
			(
				map {
					exists $self->{prim_dict}{$_}
						? ($self->{prim_dict}{$_})
						: (exists ($self->{word_dict}{$_})
							? ($bsr, $self->{word_dict}{$_})
							: (/(\d+)/
								? ($self->{prim_dict}{PUSH}, 0+$1)
								: warn "undefined word $_"
							)
						)
				} @def,
			),
			$self->{prim_dict}{EXIT},
		);
	}
}

<<RUNLOOP
: LOOP
	PARSE-WORD FIND-WORD
	DUP IMMEDIATE? NOT ( is the word regular? )
	STATE @ ( what is the state? )
	AND IF
		,		( put the word pointer in our current word )
	ELSE
		DUP COMPILE-ONLY? IF
			THROW
		THEN
		EXECUTE
	THEN
;
RUNLOOP
