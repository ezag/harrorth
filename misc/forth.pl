package Forth::Interp;

use strict;
use warnings;

sub STATE () { 0 }
sub IP () { 1 }
sub DICT_HEAD () { 2 }
sub HEADER_SIZE () { 5 }

sub new {
	my $pkg = shift;

	# lexicals in closures soon
	my @dstack = ();
	my @rstack = ();
	my @heap = ();

	my $self = bless {
		heap	=> [
			0,
			0,
			0,
		],
		rstack	=> [],
		dstack	=> [],
	}, $pkg;

	my %regPrims = (
		"BYE"		=> sub { exit },
		"RET"		=> sub {
			#warn "return stack is @{$self->{rstack}}, returning";
			if (@{$self->{rstack}}) {
				$self->{heap}[IP] = pop @{$self->{rstack}};
			} else {
				no warnings;
				last EXEC;
			}
		},
		"GET-LINE"	=> sub { $self->{buffer} = <> },
		"GET-WORD"	=> sub {
			(my($word), $self->{buffer}) = split(/\s+/, $self->{buffer}, 2);
			my @word = split //, $word;
			push @{$self->{dstack}}, scalar(@{$self->{heap}}), scalar(@word);
			push @{$self->{heap}}, @word;
		},
		"HEADER"	=> sub {
			my $length = pop @{$self->{dstack}};
			my $offset = pop @{$self->{dstack}};
			my $addr = scalar @{$self->{heap}};
			push @{$self->{heap}}, (
				$self->{heap}[DICT_HEAD],
				0, #immediate
				0, #compile only
				$offset,
				$length,
			);
			$self->{heap}[DICT_HEAD] = $addr;
		},
		"WORDS" => sub {
			my $all_words;

		   	$all_words = sub {
				my $entry = shift || return ();

				#warn "reading entry at $entry... $self->{heap}[$entry .. $entry+10]";
				
				my $next = $self->{heap}[$entry];
				my $offset = $self->{heap}[$entry + 3];
				my $length = $self->{heap}[$entry + 4];

				my $string = join("", @{$self->{heap}}[$offset .. (($offset + $length) - 1) ]);

				return $string, &{ $all_words }($next);
			};

			my %seen;
			my @words = grep { !$seen{$_}++ } &{$all_words}($self->{heap}[DICT_HEAD]);

			local $\ = "\n";
			local $, = ", ";
			print @words;
		},
		"SEARCH-WORDLIST"	=> sub {
			my $string = shift; # can also be used internally
			my $push;
			
			unless (defined $string){
				$push = 1;
				my $length = pop @{$self->{dstack}};
				my $offset = pop @{$self->{dstack}};
				$string = join("", @{$self->{heap}}[$offset .. (($offset + $length) - 1) ]);
			}

			my $dict = $self->{heap}[DICT_HEAD];
			while ($dict != 0){
				my $str_ent = $dict + 3;

				my ($offset, $length) = @{$self->{heap}}[$str_ent, $str_ent + 1];
				my $entStr = join("", @{$self->{heap}}[$offset .. (($offset + $length) - 1) ]);
				
				last if $entStr eq $string; # found
				
				$dict = $self->{heap}[$dict];
			}

			$push ? (push @{$self->{dstack}}, $dict) : $dict; # if it was found it's != 0
		},
		"HERE"		=> sub { push @{$self->{dstack}}, scalar @{$self->{heap}} },
		"ALLOT"		=> sub { push @{$self->{heap}}, (undef) x (pop @{$self->{dstack}}) },
		"CELL"		=> sub { push @{$self->{dstack}}, 1 },
		"STATE"		=> sub { push @{$self->{dstack}}, STATE },
		"IP"		=> sub { push @{$self->{dstack}}, IP },
		"DICT-HEAD"	=> sub { push @{$self->{dstack}}, 2 },
		"!"			=> sub {
			my $address = pop @{$self->{dstack}};
			my $value = pop @{$self->{dstack}};
			#warn "writing $value into address $address (state = " . STATE . ")";
			$self->{heap}[$address] = $value;
		},
		"@"			=> sub { push @{$self->{dstack}}, $self->{heap}[pop @{$self->{dstack}}] },
		"THROW"		=> sub { @{$self->{rstack}} = (); $self->{heap}[IP] = 0 },
		"PUSH"		=> sub { push @{$self->{dstack}}, $self->{heap}[$self->{heap}[IP]++] },
		"PICK"		=> sub { my $howmany = pop @{$self->{dstack}}; push @{$self->{dstack}}, ${$self->{dstack}}[-$howmany] },
		"ROLL"		=> sub { my $howmany = pop @{$self->{dstack}}; push @{$self->{dstack}}, splice(@{$self->{dstack}}, 0-($howmany+1), 1) },
		"DROP"		=> sub { pop @{$self->{dstack}} },
		"."			=> sub { print pop(@{$self->{dstack}}) . "\n" },
		".S"		=> sub { print "[ @{$self->{dstack}} ]\n" },
		"BSR"		=> sub {
			push @{$self->{rstack}}, 1 + $self->{heap}[IP];
			my $addr = $self->{heap}[ $self->{heap}[IP] ];
			#warn "jumping to addr $addr (" . $self->string_at_addr($addr) . ")";
			
			$self->{heap}[IP] = HEADER_SIZE + $self->{heap}[ $self->{heap}[IP] ];
		},
		"JNZ"		=> sub {
			my $branch = $self->{heap}[ $self->{heap}[IP]++ ];
			$self->{heap}[IP] = $branch if (pop @{$self->{dstack}} == 0);
		},
		"JMP"		=> sub {
			$self->{heap}[IP] = $self->{heap}[ $self->{heap}[IP]++ ];
		},
		"R>"		=> sub { push @{$self->{dstack}}, pop @{$self->{rstack}} },
		"SEE"		=> sub {
			(my($word), $self->{buffer}) = split /\s+/, $self->{buffer}, 2;
			my $lkup = $self->{prims}[$self->{prim_dict}{"SEARCH-WORDLIST"}];
			if (my $def = &{ $lkup }($word)){
				print ": $word ";

				$def++; # dict link
				my $immediate = $self->{heap}[$def++];
				my $compile_only = $self->{heap}[$def++];
				$def++;
				$def++;

				{
					my $cell = $self->{heap}[$def++];
					if ($cell == $self->{prim_dict}{"APPEND-TO-COMPILING"}){
						print "APPEND-TO-COMPILING $self->{heap}[$def++] ";
					}
					if ($cell == $self->{prim_dict}{RET}){
						print ";" . ($immediate ? " IMMEDIATE" : "") . "\n";
						return;
					} elsif($cell == $self->{prim_dict}{BSR}){
						my $word = $self->{heap}[$def++];
						my $str_ent = $word + 3;
						my ($offset, $length) = @{$self->{heap}}[$str_ent, $str_ent + 1];
						my $string = join("", @{$self->{heap}}[$offset .. (($offset + $length) - 1) ]);

						print "<word $string> ";
					} elsif ($cell == $self->{prim_dict}{PUSH}){
						print "$self->{heap}[$def++] ";
					} else {
						if ($cell == $self->{prim_dict}{JMP} or $cell == $self->{prim_dict}{JNZ}){
							print "<$self->{prim_name_by_code}[$cell] +" . (($self->{heap}[$def++] - $def) - 1) . "> ";
						} else {
							print "<prim $cell $self->{prim_name_by_code}[$cell]> ";
						}
					}
					redo;
				}
			} else {
				warn "$word is not a word!";
			}
		},
		"'"		=> sub {
				(my($word), $self->{buffer}) = split /\s+/, $self->{buffer}, 2;
				push @{$self->{dstack}}, &{ $self->{prims}[$self->{prim_dict}{"SEARCH-WORDLIST"}] }($word);
		},
		"COMPILE-LITERAL-AT" => sub {
				my $address = pop @{$self->{dstack}};
				my $value = pop @{$self->{dstack}};
				@{$self->{heap}}[$address, $address+1] = ($self->{prim_dict}{PUSH}, $value);
		},
		(map { $_ => eval 'sub { push @{$self->{dstack}}, (pop @{$self->{dstack}}) '. $_ .'(pop @{$self->{dstack}}) }' } qw(+ - * /)),
	#);
	#my %immediatePrims = (
		"APPEND-TO-COMPILING"	=> sub {
				my @def = @{$self->{heap}}[$self->{rstack}[-1]++, $self->{rstack}[-1]++];
				# @def == (code of BSR, word to jump to)
				push @{$self->{heap}}, @def;
		},
		"APPEND-PRIM-TO-COMPILING" => sub {
				$self->{rstack}[-1]++;
				my $def = $self->{heap}[$self->{rstack}[-1]++];
				#warn "writing prim $self->{prim_name_by_code}[$self->{heap}[$def + HEADER_SIZE]] into cell " . scalar @{$self->{heap}};
				push @{$self->{heap}}, $self->{heap}[$def + HEADER_SIZE];
		},
	);

	my %immediatePrims = ();
	$_ = [ 0, $_ ] for values %regPrims;
	$_ = [ 1, $_ ] for values %immediatePrims;

	foreach my $hash (\%regPrims, \%immediatePrims) {
		for (sort { ($a eq "RET") ? -1 : (($b eq "RET") ? 1 : 0) } keys %$hash) {
			$self->{prim_dict}{$_} = @{ $self->{prims} ||= [] };
			$self->{prim_name_by_code}[$self->{prim_dict}{$_}] = $_;
			my ($immediate, $fun) = @{ $hash->{$_} };
			push @{$self->{prims}}, $fun;

			my @name = split //;
			my $str = scalar @{$self->{heap}};
			my $len = scalar @name;
			push @{$self->{heap}}, @name;

			my $cur = scalar @{$self->{heap}};
			my $last = $self->{heap}[DICT_HEAD];

			push @{$self->{heap}}, ($last, $immediate, $immediate, $str, $len, $self->{prim_dict}{$_}, $self->{prim_dict}{RET});

			$self->{heap}[DICT_HEAD] = $cur;
		}
	}

	$self->mkprelude;

	$self;
}

sub loop {
	my $self = shift;
	REDO: {
		&{ $self->{prims}[$self->{prim_dict}{"GET-LINE"}] };
		$_ = uc for $self->{buffer};

		while ($self->{buffer} =~ /\S/) {
			(my ($word), $self->{buffer}) = split /\s+/, $self->{buffer}, 2;

			my $state = $self->{heap}[STATE];
			my $lkup = $self->{prims}[$self->{prim_dict}{"SEARCH-WORDLIST"}];
			
			#warn "looping on buffer... nibbled $word, currently in state $state";
		
			if (my $found = &{ $lkup }($word)){
				my $immediate = $self->{heap}[$found + 1];

				if ($state && !$immediate){
					#warn "compiling BSR $word ($found)";
					push @{$self->{heap}}, $self->{prim_dict}{BSR}, $found;
				} else {
					$self->{heap}[IP] = $found + HEADER_SIZE;
					$self->execute;
				}
			} elsif ($word =~ /(\d+)/){
				if ($state) {
					#warn "compiling literal $1 into stack";
					push @{$self->{heap}}, $self->{prim_dict}{PUSH}, 0+$1;
				} else {
					push @{$self->{dstack}}, 0+$1;
				}
			} else {
				warn "blah! $word is bullshit";
				$self->{buffer} = "";
			}

			#warn "finished $word... arrived at state $self->{heap}[STATE]";
		}
		redo;
	}
}

sub execute {
	my $self = shift;
	EXEC: while ($self->{heap}[IP] < @{ $self->{heap} }){
		my $instr = $self->{heap}[my $ip = $self->{heap}[IP]++];
		#warn "heap = < ... @{$self->{heap}}[ ($ip - 3) .. ($ip + 2) ] ... >";
		#warn "applying primitive $self->{prim_name_by_code}[$instr], stack=[@{$self->{dstack}}] state=$self->{heap}[STATE]";
		my $fun = $self->{prims}[$instr]
			or die "AAAH! invalid opcode";
		&{ $fun };
	}
}

my $i = Forth::Interp->new;

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

: ROT
	2 ROLL
;

: SWAP
	1 ROLL
;

: STRCMP
	ROT

;

: IF
	APPEND-PRIM-TO-COMPILING JNZ
	HERE
	0 ,
; IMMEDIATE

: THEN
	HERE SWAP !
; IMMEDIATE

: ELSE
	APPEND-PRIM-TO-COMPILING JMP
	HERE
	SWAP
	0 ,
	THEN (close off the if)
; IMMEDIATE

: EXIT
	R> (take the pointer to caller from the return stack)
	IP ! (write the value on the data stack into the instruction pointer)
;
: EXECUTE IP ! ; (EXECUTE is sort of like go to this continuation)
: ; APPEND-PRIM-TO-COMPILING RET STATE OFF ; IMMEDIATE
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
: VARIABLE
	CREATE
	0 ,
;
: LITERAL-SIZE CELL 2 * ;
: LITERAL COMPILE-LITERAL ; IMMEDIATE
: , HERE CELL ALLOT ! ;
: OP-SIZE CELL ;
: COMPILE , ; ( a call to some code is just the intruction pointer to use )
: COMPILE-VAR
	HERE LITERAL-SIZE + OP-SIZE +
	COMPILE-LITERAL
	APPEND-PRIM-TO-COMPILING RET
;
: DOES>
	HERE
	DUP OP-SIZE -
	COMPILE-LITERAL-AT (overwrite the calll to 'exit' as created by 'COMPILE-VAR')
	APPEND-TO-COMPILING DOES'
	APPEND-PRIM-TO-COMPILING RET (don't execute code appearing after DOES>)
; IMMEDIATE
: DOES'
	( in the word being written to, compile a literal containing )
	LAST-BODY COMPILE-LITERAL
	APPEND-PRIM-TO-COMPILING JMP
;
: LAST-BODY DICT-HEAD @ HEADER-SIZE + ;
: OPEN-DICT-ENTRY
	DICT-HEAD
	DUP @ , (point the current entry to the old dict head)
	HERE SWAP ! (set dictionary head to 'here')
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

	my (%bootstrap_words, %immediate);
	
	foreach my $def ($prelude =~ /:\s+(\S+.*?)(?=:|$)/sg) {
		$def || next;
		$def =~ s/\(.*?\)//gs;
		my ($name, @def) = split /\s+/, $def;
		my $immediate = 0;
		while ($def[$#def] ne ";"){
			$immediate ||= (pop @def eq "IMMEDIATE");
		}
		pop @def;
		$bootstrap_words{$name} = \@def;
		$immediate{$name} = $immediate;
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
	my $push = $self->{prim_dict}{PUSH};
	my $exit = $self->{prim_dict}{RET};
	my $lkup = $self->{prims}[$self->{prim_dict}{"SEARCH-WORDLIST"}];
	my $hdr = $self->{prims}[$self->{prim_dict}{HEADER}];

	foreach my $word (@ordered) {
		#warn "bootstrapping $word";
		my @def = @{$bootstrap_words{$word}};

		my @word = split //, $word;
		push @{$self->{dstack}}, (my @str = (scalar @{$self->{heap}}), (scalar @word));
		push @{$self->{heap}}, @word;

		my $ent = @{$self->{heap}};
		
		&{ $hdr }();

		@{$self->{heap}}[$ent+1, $ent+2] = ($immediate{$word}) x 2; # immediate, compile only
		
		push @{$self->{heap}}, my @compiled = (
			(
				map {
					my $found = &{ $lkup }($_);

					$found ? ($bsr, $found) : (
						/(\d+)/
							? ($push, 0+$1)
							: ()
					);
				} @def,
			),
			$exit,
		);
	}
}

<<RUNLOOP;
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


sub string_at_addr {
	my $self = shift;
	my $entry = shift;
	
	my $offset = $self->{heap}[$entry + 3];
	my $length = $self->{heap}[$entry + 4];

	my $string = join("", @{$self->{heap}}[$offset .. (($offset + $length) - 1) ]);
}