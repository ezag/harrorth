package Forth::Interp;

use strict;
use warnings;

$| = 1;

use Algorithm::Dependency::Objects::Ordered;

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
		"IS-SPACE"	=> sub {
			push @{$self->{dstack}}, (chr(pop @{$self->{dstack}}) =~ /\s/ ? 1 : 0);
		},
		"EMIT"	=> sub {
			print STDOUT chr(pop @{$self->{dstack}});
		},
		"SEE-CHAR"	=> sub {
			my $char = substr($self->{buffer}, 0, 1);
			push @{$self->{dstack}}, ord($char);
		},
		"SKIP-CHAR"	=> sub {
			substr($self->{buffer}, 0, 1, '');
		},
		"GET-LINE"	=> sub { $self->set_buffer(scalar <>) },
		"GET-WORD"	=> sub { # FIXME refacor
			(my($word), $self->{buffer}) = split(/\s+/, $self->{buffer}, 2);
			my @word = split //, uc($word);
			push @{$self->{dstack}}, scalar(@{$self->{heap}}), scalar(@word);
			push @{$self->{heap}}, @word;
		},
		"HEADER"	=> sub { # FIXME refacor
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
		"WORDS" => sub { # FIXME refacor
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
		"SEARCH-WORDLIST"	=> sub { # FIXME refacor
			my $string = shift; # can also be used internally
			my $push;

			unless (defined $string){
				$push = 1;
				my $length = pop @{$self->{dstack}};
				my $offset = pop @{$self->{dstack}};
				$string = join("", @{$self->{heap}}[$offset .. (($offset + $length) - 1) ]);
			}

			$string = uc($string);

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
		"HERE"		=> sub { push @{$self->{dstack}}, scalar @{$self->{heap}} }, # FIXME refacor : HERE HERE-VAR @ ;
		"ALLOT"		=> sub { push @{$self->{heap}}, (undef) x (pop @{$self->{dstack}}) },
		"CELL"		=> sub { push @{$self->{dstack}}, 1 }, # FIXME refacor constant
		"STATE"		=> sub { push @{$self->{dstack}}, STATE }, # FIXME refacor constant
		"IP"		=> sub { push @{$self->{dstack}}, IP }, # FIXME refacor constant
		"DICT-HEAD"	=> sub { push @{$self->{dstack}}, 2 }, # FIXME refacor constant
		"!"			=> sub {
			my $address = pop @{$self->{dstack}};
			my $value = pop @{$self->{dstack}};
			#warn "writing $value into address $address (state = " . STATE . ")";
			$self->{heap}[$address] = $value;
		},
		"@"			=> sub { push @{$self->{dstack}}, $self->{heap}[pop @{$self->{dstack}}] },
		"THROW"		=> sub { @{$self->{rstack}} = (); $self->{heap}[IP] = 0 }, # FIXME refacor : THROW RCLEAR 0 IP ! ;
		"PUSH"		=> sub { push @{$self->{dstack}}, $self->{heap}[$self->{heap}[IP]++] },
		"PICK"		=> sub { my $howmany = pop @{$self->{dstack}}; push @{$self->{dstack}}, ${$self->{dstack}}[0-($howmany+1)] },
		"ROLL"		=> sub { my $howmany = pop @{$self->{dstack}}; push @{$self->{dstack}}, splice(@{$self->{dstack}}, 0-($howmany+1), 1) },
		"DROP"		=> sub { pop @{$self->{dstack}} },
		"."			=> sub { print pop(@{$self->{dstack}}) . "\n" },
		".S"		=> sub { print "[ @{$self->{dstack}} ]\n" }, # FIXME refacor in terms of a loop and .
		"R.S"		=> sub { print "[ @{$self->{rstack}} ]\n" }, # FIXME refacor in terms of a loop and .
		"BSR"		=> sub {
			push @{$self->{rstack}}, 1 + $self->{heap}[IP];
			my $addr = $self->{heap}[ $self->{heap}[IP] ];
			#warn "jumping to addr $addr (" . $self->string_at_addr($addr) . ")";

			$self->{heap}[IP] = HEADER_SIZE + $self->{heap}[ $self->{heap}[IP] ];
		},
		"JZ"		=> sub {
			my $branch = $self->{heap}[ $self->{heap}[IP]++ ];
			$self->{heap}[IP] = $branch if (pop @{$self->{dstack}} == 0);
		},
		"JMP"		=> sub {
			$self->{heap}[IP] = $self->{heap}[ $self->{heap}[IP]++ ];
		},
		# FIXME - all the R primitives are broken since they are wrapped in words
		"R>DROP"	=> sub { splice(@{$self->{rstack}}, -2, 1) },
		"R@"		=> sub { push @{$self->{dstack}}, $self->{rstack}[-2] },
		"R>NIP"		=> sub { splice(@{$self->{rstack}}, -3, 1) },
		"SEE"		=> sub { # FIXME refacor
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
					} elsif ($cell == $self->{prim_dict}{TAIL}){
						$def = $self->{heap}[$def];
						print "(does:) ";
						redo;
					} elsif ($cell == $self->{prim_dict}{RET}){
						print ";" . ($immediate ? " IMMEDIATE" : "") . "\n";
						return;
					} elsif ($cell == $self->{prim_dict}{BSR}){
						my $word = $self->{heap}[$def++];
						my $str_ent = $word + 3;
						my ($offset, $length) = @{$self->{heap}}[$str_ent, $str_ent + 1];
						my $string = join("", @{$self->{heap}}[$offset .. (($offset + $length) - 1) ]);

						#print "<word $string> ";
						print "$string ";
					} elsif ($cell == $self->{prim_dict}{PUSH}){
						print "$self->{heap}[$def++] ";
					} else {
						if ($cell == $self->{prim_dict}{JMP} or $cell == $self->{prim_dict}{JZ}){
							print "<$self->{prim_name_by_code}[$cell] +" . (($self->{heap}[$def++] - $def) - 1) . "> ";
						} else {
							#print "<prim $cell $self->{prim_name_by_code}[$cell]> ";
							print "$self->{prim_name_by_code}[$cell] "
						}
					}
					redo;
				}
			} else {
				warn "$word is not a word!";
			}
		},
		"COMPILE-LITERAL-AT" => sub {
			my $address = pop @{$self->{dstack}};
			my $value = pop @{$self->{dstack}};
			@{$self->{heap}}[$address, $address+1] = ($self->{prim_dict}{PUSH}, $value);
		},
		(map { $_ => eval 'sub { use integer; my $y = pop @{$self->{dstack}}; my $x = pop @{$self->{dstack}}; push @{$self->{dstack}}, 0+($x '. $_ .' $y )}' || die $@ } qw(+ - * / < > <= >= == && ||)),
		"APPEND-TO-COMPILING"	=> sub { # FIXME refacor
			my @def = @{$self->{heap}}[$self->{rstack}[-1]++, $self->{rstack}[-1]++];
			# @def == (code of BSR, word to jump to)
			push @{$self->{heap}}, @def;
		},
		"APPEND-PRIM-TO-COMPILING" => sub { # FIXME refacor in terms of compile prim at
			$self->{rstack}[-1]++;
			my $def = $self->{heap}[$self->{rstack}[-1]++];
			#warn "writing prim $self->{prim_name_by_code}[$self->{heap}[$def + HEADER_SIZE]] into cell " . scalar @{$self->{heap}};
			push @{$self->{heap}}, $self->{heap}[$def + HEADER_SIZE];
		},
		"COMPILE-PRIM-AT" => sub { # FIXME refacor, can use rstack
			$self->{rstack}[-1]++;
			my $def = $self->{heap}[$self->{rstack}[-1]++];
			$self->{heap}[pop @{$self->{dstack}}] = $self->{heap}[$def + HEADER_SIZE];
		},
	);
	$regPrims{TAIL} = $regPrims{JMP};

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

sub set_buffer {
	my $self = shift;
	$self->{buffer} = shift;
}

sub loop {
	my $self = shift;
	{
		&{ $self->{prims}[$self->{prim_dict}{"GET-LINE"}] };
		$self->run_buffer;
		redo;
	}
}

sub run_buffer {
	my $self = shift;

	while (($self->{buffer} ||='') =~ /\S/) {
		$self->{buffer} =~ s/^\s*\(.*?\)\s*// and redo;
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

my (@ordered, %bootstrap_words, %immediate);
sub mkprelude {
	my $self = shift;

	unless (@ordered){
		#warn "analyzing prelude interdependencies";
		my $t = times;
		my $bootstrap = <<BOOTSTRAP;
: REVEAL ; ( FIXME )

: HEADER
	HERE
	DICT-HEAD @ , ( pointer to the previous word )
	0 , ( immediate flag )
	0 , ( compile only flag )
	ROT ROT ( stash the pointer to this dictionary entry below the string pointers/length )
	SWAP , , ( write the string for the name of the entry in a the logical order it's pushed on the stack )
	DICT-HEAD ! (write old here into the dictionary pointer)
;

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

: IF
	APPEND-PRIM-TO-COMPILING JZ
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
	HERE SWAP !
; IMMEDIATE

: EXIT R>DROP ;

: EXECUTE IP ! ; (EXECUTE is sort of like go to this continuation)
: ;
	APPEND-PRIM-TO-COMPILING RET
	STATE OFF
; IMMEDIATE
: :
	GET-WORD
	HEADER
	STATE ON
;
: :NONAME
	HERE
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
: CALL-SIZE OP-SIZE CELL + ;
: COMPILE , ; ( a call to some code is just the intruction pointer to use )
: COMPILE-VAR
	HERE LITERAL-SIZE + OP-SIZE + OP-SIZE +
	COMPILE-LITERAL
	APPEND-PRIM-TO-COMPILING RET
	APPEND-PRIM-TO-COMPILING RET
;
: OPEN-DICT-ENTRY
	DICT-HEAD
	DUP @ , (point the current entry to the old dict head)
	HERE SWAP ! (set dictionary head to 'here')
;
: DUP 0 PICK ;
: OVER 1 PICK ;
: HEADER
	OPEN-DICT-ENTRY
	0 , (immediate?)
	0 , (compile only?)
	, , (string position & length)
;

: LAST-DICT-ENTRY
	DICT-HEAD @
;

: IMMEDIATE-FLAG-OFFSET CELL ;

: IMMEDIATE-FLAG-OF-LAST-WORD
	LAST-DICT-ENTRY IMMEDIATE-FLAG-OFFSET +
;
: IMMEDIATE
	IMMEDIATE-FLAG-OF-LAST-WORD ON
;
: IMMEDIATE?
	IMMEDIATE-FLAG-OFFSET + @
;
: ON TRUE SWAP ! ;
: OFF FALSE SWAP ! ;
: TRUE 1 ;
: FALSE 0 ;
: ? @ . ;
BOOTSTRAP


		foreach my $def ($bootstrap =~ /:\s+(\S+.*?)(?=:|$)/sg) {
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
		@ordered = map { $_->name } $d->schedule_all;
		#warn "dependency resolution took " . (times()-$t);
	}

	my $bsr = $self->{prim_dict}{BSR};
	my $push = $self->{prim_dict}{PUSH};
	my $exit = $self->{prim_dict}{RET};
	my $lkup = $self->{prims}[$self->{prim_dict}{"SEARCH-WORDLIST"}];
	my $hdr = $self->{prims}[$self->{prim_dict}{HEADER}];

	#warn "bootstrapping interpreter";
	#my $t = times();

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

	$self->set_buffer(<<PRELUDE);
: BEGIN
	HERE
; IMMEDIATE

: UNTIL
	APPEND-PRIM-TO-COMPILING JZ
	HERE !
; IMMEDIATE

: WHILE
	APPEND-PRIM-TO-COMPILING JZ
	HERE
	0 ,
; IMMEDIATE

: REPEAT
	APPEND-PRIM-TO-COMPILING JMP
	SWAP , ( address of the WHILE is where we JMP to, as set by BEGIN )
	HERE SWAP !  ( this is where WHILE JZs when false )
; IMMEDIATE

: NOT IF 0 ELSE 1 THEN ;

: STATE? STATE ? ; IMMEDIATE

: HEADER-SIZE
	[
		0
		CELL + (linked list pointer)
		CELL + (immediate)
		CELL + (compile only)
		CELL + (string ptr)
		CELL + (string length)
	] LITERAL
;

: LAST-BODY DICT-HEAD @ HEADER-SIZE + ;

: ?DUP DUP IF DUP THEN ;

: RECURSIVE ; IMMEDIATE

: RECURSE LAST-BODY COMPILE-LITERAL APPEND-TO-COMPILING EXECUTE ; IMMEDIATE

: DEFER
	GET-WORD
	HEADER
	CALL-SIZE ALLOT
;

: ' GET-WORD SEARCH-WORDLIST HEADER-SIZE + ;

: IS'
	DUP
	COMPILE-PRIM-AT TAIL
	OP-SIZE + !
;

: IS ' IS' ;
: :DEFERRED ' HERE SWAP IS' STATE ON ;

: DOES'
	LAST-BODY ( the new word which is now only a variable)
	LITERAL-SIZE + ( after the variable address is pushed )
	DUP
	COMPILE-PRIM-AT TAIL ( we replace the call to 'RET' with a call to 'TAIL' )
	1 +

	R@ 1 + ( the generic caller is compiled after the meta word's return op, that is the cell after our caller continuation )
	SWAP !
;

: DOES>
	APPEND-TO-COMPILING DOES'
	APPEND-PRIM-TO-COMPILING RET
; IMMEDIATE

: SKIP-SPACE
	BEGIN
		SEE-CHAR IS-SPACE
	WHILE
		SKIP-CHAR
	REPEAT
;

: READ-UNTIL
	SKIP-SPACE

	HERE SWAP
	BEGIN
		SEE-CHAR OVER EXECUTE NOT
	WHILE
		SEE-CHAR ,
		SKIP-CHAR
	REPEAT

	SKIP-CHAR (the delimiter is also dropped)
	SKIP-SPACE

	DROP ( the predicate )

	DUP HERE SWAP -
;

: TYPE
	OVER OVER
	BEGIN
		DUP 0 >
	WHILE
		SWAP DUP @ EMIT
		1 +
		SWAP 1 -
	REPEAT

	DROP DROP
;

' IS-SPACE
: READ-WORD
	LITERAL
	READ-UNTIL
;

: = == ;
: AND && ;
: OR || ;

: EQ" 34 = ;

' EQ"
: S"
	APPEND-PRIM-TO-COMPILING JMP
	HERE
	0 ,
	LITERAL
	READ-UNTIL
	2 PICK HERE SWAP !
	SWAP
	COMPILE-LITERAL
	COMPILE-LITERAL
	DROP
; IMMEDIATE

PRELUDE
	$self->run_buffer;
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
