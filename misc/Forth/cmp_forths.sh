#!/bin/sh

for FORTH_TEST_BACKEND in gforth p5orth; do
	echo "backend: $FORTH_TEST_BACKEND";

	export FORTH_TEST_BACKEND;
	test_files=$*;
	if [ -z "$test_files" ]; then test_files=t; fi
	prove -Ilib $test_files

	echo
done

