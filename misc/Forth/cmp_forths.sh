#!/bin/sh

for FORTH_TEST_BACKEND in gforth p5orth; do
	echo "backend: $FORTH_TEST_BACKEND";

	export FORTH_TEST_BACKEND;
	prove -Ilib t;

	echo
done

