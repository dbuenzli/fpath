#
# A makefile for occasional contributors
#

# Build the project
.PHONY: build
build:
	ocaml pkg/pkg.ml build

# Test the project
.PHONY: test
test:
	ocaml pkg/pkg.ml test
