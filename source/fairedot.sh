#!/usr/bin/env bash

./pgoc --debug --type-only ./test.go
dot test_ast.dot -Tsvg -o test_ast.svg