#!/usr/bin/env bash

./pgoc --debug ./test.go
dot test_ast.dot -Tsvg -o test_ast.svg