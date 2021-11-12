
EXE=_build/default/main.exe

all: $(EXE)

$(EXE): *.ml*
	dune build @all
	cp $(EXE) pgoc 

test: $(EXE) test.go
	./pgoc --debug test.go
	gcc -g -no-pie test.s -o test
	./test
	go run test.go

export-%:
	cp test.go ../tests/exec/$*.go
	go run test.go > ../tests/exec/$*.out

.PHONY: clean
clean:
	dune clean
