package main

import (
	"flag"
	"fmt"
	"os"
	"os/user"

	"github.com/moritz-tiesler/monkey/compiler"
	"github.com/moritz-tiesler/monkey/evaluator"
	"github.com/moritz-tiesler/monkey/lexer"
	"github.com/moritz-tiesler/monkey/object"
	"github.com/moritz-tiesler/monkey/parser"
	"github.com/moritz-tiesler/monkey/repl"
	"github.com/moritz-tiesler/monkey/vm"
)

func main() {
	// interpretPtr := flag.Bool("i", true, "runs monkey with interpreter")
	compilePtr := flag.Bool("c", false, "runs monkey with compiler")
	filePtr := flag.String("f", "", "runs the provided monkey file")
	flag.Parse()

	in := os.Stdin
	out := os.Stdout
	switch *filePtr {
	case "":
	default:
		filePath := *filePtr
		file, err := os.ReadFile(filePath)
		if err != nil {
			println(fmt.Sprintf("Could not read file %s", filePath))
		}
		switch *compilePtr {
		case true:
			err = compileProgram(string(file))
			if err != nil {
				fmt.Println(err)
			}
		case false:
			err = interpretProgamm(string(file))
			if err != nil {
				fmt.Println(err)
			}
		}
		return
	}

	replMessage()
	switch *compilePtr {
	case true:
		fmt.Printf("Compiling\n")
		repl.StartVM(in, out)
	default:
		fmt.Printf("Interpreting\n")
		repl.StartInterpreter(in, out)
	}
}

func interpretProgamm(code string) error {
	env := object.NewEnvironment()
	l := lexer.New(code)
	p := parser.New(l)

	program := p.ParseProgram()
	if len(p.Errors()) != 0 {
		for _, e := range p.Errors() {
			fmt.Println(e)
		}
		return fmt.Errorf("exited with parser error")
	}

	evaluated := evaluator.Eval(program, env)

	if evalError, ok := evaluated.(*object.Error); ok {
		fmt.Printf("Woops! Evaluation failed:\n %s\n", evalError.Inspect())
		return fmt.Errorf("exited with evaluation error")
	}

	return nil
}

func compileProgram(sourceCode string) error {

	constants := []object.Object{}
	globals := make([]object.Object, vm.GlobalsSize)
	symbolTable := compiler.NewSymbolTable()

	for i, v := range object.Builtins {
		symbolTable.DefineBuiltin(i, v.Name)
	}
	l := lexer.New(sourceCode)
	p := parser.New(l)
	program := p.ParseProgram()
	if len(p.Errors()) != 0 {
		for _, e := range p.Errors() {
			println(e.Error())
		}
		return fmt.Errorf("exited with parser errors")
	}
	comp := compiler.NewWithState(symbolTable, constants)
	err := comp.Compile(program)
	if err != nil {
		fmt.Printf("Woops! Compilation failed:\n %s\n", err)
		return fmt.Errorf("exited with compiler error")
	}

	code := comp.Bytecode()

	machine := vm.NewWithGlobalStore(code, globals)
	err = machine.Run()
	if err != nil {
		fmt.Printf("Woops! Executing bytecode failed:\n %s\n", err)
		return fmt.Errorf("exited with vm error")
	}
	return nil
}

func replMessage() {
	user, err := user.Current()
	if err != nil {
		panic(err)
	}
	fmt.Printf("Hello %s! This is the monkey programming language!\n", user.Username)
	fmt.Printf("Feel free to type in commands\n")
}
