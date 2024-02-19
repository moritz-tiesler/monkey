package main

import (
	"flag"
	"fmt"
	"monkey/evaluator"
	"monkey/lexer"
	"monkey/object"
	"monkey/parser"
	"monkey/repl"
	"os"
	"os/user"
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
		output := interpretProgamm(string(file))
		println(output)
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

func interpretProgamm(code string) string {
	env := object.NewEnvironment()
	l := lexer.New(code)
	p := parser.New(l)

	program := p.ParseProgram()

	evaluated := evaluator.Eval(program, env)

	return evaluated.Inspect()

}

func replMessage() {
	user, err := user.Current()
	if err != nil {
		panic(err)
	}
	fmt.Printf("Hello %s! This is the monkey programming language!\n", user.Username)
	fmt.Printf("Feel free to type in commands\n")
}
