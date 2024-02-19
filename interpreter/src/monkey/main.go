package main

import (
	"flag"
	"fmt"
	"monkey/repl"
	"os"
	"os/user"
)

func main() {
	// interpretPtr := flag.Bool("i", true, "runs monkey with interpreter")
	compilePtr := flag.Bool("c", false, "runs monkey with compiler")
	flag.Parse()
	user, err := user.Current()
	if err != nil {
		panic(err)
	}
	fmt.Printf("Hello %s! This is the monkey programming language!\n", user.Username)
	fmt.Printf("Feel free to type in commands\n")

	switch *compilePtr {
	case true:
		fmt.Printf("Compiling\n")
		repl.StartVM(os.Stdin, os.Stdout)
	default:
		fmt.Printf("Interpreting\n")
		repl.StartInterpreter(os.Stdin, os.Stdout)
	}
}
