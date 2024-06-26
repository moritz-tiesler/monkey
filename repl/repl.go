package repl

import (
	"bufio"
	"bytes"
	"fmt"
	"io"

	"github.com/moritz-tiesler/monkey/compiler"
	"github.com/moritz-tiesler/monkey/evaluator"
	"github.com/moritz-tiesler/monkey/exception"
	"github.com/moritz-tiesler/monkey/lexer"
	"github.com/moritz-tiesler/monkey/object"
	"github.com/moritz-tiesler/monkey/parser"
	"github.com/moritz-tiesler/monkey/vm"
)

const PROMPT = ">>"

const MONKEY_FACE = ` /~\
 C oo
 _( ^)
/   ~\
`

func Start(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)
	env := object.NewEnvironment()

	symbolTable := compiler.NewSymbolTable()
	for i, v := range object.Builtins {
		symbolTable.DefineBuiltin(i, v.Name)
	}
	for run := true; run; run = scanner.Text() != "exit" {
		fmt.Fprint(out, PROMPT)
		scanned := scanner.Scan()
		if !scanned {
			return
		}

		line := scanner.Text()
		l := lexer.New(line)
		p := parser.New(l)

		program := p.ParseProgram()
		if len(p.Errors()) != 0 {
			printParserErrors(out, p.Errors())
			continue
		}

		evaluated := evaluator.Eval(program, env)
		if evaluated != nil {
			io.WriteString(out, evaluated.Inspect())
			io.WriteString(out, "\n")
		}

	}
}

func printParserErrors(out io.Writer, errors []exception.Exception) {
	io.WriteString(out, MONKEY_FACE)
	io.WriteString(out, "Woops! We ran into some monkey business here!\n")
	io.WriteString(out, " parser errors:\n")
	for _, e := range errors {
		io.WriteString(out, "\t"+e.Error()+"\n")
	}
}

func StartVM(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)

	constants := []object.Object{}
	globals := make([]object.Object, vm.GlobalsSize)
	symbolTable := compiler.NewSymbolTable()

	for i, v := range object.Builtins {
		symbolTable.DefineBuiltin(i, v.Name)
	}
	for {
		fmt.Fprintf(out, PROMPT)
		scanned := scanner.Scan()
		if !scanned {
			return
		}
		line := scanner.Text()
		if line == "exit()" {
			return
		}
		l := lexer.New(line)
		p := parser.New(l)
		program := p.ParseProgram()
		if len(p.Errors()) != 0 {
			printParserErrors(out, p.Errors())
			continue
		}
		comp := compiler.NewWithState(symbolTable, constants)
		err := comp.Compile(program)
		if err != nil {
			fmt.Fprintf(out, "Woops! Compilation failed:\n %s\n", err)
			continue
		}

		code := comp.Bytecode()
		constants = code.Constants

		machine := vm.NewWithGlobalStore(code, globals)
		err = machine.Run()
		if err != nil {
			fmt.Fprintf(out, "Woops! Executing bytecode failed:\n %s\n", err)
			continue
		}
		stackTop := machine.LastPoppedStackElem()
		io.WriteString(out, stackTop.Inspect())
		io.WriteString(out, "\n")
	}
}

func sendErrors(ch chan<- string, errors []exception.Exception) {
	message := MONKEY_FACE
	message += "Woops! We ran into some monkey business here!\n"
	message += " parser errors:\n"
	for _, e := range errors {
		message += "\t" + e.Error() + "\n"
	}
	ch <- message
}

func StartStream(in chan string, out chan string) {
	constants := []object.Object{}
	globals := make([]object.Object, vm.GlobalsSize)
	symbolTable := compiler.NewSymbolTable()

	for i, v := range object.Builtins {
		symbolTable.DefineBuiltin(i, v.Name)
	}
	for {
		var b bytes.Buffer
		b.WriteString(<-in)
		scanner := bufio.NewScanner(bytes.NewBuffer(b.Bytes()))
		scanned := scanner.Scan()
		if !scanned {
			return
		}
		line := scanner.Text()
		if line == "exit()" {
			out <- "Exited monkey repl"
			return
		}
		l := lexer.New(line)
		p := parser.New(l)
		program := p.ParseProgram()
		if len(p.Errors()) != 0 {
			sendErrors(out, p.Errors())
			continue
		}
		comp := compiler.NewWithState(symbolTable, constants)
		err := comp.Compile(program)
		if err != nil {
			// fmt.Fprintf(out, "Woops! Compilation failed:\n %s\n", err)
			out <- fmt.Sprintf("Woops! Compilation failed:\n %s\n", err)
			continue
		}

		code := comp.Bytecode()
		constants = code.Constants

		machine := vm.NewWithGlobalStore(code, globals)
		err = machine.Run()
		if err != nil {
			// fmt.Fprintf(out, "Woops! Executing bytecode failed:\n %s\n", err)
			out <- fmt.Sprintf("Woops! Executing bytecode failed:\n %s\n", err)
			continue
		}
		stackTop := machine.LastPoppedStackElem()
		// io.WriteString(out, stackTop.Inspect())
		// io.WriteString(out, "\n")
		out <- (stackTop.Inspect() + "\n")
	}
}

func StartInterpreter(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)
	env := object.NewEnvironment()

	for {
		fmt.Fprintf(out, PROMPT)
		scanned := scanner.Scan()
		if !scanned {
			return
		}

		line := scanner.Text()
		l := lexer.New(line)
		p := parser.New(l)

		program := p.ParseProgram()
		if len(p.Errors()) != 0 {
			printParserErrors(out, p.Errors())
			continue
		}

		evaluated := evaluator.Eval(program, env)
		if evaluated != nil {
			io.WriteString(out, evaluated.Inspect())
			io.WriteString(out, "\n")
		}
	}
}
