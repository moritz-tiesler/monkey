package main

import (
	"fmt"
	"monkey/repl"
	"syscall/js"
)

func replWrapper() js.Func {

	in := make(chan string)
	out := make(chan string)

	go func() {
		repl.StartStream(in, out)
		close(out)
	}()
	var jsonFunc js.Func
	jsonFunc = js.FuncOf(func(this js.Value, args []js.Value) any {
		if len(args) != 1 {
			return "Invalid no of arguments passed"
		}

		inputLine := args[0].String()

		go func() {
			in <- inputLine
		}()

		res := <-out
		jsonFunc.Release()
		return res

	})
	return jsonFunc
}

func main() {
	fmt.Println("Go Web Assembly")
	// in := make(chan string)
	// out := make(chan string)

	// userInput := []string{
	// "let a = 2 + 2\n",
	// "let b = a + 1\n",
	// "let c = a + d\n",
	// }

	// go func() {
	// for _, prompt := range userInput {
	// in <- prompt
	// }
	// close(in)
	// }()

	// go func() {
	// repl.StartStream(in, out)
	// close(out)
	// }()

	// for l := range out {
	// fmt.Println(l)
	// }
	js.Global().Set("runREPL", replWrapper())
	<-make(chan struct{})

}
