package main

import (
	"fmt"
	"monkey/repl"
)

func main() {
	fmt.Println("Go Web Assembly")
	in := make(chan string)
	out := make(chan string)

	userInput := []string{
		"let a = 2 + 2\n",
		"let b = a + 1\n",
		"let c = a + d\n",
	}

	go func() {
		for _, prompt := range userInput {
			in <- prompt
		}
		close(in)
	}()

	go func() {
		repl.StartStream(in, out)
		close(out)
	}()

	for l := range out {
		fmt.Println(l)
	}

}
