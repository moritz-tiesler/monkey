package vm

import (
	"fmt"
	"monkey/ast"
	"monkey/compiler"
	"monkey/lexer"
	"monkey/object"
	"monkey/parser"
	"testing"
)

type vmTestCase struct {
	input    string
	expected interface{}
}

func runVmTests(t *testing.T, tests []vmTestCase) {
	t.Helper()
	for _, tt := range tests {
		program := parse(tt.input)
		comp := compiler.New()
		err := comp.Compile(program)
		if err != nil {
			t.Fatalf("compiler error: %s", err)
		}

		//for i, constant := range comp.Bytecode().Constants {
		//fmt.Printf("CONSTANT %d %p (%T):\n", i, constant, constant)

		//switch constant := constant.(type) {
		//case *object.CompiledFunction:
		//fmt.Printf(" Instructions:\n%s", constant.Instructions)
		//case *object.Integer:
		//fmt.Printf(" Value: %d\n", constant.Value)

		//}

		//fmt.Printf("\n")
		//}

		vm := New(comp.Bytecode())
		err = vm.Run()
		if err != nil {
			t.Fatalf("vm error: %s", err)
		}
		stackElem := vm.LastPoppedStackElem()
		testExpectedObject(t, tt.expected, stackElem)
	}
}

func parse(input string) *ast.Program {
	l := lexer.New(input)
	p := parser.New(l)
	return p.ParseProgram()
}

func testExpectedObject(
	t *testing.T,
	expected interface{},
	actual object.Object,
) {
	t.Helper()
	switch expected := expected.(type) {
	case int:
		err := testIntegerObject(int64(expected), actual)
		if err != nil {
			t.Errorf("testIntegerObject failed: %s", err)
		}
	case bool:
		err := testBooleanObject(bool(expected), actual)
		if err != nil {
			t.Errorf("testBooleanObject failed: %s", err)
		}
	case string:
		err := testStringObject(expected, actual)
		if err != nil {
			t.Errorf("testStringObject failed: %s", err)
		}
	case []int:
		array, ok := actual.(*object.Array)
		if !ok {
			t.Errorf("object not Array: %T (%+v)", actual, actual)
			return
		}

		if len(array.Elements) != len(expected) {
			t.Errorf("wrong num of elements. want=%d, got=%d",
				len(expected), len(array.Elements))
			return
		}

		for i, expectedElem := range expected {
			err := testIntegerObject(int64(expectedElem), array.Elements[i])
			if err != nil {
				t.Errorf("testIntegerObject failed: %s", err)
			}
		}

	case map[object.HashKey]int64:
		hash, ok := actual.(*object.Hash)
		if !ok {
			t.Errorf("object is not Hash. got=%T (%+v)", actual, actual)
			return
		}
		if len(hash.Pairs) != len(expected) {
			t.Errorf("hash has wrong number of Pairs. want=%d, got=%d",
				len(expected), len(hash.Pairs))
			return
		}
		for expectedKey, expectedValue := range expected {
			pair, ok := hash.Pairs[expectedKey]
			if !ok {
				t.Errorf("no pair for given key in Pairs")
			}
			err := testIntegerObject(expectedValue, pair.Value)
			if err != nil {
				t.Errorf("testIntegerObject failed: %s", err)
			}
		}
	case *object.Null:
		if actual != Null {
			t.Errorf("object is not Null: %T (%+v)", actual, actual)
		}
	case *object.Error:
		errObj, ok := actual.(*object.Error)
		if !ok {
			t.Errorf("object is not Error: %T (%+v)", actual, actual)
			return
		}
		if errObj.Message != expected.Message {
			t.Errorf("wrong error message. expected=%q, got=%q",
				expected.Message, errObj.Message)
		}

	}
}

func testIntegerObject(expected int64, actual object.Object) error {
	result, ok := actual.(*object.Integer)
	if !ok {
		return fmt.Errorf("object is not Integer. got=%T (%+v)",
			actual, actual)
	}
	if result.Value != expected {
		return fmt.Errorf("object has wrong value. got=%d, want=%d",
			result.Value, expected)
	}
	return nil
}

func testBooleanObject(expected bool, actual object.Object) error {
	result, ok := actual.(*object.Boolean)
	if !ok {
		return fmt.Errorf("object is not Boolean. got=%T (%+v)",
			actual, actual)
	}

	if result.Value != expected {
		return fmt.Errorf("object has wrong value. got=%t, want=%t",
			result.Value, expected)
	}

	return nil
}

func testStringObject(expected string, actual object.Object) error {
	result, ok := actual.(*object.String)
	if !ok {
		return fmt.Errorf("object is not String, got=%T (%+v)",
			actual, actual)
	}
	if result.Value != expected {
		return fmt.Errorf("object has wrong value, got=%q, want=%q",
			result.Value, expected)
	}

	return nil
}

func TestIntegerArithmetic(t *testing.T) {
	tests := []vmTestCase{
		{"1", 1},
		{"2", 2},
		{"1 + 2", 3},
		{"1 - 2", -1},
		{"1 * 2", 2},
		{"4 / 2", 2},
		{"50 / 2 * 2 + 10 - 5", 55},
		{"5 + 5 + 5 + 5 - 10", 10},
		{"2 * 2 * 2 * 2 * 2", 32},
		{"5 * 2 + 10", 20},
		{"5 + 2 * 10", 25},
		{"5 * (2 + 10)", 60},
		{"-5", -5},
		{"-10", -10},
		{"-50 + 100 + -50", 0},
		{"(5 + 10 * 2 + 15 / 3) * 2 + -10", 50},
	}
	runVmTests(t, tests)
}

func TestBooleanExpressions(t *testing.T) {
	tests := []vmTestCase{
		{"true", true},
		{"false", false},
		{"1 < 2", true},
		{"1 > 2", false},
		{"1 < 1", false},
		{"1 > 1", false},
		{"1 == 1", true},
		{"1 != 1", false},
		{"1 == 2", false},
		{"1 != 2", true},
		{"true == true", true},
		{"false == false", true},
		{"true == false", false},
		{"true != false", true},
		{"false != true", true},
		{"(1 < 2) == true", true},
		{"(1 < 2) == false", false},
		{"(1 > 2) == true", false},
		{"(1 > 2) == false", true},
		{"!true", false},
		{"!false", true},
		{"!5", false},
		{"!!true", true},
		{"!!false", false},
		{"!!5", true},
		{"!(if (false) { 5; })", true},
		{"if ((if (false) { 10 })) { 10 } else { 20 }", 20},
	}
	runVmTests(t, tests)
}

func TestConditionals(t *testing.T) {
	tests := []vmTestCase{
		{"if (true) { 10 }", 10},
		{"if (true) { 10 } else { 20 }", 10},
		{"if (false) { 10 } else { 20 } ", 20},
		{"if (1) { 10 }", 10},
		{"if (1 < 2) { 10 }", 10},
		{"if (1 < 2) { 10 } else { 20 }", 10},
		{"if (1 > 2) { 10 } else { 20 }", 20},
		{"if (1 > 2) { 10 }", Null},
		{"if (false) { 10 }", Null},
	}
	runVmTests(t, tests)
}

func TestGlobalLetStatements(t *testing.T) {
	tests := []vmTestCase{
		{"let one = 1; one", 1},
		{"let one = 1; let two = 2; one + two", 3},
		{"let one = 1; let two = one + one; one + two", 3},
	}
	runVmTests(t, tests)
}

func TestStringExpressions(t *testing.T) {
	tests := []vmTestCase{
		{`"monkey"`, "monkey"},
		{`"mon" + "key"`, "monkey"},
		{`"mon" + "key" + "banana"`, "monkeybanana"},
	}
	runVmTests(t, tests)
}

func TestArrayLiterals(t *testing.T) {
	tests := []vmTestCase{
		{"[]", []int{}},
		{"[1, 2, 3]", []int{1, 2, 3}},
		{"[1 + 2, 3 * 4, 5 + 6]", []int{3, 12, 11}},
	}
	runVmTests(t, tests)
}

func TestHashLiterals(t *testing.T) {
	tests := []vmTestCase{
		{
			"{}", map[object.HashKey]int64{},
		},
		{
			"{1: 2, 2: 3}",
			map[object.HashKey]int64{
				(&object.Integer{Value: 1}).HashKey(): 2,
				(&object.Integer{Value: 2}).HashKey(): 3,
			},
		},
		{
			"{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
			map[object.HashKey]int64{
				(&object.Integer{Value: 2}).HashKey(): 4,
				(&object.Integer{Value: 6}).HashKey(): 16,
			},
		},
	}
	runVmTests(t, tests)
}

func TestIndexExpressions(t *testing.T) {
	tests := []vmTestCase{
		{"[1, 2, 3][1]", 2},
		{"[1, 2, 3][0 + 2]", 3},
		{"[[1, 1, 1]][0][0]", 1},
		{"[][0]", Null},
		{"[1, 2, 3][99]", Null},
		{"[1][-1]", Null},
		{"{1: 1, 2: 2}[1]", 1},
		{"{1: 1, 2: 2}[2]", 2},
		{"{1: 1}[0]", Null},
		{"{}[0]", Null},
	}
	runVmTests(t, tests)
}

func TestCallingFunctionsWithoutArguments(t *testing.T) {
	tests := []vmTestCase{
		{
			input: `
			let fivePlusTen = fn() { 5 + 10; };
			fivePlusTen();
			`,
			expected: 15,
		},
		{
			input: `
	let one = fn() { 1; };
	let two = fn() { 2; };
	one() + two()
	`,
			expected: 3,
		},
		{
			input: `
	let a = fn() { 1 };
	let b = fn() { a() + 1 };
	let c = fn() { b() + 1 };
	c();
	`,
			expected: 3,
		},
	}
	runVmTests(t, tests)
}
func TestFunctionsWithReturnStatement(t *testing.T) {
	tests := []vmTestCase{
		{
			input: `
	let earlyExit = fn() { return 99; 100; };
	earlyExit();
	`,
			expected: 99,
		},
		{
			input: `
	let earlyExit = fn() { return 99; return 100; };
	earlyExit();
	`,
			expected: 99,
		},
	}
	runVmTests(t, tests)
}
func TestFunctionsWithoutReturnValue(t *testing.T) {
	tests := []vmTestCase{
		{
			input: `
	let noReturn = fn() { };
	noReturn();
	`,
			expected: Null,
		},
		{
			input: `
	let noReturn = fn() { };
	let noReturnTwo = fn() { noReturn(); };
	noReturn();
	noReturnTwo();
	`,
			expected: Null,
		},
	}
	runVmTests(t, tests)
}

func TestFirstClassFunctions(t *testing.T) {
	tests := []vmTestCase{
		{
			input: `
	let returnsOne = fn() { 1; };
	let returnsOneReturner = fn() { returnsOne; };
	returnsOneReturner()();
	`,
			expected: 1,
		},
		{
			input: `
			let returnsOneReturner = fn() {
			let returnsOne = fn() { 1; };
			returnsOne;
			};
			returnsOneReturner()();
			`,
			expected: 1,
		},
	}
	runVmTests(t, tests)
}

func TestCallingFunctionsWithBindings(t *testing.T) {
	tests := []vmTestCase{
		{
			input: `
	let one = fn() { let one = 1; one };
	one();
	`,
			expected: 1,
		},
		{
			input: `
	let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
	oneAndTwo();
	`,
			expected: 3,
		},
		{
			input: `
	let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
	let threeAndFour = fn() { let three = 3; let four = 4; three + four; };
	oneAndTwo() + threeAndFour();
	`,
			expected: 10,
		},
		{
			input: `
	let firstFoobar = fn() { let foobar = 50; foobar; };
	let secondFoobar = fn() { let foobar = 100; foobar; };
	firstFoobar() + secondFoobar();
	`,
			expected: 150,
		},
		{
			input: `
	let globalSeed = 50;
	let minusOne = fn() {
	let num = 1;
	globalSeed - num;
	}
	let minusTwo = fn() {
	let num = 2;
	globalSeed - num;
	}
	minusOne() + minusTwo();
	`,
			expected: 97,
		},
	}
	runVmTests(t, tests)
}

func TestCallingFunctionsWithArgumentsAndBindings(t *testing.T) {
	tests := []vmTestCase{
		{
			input: `
	let identity = fn(a) { a; };
	identity(4);
	`,
			expected: 4,
		},
		{
			input: `
	let sum = fn(a, b) { a + b; };
	sum(1, 2);
	`,
			expected: 3,
		},
		{
			input: `
			let sum = fn(a, b) {
			let c = a + b;
			c;
			};
			sum(1, 2);
			`,
			expected: 3,
		},
		{
			input: `
			let sum = fn(a, b) {
			let c = a + b;
			c;
			};
			sum(1, 2) + sum(3, 4);`,
			expected: 10,
		},
		{
			input: `
			let sum = fn(a, b) {
			let c = a + b;
			c;
			};
			let outer = fn() {
			sum(1, 2) + sum(3, 4);
			};
			outer();
			`,
			expected: 10,
		},
		{
			input: `
			let globalNum = 10;
			let sum = fn(a, b) {
			let c = a + b;
			c + globalNum;
			};
			let outer = fn() {
			sum(1, 2) + sum(3, 4) + globalNum;
			};
			outer() + globalNum;
			`,
			expected: 50,
		},
	}
	runVmTests(t, tests)
}

func TestCallingFunctionsWithWrongArguments(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    `fn() { 1; }(1);`,
			expected: `wrong number of arguments: want=0, got=1`,
		},
		{
			input:    `fn(a) { a; }();`,
			expected: `wrong number of arguments: want=1, got=0`,
		},
		{
			input:    `fn(a, b) { a + b; }(1);`,
			expected: `wrong number of arguments: want=2, got=1`,
		},
	}
	for _, tt := range tests {
		program := parse(tt.input)
		comp := compiler.New()
		err := comp.Compile(program)
		if err != nil {
			t.Fatalf("compiler error: %s", err)
		}
		vm := New(comp.Bytecode())
		err = vm.Run()
		if err == nil {
			t.Fatalf("expected VM error but resulted in none.")
		}
		if err.Error() != tt.expected {
			t.Fatalf("wrong VM error: want=%q, got=%q", tt.expected, err)
		}
	}
}

func TestBuiltinFunctions(t *testing.T) {
	tests := []vmTestCase{
		{`len("")`, 0},
		{`len("four")`, 4},
		{`len("hello world")`, 11},
		{
			`len(1)`,
			&object.Error{
				Message: "argument to `len` not supported, got INTEGER",
			},
		},
		{`len("one", "two")`,
			&object.Error{
				Message: "wrong number of arguments. got=2, want=1",
			},
		},
		{`len([1, 2, 3])`, 3},
		{`len([])`, 0},
		{`puts("hello", "world!")`, Null},
		{`first([1, 2, 3])`, 1},
		{`first([])`, Null},
		{`first(1)`,
			&object.Error{
				Message: "argument to `first` must be ARRAY, got INTEGER",
			},
		},
		{`last([1, 2, 3])`, 3},
		{`last([])`, Null},
		{`last(1)`,
			&object.Error{
				Message: "argument to `last` must be ARRAY, got INTEGER",
			},
		},
		{`rest([1, 2, 3])`, []int{2, 3}},
		{`rest([])`, Null},
		{`push([], 1)`, []int{1}},
		{`push(1, 1)`,
			&object.Error{
				Message: "argument to `push` must be ARRAY, got INTEGER",
			},
		},
	}
	runVmTests(t, tests)
}

func TestClosures(t *testing.T) {
	tests := []vmTestCase{
		{
			input: `
	let newClosure =	fn(a) {
							fn() { a; };
						};
	let closure = newClosure(99);
	closure();
	`,
			expected: 99,
		},
		{
			input: `
			let newAdder = fn(a, b) {
			fn(c) { a + b + c };
			};
			let adder = newAdder(1, 2);
			adder(8);
			`,
			expected: 11,
		},
		{
			input: `
			let newAdder = fn(a, b) {
			let c = a + b;
			fn(d) { c + d };
			};
			let adder = newAdder(1, 2);
			adder(8);
			`,
			expected: 11,
		},
		{
			input: `
			let newAdderOuter = fn(a, b) {
			let c = a + b;
			fn(d) {
			let e = d + c;
			fn(f) { e + f; };
			};
			};
			let newAdderInner = newAdderOuter(1, 2)
			let adder = newAdderInner(3);
			adder(8);
			`,
			expected: 14,
		},
		{
			input: `
			let a = 1;
			let newAdderOuter = fn(b) {
			fn(c) {
			fn(d) { a + b + c + d };
			};
			};
			let newAdderInner = newAdderOuter(2)
			let adder = newAdderInner(3);
			adder(8);
			`,
			expected: 14,
		},
		{
			input: `
			let newClosure = fn(a, b) {
			let one = fn() { a; };
			let two = fn() { b; };
			fn() { one() + two(); };
			};
			let closure = newClosure(9, 90);
			closure();
			`,
			expected: 99,
		},
	}
	runVmTests(t, tests)
}

func TestRecursiveFunctions(t *testing.T) {
	tests := []vmTestCase{
		{
			input: `
	let countDown = fn(x) {
	if (x == 0) {
	return 0;
	} else {
	countDown(x - 1);
	}
	};
	countDown(1);
	`,
			expected: 0,
		},
		{
			input: `
			let countDown = fn(x) {
			if (x == 0) {
			return 0;
			} else {
			countDown(x - 1);
			}
			};
			let wrapper = fn() {
			countDown(1);
			};
			wrapper();
			`,
			expected: 0,
		},
		{
			input: `
			let wrapper = fn() {
			let countDown = fn(x) {
			if (x == 0) {
			return 0;
			} else {
			countDown(x - 1);
			}
			};
			countDown(1);
			};
			wrapper();
			`,
			expected: 0,
		},
	}
	runVmTests(t, tests)
}

func TestRecursiveFibonacci(t *testing.T) {
	tests := []vmTestCase{
		{
			input: `
	let fibonacci = fn(x) {
	if (x == 0) {
	return 0;
	} else {
	if (x == 1) {
	return 1;
	} else {
	fibonacci(x - 1) + fibonacci(x - 2);
	}
	}
	};
	fibonacci(15);
	`,
			expected: 610,
		},
	}
	runVmTests(t, tests)
}

type vmDebuggerTestCase struct {
	input            string
	debugFuncInput   compiler.LocationData
	expectedLocation compiler.LocationData
	debugAction      func(compiler.LocationData) func(*VM) (bool, error)
	running          bool
}

type vmDebuggerTestCaseWithPreparation struct {
	input            string
	debugFuncInput   compiler.LocationData
	prepFunc         func(*VM) *VM
	expectedLocation compiler.LocationData
	debugAction      func(*VM, compiler.LocationData) func(*VM) (bool, error)
	running          bool
}

func runVmDebuggingTests(t *testing.T, tests []vmDebuggerTestCase) {

	for _, tt := range tests {
		program := parse(tt.input)
		comp := compiler.New()
		err := comp.Compile(program)
		if err != nil {
			t.Fatalf("compiler error: %s", err)
		}

		for i, constant := range comp.Bytecode().Constants {
			fmt.Printf("CONSTANT %d %p (%T):\n", i, constant, constant)

			switch constant := constant.(type) {
			case *object.CompiledFunction:
				fmt.Printf(" Instructions:\n%s", constant.Instructions)
			case *object.Integer:
				fmt.Printf(" Value: %d\n", constant.Value)

			}

			fmt.Printf("\n")
		}

		vm := NewFromMain(comp.MainFn(), comp.Bytecode(), comp.LocationMap)
		step := tt.debugAction(tt.debugFuncInput)
		vm, err = vm.RunWithCondition(step)
		if err != nil {
			t.Fatalf("vm error: %s", err)
		}
		// stackElem := vm.LastPoppedStackElem()
		var expected compiler.LocationData
		if vm.Running() {
			expected = tt.expectedLocation
		} else {
			expected = compiler.LocationData{}
		}
		currentLocation := vm.SourceLocation()
		if currentLocation != expected {
			t.Errorf("wrong source location: expected=%v, got=%v", tt.expectedLocation, vm.SourceLocation())
		}

		if vm.Running() != tt.running {

			t.Errorf("expected running=%v, got=%v", tt.running, vm.Running())
		}

	}
}

func runVmDebuggingTestsWithPrep(t *testing.T, tests []vmDebuggerTestCaseWithPreparation) {

	for _, tt := range tests {
		program := parse(tt.input)
		comp := compiler.New()
		err := comp.Compile(program)
		if err != nil {
			t.Fatalf("compiler error: %s", err)
		}

		for i, constant := range comp.Bytecode().Constants {
			fmt.Printf("CONSTANT %d %p (%T):\n", i, constant, constant)

			switch constant := constant.(type) {
			case *object.CompiledFunction:
				fmt.Printf(" Instructions:\n%s", constant.Instructions)
			case *object.Integer:
				fmt.Printf(" Value: %d\n", constant.Value)

			}

			fmt.Printf("\n")
		}

		vm := NewFromMain(comp.MainFn(), comp.Bytecode(), comp.LocationMap)
		vm = tt.prepFunc(vm)
		step := tt.debugAction(vm, tt.debugFuncInput)
		vm, err = vm.RunWithCondition(step)
		if err != nil {
			t.Fatalf("vm error: %s", err)
		}
		// stackElem := vm.LastPoppedStackElem()
		currentLocation := vm.SourceLocation()
		if currentLocation != tt.expectedLocation {
			t.Errorf("wrong source location: expected=%v, got=%v", tt.expectedLocation, vm.SourceLocation())
		}

		if vm.Running() != tt.running {

			t.Errorf("expected running=%v, got=%v", tt.running, vm.Running())
		}

	}
}

func stepOver(current compiler.LocationData) func(vm *VM) (bool, error) {

	return func(vm *VM) (bool, error) {

		cycleLocation := vm.SourceLocation()
		cycleLine := cycleLocation.Range.Start.Line
		if current.Range.Start.Line != cycleLine {
			vm.CurrentFrame().Ip--
			return true, nil
		} else {
			return false, nil
		}
	}
}

func runUntilBreakPoint(breakOn compiler.LocationData) func(vm *VM) (bool, error) {
	return func(vm *VM) (bool, error) {
		if !vm.Running() {
			return true, nil
		}
		nextCycleLocation := vm.SourceLocation()
		cycleLine := nextCycleLocation.Range.Start.Line
		if breakOn.Range.Start.Line == cycleLine {
			vm.CurrentFrame().Ip--
			return true, nil
		} else {
			return false, nil
		}
	}
}

func TestStepOver(t *testing.T) {

	tests := []vmDebuggerTestCase{
		{
			input: `
let a = 2
let b = 3
let c = 4
`,
			debugFuncInput: compiler.LocationData{
				Depth: 0,
				Range: ast.NodeRange{
					Start: ast.Position{Line: 1, Col: 0},
					End:   ast.Position{Line: 1, Col: 9}}},

			expectedLocation: compiler.LocationData{
				Depth: 0,
				Range: ast.NodeRange{
					Start: ast.Position{Line: 2, Col: 0},
					End:   ast.Position{Line: 2, Col: 9},
				},
			},
			debugAction: stepOver,
			running:     true,
		},
	}

	runVmDebuggingTests(t, tests)
}

func TestBreakPoints(t *testing.T) {

	tests := []vmDebuggerTestCase{
		{
			input: `
let func = fn(a) {
    let b = a + 1
    return b
}
let x = 2
`,

			debugFuncInput: compiler.LocationData{
				Depth: 1,
				Range: ast.NodeRange{
					Start: ast.Position{Line: 3, Col: 4},
					End:   ast.Position{Line: 3, Col: 11},
				},
			},
			expectedLocation: compiler.LocationData{
				Depth: 1,
				Range: ast.NodeRange{
					Start: ast.Position{Line: 3, Col: 11},
					End:   ast.Position{Line: 3, Col: 12},
				},
			},
			debugAction: runUntilBreakPoint,
			running:     false,
		},
		{
			input: `
let a = 2
let b = 3
a
let c = 4
`,
			debugFuncInput: compiler.LocationData{
				Depth: 0,
				Range: ast.NodeRange{
					Start: ast.Position{Line: 3, Col: 0},
					End:   ast.Position{Line: 3, Col: 1}}},
			expectedLocation: compiler.LocationData{
				Depth: 0,
				Range: ast.NodeRange{
					Start: ast.Position{Line: 3, Col: 0},
					End:   ast.Position{Line: 3, Col: 1},
				},
			},
			debugAction: runUntilBreakPoint,
			running:     true,
		},
		{
			input: `
let func = fn(a) {
    let b = a + 1
	return b
}
func(2)
`,

			debugFuncInput: compiler.LocationData{
				Depth: 1,
				Range: ast.NodeRange{
					Start: ast.Position{Line: 3, Col: 4},
					End:   ast.Position{Line: 3, Col: 12},
				},
			},
			expectedLocation: compiler.LocationData{
				Depth: 1,
				Range: ast.NodeRange{
					Start: ast.Position{Line: 3, Col: 8},
					End:   ast.Position{Line: 3, Col: 9},
				},
			},
			debugAction: runUntilBreakPoint,
			running:     true,
		},
		{
			input: `
let func = fn(a) {
    let b = a + 1
    return b
}
func(2)
`,

			debugFuncInput: compiler.LocationData{
				Depth: 1,
				Range: ast.NodeRange{
					Start: ast.Position{Line: 3, Col: 4},
					End:   ast.Position{Line: 3, Col: 11},
				},
			},
			expectedLocation: compiler.LocationData{
				Depth: 1,
				Range: ast.NodeRange{
					Start: ast.Position{Line: 3, Col: 11},
					End:   ast.Position{Line: 3, Col: 12},
				},
			},
			debugAction: runUntilBreakPoint,
			running:     true,
		},
		{
			input: `
let square = fn(x) {
    return x
}
let squareAndDouble = fn(a) {
    let b = square(a) * 2
    return b
}
squareAndDouble(2)
`,

			debugFuncInput: compiler.LocationData{
				Depth: 1,
				Range: ast.NodeRange{
					Start: ast.Position{Line: 2, Col: 11},
					End:   ast.Position{Line: 2, Col: 12},
				},
			},
			expectedLocation: compiler.LocationData{
				Depth: 1,
				Range: ast.NodeRange{
					Start: ast.Position{Line: 2, Col: 11},
					End:   ast.Position{Line: 2, Col: 12},
				},
			},
			debugAction: runUntilBreakPoint,
			running:     true,
		},
	}

	runVmDebuggingTests(t, tests)
}

func stepInto(vm *VM, loc compiler.LocationData) func(*VM) (bool, error) {
	startingDepth := vm.CallDepth
	return func(vm *VM) (bool, error) {
		callDepth := vm.CallDepth
		if callDepth > startingDepth {
			return true, nil
		} else {
			return false, nil
		}
	}

}

func TestStepInto(t *testing.T) {

	tests := []vmDebuggerTestCaseWithPreparation{
		{
			input: `
let func = fn(x) {
    let y = x + 1
    return y
}
let b = 3
func(b)
let c = 5
`,
			debugFuncInput: compiler.LocationData{
				Depth: 0,
				Range: ast.NodeRange{
					Start: ast.Position{Line: 6, Col: 0},
					End:   ast.Position{Line: 6, Col: 7}}},
			expectedLocation: compiler.LocationData{
				Depth: 1,
				Range: ast.NodeRange{
					Start: ast.Position{Line: 2, Col: 4},
					End:   ast.Position{Line: 2, Col: 17},
				},
			},
			debugAction: stepInto,
			prepFunc: func(vm *VM) *VM {
				bp := compiler.LocationData{
					Depth: 0,
					Range: ast.NodeRange{
						Start: ast.Position{Line: 6, Col: 0},
						End:   ast.Position{Line: 6, Col: 7},
					},
				}
				vm, _ = vm.RunWithCondition(runUntilBreakPoint(bp))
				return vm
			},
			running: true,
		},
	}

	runVmDebuggingTestsWithPrep(t, tests)
}

func stepOut(vm *VM, outOf compiler.LocationData) func(*VM) (bool, error) {
	startingDepth := vm.CallDepth
	return func(vm *VM) (bool, error) {
		callDepth := vm.CallDepth
		if callDepth < startingDepth {
			return true, nil
		} else {
			return false, nil
		}
	}

}

func TestStepOut(t *testing.T) {

	tests := []vmDebuggerTestCaseWithPreparation{
		{
			input: `
let func = fn(x) {
    let y = x + 1
    return y
}
let b = 3
func(b)
let c = 5
`,
			debugFuncInput: compiler.LocationData{
				Depth: 1,
				Range: ast.NodeRange{
					Start: ast.Position{Line: 2, Col: 4},
					End:   ast.Position{Line: 2, Col: 17}}},
			expectedLocation: compiler.LocationData{
				Depth: 0,
				Range: ast.NodeRange{
					Start: ast.Position{Line: 6, Col: 0},
					End:   ast.Position{Line: 6, Col: 7},
				},
			},
			debugAction: stepOut,
			prepFunc: func(vm *VM) *VM {
				bp := compiler.LocationData{
					Depth: 1,
					Range: ast.NodeRange{
						Start: ast.Position{Line: 2, Col: 4},
						End:   ast.Position{Line: 2, Col: 17},
					},
				}
				vm, _ = vm.RunWithCondition(runUntilBreakPoint(bp))
				return vm
			},
			running: true,
		},
	}

	runVmDebuggingTestsWithPrep(t, tests)
}
