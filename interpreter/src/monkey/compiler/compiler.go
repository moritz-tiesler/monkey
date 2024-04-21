package compiler

import (
	"fmt"
	"monkey/ast"
	"monkey/code"
	"monkey/exception"
	"monkey/object"
	"sort"
)

const StackSize = 2048
const GlobalsSize = 65536

type EmittedInstruction struct {
	Opcode   code.Opcode
	Position int
}

type LocationKey struct {
	ScopeId          *object.CompiledFunction
	InstructionIndex int
}

type LocationMap map[LocationKey]LocationData

func (lm LocationMap) Locations() []LocationData {
	locs := make([]LocationData, 0, len(lm))
	for _, v := range lm {
		locs = append(locs, v)
	}
	return locs

}

type CompilationScope struct {
	scopeId             *object.CompiledFunction
	instructions        code.Instructions
	lastInstruction     EmittedInstruction
	previousInstruction EmittedInstruction
}

type LocationScope struct {
	Depth     int
	Locations []LocationData
}

type Compiler struct {
	NameStore
	constants   []object.Object
	symbolTable *SymbolTable
	scopes      []CompilationScope
	scopeIndex  int
	// Used for mapping OpCode to source location
	scopeDepth  int
	LocationMap LocationMap
}

func New() *Compiler {
	mainFn := &object.CompiledFunction{}
	mainScope := CompilationScope{
		scopeId:             mainFn,
		instructions:        code.Instructions{},
		lastInstruction:     EmittedInstruction{},
		previousInstruction: EmittedInstruction{},
	}

	symbolTable := NewSymbolTable()

	for i, v := range object.Builtins {
		symbolTable.DefineBuiltin(i, v.Name)
	}
	return &Compiler{
		constants:   []object.Object{},
		symbolTable: symbolTable,
		scopes:      []CompilationScope{mainScope},
		scopeIndex:  0,

		scopeDepth:  0,
		LocationMap: make(map[LocationKey]LocationData),
		NameStore:   *NewNameStore(StackSize, GlobalsSize),
	}
}

func NewWithState(s *SymbolTable, constants []object.Object) *Compiler {
	compiler := New()
	compiler.symbolTable = s
	compiler.constants = constants
	return compiler
}

type CompilerError struct {
	message string
	line    int
	col     int
}

func NewCompilerError(message string, line int, col int) CompilerError {
	msg := "Compiler error: " + message
	return CompilerError{message: msg, line: line, col: col}
}

func (ce CompilerError) Error() string {
	return fmt.Sprintf("%s: Line: %d, Col: %d", ce.message, ce.line, ce.col)
}

func (ce CompilerError) Line() int {
	return ce.line
}

func (ce CompilerError) Col() int {
	return ce.col
}

func (c *Compiler) Compile(node ast.Node) exception.Exception {
	switch node := node.(type) {
	case *ast.Program:
		for _, s := range node.Statements {
			err := c.Compile(s)
			if err != nil {
				return err
			}
		}

	case *ast.ExpressionStatement:
		err := c.Compile(node.Expression)
		if err != nil {
			return err
		}
		_ = c.emit(code.OpPop)

	case *ast.InfixExpression:
		if node.Operator == "<" {
			err := c.Compile(node.Right)
			if err != nil {
				return err
			}

			err = c.Compile(node.Left)
			if err != nil {
				return err
			}
			c.emit(code.OpGreaterThan)
			return nil
		}
		err := c.Compile(node.Left)
		if err != nil {
			return err
		}

		err = c.Compile(node.Right)
		if err != nil {
			return err
		}

		switch node.Operator {
		case "+":
			c.emit(code.OpAdd)
		case "-":
			c.emit(code.OpSub)
		case "*":
			c.emit(code.OpMul)
		case "/":
			c.emit(code.OpDiv)
		case ">":
			c.emit(code.OpGreaterThan)
		case "==":
			c.emit(code.OpEqual)
		case "!=":
			c.emit(code.OpNotEqual)
		default:
			//return fmt.Errorf("unknown operator %s", node.Operator)
			return NewCompilerError(
				fmt.Sprintf("unknown operator %s", node.Operator),
				node.Range().Start.Line,
				node.Range().Start.Col,
			)
		}

	case *ast.PrefixExpression:
		err := c.Compile(node.Right)
		if err != nil {
			return err
		}

		switch node.Operator {
		case "!":
			c.emit(code.OpBang)
		case "-":
			c.emit(code.OpMinus)
		default:
			//return fmt.Errorf("unknown operator %s", node.Operator)
			return NewCompilerError(
				fmt.Sprintf("unknown operator %s", node.Operator),
				node.Range().Start.Line,
				node.Range().Start.Col,
			)
		}

	case *ast.IfExpression:
		err := c.Compile(node.Condition)
		if err != nil {
			return err
		}
		// Emit an `OpJumpNotTruthy` with a bogus value
		jumpNotTruthyPos := c.emit(code.OpJumpNotTruthy, 9999)
		c.mapInstructionToNode(c.currenScopeId(), jumpNotTruthyPos, node)

		err = c.Compile(node.Consequence)
		if err != nil {
			return err
		}
		if c.lastInstructionIs(code.OpPop) {
			c.removeLastPop()
		}
		// Emit an `OpJump` with a bogus value
		jumpPos := c.emit(code.OpJump, 9999)
		c.mapInstructionToNode(c.currenScopeId(), jumpPos, node.Consequence)

		afterConsequencePos := len(c.currentInstructions())
		c.changeOperand(jumpNotTruthyPos, afterConsequencePos)

		if node.Alternative == nil {
			c.emit(code.OpNull)
		} else {
			err := c.Compile(node.Alternative)
			if err != nil {
				return err
			}
			if c.lastInstructionIs(code.OpPop) {
				c.removeLastPop()
			}
		}
		afterAlternativePos := len(c.currentInstructions())
		c.changeOperand(jumpPos, afterAlternativePos)

	case *ast.BlockStatement:
		for _, s := range node.Statements {
			err := c.Compile(s)
			if err != nil {
				return err
			}
		}

	case *ast.IntegerLiteral:
		integer := &object.Integer{Value: node.Value}
		pos := c.emit(code.OpConstant, c.addConstant(integer))
		c.mapInstructionToNode(c.currenScopeId(), pos, node)

	case *ast.StringLiteral:
		str := &object.String{Value: node.Value}
		c.emit(code.OpConstant, c.addConstant(str))

	case *ast.ArrayLiteral:
		for _, el := range node.Elements {
			err := c.Compile(el)
			if err != nil {
				return err
			}
		}
		c.emit(code.OpArray, len(node.Elements))

	case *ast.HashLiteral:
		keys := []ast.Expression{}
		for k := range node.Pairs {
			keys = append(keys, k)
		}
		sort.Slice(keys, func(i, j int) bool {
			return keys[i].String() < keys[j].String()
		})

		for _, k := range keys {
			err := c.Compile(k)
			if err != nil {
				return err
			}
			err = c.Compile(node.Pairs[k])
			if err != nil {
				return err
			}
		}

		c.emit(code.OpHash, len(node.Pairs)*2)

	case *ast.IndexExpression:
		err := c.Compile(node.Left)
		if err != nil {
			return err
		}

		err = c.Compile(node.Index)
		if err != nil {
			return err
		}

		c.emit(code.OpIndex)

	case *ast.LetStatement:
		var ii int
		symbol := c.symbolTable.Define(node.Name.Value)
		err := c.Compile(node.Value)
		if err != nil {
			return err
		}
		if symbol.Scope == GlobalScope {
			ii = c.emit(code.OpSetGlobal, symbol.Index)
			c.StoreGlobalName(node.Name.Value, symbol.Index)

		} else {
			ii = c.emit(code.OpSetLocal, symbol.Index)
			c.StoreLocalName(node.Name.Value, c.currenScopeId(), symbol.Index)
		}
		c.mapInstructionToNode(c.currenScopeId(), ii, node)

	case *ast.Identifier:
		symbol, ok := c.symbolTable.Resolve(node.Value)
		if !ok {
			//return fmt.Errorf("undefined variable %s", node.Value)
			return NewCompilerError(
				fmt.Sprintf("undefined variable %s", node.Value),
				node.Range().Start.Line,
				node.Range().Start.Col,
			)
		}

		ii := c.loadSymbol(symbol)
		c.mapInstructionToNode(c.currenScopeId(), ii, node)

	case *ast.Boolean:
		if node.Value {
			c.emit(code.OpTrue)
		} else {
			c.emit(code.OpFalse)
		}
	case *ast.FunctionLiteral:
		var ii int
		c.enterScope()

		if node.Name != "" {
			c.symbolTable.DefineFunctionName(node.Name)
		}

		compiledFn := &object.CompiledFunction{}
		compiledFn.Name = node.Name
		c.scopes[c.scopeIndex].scopeId = compiledFn

		for _, p := range node.Parameters {
			symbol := c.symbolTable.Define(p.Value)
			c.StoreLocalName(symbol.Name, c.currenScopeId(), symbol.Index)
		}

		err := c.Compile(node.Body)
		if err != nil {
			return err
		}

		if c.lastInstructionIs(code.OpPop) {
			lastNode := node.Body.Statements[len(node.Body.Statements)-1]
			ii = c.scopes[c.scopeIndex].lastInstruction.Position
			c.mapInstructionToNode(c.currenScopeId(), ii, lastNode)
			c.replaceLastPopWithReturn()
		}
		if !c.lastInstructionIs(code.OpReturnValue) {
			c.emit(code.OpReturn)
		}

		freeSymbols := c.symbolTable.FreeSymbols
		numLocals := c.symbolTable.numDefinitions
		compiledFn.NumLocals = numLocals
		compiledFn.NumParameters = len(node.Parameters)

		instructions := c.leaveScope()
		compiledFn.Instructions = instructions
		for _, s := range freeSymbols {
			c.loadSymbol(s)
		}

		fnIndex := c.addConstant(compiledFn)
		ii = c.emit(code.OpClosure, fnIndex, len(freeSymbols))
		c.mapInstructionToNode(c.currenScopeId(), ii, node)

	case *ast.ReturnStatement:
		err := c.Compile(node.ReturnValue)
		if err != nil {
			return err
		}

		ii := c.emit(code.OpReturnValue)
		c.mapInstructionToNode(c.currenScopeId(), ii, node)
	case *ast.CallExpression:
		err := c.Compile(node.Function)
		if err != nil {
			return err
		}

		for _, a := range node.Arguments {
			err := c.Compile(a)
			if err != nil {
				return err
			}
		}

		ii := c.emit(code.OpCall, len(node.Arguments))
		c.mapInstructionToNode(c.currenScopeId(), ii, node)
	}

	return nil
}

func (c *Compiler) addConstant(obj object.Object) int {
	c.constants = append(c.constants, obj)
	return len(c.constants) - 1
}

func (c *Compiler) emit(op code.Opcode, operands ...int) int {
	ins := code.Make(op, operands...)
	pos := c.addInstruction(ins)

	c.setLastInstruction(op, pos)

	return pos
}

func (c *Compiler) setLastInstruction(op code.Opcode, pos int) {
	previous := c.scopes[c.scopeIndex].lastInstruction
	last := EmittedInstruction{Opcode: op, Position: pos}

	c.scopes[c.scopeIndex].previousInstruction = previous
	c.scopes[c.scopeIndex].lastInstruction = last
}

func (c *Compiler) lastInstructionIs(op code.Opcode) bool {
	if len(c.currentInstructions()) == 0 {
		return false
	}
	return c.scopes[c.scopeIndex].lastInstruction.Opcode == op
}

func (c *Compiler) removeLastPop() {
	last := c.scopes[c.scopeIndex].lastInstruction
	previous := c.scopes[c.scopeIndex].previousInstruction

	old := c.currentInstructions()
	new := old[:last.Position]

	c.scopes[c.scopeIndex].instructions = new
	c.scopes[c.scopeIndex].lastInstruction = previous

}

func (c *Compiler) replaceInstruction(pos int, newInstruction []byte) {
	ins := c.currentInstructions()

	for i := 0; i < len(newInstruction); i++ {
		ins[pos+i] = newInstruction[i]
	}
}

func (c *Compiler) replaceLastPopWithReturn() {
	lastPos := c.scopes[c.scopeIndex].lastInstruction.Position

	c.replaceInstruction(lastPos, code.Make(code.OpReturnValue))

	c.scopes[c.scopeIndex].lastInstruction.Opcode = code.OpReturnValue
}

func (c *Compiler) changeOperand(opPos int, operand int) {
	op := code.Opcode(c.currentInstructions()[opPos])
	newInstruction := code.Make(op, operand)

	c.replaceInstruction(opPos, newInstruction)
}

func (c *Compiler) currentInstructions() code.Instructions {
	return c.scopes[c.scopeIndex].instructions
}

func (c *Compiler) addInstruction(ins []byte) int {
	posNewInstruction := len(c.currentInstructions())
	updatedInstructions := append(c.currentInstructions(), ins...)

	c.scopes[c.scopeIndex].instructions = updatedInstructions

	return posNewInstruction
}

func (c *Compiler) loadSymbol(s Symbol) int {
	var pos int
	switch s.Scope {

	case GlobalScope:
		pos = c.emit(code.OpGetGlobal, s.Index)
	case LocalScope:
		pos = c.emit(code.OpGetLocal, s.Index)
	case BuiltinScope:
		pos = c.emit(code.OpGetBuiltin, s.Index)
	case FreeScope:
		pos = c.emit(code.OpGetFree, s.Index)
	case FunctionScope:
		pos = c.emit(code.OpCurrentClosure)

	}
	return pos
}

func (c *Compiler) enterScope() {
	c.scopeDepth++
	scope := CompilationScope{
		scopeId:             nil,
		instructions:        code.Instructions{},
		lastInstruction:     EmittedInstruction{},
		previousInstruction: EmittedInstruction{},
	}

	c.scopes = append(c.scopes, scope)
	c.scopeIndex++

	c.symbolTable = NewEnclosedSymbolTable(c.symbolTable)
}

func (c *Compiler) leaveScope() code.Instructions {
	c.scopeDepth -= 1
	instructions := c.currentInstructions()

	c.scopes = c.scopes[:len(c.scopes)-1]
	c.scopeIndex--

	c.symbolTable = c.symbolTable.Outer

	return instructions
}

func (c *Compiler) Bytecode() *Bytecode {
	return &Bytecode{
		Instructions: c.currentInstructions(),
		Constants:    c.constants,
	}
}

func (c *Compiler) MainFn() *object.CompiledFunction {
	if c.scopeIndex != 0 {
		panic("Scope index does not point to main scope. Did compilation finish without errors?")
	}
	mainFn := c.scopes[c.scopeIndex].scopeId
	mainFn.Instructions = c.Bytecode().Instructions
	return mainFn
}

type LocationData struct {
	Depth int
	Range ast.NodeRange
}

//func (c *Compiler) Locations() []LocationScope {
//return c.locationScopes
//}

type Bytecode struct {
	Instructions code.Instructions
	Constants    []object.Object
}

//func (c *Compiler) trackNode(node ast.Node) (int, LocationData) {
//posNewLocation := len(c.currentLocations())
//newLoc := LocationData{Depth: c.scopeDepth, Range: node.Range()}

//existingLocations := c.currentLocations()
//updatedLocations := append(existingLocations, newLoc)

//c.locationScopes[c.scopeIndex].Locations = updatedLocations

//return posNewLocation, newLoc
//}

//func (c *Compiler) currentLocations() []LocationData {
//return c.locationScopes[c.scopeIndex].Locations
//}

func (c *Compiler) mapInstructionToNode(scopeId *object.CompiledFunction, insPos int, node ast.Node) {
	newLoc := LocationData{Depth: c.scopeDepth, Range: node.Range()}
	key := LocationKey{ScopeId: scopeId, InstructionIndex: insPos}

	c.LocationMap[key] = newLoc
}

func (c *Compiler) currenScopeId() *object.CompiledFunction {
	id := c.scopes[c.scopeIndex].scopeId
	if id == nil {
		panic(fmt.Sprintf("Invalid id: scopeIndex=%d, scopeId=%v", c.scopeIndex, id))
	}
	return c.scopes[c.scopeIndex].scopeId

}

type NameStore struct {
	stackIndex  int
	globalIndex int
	stackNames  map[*object.CompiledFunction][]string
	globalNames []string
}

func NewNameStore(stackSize int, globalsSize int) *NameStore {
	return &NameStore{
		stackIndex:  0,
		globalIndex: 0,
		stackNames:  make(map[*object.CompiledFunction][]string),
		globalNames: make([]string, globalsSize),
	}
}

func (ns *NameStore) StoreLocalName(name string, scopeId *object.CompiledFunction, index int) {
	if _, ok := ns.stackNames[scopeId]; !ok {
		ns.stackNames[scopeId] = []string{}
	}
	ns.stackNames[scopeId] = append(ns.stackNames[scopeId], name)
}

func (ns *NameStore) StoreGlobalName(name string, index int) {
	ns.globalNames[index] = name
}

func (ns *NameStore) GetLocalName(scopeId *object.CompiledFunction, index int) string {
	return ns.stackNames[scopeId][index]
}

func (ns *NameStore) GetGlobalName(index int) string {
	return ns.globalNames[index]
}
