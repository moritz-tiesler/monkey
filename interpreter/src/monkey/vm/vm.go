package vm

import (
	"fmt"
	"monkey/code"
	"monkey/compiler"
	"monkey/object"
)

const StackSize = compiler.StackSize
const GlobalsSize = compiler.GlobalsSize

var True = &object.Boolean{Value: true}
var False = &object.Boolean{Value: false}
var Null = &object.Null{}

type Frame struct {
	cl          *object.Closure
	Ip          int
	basePointer int
}

func (f *Frame) Closure() *object.Closure {
	return f.cl
}

func (f Frame) Name() string {
	return f.cl.Fn.Name
}

func NewFrame(cl *object.Closure, basePointer int) *Frame {
	f := &Frame{
		cl:          cl,
		Ip:          -1,
		basePointer: basePointer,
	}

	return f
}

func (f Frame) Copy() *Frame {
	oldCl := f.cl
	oldBp := f.basePointer
	oldIp := f.Ip

	copy := &Frame{
		cl:          oldCl,
		Ip:          oldIp,
		basePointer: oldBp,
	}

	return copy
}

func (f *Frame) Instructions() code.Instructions {
	return f.cl.Fn.Instructions
}

type VM struct {
	constants []object.Object

	stack []object.Object
	sp    int // Always points to the next value. Top of stack is stack[sp-1]

	globals []object.Object

	frames      []*Frame
	framesIndex int
	CallDepth   int

	LocationMap compiler.LocationMap
	compiler.NameStore
}

const MaxFrames = 1024

func New(bytecode *compiler.Bytecode) *VM {
	mainFn := &object.CompiledFunction{Instructions: bytecode.Instructions}
	mainClosure := &object.Closure{Fn: mainFn}
	mainFrame := NewFrame(mainClosure, 0)

	frames := make([]*Frame, MaxFrames)
	frames[0] = mainFrame

	return &VM{
		constants: bytecode.Constants,

		stack: make([]object.Object, StackSize),
		sp:    0,

		globals: make([]object.Object, GlobalsSize),

		frames:      frames,
		framesIndex: 1,
		CallDepth:   0,
	}
}

func NewWithGlobalStore(bytecode *compiler.Bytecode, s []object.Object) *VM {
	vm := New(bytecode)
	vm.globals = s
	return vm
}

func NewWithLocations(bytecode *compiler.Bytecode, lm compiler.LocationMap) *VM {
	vm := New(bytecode)
	vm.LocationMap = lm
	return vm
}

func NewFromMain(
	mainFn *object.CompiledFunction,
	bytecode *compiler.Bytecode,
	locationMap compiler.LocationMap,
	nameStore compiler.NameStore,
) *VM {

	mainClosure := &object.Closure{Fn: mainFn}

	mainFrame := NewFrame(mainClosure, 0)

	frames := make([]*Frame, MaxFrames)
	frames[0] = mainFrame

	return &VM{
		constants: bytecode.Constants,

		stack: make([]object.Object, StackSize),
		sp:    0,

		globals: make([]object.Object, GlobalsSize),

		frames:      frames,
		framesIndex: 1,
		CallDepth:   0,
		LocationMap: locationMap,
		NameStore:   nameStore,
	}
}

func (vm *VM) CurrentFrame() *Frame {
	return vm.frames[vm.framesIndex-1]
}

func (vm *VM) pushFrame(f *Frame) {
	vm.CallDepth++
	vm.frames[vm.framesIndex] = f
	vm.framesIndex++
}

func (vm *VM) popFrame() *Frame {
	vm.CallDepth--
	vm.framesIndex--
	return vm.frames[vm.framesIndex]
}

func (vm *VM) StackTop() object.Object {
	if vm.sp == 0 {
		return nil
	}

	return vm.stack[vm.sp-1]
}

func (vm *VM) Run() error {

	for vm.CurrentFrame().Ip < len(vm.CurrentFrame().Instructions())-1 {
		err := vm.Cycle()
		if err != nil {
			return err
		}
	}

	return nil
}

func (vm *VM) push(o object.Object) error {
	if vm.sp >= StackSize {
		return fmt.Errorf("stack overflow")
	}

	vm.stack[vm.sp] = o
	vm.sp++

	return nil
}

func (vm *VM) pushClosure(constIndex, numFree int) error {
	constant := vm.constants[constIndex]
	function, ok := constant.(*object.CompiledFunction)

	if !ok {
		return fmt.Errorf("not a function: %+v", constant)
	}

	free := make([]object.Object, numFree)
	for i := 0; i < numFree; i++ {
		free[i] = vm.stack[vm.sp-numFree+i]
	}
	vm.sp -= numFree

	closure := &object.Closure{Fn: function, Free: free}
	return vm.push(closure)
}

func (vm *VM) pop() object.Object {
	o := vm.stack[vm.sp-1]
	vm.sp--
	return o
}

func (vm *VM) LastPoppedStackElem() object.Object {
	return vm.stack[vm.sp]
}

func (vm *VM) executeBinaryOperation(op code.Opcode) error {
	right := vm.pop()
	left := vm.pop()

	leftType := left.Type()
	rightType := right.Type()

	switch {

	case leftType == object.INTEGER_OBJ && rightType == object.INTEGER_OBJ:
		return vm.executeBinaryIntegerOperation(op, left, right)
	case leftType == object.STRING_OBJ && rightType == object.STRING_OBJ:
		return vm.executeBinaryStringOperation(op, left, right)
	default:
		return fmt.Errorf("unsupported  types for binary operation: %s %s",
			leftType, rightType)

	}

}

func (vm *VM) executeBinaryIntegerOperation(op code.Opcode, left, right object.Object) error {
	leftValue := left.(*object.Integer).Value
	rightValue := right.(*object.Integer).Value

	var result int64

	switch op {
	case code.OpAdd:
		result = leftValue + rightValue
	case code.OpSub:
		result = leftValue - rightValue
	case code.OpMul:
		result = leftValue * rightValue
	case code.OpDiv:
		result = leftValue / rightValue
	default:
		return fmt.Errorf("unknown integer operator: %d", op)
	}

	return vm.push(&object.Integer{Value: result})
}

func (vm *VM) executeBinaryStringOperation(op code.Opcode, left, right object.Object) error {
	if op != code.OpAdd {
		return fmt.Errorf("unknown string operator: %d", op)
	}

	leftValue := left.(*object.String).Value
	rightValue := right.(*object.String).Value

	return vm.push(&object.String{Value: leftValue + rightValue})
}

func (vm *VM) executeComparison(op code.Opcode) error {
	right := vm.pop()
	left := vm.pop()

	if left.Type() == object.INTEGER_OBJ && right.Type() == object.INTEGER_OBJ {
		return vm.executeIntegerComparison(op, left, right)
	}

	switch op {
	case code.OpEqual:
		return vm.push(nativeBoolToBooleanObject(right == left))
	case code.OpNotEqual:
		return vm.push(nativeBoolToBooleanObject(right != left))
	default:
		return fmt.Errorf("unknown operator: %d (%s %s)",
			op, left.Type(), right.Type())
	}

}

func (vm *VM) executeIntegerComparison(op code.Opcode, left, right object.Object) error {
	leftValue := left.(*object.Integer).Value
	rightValue := right.(*object.Integer).Value

	switch op {
	case code.OpEqual:
		return vm.push(nativeBoolToBooleanObject(rightValue == leftValue))
	case code.OpNotEqual:
		return vm.push(nativeBoolToBooleanObject(rightValue != leftValue))
	case code.OpGreaterThan:
		return vm.push(nativeBoolToBooleanObject(leftValue > rightValue))
	default:
		return fmt.Errorf("unknown operator: %d", op)
	}
}

func (vm *VM) executeBangOperator() error {
	operand := vm.pop()

	switch operand {
	case True:
		return vm.push(False)
	case False:
		return vm.push(True)
	case Null:
		return vm.push(True)
	default:
		return vm.push(False)
	}
}

func (vm *VM) executeMinusOperator() error {
	operand := vm.pop()

	if operand.Type() != object.INTEGER_OBJ {
		return fmt.Errorf("unsupported type for negation: %s", operand.Type())
	}

	value := operand.(*object.Integer).Value
	return vm.push(&object.Integer{Value: -value})
}

func (vm *VM) buildArray(startIndex, endIndex int) object.Object {
	elememts := make([]object.Object, endIndex-startIndex)

	for i := startIndex; i < endIndex; i++ {
		elememts[i-startIndex] = vm.stack[i]
	}

	return &object.Array{Elements: elememts}
}

func (vm *VM) buildHash(startIndex, endIndex int) (object.Object, error) {
	hashedPairs := make(map[object.HashKey]object.HashPair)

	for i := startIndex; i < endIndex; i += 2 {
		key := vm.stack[i]
		value := vm.stack[i+1]

		pair := object.HashPair{Key: key, Value: value}

		hashKey, ok := key.(object.Hashable)

		if !ok {
			return nil, fmt.Errorf("unusable as hash key: %s", key.Type())
		}

		hashedPairs[hashKey.HashKey()] = pair
	}

	return &object.Hash{Pairs: hashedPairs}, nil
}

func (vm *VM) executeIndexExpression(left, index object.Object) error {
	switch {
	case left.Type() == object.ARRAY_OBJ && index.Type() == object.INTEGER_OBJ:
		return vm.executeArrayIndex(left, index)
	case left.Type() == object.HASH_OBJ:
		return vm.executeHashIndex(left, index)
	default:
		return fmt.Errorf("index  operator not supported: %s", left.Type())

	}
}

func (vm *VM) executeArrayIndex(array, index object.Object) error {
	arrayObject := array.(*object.Array)
	i := index.(*object.Integer).Value
	max := int64(len(arrayObject.Elements) - 1)

	if i < 0 || i > max {
		return vm.push(Null)
	}

	return vm.push(arrayObject.Elements[i])
}

func (vm *VM) executeHashIndex(hash, index object.Object) error {
	hashObject := hash.(*object.Hash)

	key, ok := index.(object.Hashable)
	if !ok {
		return fmt.Errorf("unusable as hash key: %s", index.Type())
	}

	pair, ok := hashObject.Pairs[key.HashKey()]
	if !ok {
		return vm.push(Null)
	}

	return vm.push(pair.Value)
}

func (vm *VM) executeCall(numArgs int) error {
	callee := vm.stack[vm.sp-1-numArgs]

	switch callee := callee.(type) {

	case *object.Closure:
		return vm.callClosure(callee, numArgs)

	case *object.Builtin:
		return vm.callBuiltin(callee, numArgs)

	default:
		return fmt.Errorf("calling non-function and non-built-in")

	}

}

func (vm *VM) callClosure(cl *object.Closure, numArgs int) error {
	if numArgs != cl.Fn.NumParameters {
		return fmt.Errorf("wrong number of arguments: want=%d, got=%d",
			cl.Fn.NumParameters, numArgs)
	}

	frame := NewFrame(cl, vm.sp-numArgs)
	vm.pushFrame(frame)

	vm.sp = frame.basePointer + cl.Fn.NumLocals

	return nil
}

func (vm *VM) callBuiltin(builtin *object.Builtin, numArgs int) error {
	args := vm.stack[vm.sp-numArgs : vm.sp]

	result := builtin.Fn(args...)
	vm.sp = vm.sp - numArgs - 1

	if result != nil {
		vm.push(result)
	} else {
		vm.push(Null)
	}

	return nil

}
func nativeBoolToBooleanObject(input bool) *object.Boolean {
	if input {
		return True
	}
	return False
}

func isTruthy(obj object.Object) bool {
	switch obj := obj.(type) {

	case *object.Boolean:
		return obj.Value
	case *object.Null:
		return false
	default:
		return true
	}
}

func (vm *VM) SourceLocation() compiler.LocationData {
	if vm.State() == DONE {
		return compiler.LocationData{}
	}
	startingP := vm.CurrentFrame().Ip
	ip := vm.CurrentFrame().Ip
	var location compiler.LocationData
	var found bool
	var lk compiler.LocationKey
	var scopeId *object.CompiledFunction
	for ip <= len(vm.frames[vm.framesIndex-1].Instructions()) {
		scopeId = vm.CurrentFrame().cl.Fn
		lk = compiler.LocationKey{ScopeId: scopeId, InstructionIndex: ip}
		location, found = vm.LocationMap[lk]
		if found {
			break
		}
		ip = ip + 1
	}
	if found {
		return location
	} else {
		panic(fmt.Sprintf("Could not find location for ins=%d\n%s", startingP, scopeId.Instructions.String()))
	}

}

type RunCondition func(*VM) (bool, error)

func (vm *VM) RunWithCondition(runCondition RunCondition) (*VM, error, bool) {
	for vm.CurrentFrame().Ip < len(vm.CurrentFrame().Instructions())-1 {
		vm.AdvancePointers()
		stop, err := runCondition(vm)
		if err != nil {
			return vm, fmt.Errorf("error when testing condition"), false
		}
		if stop {
			return vm, nil, true
		} else {
			err := vm.RunOp()
			if err != nil {
				return vm, err, false
			}
		}
	}

	return vm, nil, false
}

func (vm *VM) AdvancePointers() {
	vm.CurrentFrame().Ip++
}

func (vm *VM) Cycle() error {

	vm.CurrentFrame().Ip++

	err := vm.RunOp()
	if err != nil {
		return err
	}

	return nil
}

func (vm *VM) RunOp() error {
	ip := vm.CurrentFrame().Ip
	ins := vm.CurrentFrame().Instructions()
	op := code.Opcode(ins[ip])
	switch op {
	case code.OpConstant:
		constIndex := code.ReadUint16(ins[ip+1:])
		vm.CurrentFrame().Ip += 2
		err := vm.push(vm.constants[constIndex])
		if err != nil {
			return err
		}

	case code.OpSetGlobal:
		globalIndex := code.ReadUint16(ins[ip+1:])
		vm.CurrentFrame().Ip += 2
		vm.globals[globalIndex] = vm.pop()

	case code.OpGetGlobal:
		globalIndex := code.ReadUint16(ins[ip+1:])
		vm.CurrentFrame().Ip += 2
		err := vm.push(vm.globals[globalIndex])
		if err != nil {
			return err
		}

	case code.OpAdd, code.OpSub, code.OpMul, code.OpDiv:
		err := vm.executeBinaryOperation(op)
		if err != nil {
			return err
		}

	case code.OpEqual, code.OpNotEqual, code.OpGreaterThan:
		err := vm.executeComparison(op)
		if err != nil {
			return err
		}

	case code.OpBang:
		err := vm.executeBangOperator()
		if err != nil {
			return err
		}

	case code.OpMinus:
		err := vm.executeMinusOperator()
		if err != nil {
			return err
		}

	case code.OpTrue:
		err := vm.push(True)
		if err != nil {
			return err
		}

	case code.OpFalse:
		err := vm.push(False)
		if err != nil {
			return err
		}

	case code.OpPop:
		vm.pop()

	case code.OpJump:
		pos := int(code.ReadUint16(ins[ip+1:]))
		vm.CurrentFrame().Ip = pos - 1

	case code.OpJumpNotTruthy:
		pos := int(code.ReadUint16(ins[ip+1:]))
		vm.CurrentFrame().Ip += 2
		condition := vm.pop()
		if !isTruthy(condition) {
			vm.CurrentFrame().Ip = pos - 1
		}

	case code.OpNull:
		err := vm.push(Null)
		if err != nil {
			return err
		}
	case code.OpArray:
		numElements := int(code.ReadUint16(ins[ip+1:]))
		vm.CurrentFrame().Ip += 2
		array := vm.buildArray(vm.sp-numElements, vm.sp)
		vm.sp -= numElements

		err := vm.push(array)
		if err != nil {
			return err
		}

	case code.OpHash:
		numElements := int(code.ReadUint16(ins[ip+1:]))
		vm.CurrentFrame().Ip += 2

		hash, err := vm.buildHash(vm.sp-numElements, vm.sp)
		if err != nil {
			return err
		}
		vm.sp = vm.sp - numElements

		err = vm.push(hash)
		if err != nil {
			return err
		}
	case code.OpIndex:
		index := vm.pop()
		left := vm.pop()

		err := vm.executeIndexExpression(left, index)
		if err != nil {
			return err
		}

	case code.OpSetLocal:
		localIndex := code.ReadUint8(ins[ip+1:])
		vm.CurrentFrame().Ip += 1

		frame := vm.CurrentFrame()

		vm.stack[frame.basePointer+int(localIndex)] = vm.pop()

	case code.OpGetLocal:
		localIndex := code.ReadUint8(ins[ip+1:])
		vm.CurrentFrame().Ip += 1

		frame := vm.CurrentFrame()

		err := vm.push(vm.stack[frame.basePointer+int(localIndex)])
		if err != nil {
			return err
		}

	case code.OpCall:
		numArgs := code.ReadUint8(ins[ip+1:])
		vm.CurrentFrame().Ip += 1

		err := vm.executeCall(int(numArgs))
		if err != nil {
			return err
		}

	case code.OpReturnValue:
		returnValue := vm.pop()

		frame := vm.popFrame()
		vm.sp = frame.basePointer - 1

		err := vm.push(returnValue)
		if err != nil {
			return err
		}

	case code.OpReturn:
		frame := vm.popFrame()
		vm.sp = frame.basePointer - 1

		err := vm.push(Null)
		if err != nil {
			return err
		}
	case code.OpGetBuiltin:
		builinIndex := code.ReadUint8(ins[ip+1:])
		vm.CurrentFrame().Ip += 1

		definition := object.Builtins[builinIndex]

		err := vm.push(definition.Builtin)
		if err != nil {
			return err
		}

	case code.OpClosure:
		constIndex := code.ReadUint16(ins[ip+1:])
		numFree := code.ReadUint8(ins[ip+3:])
		vm.CurrentFrame().Ip += 3

		err := vm.pushClosure(int(constIndex), int(numFree))
		if err != nil {
			return err
		}

	case code.OpGetFree:
		freeIndex := code.ReadUint8(ins[ip+1:])
		vm.CurrentFrame().Ip += 1

		currentClosure := vm.CurrentFrame().cl

		err := vm.push(currentClosure.Free[freeIndex])
		if err != nil {
			return err
		}
	case code.OpCurrentClosure:
		currentClosure := vm.CurrentFrame().cl
		err := vm.push(currentClosure)
		if err != nil {
			return err
		}

	}

	return nil
}

type State int

const (
	OFF State = iota
	STOPPED
	DONE
)

func (vm VM) State() State {
	if vm.framesIndex > 1 {
		return STOPPED
	}

	mainFrame := vm.CurrentFrame()
	mainIp := mainFrame.Ip
	mainIns := mainFrame.Instructions()
	if mainIp == -1 {
		return OFF
	}
	if mainIp == len(mainIns)-1 {
		return DONE
	}

	return STOPPED
}

func (vm *VM) Copy() *VM {
	copy := &VM{
		constants: vm.constants,

		stack: make([]object.Object, StackSize),
		sp:    0,

		globals: make([]object.Object, GlobalsSize),

		frames:      make([]*Frame, MaxFrames),
		framesIndex: 1,
		CallDepth:   0,
	}
	oldConstants := vm.constants
	oldStack := vm.stack
	oldSp := vm.sp
	oldGlobals := vm.globals
	oldFramesIndex := vm.framesIndex
	oldCallDepth := vm.CallDepth

	copy.constants = append(copy.constants, oldConstants...)
	copy.stack = append(copy.stack, oldStack...)
	copy.sp = oldSp
	copy.globals = append(copy.globals, oldGlobals...)
	// TODO frameIp still gets changed in the copy when running original vm
	for i := 0; i < vm.framesIndex; i++ {
		copy.frames[i] = vm.frames[i].Copy()
	}
	copy.framesIndex = oldFramesIndex
	copy.CallDepth = oldCallDepth
	if vm.LocationMap != nil {
		copy.LocationMap = vm.LocationMap
	}

	return copy

}

func (vm VM) FramesIndex() int {
	return vm.framesIndex
}

func (vm VM) Frames() []*Frame {
	return vm.frames
}

func (vm VM) ActiveObjects(f Frame) ([]*object.Object, map[*object.Object]string) {
	vars := []*object.Object{}
	names := make(map[*object.Object]string)
	ins := f.Instructions()
	for i := 0; i < f.Ip; i++ {
		op := code.Opcode(ins[i])
		switch op {
		case code.OpSetGlobal:
			globalIndex := code.ReadUint16(ins[i+1:])
			name := vm.GetGlobalName(int(globalIndex))
			global := vm.globals[globalIndex]
			names[&global] = name
			vars = append(vars, &global)

		case code.OpSetLocal:
			localIndex := code.ReadUint8(ins[i+1:])
			name := vm.GetLocalName(int(localIndex))
			local := vm.stack[f.basePointer+int(localIndex)]
			names[&local] = name
			vars = append(vars, &local)
		}
	}

	return vars, names
}
