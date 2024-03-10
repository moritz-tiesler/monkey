package ast

import (
	"bytes"
	"fmt"
	"monkey/token"
	"strings"
)

type Position struct {
	Line int
	Col  int
}

type NodeRange struct {
	Start Position
	End   Position
}

type Node interface {
	TokenLiteral() string
	String() string
	Range() NodeRange
}

type Statement interface {
	Node
	statementNode()
}

type Expression interface {
	Node
	expressionNode()
}

type FunctionLiteral struct {
	Token      token.Token
	Parameters []*Identifier
	Body       *BlockStatement
	Name       string
	Start      Position
	End        Position
}

func (fl *FunctionLiteral) expressionNode()      {}
func (fl *FunctionLiteral) TokenLiteral() string { return fl.Token.Literal }
func (fl *FunctionLiteral) String() string {
	var out bytes.Buffer

	params := []string{}
	for _, p := range fl.Parameters {
		params = append(params, p.String())
	}

	out.WriteString(fl.TokenLiteral())
	if fl.Name != "" {
		out.WriteString(fmt.Sprintf("<%s>", fl.Name))
	}
	out.WriteString("(")
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(") ")
	out.WriteString(fl.Body.String())

	return out.String()
}
func (fl *FunctionLiteral) Range() NodeRange {
	return NodeRange{
		Start: fl.Start,
		End:   fl.End,
	}
}

type StringLiteral struct {
	Token token.Token
	Value string
	Start Position
	End   Position
}

func (sl *StringLiteral) expressionNode()      {}
func (sl *StringLiteral) TokenLiteral() string { return sl.Token.Literal }
func (sl *StringLiteral) String() string       { return sl.Token.Literal }
func (sl *StringLiteral) Range() NodeRange {
	return NodeRange{
		Start: Position{sl.Token.Line, sl.Token.Col},
		End:   Position{sl.Token.Line, sl.Token.Col + len(sl.Token.Literal)},
	}
}

type Program struct {
	Statements []Statement
	Start      Position
	End        Position
}

func (p *Program) TokenLiteral() string {
	if len(p.Statements) > 0 {
		return p.Statements[0].TokenLiteral()
	} else {
		return ""
	}
}

func (p *Program) String() string {
	var out bytes.Buffer

	for _, s := range p.Statements {
		out.WriteString(s.String())
	}

	return out.String()
}
func (p *Program) Range() NodeRange {
	first := p.Statements[0]
	last := p.Statements[len(p.Statements)-1]
	return NodeRange{
		Start: Position{first.Range().Start.Line, first.Range().Start.Col},
		End:   Position{last.Range().End.Line, last.Range().End.Col},
	}
}

type LetStatement struct {
	Token token.Token
	Name  *Identifier
	Value Expression
	Start Position
	End   Position
}

func (ls *LetStatement) statementNode()       {}
func (ls *LetStatement) TokenLiteral() string { return ls.Token.Literal }

func (ls *LetStatement) String() string {
	var out bytes.Buffer

	out.WriteString(ls.TokenLiteral() + " ")
	out.WriteString(ls.Name.String())
	out.WriteString(" = ")

	if ls.Value != nil {
		out.WriteString(ls.Value.String())
	}

	out.WriteString(";")

	return out.String()
}
func (ls *LetStatement) Range() NodeRange {
	return NodeRange{
		Start: ls.Start,
		End:   ls.End,
	}
}

func (rs *ReturnStatement) String() string {
	var out bytes.Buffer

	out.WriteString(rs.TokenLiteral() + " ")
	out.WriteString(" = ")

	if rs.ReturnValue != nil {
		out.WriteString(rs.ReturnValue.String())
	}

	out.WriteString(";")

	return out.String()
}

func (es *ExpressionStatement) String() string {

	if es.Expression != nil {
		return es.Expression.String()
	}
	return ""
}

type Identifier struct {
	Token token.Token
	Value string
	Start Position
	End   Position
}

func (i *Identifier) expressionNode()      {}
func (i *Identifier) TokenLiteral() string { return i.Token.Literal }
func (i *Identifier) String() string       { return i.Value }
func (i *Identifier) Range() NodeRange {
	return NodeRange{
		Start: i.Start,
		End:   i.End,
	}
}

type ReturnStatement struct {
	Token       token.Token
	ReturnValue Expression
	Start       Position
	End         Position
}

func (rs *ReturnStatement) statementNode()       {}
func (rs *ReturnStatement) TokenLiteral() string { return rs.Token.Literal }
func (rs *ReturnStatement) Range() NodeRange {
	return NodeRange{
		Start: Position{rs.ReturnValue.Range().Start.Line, rs.ReturnValue.Range().Start.Col},
		End:   Position{rs.ReturnValue.Range().End.Line, rs.ReturnValue.Range().End.Col},
	}
}

type ExpressionStatement struct {
	Token      token.Token
	Expression Expression
	Start      Position
	End        Position
}

func (es *ExpressionStatement) statementNode()       {}
func (es *ExpressionStatement) TokenLiteral() string { return es.Token.Literal }
func (es *ExpressionStatement) Range() NodeRange {
	return NodeRange{
		Start: Position{es.Expression.Range().Start.Line, es.Expression.Range().Start.Col},
		End:   Position{es.Expression.Range().End.Line, es.Expression.Range().End.Col},
	}
}

type IntegerLiteral struct {
	Token token.Token
	Value int64
	Start Position
	End   Position
}

func (il *IntegerLiteral) expressionNode()      {}
func (il *IntegerLiteral) TokenLiteral() string { return il.Token.Literal }
func (il *IntegerLiteral) String() string       { return il.Token.Literal }
func (il *IntegerLiteral) Range() NodeRange {
	return NodeRange{
		Start: Position{Line: il.Token.Line, Col: il.Token.Col},
		End:   Position{Line: il.Token.Line, Col: il.Token.Col + len(il.Token.Literal)},
	}
}

type Boolean struct {
	Token token.Token
	Value bool
	Start Position
	End   Position
}

func (b *Boolean) expressionNode()      {}
func (b *Boolean) TokenLiteral() string { return b.Token.Literal }
func (b *Boolean) String() string       { return b.Token.Literal }
func (il *Boolean) Range() NodeRange {
	return NodeRange{
		Start: Position{Line: il.Token.Line, Col: il.Token.Col},
		End:   Position{Line: il.Token.Line, Col: il.Token.Col + len(il.Token.Literal)},
	}
}

type PrefixExpression struct {
	Token    token.Token
	Operator string
	Right    Expression
	Start    Position
	End      Position
}

func (pe *PrefixExpression) expressionNode()      {}
func (pe *PrefixExpression) TokenLiteral() string { return pe.Token.Literal }
func (pe *PrefixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(pe.Operator)
	out.WriteString(pe.Right.String())
	out.WriteString(")")

	return out.String()
}

func (pe *PrefixExpression) Range() NodeRange {
	return NodeRange{
		Start: Position{Line: pe.Token.Line, Col: pe.Token.Col},
		End:   Position{Line: pe.Right.Range().End.Line, Col: pe.Right.Range().End.Col},
	}
}

type InfixExpression struct {
	Token    token.Token
	Operator string
	Left     Expression
	Right    Expression
	Start    Position
	End      Position
}

func (ie *InfixExpression) expressionNode()      {}
func (ie *InfixExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *InfixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(ie.Left.String())
	out.WriteString(" " + ie.Operator + " ")
	out.WriteString(ie.Right.String())
	out.WriteString(")")

	return out.String()
}

func (ie *InfixExpression) Range() NodeRange {
	return NodeRange{
		Start: Position{Line: ie.Left.Range().Start.Line, Col: ie.Left.Range().Start.Col},
		End:   Position{Line: ie.Right.Range().End.Line, Col: ie.Right.Range().End.Col},
	}
}

type IfExpression struct {
	Token       token.Token
	Condition   Expression
	Consequence *BlockStatement
	Alternative *BlockStatement
	Start       Position
	End         Position
}

func (ie *IfExpression) expressionNode()      {}
func (ie *IfExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *IfExpression) String() string {
	var out bytes.Buffer
	out.WriteString("if")
	out.WriteString(ie.Condition.String())
	out.WriteString(" ")
	out.WriteString(ie.Consequence.String())
	if ie.Alternative != nil {
		out.WriteString("else ")
		out.WriteString(ie.Alternative.String())
	}
	return out.String()
}

func (ie *IfExpression) Range() NodeRange {
	return NodeRange{
		Start: ie.Start,
		End:   ie.End,
	}
}

type BlockStatement struct {
	Token      token.Token
	Statements []Statement
	Start      Position
	End        Position
}

func (bs *BlockStatement) statementNode()       {}
func (bs *BlockStatement) TokenLiteral() string { return bs.Token.Literal }
func (bs *BlockStatement) String() string {
	var out bytes.Buffer
	for _, s := range bs.Statements {
		out.WriteString(s.String())
	}
	return out.String()
}
func (bs *BlockStatement) Range() NodeRange {
	return NodeRange{
		Start: bs.Start,
		End:   bs.End,
	}
}

type CallExpression struct {
	Token     token.Token
	Function  Expression
	Arguments []Expression
	Start     Position
	End       Position
}

func (ce *CallExpression) expressionNode()      {}
func (ce *CallExpression) TokenLiteral() string { return ce.Token.Literal }
func (ce *CallExpression) String() string {
	var out bytes.Buffer

	args := []string{}
	for _, a := range ce.Arguments {
		args = append(args, a.String())
	}

	out.WriteString(ce.Function.String())
	out.WriteString("(")
	out.WriteString(strings.Join(args, ", "))
	out.WriteString(")")

	return out.String()
}
func (ce *CallExpression) Range() NodeRange {
	return NodeRange{
		Start: ce.Function.Range().Start,
		End:   ce.End,
	}
}

type ArrayLiteral struct {
	Token    token.Token
	Elements []Expression
	Start    Position
	End      Position
}

func (al *ArrayLiteral) expressionNode()      {}
func (al *ArrayLiteral) TokenLiteral() string { return al.Token.Literal }
func (al *ArrayLiteral) String() string {
	var out bytes.Buffer

	elements := []string{}
	for _, el := range al.Elements {
		elements = append(elements, el.String())
	}

	out.WriteString("[")
	out.WriteString(strings.Join(elements, ", "))
	out.WriteString("]")

	return out.String()
}
func (al *ArrayLiteral) Range() NodeRange {
	return NodeRange{
		Start: al.Start,
		End:   al.End,
	}
}

type HashPair struct {
	Key   Expression
	Value Expression
}

type HashLiteral struct {
	Token        token.Token
	Pairs        map[Expression]Expression
	OrderedPairs []HashPair
	Start        Position
	End          Position
}

func (hl *HashLiteral) expressionNode()      {}
func (hl *HashLiteral) TokenLiteral() string { return hl.Token.Literal }
func (hl *HashLiteral) String() string {
	var out bytes.Buffer
	pairs := []string{}
	for key, value := range hl.Pairs {
		pairs = append(pairs, key.String()+":"+value.String())
	}
	out.WriteString("{")
	out.WriteString(strings.Join(pairs, ","))
	out.WriteString("}")

	return out.String()
}

// TODO fix this function
func (hl *HashLiteral) Range() NodeRange {
	return NodeRange{
		Start: hl.Start,
		End:   hl.End,
	}
}

type IndexExpression struct {
	Token token.Token // The [ token
	Left  Expression
	Index Expression
	Start Position
	End   Position
}

func (ie *IndexExpression) expressionNode()      {}
func (ie *IndexExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *IndexExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(ie.Left.String())
	out.WriteString("[")
	out.WriteString(ie.Index.String())
	out.WriteString("])")

	return out.String()

}
func (ie *IndexExpression) Range() NodeRange {
	return NodeRange{
		Start: Position{ie.Left.Range().Start.Line, ie.Left.Range().Start.Col},
		End:   Position{ie.Index.Range().End.Line, ie.Index.Range().End.Col},
	}
}
