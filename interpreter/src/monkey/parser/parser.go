package parser

import (
	"fmt"
	"monkey/ast"
	"monkey/lexer"
	"monkey/token"
	"strconv"
)

type (
	prefixParseFn func() ast.Expression
	infixParseFn  func(ast.Expression) ast.Expression
)

type Parser struct {
	l *lexer.Lexer

	curToken  token.Token
	peekToken token.Token

	errors []string

	prefixParseFns map[token.TokenType]prefixParseFn
	infixParseFns  map[token.TokenType]infixParseFn
}

func New(l *lexer.Lexer) *Parser {
	p := &Parser{
		l:      l,
		errors: []string{},
	}

	p.prefixParseFns = make(map[token.TokenType]prefixParseFn)
	p.registerPrefix(token.IDENT, p.parseIdentifier)
	p.registerPrefix(token.INT, p.parseIntegerLiteral)
	p.registerPrefix(token.BANG, p.parsePrefixExpression)
	p.registerPrefix(token.MINUS, p.parsePrefixExpression)
	p.registerPrefix(token.IF, p.parseIfExpression)

	p.infixParseFns = make(map[token.TokenType]infixParseFn)
	p.registerInfix(token.PLUS, p.parseInfixExpression)
	p.registerInfix(token.MINUS, p.parseInfixExpression)
	p.registerInfix(token.SLASH, p.parseInfixExpression)
	p.registerInfix(token.ASTERISK, p.parseInfixExpression)
	p.registerInfix(token.EQ, p.parseInfixExpression)
	p.registerInfix(token.NOT_EQ, p.parseInfixExpression)
	p.registerInfix(token.LT, p.parseInfixExpression)
	p.registerInfix(token.GT, p.parseInfixExpression)
	p.registerInfix(token.LBRACKET, p.parseIndexExpression)

	p.registerPrefix(token.TRUE, p.parseBoolean)
	p.registerPrefix(token.FALSE, p.parseBoolean)
	p.registerPrefix(token.LPAREN, p.parseGroupedExpression)
	p.registerPrefix(token.LBRACKET, p.parseArrayLiteral)
	//p.registerPrefix(token.LBRACE, p.parseHashLiteral)
	//p.registerPrefix(token.LBRACE, p.parseLambdaLiteral)
	p.registerPrefix(token.LBRACE, p.dispatchLBrace)
	p.registerPrefix(token.FUNCTION, p.parseFunctionLiteral)
	p.registerPrefix(token.STRING, p.parseStringLiteral)

	p.registerInfix(token.LPAREN, p.parseCallExpression)
	p.registerInfix(token.LBRACE, p.parseCallWithSingleTrailingLambda)
	p.registerInfix(token.PERIOD, p.parseMethodCallExpression)
	p.nextToken()
	p.nextToken()

	return p
}

func (p *Parser) registerPrefix(tokenType token.TokenType, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn

}
func (p *Parser) registerInfix(tokenType token.TokenType, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
}

func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}
	program.Statements = []ast.Statement{}

	for !p.curTokenIs(token.EOF) {
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		p.nextToken()
	}
	return program
}

func (p *Parser) parseIdentifier() ast.Expression {
	ie := &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
	ie.Start = ast.Position{Line: ie.Token.Line, Col: ie.Token.Col}
	ie.End = ast.Position{Line: ie.Token.Line, Col: ie.Token.Col + len(ie.Token.Literal)}
	return ie
}

func (p *Parser) parseStatement() ast.Statement {
	switch p.curToken.Type {
	case token.LET:
		return p.parseLetStatement()
	case token.RETURN:
		return p.parseReturnStatement()
	default:
		return p.parseExpressionStatement()
	}
}

func (p *Parser) parseLetStatement() *ast.LetStatement {
	stmt := &ast.LetStatement{Token: p.curToken}
	stmt.Start = ast.Position{Line: stmt.Token.Line, Col: stmt.Token.Col}

	if !p.expectPeek(token.IDENT) {
		return nil
	}

	stmt.Name = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
	stmt.Name.Start = ast.Position{Line: stmt.Name.Token.Line, Col: stmt.Name.Token.Col}
	stmt.Name.End = ast.Position{Line: stmt.Name.Token.Line, Col: stmt.Name.Token.Col + len(stmt.Name.Token.Literal)}

	if !p.expectPeek(token.ASSIGN) {
		return nil
	}

	p.nextToken()

	stmt.Value = p.parseExpression(LOWEST)

	if fl, ok := stmt.Value.(*ast.FunctionLiteral); ok {
		fl.Name = stmt.Name.Value
	}

	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}
	stmt.End = ast.Position{Line: p.curToken.Line, Col: p.curToken.Col + 1}

	return stmt
}

func (p *Parser) parseReturnStatement() *ast.ReturnStatement {
	stmt := &ast.ReturnStatement{Token: p.curToken}

	p.nextToken()

	stmt.ReturnValue = p.parseExpression(LOWEST)

	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	// defer untrace(trace("parseExpressionStatement"))
	stmt := &ast.ExpressionStatement{Token: p.curToken}

	stmt.Expression = p.parseExpression(LOWEST)

	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}
	return stmt
}

func (p *Parser) parseExpression(precedence int) ast.Expression {
	// defer untrace(trace("parseExpression"))

	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		p.noPrefixParseFnError(p.curToken.Type)
		return nil
	}
	leftExp := prefix()

	for !p.peekTokenIs(token.SEMICOLON) && precedence < p.peekPrecedence() {
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			return leftExp
		}

		p.nextToken()

		leftExp = infix(leftExp)
	}

	return leftExp
}

func (p *Parser) parseGroupedExpression() ast.Expression {
	p.nextToken()

	exp := p.parseExpression(LOWEST)

	if !p.expectPeek(token.RPAREN) {
		return nil
	}

	return exp
}

func (p *Parser) parsePrefixExpression() ast.Expression {
	// defer untrace(trace("parsePrefixExpression"))

	expression := &ast.PrefixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}

	p.nextToken()

	expression.Right = p.parseExpression(PREFIX)

	return expression
}

func (p *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	// defer untrace(trace("parseInfixExpression"))
	expression := &ast.InfixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
		Left:     left,
	}

	precedence := p.curPrecedence()
	p.nextToken()
	expression.Right = p.parseExpression(precedence)

	return expression
}

func (p *Parser) parseIfExpression() ast.Expression {
	expression := &ast.IfExpression{Token: p.curToken}
	expression.Start = ast.Position{Line: expression.Token.Line, Col: expression.Token.Col}
	if !p.expectPeek(token.LPAREN) {
		return nil
	}

	p.nextToken()
	expression.Condition = p.parseExpression(LOWEST)
	if !p.expectPeek(token.RPAREN) {
		return nil
	}

	expression.End = ast.Position{Line: p.curToken.Line, Col: p.curToken.Col}
	if !p.expectPeek(token.LBRACE) {
		return nil
	}
	expression.Consequence = p.parseBlockStatement()

	if p.peekTokenIs(token.ELSE) {
		p.nextToken()
		if !p.expectPeek(token.LBRACE) {
			return nil
		}
		expression.Alternative = p.parseBlockStatement()
	}

	expression.End = ast.Position{Line: p.curToken.Line, Col: p.curToken.Col + 1}

	return expression
}

func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	block := &ast.BlockStatement{Token: p.curToken}
	block.Start = ast.Position{Line: block.Token.Line, Col: block.Token.Col}
	block.Statements = []ast.Statement{}
	p.nextToken()
	for !p.curTokenIs(token.RBRACE) && !p.curTokenIs(token.EOF) {
		stmt := p.parseStatement()
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}
		p.nextToken()
	}
	block.End = ast.Position{Line: p.curToken.Line, Col: p.curToken.Col + 1}
	return block
}

func (p *Parser) parseIntegerLiteral() ast.Expression {
	// // defer untrace(trace("parseIntegerLiteral"))
	lit := &ast.IntegerLiteral{Token: p.curToken}

	value, err := strconv.ParseInt(p.curToken.Literal, 0, 64)
	if err != nil {
		msg := fmt.Sprintf("could not parse %q as integer", p.curToken.Literal)
		p.errors = append(p.errors, msg)
		return nil
	}

	lit.Value = value
	lit.Start = ast.Position{Line: lit.Token.Line, Col: lit.Token.Col}
	lit.End = ast.Position{Line: lit.Token.Line, Col: lit.Token.Col + len(lit.Token.Literal)}

	return lit
}

func (p *Parser) parseStringLiteral() ast.Expression {
	sl := &ast.StringLiteral{Token: p.curToken, Value: p.curToken.Literal}
	sl.Start = ast.Position{Line: sl.Token.Line, Col: sl.Token.Col}
	// Plus two to account for two quotes around string
	sl.End = ast.Position{Line: sl.Token.Line, Col: sl.Token.Col + len(sl.Token.Literal) + 2}
	return sl

}

func (p *Parser) parseFunctionLiteral() ast.Expression {
	lit := &ast.FunctionLiteral{Token: p.curToken}
	lit.Start = ast.Position{Line: lit.Token.Line, Col: lit.Token.Col}

	if !p.expectPeek(token.LPAREN) {
		return nil
	}

	lit.Parameters = p.parserFunctionParameters()

	if !p.expectPeek(token.LBRACE) {
		return nil
	}

	lit.Body = p.parseBlockStatement()
	lit.End = ast.Position{Line: lit.Body.Range().End.Line, Col: lit.Body.Range().End.Col}

	return lit
}

func (p *Parser) parserFunctionParameters() []*ast.Identifier {
	identifiers := []*ast.Identifier{}
	if p.peekTokenIs(token.RPAREN) {
		p.nextToken()
		return identifiers
	}

	p.nextToken()

	ident := &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
	ident.Start = ast.Position{Line: ident.Token.Line, Col: ident.Token.Col}
	ident.End = ast.Position{Line: ident.Token.Line, Col: ident.Token.Col + len(ident.Token.Literal)}
	identifiers = append(identifiers, ident)
	for p.peekTokenIs(token.COMMA) {
		p.nextToken()
		p.nextToken()
		ident := &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
		ident.Start = ast.Position{Line: ident.Token.Line, Col: ident.Token.Col}
		ident.End = ast.Position{Line: ident.Token.Line, Col: ident.Token.Col + len(ident.Token.Literal)}
		identifiers = append(identifiers, ident)
	}

	if !p.expectPeek(token.RPAREN) {
		return nil
	}

	return identifiers
}

func (p *Parser) parseCallExpression(function ast.Expression) ast.Expression {
	exp := &ast.CallExpression{Token: p.curToken, Function: function}
	exp.Start = ast.Position{Line: exp.Token.Line, Col: exp.Token.Col}
	exp.Arguments = p.parseExpressionList(token.RPAREN)
	if p.peekTokenIs(token.LBRACE) {
		p.nextToken()
		lambda := p.parseLambdaLiteral()
		exp.Arguments = append(exp.Arguments, lambda)
	}
	exp.End = ast.Position{Line: p.curToken.Line, Col: p.curToken.Col + 1}
	return exp
}

func (p *Parser) parseCallWithSingleTrailingLambda(function ast.Expression) ast.Expression {
	exp := &ast.CallExpression{Token: p.curToken, Function: function}
	exp.Start = ast.Position{Line: exp.Token.Line, Col: exp.Token.Col}

	lambda := &ast.FunctionLiteral{Token: p.curToken}
	lambda.Start = ast.Position{Line: lambda.Token.Line, Col: lambda.Token.Col}
	lambda.Parameters = p.parserLambdaParameters()
	lambda.Body = p.parseBlockStatement()

	exp.Arguments = []ast.Expression{lambda}
	exp.End = ast.Position{Line: p.curToken.Line, Col: p.curToken.Col + 1}
	return exp
}

func (p *Parser) parseMethodCallExpression(firstArg ast.Expression) ast.Expression {
	if !p.expectPeekOneOf(token.IDENT, token.LPAREN) {
		return nil
	}
	methodCall := p.parseExpression(METHOD)
	call, ok := methodCall.(*ast.CallExpression)
	if !ok {
		msg := fmt.Sprintf("Not a function call %s", methodCall.String())
		p.errors = append(p.errors, msg)
		return nil
	}
	call.Arguments = append([]ast.Expression{firstArg}, call.Arguments...)

	return call
}

func (p *Parser) dispatchLBrace() ast.Expression {

	tokenCur, tokenPeek := p.curToken, p.peekToken
	anchorCurrent, anchorReading, anchorChar := p.l.Positions()
	hash := p.parseHashLiteral()
	if hash == nil {
		p.errors = p.errors[:len(p.errors)-1]
		p.curToken = tokenCur
		p.peekToken = tokenPeek
		p.l.SetPositions(anchorCurrent, anchorReading, anchorChar)
		lit := p.parseLambdaLiteral()
		return lit
	}
	return hash

}

func (p *Parser) parseLambdaLiteral() *ast.FunctionLiteral {

	lit := &ast.FunctionLiteral{Token: p.curToken}
	lit.Start = ast.Position{Line: lit.Token.Line, Col: lit.Token.Col}
	lit.Parameters = p.parserLambdaParameters()
	lit.Body = p.parseBlockStatement()
	return lit
}

func (p *Parser) parserLambdaParameters() []*ast.Identifier {
	identifiers := []*ast.Identifier{}

	p.nextToken()

	ident := &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
	ident.Start = ast.Position{Line: ident.Token.Line, Col: ident.Token.Col}
	ident.End = ast.Position{Line: ident.Token.Line, Col: ident.Token.Col + len(ident.Token.Literal)}
	identifiers = append(identifiers, ident)
	for p.peekTokenIs(token.COMMA) {
		p.nextToken()
		p.nextToken()
		ident := &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
		ident.Start = ast.Position{Line: ident.Token.Line, Col: ident.Token.Col}
		ident.End = ast.Position{Line: ident.Token.Line, Col: ident.Token.Col + len(ident.Token.Literal)}
		identifiers = append(identifiers, ident)
	}

	if !p.expectPeek(token.SLIMARROW) {
		return nil
	}

	return identifiers
}

func (p *Parser) parseBoolean() ast.Expression {
	be := &ast.Boolean{Token: p.curToken, Value: p.curTokenIs(token.TRUE)}
	be.Start = ast.Position{Line: be.Token.Line, Col: be.Token.Col}
	be.End = ast.Position{Line: be.Token.Line, Col: be.Token.Col + len(be.Token.Literal)}
	return be
}

func (p *Parser) parseArrayLiteral() ast.Expression {
	array := &ast.ArrayLiteral{Token: p.curToken}
	array.Start = ast.Position{Line: array.Token.Line, Col: array.Token.Col}

	array.Elements = p.parseExpressionList(token.RBRACKET)
	array.End = ast.Position{Line: p.curToken.Line, Col: p.curToken.Col + 1}
	return array
}

func (p *Parser) parseHashLiteral() ast.Expression {
	hash := &ast.HashLiteral{Token: p.curToken}
	hash.Start = ast.Position{Line: hash.Token.Line, Col: hash.Token.Col}
	hash.Pairs = make(map[ast.Expression]ast.Expression)
	hash.OrderedPairs = []ast.HashPair{}

	for !p.peekTokenIs(token.RBRACE) {
		p.nextToken()
		key := p.parseExpression(LOWEST)

		if !p.expectPeek(token.COLON) {
			return nil
		}

		p.nextToken()
		value := p.parseExpression(LOWEST)

		hash.Pairs[key] = value
		hash.OrderedPairs = append(hash.OrderedPairs, ast.HashPair{Key: key, Value: value})

		if !p.peekTokenIs(token.RBRACE) && !p.expectPeek(token.COMMA) {
			return nil
		}
	}

	if !p.expectPeek(token.RBRACE) {
		return nil
	}
	hash.End = ast.Position{Line: p.curToken.Line, Col: p.curToken.Col + 1}
	return hash
}

func (p *Parser) parseIndexExpression(left ast.Expression) ast.Expression {
	exp := &ast.IndexExpression{Token: p.curToken, Left: left}
	p.nextToken()
	exp.Index = p.parseExpression(LOWEST)
	if !p.expectPeek(token.RBRACKET) {
		return nil
	}
	return exp
}

func (p *Parser) parseExpressionList(end token.TokenType) []ast.Expression {
	list := []ast.Expression{}

	if p.peekTokenIs(end) {
		p.nextToken()
		return list
	}

	p.nextToken()
	list = append(list, p.parseExpression(LOWEST))

	for p.peekTokenIs(token.COMMA) {
		p.nextToken()
		p.nextToken()
		list = append(list, p.parseExpression(LOWEST))
	}

	if !p.expectPeek(end) {
		return nil
	}

	return list

}

func (p *Parser) curTokenIs(t token.TokenType) bool {
	return p.curToken.Type == t
}

func (p *Parser) peekTokenIs(t token.TokenType) bool {
	return p.peekToken.Type == t
}

func (p *Parser) expectPeek(t token.TokenType) bool {
	if p.peekTokenIs(t) {
		p.nextToken()
		return true
	} else {
		p.peekError(t)
		return false
	}
}

func (p *Parser) expectPeekOneOf(ts ...token.TokenType) bool {
	for _, t := range ts {
		if p.peekTokenIs(t) {
			p.nextToken()
			return true
		}
	}
	p.peekError(ts...)
	return false
}

func (p *Parser) peekPrecedence() int {
	if p, ok := precedences[p.peekToken.Type]; ok {
		return p
	}

	return LOWEST
}

func (p *Parser) curPrecedence() int {
	if p, ok := precedences[p.curToken.Type]; ok {
		return p
	}

	return LOWEST
}

const (
	_ int = iota
	LOWEST
	EQUALS
	LESSGREATER
	SUM
	PRODUCT
	PREFIX
	METHOD
	LAMBDA
	CALL
	INDEX // array[index]
)

// TODO sometimes a missing semicolon leads to wonky parsing, could be the precedences
var precedences = map[token.TokenType]int{
	token.EQ:       EQUALS,
	token.NOT_EQ:   EQUALS,
	token.LT:       LESSGREATER,
	token.GT:       LESSGREATER,
	token.PLUS:     SUM,
	token.MINUS:    SUM,
	token.SLASH:    PRODUCT,
	token.ASTERISK: PRODUCT,
	token.LBRACE:   LAMBDA,
	token.PERIOD:   METHOD,
	token.LPAREN:   CALL,
	token.LBRACKET: INDEX,
}

func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) peekError(t ...token.TokenType) {
	msg := fmt.Sprintf("Error at line %d col %d, expected next token to be %s, got %s instead",
		p.peekToken.Line, p.peekToken.Col, t, p.peekToken.Type)
	p.errors = append(p.errors, msg)
}

func (p *Parser) noPrefixParseFnError(t token.TokenType) {
	msg := fmt.Sprintf("Error at line %d col %d, no prefix parse function of %s found", p.curToken.Line, p.curToken.Col, t)
	p.errors = append(p.errors, msg)
}
