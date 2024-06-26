package lexer

import (
	"github.com/moritz-tiesler/monkey/token"
)

type Lexer struct {
	input        string
	position     int  // current position in input (points to current char)
	readPosition int  // current reading position in input (after current char)
	ch           byte // current char under examination
	line         int
	col          int
}

func New(input string) *Lexer {
	l := &Lexer{input: input, line: 1, col: 0}
	l.readChar()
	return l
}

func (l *Lexer) NextToken() token.Token {
	var tok token.Token

	l.skipWhitespace()
	currCol := l.col

	switch l.ch {
	case '=':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			literal := string(ch) + string(l.ch)
			tok = token.Token{Type: token.EQ, Literal: literal}
		} else {
			tok = newToken(token.ASSIGN, l.ch, l.line, currCol)
		}
	case '+':
		tok = newToken(token.PLUS, l.ch, l.line, currCol)
	case '-':
		if l.peekChar() == '>' {
			ch := l.ch
			l.readChar()
			literal := string(ch) + string(l.ch)
			tok = token.Token{Type: token.SLIMARROW, Literal: literal}
		} else {
			tok = newToken(token.MINUS, l.ch, l.line, currCol)
		}
	case '!':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			literal := string(ch) + string(l.ch)
			tok = token.Token{Type: token.NOT_EQ, Literal: literal}
		} else {
			tok = newToken(token.BANG, l.ch, l.line, currCol)
		}
	case '/':
		tok = newToken(token.SLASH, l.ch, l.line, currCol)
	case '*':
		tok = newToken(token.ASTERISK, l.ch, l.line, currCol)
	case '<':
		tok = newToken(token.LT, l.ch, l.line, currCol)
	case '>':
		tok = newToken(token.GT, l.ch, l.line, currCol)
	case ';':
		tok = newToken(token.SEMICOLON, l.ch, l.line, currCol)
	case ':':
		tok = newToken(token.COLON, l.ch, l.line, currCol)
	case ',':
		tok = newToken(token.COMMA, l.ch, l.line, currCol)
	case '{':
		tok = newToken(token.LBRACE, l.ch, l.line, currCol)
	case '}':
		tok = newToken(token.RBRACE, l.ch, l.line, currCol)
	case '(':
		tok = newToken(token.LPAREN, l.ch, l.line, currCol)
	case ')':
		tok = newToken(token.RPAREN, l.ch, l.line, currCol)
	case '[':
		tok = newToken(token.LBRACKET, l.ch, l.line, currCol)
	case ']':
		tok = newToken(token.RBRACKET, l.ch, l.line, currCol)
	case '.':
		tok = newToken(token.PERIOD, l.ch, l.line, currCol)
	case '"':
		tok.Type = token.STRING
		tok.Literal = l.readString()
		tok.Line = l.line
		tok.Col = currCol

	case 0:
		tok.Literal = ""
		tok.Type = token.EOF
	default:
		if isLetter(l.ch) {
			tok.Literal = l.readIdentifier()
			tok.Type = token.LookupIdent(tok.Literal)
			tok.Col = currCol
			tok.Line = l.line
			return tok
		} else if isDigit(l.ch) {
			tok.Type = token.INT
			tok.Literal = l.readNumber()
			tok.Col = currCol
			tok.Line = l.line
			return tok
		} else {
			tok = newToken(token.ILLEGAL, l.ch, l.line, currCol)
		}
	}

	l.readChar()
	return tok
}

func (l *Lexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' {
		if l.ch == '\n' {
			l.line += 1
			l.col = 0
		}
		l.readChar()
	}
}

func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0
	} else {
		l.ch = l.input[l.readPosition]
	}
	l.position = l.readPosition
	l.readPosition += 1
	l.col += 1
}

func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	} else {
		return l.input[l.readPosition]
	}
}

func (l *Lexer) readIdentifier() string {
	position := l.position
	for isLetter(l.ch) {
		l.readChar()
	}
	return l.input[position:l.position]
}

func (l *Lexer) readNumber() string {
	position := l.position
	for isDigit(l.ch) {
		l.readChar()
	}
	return l.input[position:l.position]
}

func (l *Lexer) readString() string {
	position := l.position + 1
	for {
		l.readChar()
		if l.ch == '"' || l.ch == 0 {
			break
		}
	}
	return l.input[position:l.position]
}

func isLetter(ch byte) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

func isDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}

func newToken(tokenType token.TokenType, ch byte, line int, col int) token.Token {
	return token.Token{Type: tokenType, Literal: string(ch), Line: line, Col: col}
}

func (l Lexer) Positions() (current int, reading int, ch byte) {
	current = l.position
	reading = l.readPosition
	ch = l.ch
	return
}

func (l *Lexer) SetPositions(current int, reading int, ch byte) {
	l.position = current
	l.readPosition = reading
	l.ch = ch
}
