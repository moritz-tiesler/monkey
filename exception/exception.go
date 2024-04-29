package exception

type Exception interface {
	error
	Line() int
	Col() int
}
