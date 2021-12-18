package llvm

import (
	"fmt"
	"strings"

	"github.com/llir/llvm/ir"
)

func FuncIdxOf(m *ir.Module, f *ir.Func) int {
	for i, f2 := range m.Funcs {
		if f == f2 {
			return i
		}
	}
	return -1
}
func BlkIdxOf(m *ir.Func, f *ir.Block) int {
	for i, f2 := range m.Blocks {
		if f == f2 {
			return i
		}
	}
	return -1
}
func GenCore(m *ir.Module) string {
	buf := &strings.Builder{}
	for i, f := range m.Funcs {
		fmt.Fprintf(buf, "if funcid == %d{\nwhile ret == 0{", i)
		for bi, blk := range f.Blocks {
			fmt.Fprintf(buf, "if blkid == %d{", bi)
			for _, ins := range blk.Insts {
				switch ins := ins.(type) {
				case *ir.InstCall:
					fmt.Fprintf(buf, "%s := self(%d,%s) - 1", ins.LocalName, FuncIdxOf(m, ins.Callee), ins.Args[0])
				case *ir.InstAdd:
					fmt.Fprintf(buf, "%s := %s + %s", ins.LocalName, ins.X, ins.Y)
				case *ir.InstSub:
					fmt.Fprintf(buf, "%s := %s - %s", ins.LocalName, ins.X, ins.Y)
				case *ir.InstMul:
					fmt.Fprintf(buf, "%s := %s * %s", ins.LocalName, ins.X, ins.Y)
				case *ir.InstSDiv:
					fmt.Fprintf(buf, "%s := %s / %s", ins.LocalName, ins.X, ins.Y)
				case *ir.InstAlloca:
					fmt.Fprintf(buf, "%s := malloc(%s)", ins.LocalName, ins.Typ)
				case *ir.InstLoad:
					fmt.Fprintf(buf, "%s := [%s]", ins.LocalName, ins.Src)
				case *ir.InstStore:
					fmt.Fprintf(buf, "[%s] := %s", ins.Dst, ins.Src)
				}
			}
			switch term := blk.Term.(type) {
			case *ir.TermRet:
				fmt.Fprintf(buf, "ret := %s", term.X)
			case *ir.TermBr:
				fmt.Fprintf(buf, "blkid := %d", BlkIdxOf(f, term.Successors[0]))
			case *ir.TermCondBr:
				fmt.Fprintf(buf, `if %s{
					blkid := %d
				}
				if !%s{
					blkid := %d
				}`, term.Cond, BlkIdxOf(f, term.Successors[0]), term.Cond, BlkIdxOf(f, term.Successors[1]))
			}
			fmt.Fprintf(buf, "}")
		}
		fmt.Fprintf(buf, "}\nreturn ret\n}")
	}

	return buf.String()
}
