type id = string
type binop = Plus | Minus | Times | Div
type stm =
  CompoundStm of stm * stm
| AssignStm of id * exp
| PrintStm of exp list
and exp =
  IdExp of id
| NumExp of int
| OpExp of exp * binop * exp
| EseqExp of stm * exp
;;

type table = (id * int) list
  
val interpStm : stm -> table -> table
val interpExp : exp -> table -> (int * table)
val interp    : stm -> unit
