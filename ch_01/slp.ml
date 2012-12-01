open Core.Std
  
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

let prog =
  CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
              CompoundStm(AssignStm("b",
                                    EseqExp(PrintStm[IdExp"a";OpExp(IdExp"a",Minus,NumExp 1)],
                                            OpExp(NumExp 10, Times, IdExp"a"))),
                          PrintStm[IdExp "b"]))
;;

(* maxargs : stm -> int *)
let rec maxargs stm =
  match stm with 
  | CompoundStm (s1, s2) -> (max (maxargs s1) (maxargs s2))
  | AssignStm (_, EseqExp (s, _)) -> (max 0 (maxargs s))
  | PrintStm ((e::es) as l) ->
    (max
       (List.length l)
       (let init =
          match e with
          | EseqExp (s, _) -> maxargs s
          | _ -> 0
        in
        List.fold_left ~f:max ~init
          (List.filter_map
             ~f:(fun x -> match x with | EseqExp (s, _) -> Some (maxargs s) | _ -> None)
             es)))
  | _ -> 0
;;

(* Note: We assume (as suggested) that the first assignment in a list is the active one. *)
type table = (id * int) list
let update table id int = (id, int)::table
let rec lookup table id =
  match table with
  | (a, b)::rest -> if a = id then b else lookup rest id
  | [] -> failwithf "Unbound: %s" id ()
;;

(* interpStm : stm * table -> table *)
let rec interpStm stm table =
  match stm with
  | PrintStm [] -> (printf "\n%!"; table)
  | PrintStm (e::es) ->
    begin
      let i, table' = interpExp e table in
      printf "%i\n%!" i;
      interpStm (PrintStm es) table'
    end
  | CompoundStm (s1, s2) ->
    let table' = interpStm s1 table in
    interpStm s2 table'
  | AssignStm (id, exp) ->
    let i, table' = interpExp exp table in
    update table' id i
and
(* interpExp : exp * table -> int * table *)
interpExp exp table =
  match exp with
  | IdExp id -> (lookup table id, table)
  | NumExp n -> (n, table)
  | OpExp (e1, op, e2) ->
    begin
      let i1, t1 = interpExp e1 table in
      let i2, t2 = interpExp e2 t1 in
      match op with
      | Plus -> (i1 + i2, t2)
      | Minus -> (i1 - i2, t2)
      | Times -> (i1 * i2, t2)
      | Div -> (i1 / i2, t2)
    end
  | EseqExp (s, e) ->
      let table' = interpStm s table in
      interpExp e table'
;;

let interp prog =
  ignore (interpStm prog [])
;;
