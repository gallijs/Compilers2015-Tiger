signature TRANSLATE =
sig
  type level
  type access
  type exp

  val outermost : level
  val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
  val formals: level -> access list
  val allocLocal: level -> bool -> access

  val intExp: int -> Tree.exp
  val nilExp: unit -> Tree.exp
  val seqExp: Tree.stm list -> Tree.exp


  val procEntryExit : {level:level, body:Tree.exp} -> unit
  val callExp : {funName: Temp.label, args: Tree.exp list} -> Tree.exp
  val funDec : {label: Temp.label, level : level, body: Tree.exp} -> unit
end