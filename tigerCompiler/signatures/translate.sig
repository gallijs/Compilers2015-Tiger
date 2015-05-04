signature TRANSLATE =
sig
  type level
  type access
  type exp

  val outermost : level
  val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
  val formals: level -> access list
  val allocLocal: level -> bool -> access

  val intExp : int -> Tree.exp
  val nilExp: unit -> Tree.stm
  val seqExp: Tree.exp list -> Tree.exp
  val simpleVar: access -> level -> Tree.exp
  val opExp : Absyn.oper * Tree.exp * Tree.exp -> Tree.exp
  val assignExp : Tree.exp * Tree.exp -> Tree.stm


  val procEntryExit : {level:level, body:Tree.exp} -> unit
  val callExp : Absyn.symbol *  Tree.exp list * Temp.label -> Tree.exp
  val funDec : Temp.label * level * exp -> unit

  val frags: MipsFrame.frag list ref
  val getResult: unit ->  MipsFrame.frag list

end