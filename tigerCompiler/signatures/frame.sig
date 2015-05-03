signature FRAME =
sig
  type frame
  type access

  val newFrame : {name: Temp.label, formals: bool list} -> frame
  val name : frame -> Temp.label
  val formals : frame -> access list
  val cuantos_locales : frame -> int ref
  val allocLocal : frame -> bool -> access

  val RV: Temp.temp
  val RA: Temp.temp
  val v0: Temp.temp

  val wordsize: int

  val exp: access -> Tree.exp -> Tree.exp

  val procEntryExit1: frame * Tree.stm -> Tree.stm

  datatype frag = PROC of {body: Tree.stm, frame:frame}

end