signature FRAME =
sig
  type frame
  type access

  val newFrame : {name: Temp.label, formals: bool list} -> frame
  val name : frame -> Temp.label
  val formals : frame -> access list
  val cuantos_locales : frame -> int ref
  val allocLocal : frame -> bool -> access

  val RA: Temp.temp
  val v0: Temp.temp

  val worldsize: int

  val exp: access => Tree.exp -> Tree.exp

  val procEntryExit1: frame * Tree.stm -> Tree.stm

  datatype frag = PRC of {body: Tree.stm, frame:frame}

end