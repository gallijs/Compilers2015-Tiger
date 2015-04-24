signature FRAME =
sig
  type frame
  type access
  val newFrame : {name: Temp.label, formals: bool list} -> frame
  val name : frame -> Temp.label
  val formals : frame -> access list
  val cuantos_locales : frame -> int ref
  val allocLocal : frame -> bool -> access
end