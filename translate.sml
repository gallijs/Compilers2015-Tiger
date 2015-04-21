structure Translate : TRANSLATE =
struct
  type exp = unit
  (* level tiene el frame adentro *)
  type level = int * Frame
  type access = level * Frame.access
  val outermost = 0

  (* TODO: Estas funciones las va a utilizar semant.sml en CallExp y en VarDec *)

  (* TODO: newLevel tiene que crear un frame nuevo utilizando newFrame en mipsframe.sml.
  El level puede quedarse en 0 siempre. *)
  fun newLevel {parent, name, formals} = 0

  (* TODO: formals tiene que llamar la funcion formals de mipsframe.sml para el frame asociado al
  level que recibe. En nuestro caso level siempre es 0 *)
  fun formals level = [(0, 0)]

  (* TODO: Basicamente lo mismo que formals pero utilizando el allocLocal de mipsframe.sml.
  Devuelve un access con el mismo level  y el frame.access de la variable que se acaba de alocar *)
  fun allocLocal level = (fn x: bool => (0, 0))
end