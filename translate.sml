structure Translate : TRANSLATE =
struct
  structure Frame = MipsFrame

  type exp = unit
  (* level tiene el frame adentro *)
  type level = int * Frame
  type access = level * Frame.access

  val outermost = 0

  (* TODO: Estas funciones las va a utilizar semant.sml en CallExp y en VarDec *)

  (* TODO: newLevel tiene que crear un frame nuevo utilizando newFrame en mipsframe.sml.
  El level puede quedarse en 0 siempre. *)
  fun newLevel {parent, name, formals} = 0
    (* Adds an extra element to the formal-parameter list and calls *)
    let 
      val newFrame' = Frame.newFrame(name=name, formals=formals)
    in 
      {0, newFrame'}
    end

  (* TODO: formals tiene que llamar la funcion formals de mipsframe.sml para el frame asociado al
  level que recibe. En nuestro caso level siempre es 0 *)
  fun formals level = [(0, 0)]

  (* TODO: Basicamente lo mismo que formals pero utilizando el allocLocal de mipsframe.sml.
  Devuelve un access con el mismo level  y el frame.access de la variable que se acaba de alocar *)
  fun allocLocal {lvl, f} =
    let
      fun alloc esc:bool =
        let
          val cuantos_locales = Frame.cuantos_locales(f)
          val faccess = Frame.allocLocal(name=Frame.name(f), formals=Frame.formals(f), cuantos_locales=cuantos_locales)(esc)
        in
          {{lvl, {Frame.name(f), Frame.formals(f), !cuantos_locales-4}}, faccess}
        end
    in
      alloc
    end
end