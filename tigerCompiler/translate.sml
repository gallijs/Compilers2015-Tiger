structure Translate : TRANSLATE =
struct
  structure Frame = MipsFrame

  type exp = unit
  (* level tiene el frame adentro *)
  datatype level = Outermost of int | Level of int * Frame.frame
  type access = level * Frame.access

  val outermost = Outermost 0

  (* TODO: Estas funciones las va a utilizar semant.sml en CallExp y en VarDec *)

  (* TODO: newLevel tiene que crear un frame nuevo utilizando newFrame en mipsframe.sml.
  El level puede quedarse en 0 siempre. *)
  fun newLevel {parent : level, name : Temp.label, formals : bool list} =
    (* Adds an extra element to the formal-parameter list and calls *)
    let
      val newFrame' = Frame.newFrame({name=name, formals=formals})
    in
      Level (0, newFrame')
    end

  (* TODO: formals tiene que llamar la funcion formals de mipsframe.sml para el frame asociado al
  level que recibe. En nuestro caso level siempre es 0 *)
  fun formals level = []

  (* TODO: Basicamente lo mismo que formals pero utilizando el allocLocal de mipsframe.sml.
  Devuelve un access con el mismo level  y el frame.access de la variable que se acaba de alocar *)
  fun allocLocal (Level (lvl, f)) =
    let
      fun alloc esc : access =
        let
          val cuantos_locales = Frame.cuantos_locales f
          val faccess : Frame.access = Frame.allocLocal({name=Frame.name f, formals=Frame.formals f, cuantos_locales=cuantos_locales})(esc)
          val newLocals = ref (!cuantos_locales-4)
          val nframe : Frame.frame = {name=Frame.name(f), formals=Frame.formals(f), cuantos_locales=newLocals}
          val newLevel : level = Level (lvl, nframe)
          val returnAccess : access = (newLevel, faccess)
        in
          returnAccess
        end
    in
      alloc
    end
end