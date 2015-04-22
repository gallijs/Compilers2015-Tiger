structure MipsFrame : FRAME =
struct
  (*In chapter 7, frame module must also include instructions for view shift*)
  datatype access = InFrame of int | InReg of Temp.temp
  type frame = {name:Temp.label, formals:access list, cuantos_locales:int ref}

  fun name({name = name, formals = _ , cuantos_locales = _}) = name
  fun formals({name = _, formals = formals , cuantos_locales = _}) = formals

  fun newFrame {name=name, formals=formals} =
    let
      fun formalToAccess boolThing = (*formal -> access*)
        if boolThing
        then InFrame 0
        else InReg (Temp.newtemp())

    in
      {name = name,
      formals = map formalToAccess formals,
      cuantos_locales = ref 0
      (* TODO: Aqui puede ser que tengamos que añadir cosas. *)
      }
    end

  (* TODO: Seria bueno hacer una funcion auxiliar para reciclarla en allocLocal y formalToAccess *)
  fun allocLocal {name, formals, cuantos_locales} boolThing =
    let
      fun alloc esc =
        InFrame((!cuantos_locales*-4) - 4)
    in
      alloc
    end
end
