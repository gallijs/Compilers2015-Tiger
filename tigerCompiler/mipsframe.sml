structure MipsFrame : FRAME =
struct
  (*In chapter 7, frame module must also include instructions for view shift*)
  datatype access = InFrame of int | InReg of Temp.temp
  type frame = {name:Temp.label, formals:access list, cuantos_locales:int ref}

  fun name({name = name, formals = _ , cuantos_locales = _}) = name
  fun formals({name = _, formals = formals , cuantos_locales = _}) = formals
  fun cuantos_locales({name = _, formals = _ , cuantos_locales = cuantos_locales}) = cuantos_locales

  fun newFrame {name, formals} =
    let
      fun formalToAccess boolThing = (*formal -> access*)
        (* We assume all functions only have 1 formal argument *)
        InFrame 0
    in
      {name = name,
      formals = map formalToAccess formals,
      cuantos_locales = ref 1
      (* TODO: Aqui puede ser que tengamos que añadir cosas. *)
      }
    end

  (* TODO: Seria bueno hacer una funcion auxiliar para reciclarla en allocLocal y formalToAccess *)
  fun allocLocal {name, formals, cuantos_locales} : bool -> access =
    let
      fun alloc esc : access =
        InFrame((!cuantos_locales * (~4)) - 4)
    in
      alloc
    end
end
