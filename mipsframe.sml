structure frame : FRAME = MipsFrame
struct
  (*In chapter 7, frame module must also include instructions for view shift*)
  type frame = {name:Temp.label, formals:access list, cuantos_locales:ref int}
  datatype access = InFrame of int | InReg of Temp.temp

  fun newFrame {name=name, formals=formals} =
    {name = name,
    formals = map formalToAccess formals,
    cuantos_locales = ref 0
    (* TODO: Aqui puede ser que tengamos que añadir cosas. *)
    }

  (* TODO: Seria bueno hacer una funcion auxiliar para reciclarla en allocLocal y formalToAccess *)
  fun allocLocal {name, formals, cuantos_locales} true =
    (* TODO: Generar el access *)
  fun formalToAccess formal -> access =
    (* TODO: Crear un access nuevo. El parametro formal es un booleano para
    determinar si esta variable 'escapa' o no. No se puede llamar allocLocal, pero
    hay que escribir el mismo código que ahi para generar un access *)
end
