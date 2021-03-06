(* Figure 5.8 gives the signature for ENV *)
signature ENV =
sig
  type access
  type ty
  datatype enventry = VarEntry of {access: Translate.access, ty: ty}
    | FunEntry of {level: Translate.level, label: Temp.label, formals: ty list, result: Types.ty}
  val base_tenv : ty Symbol.table
  val base_venv : enventry Symbol.table
end

(* Now give an implementation *)
structure Env : ENV =
struct
  type access = unit
  type ty = Types.ty

  datatype enventry = VarEntry of {access: Translate.access, ty: ty}
    | FunEntry of {level: Translate.level, label: Temp.label, formals: ty list, result: ty}

  val base_tenv = Symbol.enter(Symbol.enter(Symbol.empty,
                        Symbol.symbol "int", Types.INT),
                        Symbol.symbol "string", Types.STRING)
  val base_venv = Symbol.empty
end
