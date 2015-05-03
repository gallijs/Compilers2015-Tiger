structure MipsFrame : FRAME =
struct
  (*In chapter 7, frame module must also include instructions for view shift*)
  datatype access = InFrame of int | InReg of Temp.temp

  type frame = {name:Temp.label, formals:access list, cuantos_locales:int ref}

  datatype  frag = PROC of {body: Tree.stm, frame: frame}
  
  val wordsize = 4 (* bytes *)

  val FP = Temp.newtemp() (* frame pointer *)
  val RV = Temp.newtemp() (* return address *)
  val RA = Temp.newtemp() (* return address *)
  val v0 = Temp.newtemp() (* return value *)
  val a0 = Temp.newtemp() (* function arg *)

  (* to create registers *)
  val argsregs = List.tabulate (1, (fn _ => Temp.newtemp())) (* [a0] *)
  val calleestack = List.tabulate (8, (fn _ => Temp.newtemp())) (* [s0,...,s7] *)
  val callerstack = List.tabulate (10, (fn _ => Temp.newtemp())) (* [s0,...,s9] *)

  val calldefs = calleestack @ [RA]


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

  fun exp(InFrame(k)) = 
    (fn (fp) => Tree.MEM(Tree.BINOP(Tree.PLUS, fp, Tree.CONST(k))))

  fun moveReg(reg, var) = Tree.MOVE (Tree.TEMP reg, Tree.TEMP var)

  fun seq [] = Tree.EXP (Tree.CONST 0)
    | seq [exp] = exp
    | seq (exp::exps) = (Tree.SEQ (exp, (seq exps)))

  (*frame * Tree.stm -> Tree.stm*)
  fun procEntryExit1 (frame, body) = 
    let 
      val saved = [RA] @ calleestack
      val temps = map (fn temp => Temp.newtemp ()) saved
      val registerSaves = seq (ListPair.mapEq moveReg (temps, saved))
      val registerRestores = seq (ListPair.mapEq moveReg (saved, temps))
      val body' = seq [registerSaves, body, registerRestores]

      fun moveArg (arg, access) = 
        Tree.MOVE (exp access (Tree.TEMP FP), Tree.TEMP arg)
      val formalsArgs = formals frame
      val viewShift = seq (ListPair.map moveArg (argsregs, formalsArgs))
    in 
    case formalsArgs of 
      [] => body'
      | _ => Tree.SEQ (viewShift, body')     
    end
end
