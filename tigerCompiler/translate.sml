structure Translate : TRANSLATE =
struct
  structure Frame = MipsFrame
  structure Tr = Tree

  type exp = unit
  (* level tiene el frame adentro *)
  datatype level = Outermost of int | Level of int * Frame.frame
  type access = level * Frame.access

  val addFrag = ref [] : Frame.frag list ref

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

  (* Basicamente lo mismo que formals pero utilizando el allocLocal de mipsframe.sml.
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

  fun nilExp () = (Tr.MEM (Tr.CONST 0))
  fun intExp n = (Tr.CONST n)


  fun convertSeq( [] ) = Tr.EXP(Tr.CONST 0)
    | convertSeq([seq]) = seq
    | convertSeq(seq::seqs) = Tr.SEQ(seq, convertSeq(seqs))

  fun seqExp(seqlist) = Tr.ESEQ(convertSeq(seqlist), Tr.CONST 0)

  (* procEntryExit recibe el level y el body de una funcion y se encarga de llamar
  procEntryExit1 con el body de la funcion porque el libro dice que hay que hacer eso *)
  fun procEntryExit{level:level, body:Tree.exp} =
    case level of Outermost 0 => ErrorMsg.error 0 ("no functions in Outermost level")
    | Level(lvl, f) =>
        let
          val body' = Frame.procEntryExit1(f, Tree.MOVE (Tree.TEMP Frame.RV, body))
        in
          addFrag := Frame.PROC {body=body', frame=f} :: (!addFrag)
        end

  fun callExp {funName, args} =
    Tree.CALL(Tree.NAME(funName), args)

  fun funDec {label: Temp.label, level : level, body: Tree.exp} =
    procEntryExit({level = level, body = Tree.ESEQ(Tree.LABEL(label), body)})

  (* Funcion que devuelve la lista de fragmentos *)
  fun getResult () = addFrag
end



