structure Semant :
     sig val transProg : Absyn.exp -> unit end =
struct
  structure A = Absyn

  (* dummy translate for chapter 5 *)

  type expty = {exp: Translate.exp, ty: Types.ty}
  type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table

  fun checkInt ({ty=Types.INT, exp=_}, pos) = ()
  | checkInt ({ty=_,exp=_},pos) = ErrorMsg.error pos "integer required"

  fun transProg (exp:A.exp) : unit =
    let
      val firstLvl = Translate.newLevel {parent=Translate.outermost, name=Temp.namedlabel "main", formals=[]}
      val {ty=_, exp=prog} = transExp (Env.base_venv, Env.base_tenv, firstLvl) exp
    in
      prog
    end
  and transExp(venv:venv,tenv:tenv, level) : A.exp -> expty =
    let
      fun trexp (A.VarExp var) = transvar(var)
        | trexp (A.NilExp) = {exp = (), ty = Types.NIL}
        | trexp (A.IntExp _) = {ty=Types.INT, exp=()}
        | trexp (A.StringExp (_,_)) = {ty=Types.STRING, exp=()}
        | trexp (A.OpExp{left, oper, right, pos}) =
              (checkInt (trexp left, pos);
              checkInt (trexp right, pos);
              {ty=Types.INT, exp=()})
        (* Aqui hay magia so la voy a explicar.
           Primero recibimos la lista de pares *)
        | trexp (A.SeqExp explist) =
          (* Chequeate a ver que no este vacía *)
          if not(List.null(explist))
          then
            let
              (* Rompe la lista de pares en dos listas distintas. (Powerful, I know.) *)
              val (expList, posList) = ListPair.unzip explist
              (* Haz una lista nueva corriendo trexp en cada elemento de expList.
                 (Think list comprehensions en *sob* Python *sob* ) *)
              val newExpList = (map (fn (exp) => trexp exp) expList)
            in
              (* SeqExp devuelve el resultado del ultimo exp, asi que aqui devolvemos ese tipo. *)
              {exp = (), ty = (#ty (List.last(newExpList)))}
            end
          else
            {exp = (), ty = Types.UNIT}
        | trexp (A.AssignExp {var, exp, pos}) =
            let
              val varType = transvar(var)
              val expType = trexp(exp)
            in
              if (#ty expType) = (#ty varType)
              then
                {exp = (), ty = (#ty expType)}
              else
                {exp = (ErrorMsg.error pos ("Types no matchean loko.")), ty = Types.UNIT}
            end
        | trexp (A.LetExp {decs, body, pos}) =
           let
             fun delosenv (venv, tenv, nil) = {tenv=tenv, venv=venv}
               | delosenv (venv, tenv, dec::decs) =
                   let
                      val {tenv=newtenv,venv=newvenv} = transDec(level, venv, tenv) dec
                   in
                     delosenv (newvenv, newtenv, decs)
                   end
           in
             let
                val {tenv=newtenv, venv=newvenv} = delosenv(venv,tenv,decs)
             in
               transExp (newvenv, newtenv, level) body
             end
           end
        | trexp _ = {ty=Types.UNIT, exp=ErrorMsg.error 0 "Can'typecheck this yet"}
      and transvar (A.SimpleVar (symbol, pos)) =
            (case Symbol.look(venv, symbol) of
              NONE =>
                {exp = (ErrorMsg.error pos ("loko, var sin definir: " ^ Symbol.name(symbol))), ty = Types.UNIT}

            (*Need to add actual access*)
            | SOME(Env.VarEntry{access = _, ty}) =>
                {exp = (), ty = ty}

            (*Need to add actual parameters.*)
            | SOME(Env.FunEntry _) => {exp = (ErrorMsg.error pos "loko esto es una function."), ty = Types.UNIT})
        | transvar (A.FieldVar (var, symbol, pos)) =
            {exp = (ErrorMsg.error pos "loko no estamos haciendo vars complicados."), ty = Types.UNIT}
        | transvar (A.SubscriptVar (var, exp, pos)) =
            {exp = (ErrorMsg.error pos "loko no estamos haciendo vars complicados."), ty = Types.UNIT}
    in
      trexp
    end
  and transDec (level, venv:venv, tenv:tenv) =
    let
      fun trdec (A.VarDec{name, escape, typ, init, pos}) =
        (case typ of
          NONE =>
            let
              val {exp, ty} = transExp(venv, tenv, level) init
              val access = Translate.allocLocal level (!escape)
            in
              {tenv = tenv, venv = Symbol.enter(venv, name, Env.VarEntry{access=access, ty=ty})}
            end
        | SOME(symbolo, posi) =>
            let
              val {exp, ty} = transExp(venv, tenv, level) init
              val access = Translate.allocLocal level (!escape)
            in
              case Symbol.look(tenv, symbolo) of
                NONE =>
                  (ErrorMsg.error posi "tipo no encontrado loko"; {tenv=tenv, venv=venv})
              | SOME(typ2) =>
                  if typ2 <> ty
                  then
                    (ErrorMsg.error posi "type mismatch loko."; {tenv=tenv, venv=venv})
                  else
                    {tenv = tenv, venv = Symbol.enter(venv, name, Env.VarEntry{access=access, ty=ty})}
            end)
      | trdec (A.FunctionDec(fundecs)) =
          (ErrorMsg.error 0 "No estamos haciendo fundec ahora, loko."; {tenv=tenv, venv=venv})
      | trdec (A.TypeDec(typedecs)) =
          (ErrorMsg.error 0 "No estamos haciendo typedec ahora, loko."; {tenv=tenv, venv=venv})
    in
      trdec
    end
end
