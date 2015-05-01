structure Semant :
     sig val transProg : Absyn.exp -> Translate.frag list end =
struct
  structure A = Absyn

  type expty = {exp: Translate.exp, ty: Types.ty}
  type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table

  fun checkInt ({ty=Types.INT, exp=_}, pos) = ()
  | checkInt ({ty=_,exp=_},pos) = ErrorMsg.error pos "integer required"

  fun transProg (exp:A.exp) =
    let
      val firstLvl = Translate.newLevel {parent=Translate.outermost, name=Temp.namedlabel "main", formals=[]}
      val {ty=_, exp=prog} = transExp (Env.base_venv, Env.base_tenv, firstLvl) exp
    in
      Translate.procEntryExit {level = firstLvl, body = prog};
      Translate.getResult()
    end
  and transExp(venv:venv,tenv:tenv, level) : A.exp -> expty =
    let
      fun trexp (A.VarExp var) = transvar(var)
        | trexp (A.NilExp) = {exp = Translate.nilExp(), ty = Types.NIL}
        | trexp (A.IntExp n) = {exp = Translate.intExp(n), ty=Types.INT}
        | trexp (A.StringExp (s, pos)) = {exp = Translate.strExp(s), ty=Types.STRING}
        | trexp (A.OpExp{left, oper, right, pos}) =
          let
            val leftTr = trexp left
            val rightTr = trexp right
          in
            (checkInt (leftTr, pos);
            checkInt (rightTr, pos);
            {exp = Translate.opExp(oper, (#exp leftTr), (#exp rightTr)), ty = Types.INT})
          end
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
              {exp = Translate.seqExp(newExpList), ty = (#ty (List.last(newExpList)))}
            end
          else
            {exp = (), ty = Types.NIL}
        | trexp (A.AssignExp {var, exp, pos}) =
            let
              val varType = transvar(var)
              val expType = trexp(exp)
            in
              if (#ty expType) = (#ty varType)
              then
                {exp = Translate.assignExp((#exp varType), (#exp expType)), ty = (#ty expType)}
              else
                {exp = (ErrorMsg.error pos ("Types no matchean loko.")), ty = Types.UNIT}
            end
        | trexp (A.LetExp {decs, body, pos}) =
           let
             fun delosenv (venv, tenv, nil) = {tenv=tenv, venv=venv}
               | delosenv (venv, tenv, dec::decs) =
                   let
                      val {tenv=newtenv,venv=newvenv} = transDec(venv, tenv, level) dec
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
        | trexp(A.CallExp {func, args, pos}) =
          (case Symbol.look(venv, func) of
                  SOME (Env.FunEntry {formals, label, level, result}) =>
                      if length(args) <> length(formals) then
                        {exp= ErrorMsg.error  pos ("Number of arguments incorrect: "^Int.toString(length(args))), ty=Types.UNIT}
                      else
                        let
                          fun easyTransExp(e) = transExp(venv, tenv, level) e
                          fun checkType({exp=_, ty=ty1}, ty2) =
                            if ty1 = ty2 then
                              ()
                            else
                              (ErrorMsg.error pos ("Argument types no matchean loko."))

                          val argTypes = map easyTransExp args
                          val argforms = ListPair.zip(argTypes, formals)
                        in
                          app checkType argforms;
                          {exp = Translate.callExp(func, map (#exp) argTypes), ty=result}
                        end
                  | _ => ({exp= ErrorMsg.error  pos ("Function non-existant: " ^ Symbol.name(func)), ty=Types.UNIT}))

        | trexp _ = {ty=Types.UNIT, exp=ErrorMsg.error 0 "Can'typecheck this yet"}

      and transvar (A.SimpleVar (symbol, pos)) =
            (case Symbol.look(venv, symbol) of
              NONE =>
                {exp = (ErrorMsg.error pos ("loko, var sin definir: " ^ Symbol.name(symbol))), ty = Types.UNIT}

            | SOME(Env.VarEntry{access, ty}) =>
                {exp = Translate.simpleVar(access), ty = ty}

            | SOME(Env.FunEntry _) => {exp = (ErrorMsg.error pos "loko esto es una function."), ty = Types.UNIT})
        | transvar (A.FieldVar (var, symbol, pos)) =
            {exp = (ErrorMsg.error pos "loko no estamos haciendo vars complicados."), ty = Types.UNIT}
        | transvar (A.SubscriptVar (var, exp, pos)) =
            {exp = (ErrorMsg.error pos "loko no estamos haciendo vars complicados."), ty = Types.UNIT}
    in
      trexp
    end
  and transDec (venv:venv, tenv:tenv, level) =
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
      | trdec (A.FunctionDec[{name, params, result=SOME(rt, rtpos), body, pos}]) =
        let
          val SOME(result_ty) = Symbol.look(tenv, rt)
          fun transparam {name: Symbol.symbol, escape: bool ref, typ: Symbol.symbol, pos: A.pos} =
            case Symbol.look(tenv, typ) of
              SOME t => {name=name, ty=t}
              | NONE => (ErrorMsg.error pos "Could not find arg type, loko."; {name=name, ty=Types.UNIT})
          val params' = (map transparam params)
          val funLevel = Translate.newLevel {parent=level, name=Temp.newlabel(), formals=[true]}
          val venv' = Symbol.enter(venv, name, Env.FunEntry{formals = map #ty params', result = result_ty, level = funLevel, label = Temp.newlabel()})
          fun enterparam ({name, ty}, venv) =
            let
              val varAccess = Translate.allocLocal funLevel (true)
            in
              Symbol.enter(venv, name, Env.VarEntry{access=varAccess, ty=ty})
            end
          val venv'' = foldl enterparam venv' params'
        in
          transExp(venv'', tenv, funLevel) body;
          {venv=venv', tenv=tenv}
        end
      | trdec (A.FunctionDec[]) =
          (ErrorMsg.error 0 "Empty fundec list, loko."; {tenv=tenv, venv=venv})
            | trdec (A.FunctionDec (first :: rest)) =
          (ErrorMsg.error 0 "Too many fundecs, loko."; {tenv=tenv, venv=venv})
      | trdec (A.TypeDec(typedecs)) =
          (ErrorMsg.error 0 "No estamos haciendo typedec ahora, loko."; {tenv=tenv, venv=venv})
    in
      trdec
    end
end
