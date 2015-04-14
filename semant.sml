structure Semant :
     sig val transProg : Absyn.exp -> unit end =
struct
  structure A = Absyn

  (* dummy translate for chapter 5 *)
  structure Translate = struct type exp = unit end

  type expty = {exp: Translate.exp, ty: Types.ty}
  type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table

  fun checkInt ({ty=Types.INT, exp=_}, pos) = ()
  | checkInt ({ty=_,exp=_},pos) = ErrorMsg.error pos "integer required"

  fun transProg (exp:A.exp) : unit =
    let
      val {ty=_, exp=prog} = transExp (Env.base_venv, Env.base_tenv) exp
    in
      prog
    end

  and transExp(venv:venv,tenv:tenv) : A.exp -> expty =
    let fun trexp (A.OpExp{left, oper, right, pos}) =
      (checkInt (trexp left, pos);
        checkInt (trexp right, pos);
        {ty=Types.INT, exp=()})

        | trexp (A.NilExp) = {ty=Types.NIL, exp=()}
        | trexp (A.IntExp _) = {ty=Types.INT, exp=()}
        | trexp (A.StringExp (_,_)) = {ty=Types.STRING, exp=()}
        | trexp (A.LetExp {decs, body, pos}) =
            let
              fun delosenv (venv, tenv, nil) = {tenv=tenv, venv=venv}
              | delosenv (venv, tenv, dec::decs) =
                let
                  val {tenv=newtenv,venv=newvenv} = transDec(venv,tenv,dec)
                in
                  delosenv (newvenv, newtenv, decs)
                end
            in
              let val {tenv=newtenv, venv=newvenv} = delosenv(venv,tenv,decs)
              in
                transExp (newvenv, newtenv) body
              end
            end
        | trexp (A.SeqExp exps) =
            let
              fun listExps([]) = {exp = (), ty = Types.UNIT}
              | listExps((e, p)::[]) = transExp(venv, tenv) e
              | listExps((e, p)::l) = 
                  (transExp(venv, tenv) e;
                  listExps(l))
            in
              listExps(exps)
            end

        | trexp _ = {ty=Types.UNIT, exp=()}
    in
      trexp
    end
  and transDec (venv, tenv, A.VarDec {name, escape, typ=NONE, init, pos}) =
        let 
          val {exp= _, ty} = transExp(venv, tenv) init
        in 
          {tenv=tenv, venv=Symbol.enter(venv, name, Env.VarEntry {ty=ty})}
        end

      |transDec (venv, tenv, A.VarDec{name,escape,typ=SOME(symbol, p), init, pos}) = 
        let
          val {exp= _, ty} = transExp (venv, tenv) init
        in
          case Symbol.look (tenv, symbol) of
              NONE => (ErrorMsg.error p ("type not defined: " ^ Symbol.name symbol))
              | SOME ty2=>  if ty<>ty2 then (ErrorMsg.error p "type mismatch") else ();
              {tenv=tenv,
              venv=Symbol.enter(venv, name, Env.VarEntry{ty=ty})  }
        end
end
