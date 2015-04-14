Parser to read tiger programs and produce an abstract syntax
tree. Based on the parser in Chapter 4, I added 3 new expression types
so I can parse a simple program:

plus.tig
```
3 + "Hello"
```

Running the parser:
```
- Parse.parse "plus.tig";
val it = OpExp {left=IntExp 3,oper=PlusOp,pos=2,right=StringExp ("Hello",6)}
  : Absyn.exp
-
```

I added the pretty printer to the project, so you can examine larger
expressions.

```
- Parse.parse "big.tig";
val it =
  OpExp
    {left=OpExp {left=IntExp #,oper=TimesOp,pos=2,right=IntExp #},oper=PlusOp,
     pos=2,right=OpExp {left=IntExp #,oper=TimesOp,pos=10,right=IntExp #}}
  : Absyn.exp
- PrintAbsyn.print (TextIO.stdOut, it);
[autoloading]
[autoloading done]
OpExp(PlusOp,
 OpExp(TimesOp,
  IntExp(1),
  IntExp(2)),
 OpExp(TimesOp,
  IntExp(3),
  IntExp(4)))
val it = () : unit
```

2015/03/17 - Today we extended the grammar to parse a variant of let
expressions.

It's broken because the let should have an expression secuence between
the in and the end, but I only allow a single expression.

Do now: fix it, then finish your homework.

```
- PrintAbsyn.print (TextIO.stdOut, Parse.parse "let.tig");
[autoloading]
[autoloading done]
LetExp([
 VarDec(a,true,NONE,
  IntExp(5)),
 VarDec(b,true,NONE,
  StringExp("Hello"))],
 OpExp(PlusOp,
  VarExp(
   SimpleVar(a)),
  VarExp(
   SimpleVar(b))))
val it = () : unit
```
2015/03/23 - HOZ

A bunch of changes, including renaming the directory to just "tiger".
Added a working semantic analysis phase, see env.sml, types.sml, semant.sml.
Most of the code is given in Chapter 6, but we only get the signature for
ENV, and the details of the program analysis are not quite down to working
code.

To run a semantic analysis:

```
- CM.make "sources.cm";
[scanning sources.cm]
...
val it = true : bool
- Semant.transProg (Parse.parse "sum.tig");
val it = () : unit
- Semant.transProg (Parse.parse "plus.tig");
plus.tig:1.3:integer required
val it = () : unit
- Semant.transProg (Parse.parse "let.tig");
let.tig0.0:Can't typecheck this yet
val it = () : unit
```

Guess what you have to do next ;-).
