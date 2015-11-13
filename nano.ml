exception MLFailure of string

type binop = 
      Plus 
    | Minus 
    | Mul 
    | Div 
    | Eq 
    | Ne 
    | Lt 
    | Le 
    | And 
    | Or          
    | Cons

type expr =   
      Const of int 
    | True   
    | False      
    | NilExpr
    | Var of string    
    | Bin of expr * binop * expr 
    | If  of expr * expr * expr
    | Let of string * expr * expr 
    | App of expr * expr 
    | Fun of string * expr    
    | Letrec of string * expr * expr

type value =  
      Int of int		
    | Bool of bool          
    | Closure of env * string option * string * expr 
    | Nil                    
    | Pair of value * value     

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
      Int i -> 
        Printf.sprintf "%d" i
    | Bool b -> 
        Printf.sprintf "%b" b
    | Closure (evn,fo,x,e) -> 
        let fs = match fo with None -> "Anon" | Some fs -> fs in
          Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
    | Pair (v1,v2) -> 
        Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
    | Nil -> 
        "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
    "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
          (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
          (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
          x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
          x (exprToString e1) (exprToString e2) 

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
                | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

let lookup (x,evn) = 
  match listAssoc (x, evn) with
      None -> raise(MLFailure "not found")
    | Some(x) -> Some(x)
;;

let _ = let a = "z" in 
  let evn = [("z1",Int 0);("x",Int 1);("y",Int 2);("z",Int 3);("z1",Int 4)] in
    lookup(a,evn)

let rec eval (evn,e) = match e with
  | Bin(e1, op, e2) -> 
      (match (eval(evn, e1), eval(evn, e2)) with
        | (Int e3, Int e4) ->
            (match op with
              | Plus   -> Int(e3 + e4)
              | Minus  -> Int(e3 - e4)
              | Mul    -> Int(e3 * e4)
              | Div    -> Int(e3 / e4)
              | Eq     -> Bool(e3 == e4)
              | Ne     -> Bool(e3 != e4)
              | Lt     -> Bool(e3 <  e4)
              | Le     -> Bool(e3 <= e4)
            )
        | (Bool e5, Bool e6) ->
            (match op with 
              | Eq     -> Bool(e5 == e6)
              | Ne     -> Bool(e5 != e6)
              | And    -> Bool(e5 && e6)
              | Or     -> Bool(e5 || e6)
            )
      )
  | Var x       -> (match lookup (x, evn) with
                     | None -> raise(MLFailure "Not Found!")
                     | Some x -> x)
  | Const y     -> Int y
  | If(p, t, f) -> (match eval(evn, p) with
                     | Bool true  -> eval(evn, t)
                     | Bool false -> eval(evn, f)
                     | _          -> raise(MLFailure "Invalid If")
                   )
  | Let(x, e1, e2)        -> let pair = (x, eval(evn, e1)) in eval(pair::evn, e2)
  | Letrec (b,e1,e2) -> (
      let v = eval (evn, e1) in
      let evn1 = (
        match v with
          | Closure (evn',None,x,e) -> Closure (evn',Some b,x,e)
          | _ -> v
      ) in
      let evn2 = (b,evn1)::evn in eval (evn2,e2)
    )
  | App (e1,e2) -> (
      let Closure (evn1,f,x,e) = eval (evn,e1) in
      let v = eval (evn, e2) in
      let evn' = (
        match f with
          | Some n -> (n, Closure (evn1,f,x,e))::(x,v)::evn1
          | None -> (x,v)::evn1
      ) in eval (evn', e)
    )
  | Fun(e1,e2) -> Closure(evn, None, e1, e2)

(*
| If (e1,e2,e3) -> 
if eval(evn, e1) then eval(evn, e2) else eval(evn, e3)
| Let (x,e1,e2) -> 
let eval(evn, x) = eval(evn, e1) in eval(evn, e2)
| App (e1,e2) -> 
eval (evn,e1) eval(evn,e2)
| Fun (x,e) -> 
lookup(x,evn) eval(evn, e) 
| Letrec (x,e1,e2) -> 
let rec lookup(x,evn) = eval(evn, e1) in eval(evn,e2) *)
(*
| Eq -> eval(evn, e1) = eval(evn, e2)
| Ne -> eval(evn, e1) != eval(evn, e2)
| Lt -> eval(evn, e1) < eval(evn, e2)
| Le -> eval(evn, e1) <= eval(evn, e2)
| And -> eval(evn, e1) && eval(evn, e2)
| Or -> eval(evn, e1) || eval(evn, e2)
| Cons -> eval(evn, e1) :: eval(evn, e2)
*)

(**********************     Testing Code  ******************************)

let evn = [("z1",Int 0);("x",Int 1);("y",Int 2);("z",Int 3);("z1",Int 4)]

let e1  = Bin(Bin(Var "x",Plus,Var "y"), Minus, Bin(Var "z",Plus,Var "z1"))

let _   = eval (evn, e1)        (* EXPECTED: Nano.value = Int 0 *)

(*let _   = eval (evn, Var "p") *)  (* EXPECTED:  Exception: Nano.MLFailure "variable not bound: p". *)


let evn = [("z1",Int 0);("x",Int 1);("y",Int 2);("z",Int 3);("z1",Int 4)]

let e1  = If(Bin(Var "z1",Lt,Var "x"),Bin(Var "y",Ne,Var "z"),False)

let _   = eval (evn,e1)         (* EXPECTED: Nano.value = Bool true *)

let e2  = If(Bin(Var "z1",Eq,Var "x"), 
             Bin(Var "y",Le,Var "z"),
             Bin(Var "z",Le,Var "y")
            )

let _   = eval (evn,e2)         (* EXPECTED: Nano.value = Bool false *)



let e1 = Bin(Var "x",Plus,Var "y")

let e2 = Let("x",Const 1, Let("y", Const 2, e1)) 

let _  = eval ([], e2)          (* EXPECTED: Nano.value = Int 3 *)

let e3 = Let("x", Const 1, 
             Let("y", Const 2, 
                 Let("z", e1, 
                     Let("x", Bin(Var "x",Plus,Var "z"), 
                         e1)
                    )
                )
            )

let _  = eval ([],e3)           (* EXPCETED: Nano.value = Int 6 *)





let _ = eval ([], Fun ("x",Bin(Var "x",Plus,Var "x"))) 

(* EXPECTED: Nano.value = Closure ([], None, "x", Bin (Var "x", Plus, Var "x")) *)

let _ = eval ([],App(Fun ("x",Bin(Var "x",Plus,Var "x")),Const 3));;

(* EXPECTED: Nano.value = Int 6 *)

let e3 = Let ("h", Fun("y", Bin(Var "x", Plus, Var "y")), 
              App(Var "f",Var "h"))

let e2 = Let("x", Const 100, e3)

let e1 = Let("f",Fun("g",Let("x",Const 0,App(Var "g",Const 2))),e2) 

let _  = eval ([], e1)        
(* EXPECTED: Nano.value = Int 102 *)

let _ = eval ([],Letrec("f",Fun("x",Const 0),Var "f"))
(* EXPECTED: Nano.value = Closure ([], Some "f", "x", Const 0) *)


let _ = eval ([], 
              Letrec("fac", 
                     Fun("n", If (Bin (Var "n", Eq, Const 0), 
                                  Const 1, 
                                  Bin(Var "n", Mul, App(Var "fac",Bin(Var "n",Minus,Const 1))))),
                     App(Var "fac", Const 10)))

(* EXPECTED: Nano.value = Int 3628800 *)



(* Uncomment to test part (f)

   let _ = eval ([],Bin(Const 1,Cons,Bin(Const 2,Cons,NilExpr)))

   (* EXPECTED: Nano.value = Pair (Int 1, Pair (Int 2, Nil)) *)

   let _ = eval ([],App(Var "hd",Bin(Const 1,Cons,Bin(Const 2,Cons,NilExpr))))

   (* EXPECTED: Nano.value = Int 1 *)

   let _ = eval ([],App(Var "tl",Bin(Const 1,Cons,Bin(Const 2,Cons,NilExpr))))

   (* EXPECTED: Nano.value = Pair (Int 2, Nil) *)

*)
