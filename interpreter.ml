(* Interprète Mini-ML *)

open Mml

(* Environnement : associe des valeurs à des noms de variables *)
module Env = Map.Make(String)

(* Valeurs *)
type value =
  | VInt   of int
  | VBool  of bool
  | VUnit
  | VPtr   of int
(* Élements du tas *)
type heap_value =
  | VClos  of string * expr * value Env.t
  | VStrct of (string, value) Hashtbl.t

let print_value = function
  | VInt n  -> Printf.printf "%d\n" n
  | VBool b -> Printf.printf "%b\n" b
  | VUnit   -> Printf.printf "()\n"
  | VPtr p  -> Printf.printf "@%d\n" p

(* Interprétation d'un programme complet *)
let eval_prog (p: prog): value =
  
  (* Initialisation de la mémoire globale *)
  let (mem: (int, heap_value) Hashtbl.t) = Hashtbl.create 16 in

  (* Création de nouvelles adresses *)
  let new_ptr =
    let cpt = ref 0 in
    fun () -> incr cpt; !cpt
  in

  (* Interprétation d'une expression, en fonction d'un environnement
     et de la mémoire globale *)
  let rec eval (e: expr) (env: value Env.t): value = 
    match e with
    | Int n  -> VInt n
    | Bool b -> VBool b
    | Unit -> VUnit
    | Var x -> Env.find x env
    | Bop(Add, e1, e2) -> VInt (evali e1 env + evali e2 env)
    | Bop(Mul, e1, e2) -> VInt (evali e1 env * evali e2 env)
    | Bop(Sub, e1, e2) -> VInt (evali e1 env - evali e2 env)
    | Bop(Div, e1, e2) -> VInt (evali e1 env / evali e2 env)
    | Bop(Mod, e1, e2) -> VInt (evali e1 env mod evali e2 env)
    | Uop(Neg, e) -> VInt(- evali e env)
    | Bop(And, e1, e2) -> VBool (evalb e1 env && evalb e2 env)
    | Bop(Or, e1, e2) -> VBool (evalb e1 env || evalb e2 env)
    | Uop(Not, e) -> VBool(not (evalb e env))
    | Bop(Lt,e1, e2) -> VBool(evali e1 env < evali e2 env)
    | Bop(Le,e1, e2) -> VBool(evali e1 env <= evali e2 env)
    | Bop(Eq, e1, e2) -> (
    let v1 = eval e1 env in
    let v2 = eval e2 env in
    let b = match v1, v2 with
      | VInt u1, VInt u2 -> u1 = u2
      | VBool u1, VBool u2 -> u1 = u2
      | _ -> assert false
    in
    VBool b
    )
    | Bop(Neq, e1, e2) -> (match (eval (Bop(Eq,e1,e2)) env) with | VBool(true) -> VBool(false) | VBool(false) -> VBool(true) | _ -> assert false)
    | If(e1,e2,e3) -> if (evalb e1 env) then (eval e2 env) else (eval e3 env)
    | Let (x,e1,e2) -> let v1 = eval e1 env in eval e2 (Env.add x v1 env)
    | Fun (x,_,e) -> evalfun x e env
    | App(e1,e2) -> evalapp e1 e2 env
    | Fix(f,_,e) -> evalfix f e env
    | Strct(_) -> assert false
    | GetF(_,_) -> assert false
    | SetF(_,_,_) -> assert false
    | Seq (e1,e2) -> let _ = eval e1 env in eval e2 env

  (* Évaluation d'une expression dont la valeur est supposée entière *)
  and evali (e: expr) (env: value Env.t): int = 
    match eval e env with
    | VInt n -> n
    | _ -> assert false
  and evalb (e: expr) (env: value Env.t): bool = 
    match eval e env with
    | VBool b -> b
    | _ -> assert false
  and evalfun x e env = 
    let n = new_ptr() in
    Hashtbl.add mem n (VClos(x,e,env));
    VPtr(n)
  and evalapp e1 e2 env =
    let eve1 = eval e1 env in
    match eve1 with 
    | VPtr(n) -> let block = Hashtbl.find mem n in
                        (match block with
                        | VClos (x,b,env2) -> let va = eval e2 env in eval b (Env.add x va env2)
                        | _ -> assert false
                        )
    | _ -> assert false
  and evalfix f e env =
    
    let n = new_ptr() in
    Hashtbl.add mem n (VClos(f,e,(Env.add f (VPtr(n)) env)));
    VPtr(n)
    
  in

  eval p.code Env.empty
