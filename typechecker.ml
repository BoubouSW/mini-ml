open Mml

(* Environnement de typage : associe des types aux noms de variables *)
module SymTbl = Map.Make(String)
type tenv = typ SymTbl.t

(* Pour remonter des erreurs circonstanciées *)
exception Type_error of string
let error s = raise (Type_error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s but got %s" 
           (typ_to_string ty_expected) (typ_to_string ty_actual))
(* vous pouvez ajouter d'autres types d'erreurs *)

(* Vérification des types d'un programme *)
let type_prog prog =

  (* Vérifie que l'expression [e] a le type [type] *)
  let rec check e typ tenv =
    let typ_e = type_expr e tenv in
    if typ_e <> typ then type_error typ_e typ

  (* Calcule le type de l'expression [e] *)
  and type_expr e tenv = match e with
    | Int _  -> TInt
    | Bool _ -> TBool
    | Unit -> TUnit
    | Var i -> SymTbl.find i tenv
    | Bop((Add | Mul | Sub | Div | Mod), e1, e2) -> 
       check e1 TInt tenv; check e2 TInt tenv; TInt
    | Uop(Neg,e) -> check e TInt tenv; TInt
    | Uop(Not,e) -> check e TBool tenv; TBool
    | Bop((And | Or), e1, e2) -> check e1 TBool tenv; check e2 TBool tenv; TBool
    | Bop((Lt | Le | NLt | NLe), e1, e2) -> check e1 TInt tenv; check e2 TInt tenv; TBool
    | Bop((Eq | Neq), e1, e2) -> TBool  (*à préciser *)
    | If(e1,e2,e3) -> check e1 TBool tenv; let t1 = type_expr e2 tenv in
                                           let t2 = type_expr e3 tenv in
                                           if t1 == t2 then t1 else error (Printf.sprintf "Not same type in condition : %s != %s" (typ_to_string t1) (typ_to_string t2))
    | Let(x,e1,e2) -> let t1 = type_expr e1 tenv in type_expr e2 (SymTbl.add x t1 tenv)
    | Fun(x,tx,e) -> let te = type_expr e (SymTbl.add x tx tenv) in TFun(tx, te)
    | Fix(x,tx,e) -> let te = type_expr e (SymTbl.add x tx tenv) in te
    | App(f, a) -> let tf = type_expr f tenv in
                  let ta = type_expr a tenv in
                  begin match tf with
                  | TFun(tx, te) -> if tx = ta then te else failwith "type error (fun)"
                  | _ -> failwith "type error (fun)"
                  end
    | Strct(s) -> type_strct s tenv
    (*| GetF(e,x) -> check e (TStrct(_)) tenv; let te = type_expr e tenv in (match te with 
                                            | TStrct(s) -> let stc = SymTbl.find s tenv in stc.x
                                            | _ -> assert false
                                            ) *)
    | GetF(_,_) -> assert false
    | SetF(_,_,_) -> assert false
    | Seq (e1,e2) -> let t1 = type_expr e1 tenv in 
                     let t2 = type_expr e1 tenv in
                    if t1 <> TUnit then Printf.printf "Warning : sequence not type Unit\n"; t2
    and type_strct s env =
      assert false
  in

  type_expr prog.code SymTbl.empty
