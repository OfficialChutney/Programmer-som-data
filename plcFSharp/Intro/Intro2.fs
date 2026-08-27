(* Programming language concepts for software developers, 2010-08-28 *)

(* Evaluating simple expressions with variables *)

module Intro2

(* Association lists map object language variables to their values *)

let env: (string * int) list = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)];;

let emptyenv = []; (* the empty environment *)

let rec lookup env x =
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

let cvalue = lookup env "c";;


(* Object language expressions with variables *)

type expr = 
    | CstI of int
    | Var of string
    | Prim of string * expr * expr
    | If of expr * expr * expr;;

let e1 = CstI 17;;

let e2 = Prim("+", CstI 3, Var "a");;

let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a");;

let e4 = Prim("max", Prim("*", Var "baf", CstI 9), Var "a");;

let e5 = Prim("min", Prim("-", Var "baf", CstI 27), Var "a");;

let e6 = Prim("==", Prim("+", Var "baf", CstI 24), CstI 77);;

(* Evaluation within an environment *)

let rec eval (e : expr) (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x 
    | Prim("+", e1, e2) -> eval e1 env + eval e2 env
    | Prim("*", e1, e2) -> eval e1 env * eval e2 env
    | Prim("-", e1, e2) -> 
        let i1 = eval e1 env
        let i2 = eval e2 env
        i1 - i2
    | Prim("max", e1, e2) -> 
        let i1 = eval e1 env
        let i2 = eval e2 env
        if i1 > i2 then i1 else i2
    | Prim("min", e1, e2) -> 
        let i1 = eval e1 env
        let i2 = eval e2 env
        if i1 < i2 then i1 else i2
    | Prim("==", e1, e2) -> 
        let i1 = eval e1 env
        let i2 = eval e2 env
        if i1 = i2 then 1 else 0
    | If (e1, e2, e3) -> if (eval e1 env) = 0 then eval e3 env else eval e2 env
    | Prim _            -> failwith "unknown primitive";;

let e1v  = eval e1 env;;
let e2v1 = eval e2 env;;
let e2v2 = eval e2 [("a", 314)];;
let e3v  = eval e3 env;;
let e4v  = eval e4 env;;
let e5v  = eval e5 env;;
let e6v  = eval e6 env;;


type aexpr = 
    | CstI of int
    | Var of string
    | Add of aexpr * aexpr
    | Mul of aexpr * aexpr
    | Sub of aexpr * aexpr

let rec aeval (e : aexpr) (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x 
    | Add(e1, e2) -> aeval e1 env + aeval e2 env
    | Mul(e1, e2) -> aeval e1 env * aeval e2 env
    | Sub(e1, e2) -> 
        let i1 = aeval e1 env
        let i2 = aeval e2 env
        i1 - i2

let rec fmt (e : aexpr) : string =
    match e with
    | CstI i            -> string i
    | Var x             -> x
    | Add(e1, e2) -> "(" + (fmt e1) + " + " + (fmt e2) + ")"
    | Mul(e1, e2) -> "(" + (fmt e1) + " * " + (fmt e2) + ")"
    | Sub(e1, e2) -> "(" + (fmt e1) + " - " + (fmt e2) + ")"


let aex1v = aeval (Sub (Var "v", Add (Var "w", Var "z"))) env
let aex2v = aeval (Mul (CstI 2, (Sub (Var "v", Add (Var "w", Var "z"))))) env

