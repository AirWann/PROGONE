
open Lib
open Tast

(* ici on applique les transformations suivantes

  1. (FAIT) affectations multiples

       lv1,...,lvn = e1,...,en
    => tmp1 := e1; ... tmpn := en; lv1 = tmp1; ... lvn = tmpn

  2. (FAIT) retours multiples

       func f(...) (tau1,...,taun)        { ... }
    => func f(..., r1 *tau1,...,rn *taun) { ... }

       return e1,...,en
    => *r1 = e1, ..., *rn =en; return

       lv1,...,lv = f(...)
    => f(..., &lv1,...,&lvn)

       g(f(...))
    => var v1,...,vn; f(..., &v1,...,&vn); g(v1,...,vn)

       return f(...)
    => f(..., r1,...,rn); return  // OPTIM tail call

  3. (FAIT) pas de structures sur la pile

       var s  S;          ...  s ... &s ...
    => var s *S = new(S); ... *s ...  s ...

       func f(s  S) { ...  s ... &s ... }
    => func f(s *S) { ... *s ...  s ... }

  4. (FAIT) allocation sur le tas des variables dont l'adresse est prise

       func f() { var x;        ...  x ... &x ... }
    => func f() { x := new(ty); ... *x ...  x ... }

       func f(x ty) {                         ...  x  ... &x ... }
    => func f(x ty) { x' := new(ty); *x' = x; ... *x' ... x' ... }

  5. passage par valeur des structures

       lv = e // de type S
    => memcpy(&e, &lv, sizeof(S))

       return e // de type S
    => memcpy(&e, r, sizeof(S)) // r = argument recevant le résultat

       f(..., e, ...) // e de type S
    => f(..., copy(&e, sizeof(S)), ...)

*)

let debug = ref false

let mkvar =
  let c = ref 0 in
  fun ty ->
    incr c;
    Typing.new_var ("aux" ^ string_of_int !c) Typing.dummy_loc ~used:true ty

let tvoid = Typing.tvoid
let make = Typing.make
let stmt = Typing.stmt
let ident v = make (TEident v) v.v_typ
let is_struct = function Tstruct _ -> true | _ -> false
let star v =
  make (TEunop (Ustar, ident v))
    (match v.v_typ with Tptr ty -> ty | _ -> assert false)
let addr e = make (TEunop (Uamp, e)) (Tptr e.expr_typ)
let many_results f = List.length f.fn_typ > 1

module Vmap = Map.Make(struct
                  type t = var let compare v1 v2 = v1.v_id - v2.v_id end)

type rw = {
  subst: expr Vmap.t;
  retvl: var list;
}

let rw_empty = {
  subst = Vmap.empty;  (* x -> *x *)
  retvl = [];
}
let rw_add v e rw =
  { rw with subst = Vmap.add v e rw.subst }


let rec expr rw e =
  let _ty = e.expr_typ in
  let mk d = { e with expr_desc = d } in
  match e.expr_desc with
  | TEskip
  | TEnil
  | TEconstant _ ->
     e
  | TEbinop (op, e1, e2) ->
     mk (TEbinop (op, expr rw e1, expr rw e2))
  | TEunop (op, e1) ->
     mk (TEunop (op, expr rw e1))
  | TEnew typ ->
     e
  | TEcall (g, [{expr_desc=TEcall(f, el)}]) when many_results f ->
     (* g(f(...)) => var v1,...,vn; f(..., &v1,...,&vn); g(v1,...,vn) *)
     let vl, e = many rw f el in
     let gargs = List.map ident vl in
     stmt (TEblock [stmt (TEvars vl); e; mk (TEcall (g, gargs))])
  | TEcall (f, el) when many_results f ->
     let vl, e = many rw f el in
     mk (TEblock [stmt (TEvars vl); e])
  | TEcall (f, el) ->
     mk (TEcall (f, exprs rw el))
  | TEident v when Vmap.mem v rw.subst ->
     Vmap.find v rw.subst
  | TEident v ->
     mk (TEident v)
  | TEdot (e1, f) ->
     mk (TEdot (expr rw e1, f))
  | TEassign ([], _) | TEassign (_, []) ->
     assert false
  | TEassign ([lvl], [e]) ->
     mk (TEassign ([expr rw lvl], [expr rw e]))
  | TEassign (lvl, [{expr_desc=TEcall (g, [{expr_desc=TEcall(f, el)}])}])
       when many_results f ->
     (* RW2 lv1,...lvn = g(f(...)) =>
            var v1,...,vk; f(...,&v1,...,&vk); g(v1,...,vk, &lv1,...&lvn) *)
     assert (many_results g);
     let vl, e = many rw f el in
     let gargs = List.map ident vl @ List.map addr lvl in
     stmt (TEblock [stmt (TEvars vl); e; stmt (TEcall (g, gargs))])
  | TEassign (lvl, [{expr_desc=TEcall (f, el)}]) ->
     (* RW2 lv1,...lvn = f(...) => f(..., &lv1,...,&lvn) *)
     assert (many_results f);
     mk (TEcall(f, exprs rw el @ List.map addr lvl))
  | TEassign (lvl, [_]) ->
     assert false
  | TEassign (lvl, el) ->
     assert (List.length lvl = List.length el);
     (* RW 1 *)
     let temp code e =
       let v = mkvar e.expr_typ in
       make (TEassign ([ident v], [expr rw e])) tvoid ::
       make (TEvars [v]) tvoid ::
       code, v in
     let code, tmpl = map_fold_left temp [] el in
     let assign code lv tmp =
       make (TEassign ([lv], [ident tmp])) tvoid :: code in
     let code = List.fold_left2 assign code lvl tmpl in
     mk (TEblock (List.rev code))
  | TEif (e1, e2, e3) ->
     mk (TEif (expr rw e1, expr rw e2, expr rw e3))
  | TEreturn ([{expr_desc=TEcall(f, el)}]) when many_results f ->
     (* RW 2 return f(...) => f(..., r1,...,rn); return *)
     stmt (TEblock [stmt (TEcall (f, exprs rw el @ List.map ident rw.retvl));
                    stmt (TEreturn [])])
  | TEreturn ([] | [_] as el) ->
     mk (TEreturn (exprs rw el))
  | TEreturn el ->
     (* RW 2 return e1,...,en => *r1 = e1, ..., *rn =en; return *)
     let vl = rw.retvl in
     assert (List.length el = List.length vl);
     let assign code v e =
       stmt (TEassign ([star v], [expr rw e])) :: code in
     let bl = List.fold_left2 assign [stmt (TEreturn [])] vl el in
     stmt (TEblock bl)
  | TEblock bl ->
     mk (TEblock (block rw bl))
  | TEfor (e1, e2) ->
     mk (TEfor (expr rw e1, expr rw e2))
  | TEprint el ->
     mk (TEprint (exprs rw el))
  | TEincdec (e1, op) ->
     mk (TEincdec (expr rw e1, op))
  | TEvars _ ->
     assert false

and many rw f el =
  assert (many_results f);
  (* f(...) => var v1,...,vn; f(..., &v1,...,&vn) *)
  let vl = List.map mkvar f.fn_typ in
  let fargs = List.map (fun v -> addr (ident v)) vl in
  match el with
  | [{expr_desc=TEcall(g, el')}] when many_results g ->
     (* f(g(...)) =>
        var v1,...,vn,w1,...,wk; g(...&w1,...,&wk); f(w1,..,wk, &v1,...,&vn) *)
     let vl', e' = many rw f el' in
     vl,
     stmt (TEblock [stmt (TEvars vl'); e';
                    stmt (TEcall (f, List.map ident vl' @ fargs))])
  | _ ->
     let fargs = exprs rw el @ fargs in
     vl, stmt (TEcall (f, fargs))

and block rw = function
  | [] ->
     []
  | { expr_desc = TEvars (v :: _ as vl) } :: bl
      when is_struct v.v_typ || v.v_addr ->
     (* RW 3 *)
     let change rw ({v_typ = ty} as v) =
       assert (is_struct ty || v.v_addr);
       let v' = mkvar (Tptr ty) in
       let e = stmt (TEassign ([ident v'], [make (TEnew ty) (Tptr ty)])) in
       rw_add v (make (TEunop (Ustar, ident v')) ty) rw, (v', e) in
     let rw, l = map_fold_left change rw vl in
     stmt (TEvars (List.map fst l)) :: List.map snd l @ block rw bl
  | { expr_desc = TEvars _ } as e :: bl ->
     e :: block rw bl
  | e :: bl ->
     expr rw e :: block rw bl

and exprs rw el =
  List.map (expr rw) el

let function_ f e =
  let param (rw, init as acc) ({v_typ = ty} as v) =
    if is_struct ty then
      let v' = mkvar (Tptr ty) in
      let rw = rw_add v (make (TEunop (Ustar, ident v')) ty) rw in
      (rw, init), v'
    else if v.v_addr then
      let v' = mkvar (Tptr ty) in
      let rw = rw_add v (make (TEunop (Ustar, ident v')) ty) rw in
      let init =
        stmt (TEvars [v']) ::
        stmt (TEassign ([ident v'], [make (TEnew ty) (Tptr ty)])) ::
        stmt (TEassign ([star v'], [ident v])) :: init in
      (rw, init), v
    else
      acc, v
  in
  let (rw, init), pl = map_fold_left param (rw_empty, []) f.fn_params in
  (* RW 2 *)
  let rw, pl, tyl = match f.fn_typ with
    | [] | [_] as tyl -> rw, pl, tyl
    | tyl ->
       let result ty = mkvar (Tptr ty) in
       let vl = List.map result tyl in
       { rw with retvl = vl }, pl @ vl, [] in
  let f = { f with fn_params = pl } in
  TDfunction (f, stmt (TEblock (init @ [expr rw e])))

let decl = function
  | TDfunction (f, e) -> function_ f e
  | TDstruct _ as d -> d

let file ?debug:(b=false) dl =
  debug := b;
  List.map decl dl
