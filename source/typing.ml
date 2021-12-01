
open Format
open Lib
open Ast
open Tast

let debug = ref false

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

exception Error of Ast.location * string

let error loc e = raise (Error (loc, e))

(* TODO environnement pour les types structure *)
let tablestructs = Hashtbl.create 5
(* TODO environnement pour les fonctions *)
module Funcs = struct
  module M = Map.Make(String)
  type t = pfunc M.t
  let empty = M.empty
  let all_funcs = ref empty
  let find = fun x -> M.find x !all_funcs
  let is_def f = M.mem f.pf_name.id !all_funcs
  let add f = 
    if is_def f then 
      error f.pf_name.loc ("fonction "^f.pf_name.id^"déja définie")
  else
      all_funcs := M.add f.pf_name.id f !all_funcs
  let are_vars_unique f = 
    let table = Hashtbl.create 15 in
    let rec add_table = function
      |[] -> ()
      |p::t -> (* pour tout pparam p, on vérifie que son identifiant n'est pas déjà pris *)
        if Hashtbl.mem table (fst p).id then
          error (fst p).loc ("variable"^(fst p).id^"déjà définie dans"^f.pf_name.id)
        else (
          Hashtbl.add table (fst p).id ();
          add_table t
        )
    in add_table f.pf_params;
end


let rec type_type = function
  | PTident { id = "int" } -> Tint
  | PTident { id = "bool" } -> Tbool
  | PTident { id = "string" } -> Tstring
  | PTptr ty -> Tptr (type_type ty)
  | _ -> error dummy_loc ("unknown struct ") (* TODO type structure *)
let rec eqlist l1 l2 cmp = match l1,l2 with
  |[], [] -> true
  |[], _ -> false
  |_, [] -> false
  | x::q1 , y::q2 -> (cmp x y && eqlist q1 q2 cmp)  
let rec eq_type ty1 ty2 = match ty1, ty2 with
  | Tint, Tint | Tbool, Tbool | Tstring, Tstring -> true
  | Tstruct s1, Tstruct s2 -> s1 == s2
  | Tptr ty1, Tptr ty2 -> eq_type ty1 ty2
  | Tmany l1, Tmany l2 -> eqlist l1 l2 eq_type
  | _ -> false
    (* TODO autres types *)

let fmt_used = ref false
let fmt_imported = ref false

let evar v = { expr_desc = TEident v; expr_typ = v.v_typ }

let new_var =
  let id = ref 0 in
  fun x loc ?(used=false) ty ->
    incr id;
    { v_name = x; v_id = !id; v_loc = loc; v_typ = ty; v_used = used; v_addr = false }

module Env = struct
  module M = Map.Make(String)
  type t = var M.t
  let empty = M.empty
  let find = M.find
  let add env v = M.add v.v_name v env

  let all_vars = ref []
  let check_unused () =
    let check v =
      if v.v_name <> "_" && (* TODO used *) true then error v.v_loc "unused variable" in
    List.iter check !all_vars


  let var x loc ?used ty env =
    let v = new_var x loc ?used ty in
    all_vars := v :: !all_vars;
    add env v, v

  (* TODO type () et vecteur de types *)
end

let tvoid = Tmany []
let make d ty = { expr_desc = d; expr_typ = ty }
let stmt d = make d tvoid

let rec expr env e =
 let e, ty, rt = expr_desc env e.pexpr_loc e.pexpr_desc in
  { expr_desc = e; expr_typ = ty }, rt

and expr_desc env loc = function (* TODO TODO TODO*)
  | PEskip ->
     TEskip, tvoid, false
  | PEconstant c ->
     (
      match c with
     |Cbool b -> TEconstant c, Tbool, false
     |Cint i -> TEconstant c, Tint, false
     |Cstring s -> TEconstant c, Tstring, false
     )
  | PEbinop (op, e1, e2) ->
    (* TODO nouveau pattern matching dans lequel on check les types *) assert false
  | PEunop (Uamp, e1) ->
    (* TODO *) assert false
  | PEunop (Uneg | Unot | Ustar as op, e1) ->
    (* TODO *) assert false
  | PEcall ({id = "fmt.Print"}, el) ->
    (* TODO *) TEprint [], tvoid, false
  | PEcall ({id="new"}, [{pexpr_desc=PEident {id}}]) ->
     let ty = match id with
       | "int" -> Tint | "bool" -> Tbool | "string" -> Tstring
       | _ -> (* TODO *) error loc ("no such type " ^ id) in
     TEnew ty, Tptr ty, false
  | PEcall ({id="new"}, _) ->
     error loc "new expects a type"
  | PEcall (id, el) ->
     (* TODO *) assert false
  | PEfor (e, b) ->
     (* TODO *) assert false
  | PEif (e1, e2, e3) ->
     (* TODO *) assert false
  | PEnil ->
     (* TODO *) assert false ;
  | PEident {id=id}->
     (*TODO*)(try let v = Env.find id env in TEident v, v.v_typ, false
      with Not_found -> error loc ("unbound variable " ^ id))
  | PEdot (e, id) ->
     (* TODO *) assert false
  | PEassign (lvl, el) ->
     (* TODO *) TEassign ([], []), tvoid, false 
  | PEreturn el ->
     (* TODO *) TEreturn [], tvoid, true
  | PEblock el ->
     (* TODO *) TEblock [], tvoid, false
  | PEincdec (e, op) ->
     (* TODO *) assert false
  | PEvars _ ->
     (* TODO *) assert false 

let found_main = ref true
(* TODO fonction checktype Ast.ptyp -> bool *)


(* 1. declare structures *)

let phase1 = function
  | PDstruct { ps_name = { id = id; loc = loc }; ps_fields = l} -> (* TODO *)
    if Hashtbl.mem tablestructs id then
      error loc ("Deux structures ont le même nom :" ^ id)
    else
      let h = Hashtbl.create 5 in (* on ne stocke rien au début, au cas ou des structures se référencent les unes les autres *)
      Hashtbl.add tablestructs id h
  | PDfunction _ -> ()

let rec sizeof = function
  | Tint | Tbool | Tstring | Tptr _ -> 8
  | Tmany l -> List.fold_left (fun i t -> i + sizeof t) 0 l
  | _ -> assert false (*TODO structures*)

(* 2. declare functions and type fields *) 
let phase2 = function
  | PDfunction f ->
    Funcs.add f;
    Funcs.are_vars_unique f;

     (* TODO + dire quand on a trouvé main (éditer found_main) 
     + ajouter à chaque variable utilisée le fait qu'elle l'est *) () 
  | PDstruct { ps_name = {id}; ps_fields = fl } ->
    let h = Hashtbl.find tablestructs id in
    let rec aux = function 
      | [] -> ()
      | (fieldid, fieldtyp)::xs -> (* ici *)
        if (Hashtbl.mem h fieldid) then 
          error fieldid.loc ("type :"^fieldid.id^"déjà défini dans structure :"^id)
        else 
          ();
        if fieldid.id = id then (error fieldid.loc ("structure récursive :"^id^"contien un champ faisant référence à elle-même"););
       Hashtbl.add h fieldid fieldtyp; aux xs; (* le check est ici *)
    in aux fl; 

     (* TODO check si la définition est ok (i.e. les références sont définies) *) () 



(* 3. type check function bodies *)
let decl = function
  | PDfunction { pf_name={id; loc}; pf_body = e; pf_typ=tyl } ->
    (* TODO check name and type *) 
    let f = { fn_name = id; fn_params = []; fn_typ = []} in
    let e, rt = expr Env.empty e in
    TDfunction (f, e)
  | PDstruct {ps_name={id}} ->
    (* TODO *) let s = { s_name = id; s_fields = Hashtbl.create 5 } in
     TDstruct s

let file ~debug:b (imp, dl) =
  debug := b;
  (* fmt_imported := imp; *)

  List.iter phase1 dl;
  List.iter phase2 dl;
  if not !found_main then error dummy_loc "missing method main";
  let dl = List.map decl dl in
  Env.check_unused (); (* TODO variables non utilisees *)
  if imp && not !fmt_used then error dummy_loc "fmt imported but not used";
  dl 
