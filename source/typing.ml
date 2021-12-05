
open Format
open Lib
open Ast
open Tast

let debug = ref false

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

exception Error of Ast.location * string

let error loc e = raise (Error (loc, e))

(* environnement pour les types structure *)
let tablestructs = Hashtbl.create 5


(* on ne peut faire cette vérif qu'après la phase 1, 
sinon les références d'une struct à l'autre sont jugées mauvaises*)
let rec checktype = function
  |PTident {id; loc}-> List.mem id ["bool"; "int"; "string"] || Hashtbl.mem tablestructs id
  |PTptr t -> checktype t
let listtotype = function 
  | [x] -> x
  | _ as a -> Tmany a
let typetolist = function
  |Tmany a -> a
  | _ as x -> [x]
  (* environnement pour les fonctions *)
module Funcs = struct
  module M = Map.Make(String)
  type t = pfunc M.t
  let empty = M.empty
  let all_funcs = ref empty
  let find = fun x -> M.find x !all_funcs
  let is_def f = M.mem f.pf_name.id !all_funcs
  let add f = 
    if is_def f then 
      error f.pf_name.loc ("Deux fonctions ont le même nom : " ^f.pf_name.id)
  else
      all_funcs := M.add f.pf_name.id f !all_funcs
  let are_vars_unique f = 
    let table = Hashtbl.create 15 in
    let rec add_table = function
      |[] -> ()
      |p::t -> (* pour tout pparam p, on vérifie que son identifiant n'est pas déjà pris *)
        let idparam = fst p in
        if Hashtbl.mem table idparam.id then
          error idparam.loc ("dans la fonction "^f.pf_name.id^" deux arguments ont le nom "^idparam.id)
        else (
          Hashtbl.add table idparam.id ();
          add_table t
        )
    in add_table f.pf_params
  let veriftypesparam f =
    let rec auxcheckpparam = function
    | [] -> ()
    | x::xs -> 
      let id = fst x in
      if checktype (snd x)
      then auxcheckpparam xs
      else error id.loc ("le paramètre "^id.id^" de la fonction "^f.pf_name.id^" est de type inconnu")
    in auxcheckpparam f.pf_params

  let veriftypessortie f =
    let rec auxchecksortie = function
    |[] -> ()
    | x::xs ->
      if checktype x
      then auxchecksortie xs
      else error f.pf_name.loc ("la fonction "^f.pf_name.id^" a un type de retour inconnu")
    in auxchecksortie f.pf_typ
end


let rec type_type = function
  | PTident { id = "int" } -> Tint
  | PTident { id = "bool" } -> Tbool
  | PTident { id = "string" } -> Tstring
  | PTident id -> 
    (
    try
      let s = Hashtbl.find tablestructs id.id in
      Tstruct s
    with 
    |Not_found -> error id.loc ("Déclaration de variable de type "^id.id^" inconnu")
    )
  | PTptr ty -> Tptr (type_type ty)

let rec eqlist cmp l1 l2 = match l1,l2 with
|[],[] -> true
|[],_ -> false
|_,[] -> false
|x::xs, y::ys -> cmp x y && eqlist cmp xs ys
let rec eq_type ty1 ty2 = match ty1, ty2 with
  | Tint, Tint | Tbool, Tbool | Tstring, Tstring -> true
  | Tstruct s1, Tstruct s2 -> s1.s_name = s2.s_name
  | Tptr ty1, Tptr ty2 -> eq_type ty1 ty2
  | Tmany l1, Tmany l2 -> l1 = l2 
  | _ -> false
    (* TODO autres types *)
let eq_typeLR  tyL tyR = match tyL with
|Tptr t -> eq_type tyL tyR || eq_type t tyR
|_ -> eq_type tyL tyR
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
      if v.v_name <> "_" && not v.v_used then
       error v.v_loc ("unused variable : "^v.v_name) in
    List.iter check !all_vars

  let var x loc ?used ty env =
    let v = new_var x loc ?used ty in
    all_vars := v :: !all_vars;
    add env v, v

  (* TODO type () et vecteur de types *)
end

let envactuel = ref Env.empty
let tvoid = Tmany []
let make d ty = { expr_desc = d; expr_typ = ty }
let stmt d = make d tvoid
let rec islvalue = function
  | PEident _ -> true
  | PEdot (e,x) when islvalue e.pexpr_desc -> true
  | PEunop (Ustar, e) when e.pexpr_desc <> PEnil -> true
  | _ -> false

let validassign lvl el loc =
  let listetypesR1 = List.map (fun x -> x.expr_typ) el in
  let listetypesR2 = List.map typetolist listetypesR1 in
  let listetypeR = List.flatten listetypesR2 in
  let listetypesL1 = List.map (fun x -> x.expr_typ) el in
  let listetypesL2 = List.map typetolist listetypesL1 in
  let listetypeL = List.flatten listetypesL2 in
  if not (eqlist eq_typeLR listetypeL listetypeR) then error loc ("problème de types lors de l'assignation")
let typederetour = ref tvoid
let rec expr env e =
 let e, ty, rt = expr_desc env e.pexpr_loc e.pexpr_desc in
  { expr_desc = e; expr_typ = ty }, rt
  and exprx env x = let ex, _ = expr env x in ex
  and rtx env x = let _, rx = expr env x in rx
and expr_desc env loc = function 
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
    (
      let d1, ty1, rt1 = expr_desc env loc e1.pexpr_desc in
      let d2, ty2, rt2 = expr_desc env loc e2.pexpr_desc in 
      match op with
      | Badd | Bsub | Bmul | Bdiv | Bmod  -> 
        if (ty1 = Tint && ty2 = Tint)
          then TEbinop (op, make d1 ty1, make d2 ty2), Tint, false
          else error loc ("type int attendu pour cet opérateur")
      | Beq | Bne ->
        if (ty1 = ty2)
          then TEbinop (op, make d1 ty1, make d2 ty2), Tbool, false
          else error loc ("types égaux attendus pour opérateur d'égalité")
      | Blt | Ble | Bgt | Bge ->
        if (ty1 = Tint && ty2 = Tint)
          then TEbinop (op, make d1 ty1, make d2 ty2), Tbool, false
          else error loc ("type int attendu pour cet opérateur")
      | Band | Bor ->
        if (ty1 = Tbool && ty2 = Tbool)
          then TEbinop (op, make d1 ty1, make d2 ty2), Tbool, false
          else error loc ("type bool attendu pour cet opérateur")
    )
  | PEunop (Uamp, e) ->
    if islvalue e.pexpr_desc 
      then let ex, rx = expr env e in TEunop (Uamp, ex), Tptr (ex.expr_typ), false
      else error loc ("l-value attendue pour &")
  | PEunop (Uneg, e1) ->
    let ex, rx = expr env e1 in
    if ex.expr_typ = Tint 
      then TEunop (Uneg, ex), Tint, false
      else error loc ("type int attendu pour négation")
  | PEunop (Unot, e1) ->
    let ex, rx = expr env e1 in
    if ex.expr_typ = Tbool 
      then TEunop (Unot, ex), Tbool, false
      else error loc ("type bool attendu pour non logique")
  | PEunop (Ustar, e1) ->
     let ex, rx = expr env e1 in
     (
      match ex.expr_typ with
        |Tptr t -> TEunop (Ustar, ex), t, false
        | _ -> error loc ("pointeur non égal à nil attendu pour *")
     )
  | PEcall ({id = "fmt.Print"}, el) ->
    (if not !fmt_imported then error loc ("Print demandé sans import de fmt"););
    fmt_used := true;
    let toprint = List.map (exprx env) el in
     TEprint toprint, tvoid, false
  | PEcall ({id="new"}, [{pexpr_desc=PEident {id}}]) ->
     let ty = match id with
       | "int" -> Tint | "bool" -> Tbool | "string" -> Tstring
       | _ -> 
        if Hashtbl.mem tablestructs id 
          then type_type (PTident {id ; loc})
          else error loc ("type "^id^" inconnu ")
      in
     TEnew ty, Tptr ty, false
  | PEcall ({id="new"}, _) ->
     error loc "demande de new sans type donné"
  | PEcall (id, el) ->
     (
       try
      let pf = Funcs.find id.id in
      let el_typee = List.map (exprx env) el in
      let f = {fn_name = pf.pf_name.id ; fn_typ = List.map type_type pf.pf_typ ;
                fn_params = List.map (fun (id,typ) -> new_var id.id id.loc (type_type typ)) pf.pf_params} in
      let typ = listtotype f.fn_typ in
      TEcall(f,el_typee), typ , false
     with
     |Not_found -> error loc ("appel de fonction "^id.id^" inconnue")
     )
  | PEfor (e, b) ->
     let exb, rtb = expr env b in
     (if rtb then error loc ("le test booléen retourne qqch"););
     if exb.expr_typ = Tbool 
        then
          let ex, rx = expr env e in
          TEfor (ex, exb), tvoid, rx
        else error loc ("type bool attendu dans test de for")
  | PEif (e1, e2, e3) ->
    let ex, rx = expr env e1 in
    (if rx then error loc ("le test booléen retourne qqch"););
    if ex.expr_typ = Tbool 
      then 
        let ex2, rt2 = expr env e2 in 
        let ex3, rt3 = expr env e3 in
        TEif (ex, ex2,ex3), tvoid, rt2 && rt3
      else error loc ("type bool attendu dans test de if")
  | PEnil ->
     TEnil, tvoid, false
  | PEident {id=id}->
    (
     try 
      let v = Env.find id !envactuel in 
        v.v_used <- true;
        TEident v, v.v_typ, false
     with Not_found -> error loc ("unbound variable " ^ id)
    )
  | PEdot (e, id) ->
     (
      let newe = exprx env e in
       match newe.expr_typ with
        |Tstruct a |Tptr (Tstruct a) -> 
          (
            try
            let s = Hashtbl.find tablestructs a.s_name in
            if Hashtbl.mem s.s_fields id.id 
              then let field = Hashtbl.find s.s_fields id.id in
                    TEdot(newe,field),field.f_typ,false
              else error loc ("la structure"^s.s_name^"n'a pas de champ"^id.id)
            with 
            |Not_found -> error loc ("structure inconnue")
          )
        | _ -> error loc ("dot sur qqch qui n'est pas une structure")
     )

  | PEassign (lvl, el) ->
      if List.for_all (fun x -> islvalue x.pexpr_desc) lvl
        then
          let nlvl = List.length lvl and nel = List.length el in
          (
            if nlvl < nel then error loc ("trop de r-values")
            else if nlvl > nel then error loc ("trop de l-values")
          );
          let newlvl = List.map (exprx env) lvl in
          let newel = List.map (exprx env) el in
          validassign newlvl newel loc;
          TEassign (newlvl, newel), tvoid, false 
        else error loc "l-value attendue pour assignation"
  | PEreturn el ->
    let el_typee = List.map (exprx env) el in
    let retour = listtotype (List.map (fun x -> x.expr_typ) el_typee) in
    if retour = !typederetour 
      then TEreturn el_typee, tvoid, true
      else error loc "mauvais type de retour"
  | PEblock el ->
    let el_typee = List.map (exprx env) el in
    let rt = List.exists (rtx env) el in
    envactuel := env;
    TEblock el_typee, tvoid, rt
  | PEincdec (e, op) ->
     let dx, tyx, rtx = expr_desc env loc e.pexpr_desc in
     if tyx = Tint
      then TEincdec (make dx tyx, op), Tint, false
      else error loc ("type int attendu pour ++/--")
  | PEvars (il, ty, el) ->
    let el_typee = List.map (exprx env) el in
    match ty with
    |None -> 
      (
        match el_typee with
        | [] -> error loc ("besoin de type dans déclaration sans expression")
        | [{expr_desc = TEcall (f,params)}] -> let listevars = nv_var_type il f.fn_typ loc in TEvars listevars, tvoid, false
        |_ -> 
          List.iter (fun x -> if x.expr_desc = TEnil then error loc "expression vide dans assign") el_typee;
          let typelist = typeofexprlist el_typee in
          let listevars = nv_var_type il typelist loc in TEvars listevars, tvoid, false
      )
      |Some typev ->
        (
          let t = type_type typev in
          let ltypes = List.init (List.length il) (fun n -> t) in
          match el_typee with
          |[] -> let listevars = nv_var_type il ltypes loc in TEvars listevars, tvoid, false
          |[{expr_desc = TEcall (f,params)}] -> 
            if eqlist eq_type ltypes f.fn_typ 
              then (let listevars = nv_var_type il ltypes loc in TEvars listevars, tvoid, false)
              else error loc ("types incompatibles")
          | _ -> let typelist = typeofexprlist el_typee in
            if eqlist eq_type ltypes typelist 
              then (let listevars = nv_var_type il ltypes loc in TEvars listevars, tvoid, false)
              else error loc "types incompatibles"
        )
and nv_var_type li lt loc_act = match li,lt with
    | [],[] -> []
    | {loc;id}::q1,t::q2 -> let env,v = Env.var id loc t !envactuel in (envactuel := env; v::(nv_var_type q1 q2 loc_act))
    | _,_ -> error loc_act "cannot assign"





and typeofexprlist el = List.map (fun x -> x.expr_typ) el

let found_main = ref false


(* 1. declare structures *)

let phase1 = function
  | PDstruct { ps_name = { id = id; loc = loc }; ps_fields = l} -> 
    if Hashtbl.mem tablestructs id then
      error loc ("Deux structures ont le même nom : " ^ id)
    else
      let h = Hashtbl.create 5 in 
      Hashtbl.add tablestructs id {s_name = id; s_fields =h}
      (* on ne stocke rien au début, 
      au cas ou des structures se référencent les unes les autres *)
  | PDfunction _ -> ()

let rec sizeof = function
  | Tint | Tbool | Tstring | Tptr _ -> 8
  | Tmany l -> List.fold_left (fun i t -> i + sizeof t) 0 l
  | Tstruct s -> 
    let sum = ref 0 in
    Hashtbl.iter (fun a b -> sum := !sum + sizeof (b.f_typ)) s.s_fields;
    !sum


let checkmain f =
  if not(f.pf_params = [] && f.pf_typ = []) 
  then error f.pf_name.loc "Fonction main mal typée !"
  else found_main := true

(* 2. declare functions and type fields *) 
let phase2 = function
  | PDfunction f ->
    Funcs.add f;
    Funcs.are_vars_unique f;
    Funcs.veriftypesparam f;
    Funcs.veriftypessortie f;
    (if f.pf_name.id = "main" 
      then checkmain f
    );
  | PDstruct { ps_name = {id}; ps_fields = fl } ->
    let offset = ref 0 in
    let h = Hashtbl.find tablestructs id in
    let rec aux = function 
      | [] -> ()
      | (fieldid, fieldtyp)::xs ->
        (
          (if not(checktype fieldtyp) then
            error fieldid.loc ("Dans la structure "^id^" le champ "^fieldid.id^" ne fait référence à aucun type connu")
          );
          (if (Hashtbl.mem h.s_fields fieldid.id) then 
            error fieldid.loc ("type "^fieldid.id^" déjà défini dans structure "^id)
          );
          (if fieldid.id = id then 
            error fieldid.loc ("structure récursive "^id^" contient un champ faisant référence à elle-même")
          );
        );     
        let f = {f_name = fieldid.id ; f_typ = type_type fieldtyp; f_ofs = !offset} in
        offset := !offset + sizeof f.f_typ;
        Hashtbl.add h.s_fields fieldid.id f;
        aux xs
    in aux fl



(* fonction clé dans la phase 3. 
On lui donne une liste de Ast.pparam et elle renvoie une liste de Tast.vars et un environnement les contenant *)
let paramlisttovarlist plist = 
  let rec aux = function
    | [] -> ([],Env.empty)
    | p::xs -> 
      let (vlist,e) = aux xs in 
      let (id, typ) = p in
      let (nvenv, v) = Env.var id.id id.loc (type_type typ) e in
      (v::vlist, nvenv)
    in aux plist



(* 3. type check function bodies *)
let decl = function
  | PDfunction { pf_name={id; loc}; pf_body = e; pf_typ=typl ; pf_params = params} ->
    let returntype = List.map type_type typl in
    typederetour := listtotype returntype; (* on le garde en mémoire pendant le parcours *)
    let typesortie = List.map type_type typl in (* la liste des types de sortie passe de Ast.ptyp list à Tast.typ list *)
    let listevars,environnement = paramlisttovarlist params in (* on ajoute tous les paramètres d'entrée à l'environnement, créé à la volée par cette fonction *)
    envactuel := environnement;
    let f = { fn_name = id; fn_params = listevars; fn_typ = typesortie} in (* on écrit notre Tast.function *)
    let e, rt = expr !envactuel e in (* on vérifie le corps *)
    (if not rt && !typederetour <> tvoid then error loc ("manque return dans fonction"^id));
    TDfunction (f, e)

  | PDstruct {ps_name={id}} ->
     let s = Hashtbl.find tablestructs id in
     TDstruct s

let file ~debug:b (imp, dl) =
  debug := b;
  fmt_imported := imp;
  List.iter phase1 dl;
  List.iter phase2 dl;
  if not !found_main then error dummy_loc "missing method main";
  let dl = List.map decl dl in
  Env.check_unused ();
  if imp && not !fmt_used then error dummy_loc "fmt imported but not used";
  dl 
