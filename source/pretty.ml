open Ast
open Lib
open Printf
open Tast

let fold_string f list = String.concat "" (List.map f list)
let fold_children f list parent_id = fold_string (fun item -> f item parent_id) list

let get_node_id =
  let id = ref 0 in
  fun name -> ( incr id; sprintf "%s_%d" name !id )

let link_nodes id_from id_to = sprintf "%s -> %s\n" id_from id_to

let html_attribute balise attribute name =  sprintf "<%s %s>%s</%s>" balise attribute name balise
let html balise name = html_attribute balise "" name

let draw_node id name fields children =
  let colspan = sprintf "colspan = '%d'" (max 1 (List.length children)) in
  let rowfield field = html "tr" (html_attribute "td" colspan field) in
  let rowchild child = html_attribute "td" (sprintf "port='%s'" child) child in
  let table = html_attribute "table" "border='0' cellborder='1' cellspacing='0' cellpadding='4'" (
      html "tr" (html_attribute "td" colspan (html "b" name)) ^
      ( if fields = [] then "" else fold_string rowfield fields ) ^
      ( if children = [] then "" else html "tr" (fold_string rowchild children) )
    )
  in sprintf "%s [label=<%s>]\n" id table

let create_node parent_id name fields children =
  let node_id = get_node_id name in
  let node_children = List.map (fun (child, _) -> child) children
  in draw_node node_id name fields node_children ^
     link_nodes parent_id node_id ^
     fold_string (fun (child, f) -> f (sprintf "%s:%s" node_id child)) children


let binop = function
  | Badd -> "+"
  | Bsub -> "-"
  | Bmul -> "*"
  | Bdiv -> "/"
  | Bmod -> "%"
  | Beq -> "=="
  | Bne -> "!="
  | Blt -> "&lt;"
  | Ble -> "&le;"
  | Bgt -> "&gt;"
  | Bge -> "&ge;"
  | Band -> "&amp;&amp;"
  | Bor -> "||"

let unop = function
  | Uneg -> "-"
  | Unot -> "!"
  | Uamp -> "&amp;"
  | Ustar -> "*"

let incdec = function
  | Inc -> "++"
  | Dec -> "--"


let get_ast_constant constant parent_id =
  let create_node = create_node parent_id in
  match constant with
  | Cbool bool -> create_node "Cbool" [ sprintf "bool = \"%b\"" bool ] []
  | Cint int64 -> create_node "Cint" [ sprintf "int64 = \"%Ld\"" int64 ] []
  | Cstring string -> create_node "Cstring" [ sprintf "string = \"%s\"" string ] []

let rec get_ast_typ ptyp parent_id =
  let create_node = create_node parent_id in
  match ptyp with
  | PTident ident -> create_node "PTident" [ "ident = { id = \"" ^ ident.id ^ "\" }" ] []
  | PTptr ptyp -> create_node "PTptr" [] [ ("ptyp", get_ast_typ ptyp) ]

let rec get_ast_pexpr { pexpr_desc } parent_id =
  let create_node = create_node parent_id in
  match pexpr_desc with
  | PEskip -> create_node "PEskip" [] []

  | PEconstant constant -> create_node "PEconstant" [] [
      ("constant", get_ast_constant constant)
    ]

  | PEbinop (op, pexpr_left, pexpr_right) ->
    create_node "PEbinop" [ "binop = \"" ^ binop op ^ "\"" ] [
      ("pexpr_left", get_ast_pexpr pexpr_left);
      ("pexpr_right", get_ast_pexpr pexpr_right)
    ]

  | PEunop (op, pexpr) ->
    create_node "PEunop" [ "unop = \"" ^ unop op ^ "\"" ] [
      ("pexpr", get_ast_pexpr pexpr)
    ]

  | PEnil -> create_node "PEnil" [] []

  | PEcall (ident, pexprs) -> create_node "PEcall" [ "ident = { id = \"" ^ ident.id ^ "\" }" ] [
      ("pexpr_list", fold_children get_ast_pexpr pexprs)
    ]

  | PEident ident -> create_node "PEident" [ "ident = { id = \"" ^ ident.id ^ "\" }" ] []

  | PEdot (pexpr, ident) -> create_node "PEdot" [ "ident = { id = \"" ^ ident.id ^ "\" }" ] [
      ("pexpr", get_ast_pexpr pexpr)
    ]

  | PEassign (pexprs_left, pexprs_right) -> create_node "PEassign" [] [
      ("pexpr_list_left", fold_children get_ast_pexpr pexprs_left);
      ("pexpr_list_right", fold_children get_ast_pexpr pexprs_right)
    ]

  | PEvars (idents, ptyp, pexprs) ->
    let ident_list =
      String.concat ", " (List.map (fun { id } -> "{ id = \"" ^ id ^ "\" }") idents)
    in let ptyp_field = match ptyp with
        | Some ptyp -> [("ptyp", get_ast_typ ptyp)]
        | None -> []
    in let fields = ptyp_field @ [ ("pexpr_list", fold_children get_ast_pexpr pexprs) ]
    in create_node "PEvars" [ "idents = [" ^ ident_list ^ "]" ] fields

  | PEif (cond, if_block, else_block) -> create_node "PEif" [] [
      ("pexpr_cond", get_ast_pexpr cond);
      ("pexpr_if", get_ast_pexpr if_block);
      ("pexpr_else", get_ast_pexpr else_block)
    ]

  | PEreturn pexprs -> create_node "PEreturn" [] [
      ("pexpr_list", fold_children get_ast_pexpr pexprs)
    ]

  | PEblock pexprs -> create_node "PEblock" [] [
      ("pexpr_list", fold_children get_ast_pexpr pexprs)
    ]

  | PEfor (cond, for_block) -> create_node "PEfor" [] [
      ("pexpr_cond", get_ast_pexpr cond);
      ("pexpr_for", get_ast_pexpr for_block);
    ]

  | PEincdec (pexpr, op) ->
    create_node "PEincdec" [ "incdec = \"" ^ incdec op ^ "\"" ] [
      ("pexpr_cond", get_ast_pexpr pexpr);
    ]


let get_ast_pparam (ident, ptyp) parent_id =
  create_node parent_id "pparam" [ "ident = { id = \"" ^ ident.id ^ "\" }" ] [
    ("typ", get_ast_typ ptyp)
  ]


let get_ast_pfield (ident, ptyp) parent_id =
  create_node parent_id "pfield" [ "ident = { id = \"" ^ ident.id ^ "\" }" ] [
    ("typ", get_ast_typ ptyp)
  ]


let get_ast_pdecl pdecl parent_id =
  let create_node = create_node parent_id in
  match pdecl with
  | PDfunction { pf_name; pf_params; pf_typ; pf_body } ->
    create_node "PDfunction" [ "pf_name = { id = \"" ^ pf_name.id ^ "\" }" ] [
      ("pf_params", fold_children get_ast_pparam pf_params);
      ("pf_typ", fold_children get_ast_typ pf_typ);
      ("pf_body", get_ast_pexpr pf_body)
    ]

  | PDstruct { ps_name; ps_fields } ->
    create_node "PDstruct" [ "ps_name = { id = \"" ^ ps_name.id ^ "\" }" ] [
      ("ps_fields", fold_children get_ast_pfield ps_fields)
    ]


let get_dot_ast (_, pdecls) =
  "digraph ast {\n" ^
  "node [shape=plaintext];\n" ^
  draw_node "root" "*" [] [] ^
  fold_children get_ast_pdecl pdecls "root" ^
  "}"


let rec typ fmt = function
  | Tint -> Format.fprintf fmt "int"
  | Tbool -> Format.fprintf fmt "bool"
  | Tstring -> Format.fprintf fmt "string"
  | Tstruct s -> Format.fprintf fmt "%s" s.s_name
  | Tptr ty -> Format.fprintf fmt "*%a" typ ty
(* TODO autres types utilises par l'analyse semantique *)

let rec expr fmt e = match e.expr_desc with
  | TEskip -> Format.fprintf fmt ";"
  | TEnil -> Format.fprintf fmt "ni"
  | TEconstant (Cint n) -> Format.fprintf fmt "%Ld" n
  | TEconstant (Cbool b) -> Format.fprintf fmt "%b" b
  | TEconstant (Cstring s) -> Format.fprintf fmt "%S" s
  | TEbinop (op, e1, e2) ->
    Format.fprintf fmt "@[(%a %s@ %a)@]" expr e1 (binop op) expr e2
  | TEunop (op, e1) ->
    Format.fprintf fmt "@[(%s@ %a)@]" (unop op) expr e1
  | TEnew ty ->
    Format.fprintf fmt "new(%a)" typ ty
  | TEcall (f, el) ->
    Format.fprintf fmt "%s(%a)" f.fn_name list el
  | TEident v ->
    Format.fprintf fmt "%s" v.v_name
  | TEdot (e1, f) ->
    Format.fprintf fmt "%a.%s" expr e1 f.f_name
  | TEassign ([], _) | TEassign (_, []) ->
    assert false
  | TEassign ([lvl], [e]) ->
    Format.fprintf fmt "%a = %a" expr lvl expr e
  | TEassign (lvl, el) ->
    Format.fprintf fmt "%a = %a" list lvl list el
  | TEif (e1, e2, e3) ->
    Format.fprintf fmt "if %a@ %a@ %a" expr e1 expr e2 expr e3
  | TEreturn el ->
    Format.fprintf fmt "return %a" list el
  | TEblock bl ->
    block fmt bl
  | TEfor (e1, e2) ->
    Format.fprintf fmt "for %a %a" expr e1 expr e2
  | TEprint el ->
    Format.fprintf fmt "fmt.Print(%a)" list el
  | TEincdec (e1, op) ->
    Format.fprintf fmt "%a%s" expr e1 (match op with Inc -> "++" | Dec -> "--")
  | TEvars vl ->
    Format.fprintf fmt "var %a" (print_list comma var) vl

and var fmt v =
  Format.fprintf fmt "%s" v.v_name

and block fmt bl =
  Format.fprintf fmt "{@\n%a}" (print_list newline expr) bl

and list fmt el =
  print_list comma expr fmt el

let decl fmt = function
  | TDfunction (f, e) ->
    Format.fprintf fmt "@[<hov 2>func %s(%a) %a@]@\n@\n"
      f.fn_name (print_list comma var) f.fn_params expr e
  | TDstruct s ->
    Format.fprintf fmt "type %s struct { ... }@\n" s.s_name

let file fmt dl =
  Format.fprintf fmt "---------@\n";
  List.iter (decl fmt) dl;
  Format.fprintf fmt "---------@\n"
