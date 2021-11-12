
(* arbres issus du typeur *)

type unop = Ast.unop

type binop = Ast.binop

type constant = Ast.constant

type incdec = Ast.incdec

type function_ = {
    fn_name: string;
  fn_params: var list;
     fn_typ: typ list;
}

and structure = {
          s_name: string;
        s_fields: (string, field) Hashtbl.t;
  (* TODO autres informations pour l'analyse semantique *)
}

and typ =
  | Tint | Tbool | Tstring
  | Tstruct of structure
  | Tptr of typ
  | Tmany of typ list (* 0 ou >= 2 *)
  (* TODO autres types pour l'analyse semantique *)

and var = {
          v_name: string;
            v_id: int; (* unique *)
           v_loc: Ast.location;
           v_typ: typ;
  mutable v_used: bool;
  mutable v_addr: bool; (* usage de &x *)
  (* TODO autres informations pour la production de code *)
}

and field = {
         f_name: string;
          f_typ: typ;
  mutable f_ofs: int; (* relatif à l'adresse de l'objet *)
}

and expr =
  { expr_desc: expr_desc;
    expr_typ : typ; }

and expr_desc =
  | TEskip
  | TEconstant of constant
  | TEbinop of binop * expr * expr
  | TEunop of unop * expr
  | TEnil
  | TEnew of typ
  | TEcall of function_ * expr list
  | TEident of var
  | TEdot of expr * field
  | TEassign of expr list * expr list
  | TEvars of var list
  | TEif of expr * expr * expr
  | TEreturn of expr list
  | TEblock of expr list
  | TEfor of expr * expr
  | TEprint of expr list
  | TEincdec of expr * incdec

type tdecl =
  | TDfunction of function_ * expr
  | TDstruct   of structure

type tfile = tdecl list
