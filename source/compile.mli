val debug : bool ref
val strings : (X86_64.label, string) Hashtbl.t
val alloc_string : string -> string
val malloc : int -> X86_64.text
val allocz : int -> X86_64.text
val sizeof : Tast.typ -> int
val new_label : unit -> string
type env = {
  exit_label : string;
  ofs_this : int;
  nb_locals : int ref;
  next_local : int;
}
val empty_env : env
val mk_bool : Tast.expr_desc -> Tast.expr
val compile_bool : (string -> X86_64.text) -> X86_64.text
val expr : 'a -> Tast.expr -> X86_64.text
val function_ : Tast.function_ -> 'a -> [>  ] X86_64.asm
val decl :
  ([< `data | `text ] as 'a) X86_64.asm -> Tast.tdecl -> 'a X86_64.asm
val file : ?debug:bool -> Tast.tdecl list -> X86_64.program