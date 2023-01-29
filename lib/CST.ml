(* Generated by ocaml-tree-sitter. *)
(*
   jsonnet grammar

   entrypoint: document
*)

open! Sexplib.Conv
open Tree_sitter_run

type semgrep_metavariable = Token.t
[@@deriving sexp_of]

type string_start = Token.t
[@@deriving sexp_of]

type escape_sequence = Token.t
[@@deriving sexp_of]

type number = Token.t
[@@deriving sexp_of]

type ident = Token.t (* pattern [_a-zA-Z][_a-zA-Z0-9]* *)
[@@deriving sexp_of]

type string_content = Token.t
[@@deriving sexp_of]

type string_end = Token.t
[@@deriving sexp_of]

type imm_tok_prec_p1_pat_59587ce = Token.t (* pattern "[^\\\\'\\n]+" *)
[@@deriving sexp_of]

type h = [
    `COLON of Token.t (* ":" *)
  | `COLONCOLON of Token.t (* "::" *)
  | `COLONCOLONCOLON of Token.t (* ":::" *)
]
[@@deriving sexp_of]

type imm_tok_prec_p1_pat_c7f65b4 = Token.t (* pattern "[^\\\\\"\\n]+" *)
[@@deriving sexp_of]

type unaryop = [
    `DASH of Token.t (* "-" *)
  | `PLUS of Token.t (* "+" *)
  | `BANG of Token.t (* "!" *)
  | `TILDE of Token.t (* "~" *)
]
[@@deriving sexp_of]

type id = [
    `Id of ident (*tok*)
  | `Semg_meta of semgrep_metavariable (*tok*)
]
[@@deriving sexp_of]

type str_single =
  [
      `Imm_tok_prec_p1_pat_59587ce of imm_tok_prec_p1_pat_59587ce
    | `Esc_seq of escape_sequence (*tok*)
  ]
    list (* one or more *)
[@@deriving sexp_of]

type str_double =
  [
      `Imm_tok_prec_p1_pat_c7f65b4 of imm_tok_prec_p1_pat_c7f65b4
    | `Esc_seq of escape_sequence (*tok*)
  ]
    list (* one or more *)
[@@deriving sexp_of]

type string_ = [
    `Opt_AT_single_single of (
        Token.t (* "@" *) option
      * Token.t (* "'" *)
      * Token.t (* "'" *)
    )
  | `Opt_AT_single_str_single_single of (
        Token.t (* "@" *) option
      * Token.t (* "'" *)
      * str_single
      * Token.t (* "'" *)
    )
  | `Opt_AT_double_double of (
        Token.t (* "@" *) option
      * Token.t (* "\"" *)
      * Token.t (* "\"" *)
    )
  | `Opt_AT_double_str_double_double of (
        Token.t (* "@" *) option
      * Token.t (* "\"" *)
      * str_double
      * Token.t (* "\"" *)
    )
  | `Opt_AT_str_start_str_content_str_end of (
        Token.t (* "@" *) option
      * string_start (*tok*)
      * string_content (*tok*)
      * string_end (*tok*)
    )
]
[@@deriving sexp_of]

type importstr = (Token.t (* "importstr" *) * string_)
[@@deriving sexp_of]

type import = (Token.t (* "import" *) * string_)
[@@deriving sexp_of]

type anonymous_function = (
    Token.t (* "function" *)
  * Token.t (* "(" *)
  * params option
  * Token.t (* ")" *)
  * document
)

and args = [
    `Expr_rep_COMMA_expr_rep_COMMA_named_arg_opt_COMMA of (
        document
      * (Token.t (* "," *) * document) list (* zero or more *)
      * (Token.t (* "," *) * named_argument) list (* zero or more *)
      * Token.t (* "," *) option
    )
  | `Named_arg_rep_COMMA_named_arg_opt_COMMA of (
        named_argument
      * (Token.t (* "," *) * named_argument) list (* zero or more *)
      * Token.t (* "," *) option
    )
]

and assert_ = (
    Token.t (* "assert" *)
  * document
  * (Token.t (* ":" *) * document) option
)

and binary_expr = [
    `Expr_choice_STAR_expr of (
        document
      * [
            `STAR of Token.t (* "*" *)
          | `SLASH of Token.t (* "/" *)
          | `PERC of Token.t (* "%" *)
        ]
      * document
    )
  | `Expr_choice_PLUS_expr of (
        document
      * [ `PLUS of Token.t (* "+" *) | `DASH of Token.t (* "-" *) ]
      * document
    )
  | `Expr_choice_LTLT_expr of (
        document
      * [ `LTLT of Token.t (* "<<" *) | `GTGT of Token.t (* ">>" *) ]
      * document
    )
  | `Expr_choice_LT_expr of (
        document
      * [
            `LT of Token.t (* "<" *)
          | `LTEQ of Token.t (* "<=" *)
          | `GT of Token.t (* ">" *)
          | `GTEQ of Token.t (* ">=" *)
        ]
      * document
    )
  | `Expr_choice_EQEQ_expr of (
        document
      * [ `EQEQ of Token.t (* "==" *) | `BANGEQ of Token.t (* "!=" *) ]
      * document
    )
  | `Expr_AMP_expr of (document * Token.t (* "&" *) * document)
  | `Expr_HAT_expr of (document * Token.t (* "^" *) * document)
  | `Expr_BAR_expr of (document * Token.t (* "|" *) * document)
  | `Expr_AMPAMP_expr of (document * Token.t (* "&&" *) * document)
  | `Expr_BARBAR_expr of (document * Token.t (* "||" *) * document)
]

and bind = [
    `Id_EQ_expr of named_argument
  | `Id_LPAR_opt_params_RPAR_EQ_expr of (
        id
      * Token.t (* "(" *)
      * params option
      * Token.t (* ")" *)
      * Token.t (* "=" *)
      * document
    )
]

and compspec =
  [ `Fors of forspec | `Ifspec of ifspec ] list (* one or more *)

and document = expr

and expr = [
    `Semg_ellips of Token.t (* "..." *)
  | `Deep_ellips of (Token.t (* "<..." *) * document * Token.t (* "...>" *))
  | `Choice_null of [
        `Null of Token.t (* "null" *)
      | `True of Token.t (* "true" *)
      | `False of Token.t (* "false" *)
      | `Self of Token.t (* "self" *)
      | `Dollar of Token.t (* "$" *)
      | `Str of string_
      | `Num of number (*tok*)
      | `LCURL_opt_choice_member_rep_COMMA_member_opt_COMMA_RCURL of (
            Token.t (* "{" *)
          * objinside option
          * Token.t (* "}" *)
        )
      | `LBRACK_opt_expr_rep_COMMA_expr_opt_COMMA_RBRACK of (
            Token.t (* "[" *)
          * (
                document
              * (Token.t (* "," *) * document) list (* zero or more *)
              * Token.t (* "," *) option
            )
              option
          * Token.t (* "]" *)
        )
      | `LBRACK_expr_opt_COMMA_fors_opt_comp_RBRACK of (
            Token.t (* "[" *)
          * document
          * Token.t (* "," *) option
          * forspec
          * compspec option
          * Token.t (* "]" *)
        )
      | `Expr_DOT_id of (document * Token.t (* "." *) * id)
      | `Expr_LBRACK_opt_expr_opt_COLON_opt_expr_opt_COLON_opt_expr_RBRACK of (
            document
          * Token.t (* "[" *)
          * document option
          * (
                Token.t (* ":" *)
              * document option
              * (Token.t (* ":" *) * document option) option
            )
              option
          * Token.t (* "]" *)
        )
      | `Super_DOT_id of (Token.t (* "super" *) * Token.t (* "." *) * id)
      | `Super_LBRACK_expr_RBRACK of (
            Token.t (* "super" *) * Token.t (* "[" *) * document
          * Token.t (* "]" *)
        )
      | `Expr_LPAR_opt_args_RPAR_opt_tail of (
            document
          * Token.t (* "(" *)
          * args option
          * Token.t (* ")" *)
          * Token.t (* "tailstrict" *) option
        )
      | `Id of id
      | `Local_bind of local_bind
      | `If_expr_then_expr_opt_else_expr of (
            Token.t (* "if" *)
          * document
          * Token.t (* "then" *)
          * document
          * (Token.t (* "else" *) * document) option
        )
      | `Bin_expr of binary_expr
      | `Unar_expr of (unaryop * document)
      | `Expr_LCURL_choice_member_rep_COMMA_member_opt_COMMA_RCURL of (
            document * Token.t (* "{" *) * objinside * Token.t (* "}" *)
        )
      | `Anon_func of anonymous_function
      | `Assert_SEMI_expr of (assert_ * Token.t (* ";" *) * document)
      | `Import of import
      | `Impo of importstr
      | `Expr_error of expr_error
      | `Expr_in_super of (
            document * Token.t (* "in" *) * Token.t (* "super" *)
        )
      | `LPAR_expr_RPAR of (Token.t (* "(" *) * document * Token.t (* ")" *))
    ]
]

and expr_error = (Token.t (* "error" *) * document)

and field = [
    `Semg_ellips of Token.t (* "..." *)
  | `Choice_fiel_opt_PLUS_choice_COLON_expr of [
        `Fiel_opt_PLUS_choice_COLON_expr of (
            fieldname
          * Token.t (* "+" *) option
          * h
          * document
        )
      | `Fiel_LPAR_opt_params_RPAR_choice_COLON_expr of (
            fieldname
          * Token.t (* "(" *)
          * params option
          * Token.t (* ")" *)
          * h
          * document
        )
    ]
]

and fieldname = [
    `Id of id
  | `Str of string_
  | `LBRACK_expr_RBRACK of (Token.t (* "[" *) * document * Token.t (* "]" *))
]

and forspec = (Token.t (* "for" *) * id * Token.t (* "in" *) * document)

and ifspec = (Token.t (* "if" *) * document)

and local_bind = (
    Token.t (* "local" *)
  * bind
  * (Token.t (* "," *) * bind) list (* zero or more *)
  * Token.t (* ";" *)
  * document
)

and member = [ `Objl of objlocal | `Assert of assert_ | `Field of field ]

and named_argument = (id * Token.t (* "=" *) * document)

and objinside = [
    `Member_rep_COMMA_member_opt_COMMA of (
        member
      * (Token.t (* "," *) * member) list (* zero or more *)
      * Token.t (* "," *) option
    )
  | `Rep_objl_COMMA_LBRACK_expr_RBRACK_COLON_expr_rep_COMMA_objl_opt_COMMA_fors_opt_comp of (
        (objlocal * Token.t (* "," *)) list (* zero or more *)
      * Token.t (* "[" *)
      * document
      * Token.t (* "]" *)
      * Token.t (* ":" *)
      * document
      * (Token.t (* "," *) * objlocal) list (* zero or more *)
      * Token.t (* "," *) option
      * forspec
      * compspec option
    )
]

and objlocal = (Token.t (* "local" *) * bind)

and param = [
    `Semg_ellips of Token.t (* "..." *)
  | `Id_opt_EQ_expr of (id * (Token.t (* "=" *) * document) option)
]

and params = (
    param
  * (Token.t (* "," *) * param) list (* zero or more *)
  * Token.t (* "," *) option
)
[@@deriving sexp_of]

type null (* inlined *) = Token.t (* "null" *)
[@@deriving sexp_of]

type comment (* inlined *) = Token.t
[@@deriving sexp_of]

type double (* inlined *) = Token.t (* "\"" *)
[@@deriving sexp_of]

type self (* inlined *) = Token.t (* "self" *)
[@@deriving sexp_of]

type super (* inlined *) = Token.t (* "super" *)
[@@deriving sexp_of]

type local (* inlined *) = Token.t (* "local" *)
[@@deriving sexp_of]

type single (* inlined *) = Token.t (* "'" *)
[@@deriving sexp_of]

type semgrep_ellipsis (* inlined *) = Token.t (* "..." *)
[@@deriving sexp_of]

type tailstrict (* inlined *) = Token.t (* "tailstrict" *)
[@@deriving sexp_of]

type true_ (* inlined *) = Token.t (* "true" *)
[@@deriving sexp_of]

type false_ (* inlined *) = Token.t (* "false" *)
[@@deriving sexp_of]

type dollar (* inlined *) = Token.t (* "$" *)
[@@deriving sexp_of]

type deep_ellipsis (* inlined *) = (
    Token.t (* "<..." *) * document * Token.t (* "...>" *)
)
[@@deriving sexp_of]

let dump_tree root =
  sexp_of_document root
  |> Print_sexp.to_stdout
