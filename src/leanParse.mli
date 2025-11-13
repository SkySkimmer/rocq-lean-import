type parsing_state

val empty_state : parsing_state

val do_line : lcnt:int -> parsing_state -> string ->
  parsing_state * (LeanName.t * LeanExpr.entry) option

val pp_state : parsing_state -> Pp.t
