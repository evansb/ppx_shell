
val error_at : Location.t -> 'a

val list_to_expr_assoc_list : string list -> Parsetree.expression

val mapper : 'a -> Ast_mapper.mapper
