type cell_address = string * (int * int)


module CellAddressOrderedType = struct
  type t = cell_address
  let compare v1 v2 = if v1 = v2 then 0 else if v1 > v2 then 1 else -1
end


type args = string list


type value_expr =
| Int of int
| Float of float
| Char of char
| Bool of bool
| NNone
| List of value_expr list
| Matrix of value_expr list
| Dict of (value_expr * value_expr) list
| SSet of value_expr list
| If of value_expr*value_expr*value_expr
| Let of string * value_expr * value_expr
| Var of string
| Call of value_expr * value_expr list
| Or of value_expr * value_expr
| And of value_expr * value_expr
| Not of value_expr
| ListComprehension of value_expr * string * value_expr * value_expr
| MatrixComprehension of value_expr * string * value_expr * value_expr
| DictComprehension of value_expr * value_expr * string * value_expr * value_expr
| SSetComprehension of value_expr * string * value_expr * value_expr
| IsOfType of value_expr * string
| CellAddress of cell_address
| CellAddressList of cell_address * cell_address
| CellAddressMatrix of cell_address * cell_address
| Lambda of string list * value_expr


type var_item =
| VarIdent of string
| VarItem of var_item * value_expr


type def_expr = 
| Sequence of def_expr list
| Assign of var_item * value_expr
| If of value_expr * def_expr * def_expr
| While of value_expr * def_expr
| For of string * value_expr * def_expr
| Def of string * args * def_expr
| Extend of string * args * def_expr
| Extended of string * args * def_expr
| TryCatch of def_expr * string * def_expr
| Return of value_expr
| NewType of string * string * string
| NewTypeSingleton of string * string
| Del of var_item
| Raise of value_expr
| Break
| Continue
