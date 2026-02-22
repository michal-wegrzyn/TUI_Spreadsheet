open Ast
module SekP = Sek.Persistent

module StringMap = Map.Make(String)
module CellAddressMap = Map.Make(CellAddressOrderedType)
module CellAddressSet = Set.Make(CellAddressOrderedType)

module Types = struct
  type value_type =
  | TInt
  | TFloat
  | TChar
  | TBool
  | TNone
  | TList
  | TMatrix
  | TDict
  | TSet
  | TDef
  | TCustom of int
  | TTimeLimitExceededError
  | TDivisionByZeroError
  | TInvalidArgumentsError
  | TIndexError
  | TBreak
  | TContinue
  type type_env = value_type StringMap.t
end

open Types


module rec Computation : sig
  type cell_value = 
    | Computed of Values.value
    | Computing
    | NotComputedYet
    | ComputingError of Values.error

  type cell = {value: cell_value; definition: def_expr; dependencies: CellAddressSet.t}

  type accessed_cells = CellAddressSet.t
  type dependency_graph = accessed_cells CellAddressMap.t
  type sheets = cell CellAddressMap.t
  type env = { vars : Values.var_env; types : type_env }

  type data = {
    env : env;
    config : env;
    sheets : sheets;
    dependency_graph : dependency_graph;
    cell_address : cell_address option;
    cost : int;
    max_cost : int;
    fresh : int
  }
  
  type 'a comp = data -> ('a Values.result) * data
  
end = struct
  open Values
  type cell_value = 
    | Computed of value
    | Computing
    | NotComputedYet
    | ComputingError of error

  type cell = {value: cell_value; definition: def_expr; dependencies: CellAddressSet.t}


  type accessed_cells = CellAddressSet.t
  type dependency_graph = accessed_cells CellAddressMap.t
  type sheets = cell CellAddressMap.t
  type env = { vars : var_env; types : type_env }
  
  type data = {
    env : env;
    config : env;
    sheets : sheets;
    dependency_graph : dependency_graph;
    cell_address : cell_address option;
    cost : int;
    max_cost : int;
    fresh : int
  }
  
  type 'a comp = data -> ('a result) * data
  
end


and Values : sig
    type var_env = value StringMap.t

  and error = value

  and 'a result =
    | Ok of 'a
    | Error of error

  and value =
    | VInt of int
    | VFloat of float
    | VChar of char
    | VBool of bool
    | VNone
    | VList of value SekP.t
    | VMatrix of value SekP.t * int
    | VDict of value ValueMap.t
    | VSet of ValueSet.t
    | VDef of string * args * def_expr * var_env * type_env * int
    | VBuiltin of ((value list) -> value Computation.comp) * int
    | VExtend of value list
    | VCustom of int * value
    | VTimeLimitExceededError
    | VDivisionByZeroError
    | VInvalidArgumentsError
    | VIndexError
    | VBreak
    | VContinue
end = struct
  type var_env = value StringMap.t

  and error = value

  and 'a result =
    | Ok of 'a
    | Error of error

  and value =
    | VInt of int
    | VFloat of float
    | VChar of char
    | VBool of bool
    | VNone
    | VList of value SekP.t
    | VMatrix of value SekP.t * int
    | VDict of value ValueMap.t
    | VSet of ValueSet.t
    | VDef of string * args * def_expr * var_env * type_env * int
    | VBuiltin of ((value list) -> value Computation.comp) * int
    | VExtend of value list
    | VCustom of int * value
    | VTimeLimitExceededError
    | VDivisionByZeroError
    | VInvalidArgumentsError
    | VIndexError
    | VBreak
    | VContinue
end


and ValueOrderedType : sig
  type t = Values.value
  val compare : t -> t -> int
end = struct
  open Values

  type t = value
  let rec compare v1 v2 = match (v1, v2) with
    | (VInt a, VInt b) -> if a = b then 0 else if a > b then 1 else -1
    | (VFloat a, VFloat b) -> if a = b then 0 else if a > b then 1 else -1
    | (VChar a, VChar b) -> if a = b then 0 else if a > b then 1 else -1
    | (VBool a, VBool b) -> if a = b then 0 else if a then 1 else -1
    | (VNone, VNone) -> 0
    | (VList a, VList b) ->
      SekP.compare compare a b
    | (VMatrix (a, _), VMatrix (b, _)) ->
      SekP.compare compare a b
    | VDict dict1, VDict dict2 ->
      let bindings1 = ValueMap.bindings dict1 in
      let bindings2 = ValueMap.bindings dict2 in
      let rec aux lst1 lst2 = match (lst1, lst2) with
        | ([], []) -> 0
        | ([], _::_) -> -1
        | (_::_, []) -> 1
        | ((k1,v1)::xs, (k2,v2)::ys) ->
          let c = compare k1 k2 in
          if c <> 0 then c else
          let c' = compare v1 v2 in
          if c' <> 0 then c' else
          aux xs ys
      in
      aux bindings1 bindings2
    | (VSet s1, VSet s2) ->
      let lst1 = ValueSet.elements s1 in
      let lst2 = ValueSet.elements s2 in
      let rec aux l1 l2 = match (l1, l2) with
        | ([], []) -> 0
        | ([], _::_) -> -1
        | (_::_, []) -> 1
        | (x::xs, y::ys) ->
          let c = compare x y in
          if c = 0 then aux xs ys else c
      in
      aux lst1 lst2
    | (VDef (_, _, _, _, _, id1), VDef (_, _, _, _, _, id2)) -> if id1 = id2 then 0 else if id1 > id2 then 1 else -1
    | (VBuiltin (_, id1), VBuiltin (_, id2)) -> if id1 = id2 then 0 else if id1 > id2 then 1 else -1
    | (VCustom (id1, v1), VCustom (id2, v2)) -> let c = if id1 = id2 then 0 else if id1 > id2 then 1 else -1 in if c = 0 then compare v1 v2 else c
    | (VTimeLimitExceededError, VTimeLimitExceededError) -> 0
    | (VDivisionByZeroError, VDivisionByZeroError) -> 0
    | (VInvalidArgumentsError, VInvalidArgumentsError) -> 0
    | (VBreak, VBreak) -> 0
    | (VContinue, VContinue) -> 0
    | _ -> let order v = match v with
      | VInt _ -> -1
      | VFloat _ -> -2
      | VChar _ -> -3
      | VBool _ -> -4
      | VNone -> -5
      | VList _ -> -6
      | VMatrix _ -> -7
      | VDict _ -> -8
      | VSet _ -> -9
      | VDef _ -> -10
      | VBuiltin _ -> -11
      | VExtend _ -> -12
      | VTimeLimitExceededError -> -13
      | VDivisionByZeroError -> -14
      | VInvalidArgumentsError -> -15
      | VIndexError -> -16
      | VBreak -> -17
      | VContinue -> -18
      | VCustom (id, _) -> id
      in
      let o1 = order v1 in
      let o2 = order v2 in
      if o1 = o2 then 0 else if o1 > o2 then 1 else -1
end

and ValueMap : Map.S with type key = ValueOrderedType.t = Map.Make(ValueOrderedType)

and ValueSet : Set.S with type elt = ValueOrderedType.t = Set.Make(ValueOrderedType)


module Prelude = struct
  module Computation = Computation
  module Values = Values
  module Types = Types
  module Ast = Ast
  module SekP = SekP
  module ValueOrderedType = ValueOrderedType
  module ValueMap = ValueMap
  module ValueSet = ValueSet
  module StringMap = StringMap
  module CellAddressMap = CellAddressMap
  module CellAddressSet = CellAddressSet

  include Computation
  include Values
  include Types
  include Ast
end

