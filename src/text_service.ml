open Sheet_types.Prelude
open Spreadsheet
open Print
open Re


module StringIntOrderedType = struct
  type t = string * int
  let compare v1 v2 = if v1 = v2 then 0 else if v1 > v2 then 1 else -1
end

module StringIntMap = Map.Make(StringIntOrderedType)


type cell_text =
  | Parsed of string
  | NotParsed of string * string  (* current text, last successful text *)

type config_text =
| Saved of string * value result
| NotSaved of string * int * ((value result*env) option) * string * (value result)


type state = {
  config_text : config_text;
  config : env;
  sheets_text : cell_text CellAddressMap.t;
  sheets_formatter : (cell_text * (value option)) CellAddressMap.t;
  sheets : sheets;
  sheet_names : string list;
  dependency_graph : dependency_graph;
  max_cost : int;
  fresh : int;
  default_column_width : int;
  default_row_height : int;
  column_widths : int StringIntMap.t;
  row_heights : int StringIntMap.t;
  filename : string;
}


let data_of_state (state : state) : data =
  { env = state.config; config = state.config; sheets = state.sheets; dependency_graph = state.dependency_graph; cell_address = None; cost = 0; max_cost = state.max_cost; fresh = state.fresh }


let update_state_from_data (data : data) (state : state) : state =
  { state with config = data.config; sheets = data.sheets; dependency_graph = data.dependency_graph; max_cost = data.max_cost; fresh = data.fresh }


let def_expr_of_string (text : string) : def_expr =
  Lexer.reset_lexer_state ();
  let lexbuf = Lexing.from_string text in
  let parsed = Parser.program Lexer.token lexbuf in
  parsed


let def_expr_of_string_with_equals (text : string) : def_expr =
    let int_pattern = Pcre.regexp "^[ \n\r]*[+-]?[0-9]+[ \n\r]*$" in
    let float_pattern = Pcre.regexp "^[ \n\r]*[+-]?[0-9]*\\.[0-9]+[ \n\r]*$" in
    if text = "" then Sequence [] else
      if String.starts_with ~prefix:"==\n" text
      then def_expr_of_string (String.sub text 3 (String.length text - 3))
      else if String.starts_with ~prefix:"=" text then
        def_expr_of_string ("return "^(String.sub text 1 (String.length text - 1)))
      else if Pcre.pmatch ~rex:int_pattern text then
        let n = int_of_string (String.trim text) in
        Return (Int n)
      else
        if Pcre.pmatch ~rex:float_pattern text then
          let f = float_of_string (String.trim text) in
          Return (Float f)
        else
          Return (List (List.map (fun c -> Char c) (List.of_seq (String.to_seq text))))


let set_config_text (text : string) (max_cost : int) (state : state) : state =
  let curr_text, curr_mc = match state.config_text with
    | Saved (t, _) -> t, state.max_cost
    | NotSaved (t, c, _, _, _) -> t, c in
  if text = curr_text && max_cost = curr_mc then state
  else
  let curr_saved_text, saved_value = match state.config_text with
    | Saved (t, v) -> (t, v)
    | NotSaved (_, _, _, t, v) -> (t, v) in
  if text = curr_saved_text && max_cost = state.max_cost then
    {state with config_text = Saved (curr_saved_text, saved_value)}
  else
  try
    let def_expr = def_expr_of_string text in
    let res, data = eval_def_expr def_expr {Start_env.start_data with max_cost = max_cost * 10} in
    let res = match res with
    | Ok (Some v) -> Ok v
    | Ok (None) -> Ok VNone
    | Error e -> Error e in
    match state.config_text with
    | Saved (t, v) ->
      {state with config_text = NotSaved(text, max_cost, Some (res, data.env), t, v)}
    | NotSaved (_, _, _, t, v) -> 
      {state with config_text = NotSaved(text, max_cost, Some (res, data.env), t, v)}
  with _ ->
    match state.config_text with
    | Saved(t, v) ->
      {state with config_text = NotSaved(text, max_cost, None, t, v)}
    | NotSaved(_, _, _, t, v) ->
      {state with config_text = NotSaved(text, max_cost, None, t, v)}


let delete_sheet (sheet_name : string) (state : state) : state =
  let new_sheet_names = List.filter (fun name -> name <> sheet_name) state.sheet_names in
  let new_sheets_text = CellAddressMap.filter (fun (s, _) _ -> s <> sheet_name) state.sheets_text in
  let _, new_data = delete_spreadsheet sheet_name (data_of_state state) in
  let new_state = update_state_from_data new_data state in
  {new_state with sheet_names = new_sheet_names; sheets_text = new_sheets_text}


let rename_sheet (old_name : string) (new_name : string) (state : state) : state =
  if old_name = new_name || (List.mem new_name state.sheet_names) then state
  else if List.exists (fun name -> name = new_name) state.sheet_names then state
  else
  let new_sheet_names = List.map (fun name -> if name = old_name then new_name else name) state.sheet_names in
  let new_sheets_text = CellAddressMap.fold (fun (s, addr) ct acc ->
    let s' = if s = old_name then new_name else s in
    CellAddressMap.add (s', addr) ct acc
  ) state.sheets_text CellAddressMap.empty in
  let new_sheets_formatter = CellAddressMap.fold (fun (s, addr) fmt acc ->
    let s' = if s = old_name then new_name else s in
    CellAddressMap.add (s', addr) fmt acc
  ) state.sheets_formatter CellAddressMap.empty in
  let new_column_widths = StringIntMap.fold (fun (s, col) w acc ->
    let s' = if s = old_name then new_name else s in
    StringIntMap.add (s', col) w acc
  ) state.column_widths StringIntMap.empty in
  let new_row_heights = StringIntMap.fold (fun (s, row) h acc ->
    let s' = if s = old_name then new_name else s in
    StringIntMap.add (s', row) h acc
  ) state.row_heights StringIntMap.empty in
  let _, new_data = rename_spreadsheet old_name new_name (data_of_state state) in
  let new_state = update_state_from_data new_data state in
  {new_state with sheet_names = new_sheet_names; sheets_text = new_sheets_text; sheets_formatter = new_sheets_formatter; column_widths = new_column_widths; row_heights = new_row_heights}


let save_config (state : state) : state =
  match state.config_text with
  | Saved _ -> state
  | NotSaved (_, _, None, _, _) -> state
  | NotSaved (t, c, Some(v, env), _, _) ->
    let (_, data) = set_config env (data_of_state {state with max_cost = c}) in
    let state' = update_state_from_data data state in
    {state' with config_text = Saved(t, v)}


let rollback_config (state : state) : state =
  match state.config_text with
  | Saved _ -> state
  | NotSaved (_, _, _, t, v) -> {state with config_text = Saved (t, v)}


let set_cell (address : cell_address) (text : string) (state : state) : state =
  try
    let def_expr = def_expr_of_string_with_equals text in
    let start_data : data = {env = state.config; config = state.config; sheets = state.sheets; dependency_graph = state.dependency_graph; cell_address = Some address; cost = 0; max_cost = state.max_cost; fresh = state.fresh} in
    let (_, data) = set_cell_def def_expr start_data in
    let new_sheets_text = if text=""
      then CellAddressMap.remove address state.sheets_text
      else CellAddressMap.add address (Parsed text) state.sheets_text in
    let new_state = {state with config = state.config; sheets_text = new_sheets_text; sheets = data.sheets; dependency_graph = data.dependency_graph; fresh = data.fresh} in
    new_state
  with _ ->
      let last_successful_text =
        match CellAddressMap.find_opt address state.sheets_text with
        | Some (Parsed t) -> t
        | Some (NotParsed (t, _)) -> t
        | None -> ""
      in
      let new_sheets_text = CellAddressMap.add address (NotParsed (text, last_successful_text)) state.sheets_text in
      let new_state = {state with sheets_text = new_sheets_text} in
      new_state


let modify_column_width (sheet_name : string) (column : int) (change : int) (state : state) : state =
  let current_width = match StringIntMap.find_opt (sheet_name, column) state.column_widths with
    | Some w -> w
    | None -> state.default_column_width
  in
  let new_width = max 3 (current_width + change) in
  let new_column_widths = StringIntMap.add (sheet_name, column) new_width state.column_widths in
  {state with column_widths = new_column_widths}


let modify_row_height (sheet_name : string) (row : int) (change : int) (state : state) : state =
  let current_height = match StringIntMap.find_opt (sheet_name, row) state.row_heights with
    | Some h -> h
    | None -> state.default_row_height
  in
  let new_height = max 1 (current_height + change) in
  let new_row_heights = StringIntMap.add (sheet_name, row) new_height state.row_heights in
  {state with row_heights = new_row_heights}


let get_column_width (sheet_name : string) (column : int) (state : state) : int =
  match StringIntMap.find_opt (sheet_name, column) state.column_widths with
  | Some w -> w
  | None -> state.default_column_width


let get_row_height (sheet_name : string) (row : int) (state : state) : int =
  match StringIntMap.find_opt (sheet_name, row) state.row_heights with
  | Some h -> h
  | None -> state.default_row_height


let get_start_data_with_config (state : state) : data =
  {env = state.config; config = state.config; sheets = CellAddressMap.empty; dependency_graph = CellAddressMap.empty; cell_address = None; cost = 0; max_cost = state.max_cost; fresh = state.fresh}


let get_cell_text (address : cell_address) (state : state) : string =
  let width = match StringIntMap.find_opt (fst address, fst (snd address)) state.column_widths with
    | Some w -> w
    | None -> state.default_column_width
  in
  let height = match StringIntMap.find_opt (fst address, snd (snd address)) state.row_heights with
    | Some h -> h
    | None -> state.default_row_height
  in
  let start_data = get_start_data_with_config state in
  let start_data = {start_data with max_cost = state.max_cost / 10} in
  let formatter, _found =
    match CellAddressMap.find_opt address state.sheets_formatter with
    | None -> (match StringMap.find_opt "default_formatter" state.config.vars with
      | None -> Start_env.default_formatter, false
      | Some def_expr -> def_expr, true)
    | Some (_, def_opt) -> (match def_opt with
      | Some def_expr -> def_expr, true
      | None -> Start_env.default_formatter, false)
  in
  let value_to_repr v =
    let repr_value, _data = eval_call formatter [v; VInt width; VInt height] start_data in
    let res = (match repr_value with
    | Ok (VList chars) ->
      let chars = SekP.to_list chars in
      let rec build_string chars acc =
        (match chars with
        | [] -> Some (String.of_seq (List.to_seq (List.rev acc)))
        | VChar c :: rest -> build_string rest (c::acc)
        | _ :: _rest -> None)
      in
      build_string chars []
    | _ -> None)
    in
    match res with
    | Some s -> s, true
    | None ->
      let repr_value, _ = eval_call Start_env.default_formatter [v; VInt width; VInt height] start_data in
      match repr_value with 
      | Ok (VList chars) ->
        let chars = SekP.to_list chars in
        let rec build_string chars acc =
          (match chars with
          | [] -> String.of_seq (List.to_seq (List.rev acc))
          | VChar c :: rest -> build_string rest (c::acc)
          | _ :: rest -> build_string rest acc)
        in
        build_string chars [], false
      | _ -> "", false
  in
  match CellAddressMap.find_opt address state.sheets with
  | None -> string_to_box "" width height
  | Some cell -> cell.value |> function
    | Computed v -> string_to_box (fst(value_to_repr v)) width height
    | ComputingError err ->
      if height = 1 then
        string_to_box ("Error: " ^ (fst(value_to_repr err))) width height
      else
        string_to_box ("Error:\n" ^ (fst(value_to_repr err))) width height
    | NotComputedYet -> string_to_box "" width height
    | Computing -> string_to_box "" width height


let type_text_of_value (v : Values.value) (env : type_env) : string =
  let value_type = type_of_value v in
    let name = (StringMap.fold (fun k v acc ->
      match acc with
      | Some _ -> acc
      | None -> if v = value_type then Some k else None
    ) env None) in
    match name with
    | Some n -> n
    | None -> "???"


let get_cell_box (address : cell_address) (width : int) (height : int) (state : state) : string =
  let start_data = get_start_data_with_config state in
  let start_data = {start_data with max_cost = state.max_cost / 10} in
  let formatter, found =
    match CellAddressMap.find_opt address state.sheets_formatter with
    | None -> (match StringMap.find_opt "default_formatter" state.config.vars with
      | None -> Start_env.default_formatter, false
      | Some def_expr -> def_expr, true)
    | Some (_, def_opt) -> (match def_opt with
      | Some def_expr -> def_expr, true
      | None -> Start_env.default_formatter, false)
  in
  let value_to_repr v =
    let repr_value, _data = eval_call formatter [v; VInt width; VInt (height-1)] start_data in
    let res = (match repr_value with
    | Ok (VList chars) ->
      let chars = SekP.to_list chars in
      let rec build_string chars acc =
        (match chars with
        | [] -> Some (String.of_seq (List.to_seq (List.rev acc)))
        | VChar c :: rest -> build_string rest (c::acc)
        | _ :: _rest -> None)
      in
      build_string chars []
    | _ -> None)
    in
    match res with
    | Some s -> s, true
    | None ->
      let repr_value, _ = eval_call Start_env.default_formatter [v; VInt width; VInt (height-1)] start_data in
      match repr_value with 
      | Ok (VList chars) ->
        let chars = SekP.to_list chars in
        let rec build_string chars acc =
          (match chars with
          | [] -> String.of_seq (List.to_seq (List.rev acc))
          | VChar c :: rest -> build_string rest (c::acc)
          | _ :: rest -> build_string rest acc)
        in
        build_string chars [], false
      | _ -> "", false
  in
  let res_to_str v b=
    let str, success = value_to_repr v in
    let is_long = width >= 30 in
    let header = (if b then "Value" else "Error") ^ (if success && found then ":\n" else (if is_long then " (default formatter):\n" else " (!df):\n")) in
    string_to_box (header ^ str) width height
  in
  match CellAddressMap.find_opt address state.sheets with
  | None -> string_to_box "Value:\nNone" width height
  | Some cell -> cell.value |> function
    | Computed v -> res_to_str v true
    | ComputingError err -> res_to_str err false
    | NotComputedYet -> res_to_str VNone true
    | Computing -> res_to_str VNone true


let get_cell_definition (address : cell_address) (state : state) : cell_text =
  match CellAddressMap.find_opt address state.sheets_text with
  | None -> Parsed ""
  | Some ct -> ct


let get_cell_formatter (address : cell_address) (state : state) : cell_text=
  match CellAddressMap.find_opt address state.sheets_formatter with
  | None -> Parsed "return default_formatter"
  | Some (t, _) -> t


let set_cell_formatter (address : cell_address) (text : string) (state : state) : state =
  try
    if text = "" || text = "return default_formatter" then
      let new_sheets_formatter = CellAddressMap.remove address state.sheets_formatter in
      {state with sheets_formatter = new_sheets_formatter}
    else
    let def_expr = def_expr_of_string text in
    let start_data = get_start_data_with_config state in
    let start_data = {start_data with max_cost = state.max_cost / 10} in
    let def_res, _ = eval_def_expr def_expr start_data in
    let def_value = match def_res with
    | Ok (Some v) -> Some v
    | Ok (None) -> Some VNone
    | Error _ -> None in
    let new_sheets_formatter = CellAddressMap.add address (Parsed text, def_value) state.sheets_formatter in
    let new_state = {state with sheets_formatter = new_sheets_formatter} in
    new_state
  with _ ->
      let last_successful =
        match CellAddressMap.find_opt address state.sheets_formatter with
        | Some (Parsed t, d) -> t, d
        | Some (NotParsed (_, last_successful_text), d) -> last_successful_text, d
        | None -> "return default_formatter", state.config.vars |> StringMap.find_opt "default_formatter"
      in
      let new_sheets_formatter = CellAddressMap.add address (NotParsed (text, fst last_successful), snd last_successful) state.sheets_formatter in
      let new_state = {state with sheets_formatter = new_sheets_formatter} in
      new_state


let get_cell_type_text (address : cell_address) (state : state) : string =
  match CellAddressMap.find_opt address state.sheets with
  | None -> "None"
  | Some cell -> cell.value |> function
    | Computed v -> type_text_of_value v state.config.types
    | ComputingError err -> "Error(" ^ type_text_of_value err state.config.types ^ ")"
    | NotComputedYet -> "None"
    | Computing -> "None"


let start_config =
"def min(a, b):
    if a <= b:
        return a
    return b

def max(a, b):
    if a >= b:
        return a
    return b

def sum(lst):
    total = 0
    for item in lst:
        total += item
    return total

def enumerate(lst):
    lst = list(lst)
    return [[i, lst[i]] for i in range(len(lst))]

def reverse(lst):
    if not (lst : list):
        raise InvalidArgumentsError
    l = len(lst)
    return [lst[l-1-i] for i in range(l)]

def @(matrix1, matrix2):
    # matrix multiplication
    if not matrix1 : matrix:
        raise InvalidArgumentsError
    if not matrix2 : matrix:
        raise InvalidArgumentsError

    num_cols1 = len(matrix1)[1]
    num_rows2 = len(matrix2)[0]
    if num_cols1 != num_rows2:
        raise \"Can\'t multiply matrices with incompatible dimensions\"
    
    return [|
            [
                sum([v[0]*v[1] for v in zip(row, col)])
                for col in columns(matrix2)
            ]
            for row in rows(matrix1)
        |]

"


let env_with_start_config, start_config_res =
  let def_expr = def_expr_of_string start_config in
  let res, data = eval_def_expr def_expr Start_env.start_data in
  data.env, (match res with
  | Ok (Some v) -> Ok v
  | Ok (None) -> Ok VNone
  | Error e -> Error e)


let start_state : state = {
  config_text = Saved(start_config, start_config_res);
  config = env_with_start_config;
  sheets_text = CellAddressMap.empty;
  sheets_formatter = CellAddressMap.empty;
  sheets = CellAddressMap.empty;
  sheet_names = ["Sheet1"];
  dependency_graph = CellAddressMap.empty;
  max_cost = 500000;
  fresh = 0;
  default_column_width = 13;
  default_row_height = 1;
  column_widths = StringIntMap.empty;
  row_heights = StringIntMap.empty;
  filename = "Untitled.json"
}


module Saved_state_json = struct
  type ('a, 'b) stdlib_result = ('a, 'b) Stdlib.result =
    | Ok of 'a
    | Error of 'b

  type 'a result = ('a, string) stdlib_result

  type saved_state = {
    config_text : string * ((string*int) option);
    sheets_text : ((string*(int*int)) * (string * (string option))) list;
    sheets_formatter : ((string*(int*int)) * (string * (string option))) list;
    sheet_names : string list;
    max_cost : int;
    default_column_width : int;
    default_row_height : int;
    column_widths : ((string * int) * int) list;
    row_heights : ((string * int) * int) list;
  }
  [@@deriving yojson]
end

type saved_state = Saved_state_json.saved_state
let saved_state_to_yojson = Saved_state_json.saved_state_to_yojson
let saved_state_of_yojson = Saved_state_json.saved_state_of_yojson


let state_to_saved_state (state : state) : saved_state =
  let config_text_str, config_text_last =
    match state.config_text with
    | Saved (t, _) -> t, None
    | NotSaved (last, c, _, t, _) -> t, Some (last, c)
  in
  let sheets_text_list = CellAddressMap.fold (fun addr ct acc ->
    let ct_str, ct_last =
      match ct with
      | Parsed t -> t, None
      | NotParsed (last, t) -> t, Some last
    in
    (addr, (ct_str, ct_last)) :: acc
  ) state.sheets_text [] in
  let sheets_formatter_list = CellAddressMap.fold (fun addr (ct, _) acc ->
    let ct_str, ct_last =
      match ct with
      | Parsed t -> t, None
      | NotParsed (last, t) -> t, Some last
    in
    (addr, (ct_str, ct_last)) :: acc
  ) state.sheets_formatter [] in
  {
    config_text = (config_text_str, config_text_last);
    sheets_text = sheets_text_list;
    sheets_formatter = sheets_formatter_list;
    sheet_names = state.sheet_names;
    max_cost = state.max_cost;
    default_column_width = state.default_column_width;
    default_row_height = state.default_row_height;
    column_widths = StringIntMap.bindings state.column_widths;
    row_heights = StringIntMap.bindings state.row_heights;
  }


let saved_state_to_state (saved_state : saved_state) : state =
  let sheets_text, sheets = List.fold_left (fun acc (addr, (ct_str, ct_last)) ->
    if ct_str = "" && ct_last = None then acc else
    let ct =
      match ct_last with
      | None -> Parsed ct_str
      | Some last -> NotParsed (last, ct_str)
    in
    let def = def_expr_of_string_with_equals ct_str in
    let cell_value = {value = NotComputedYet; definition = def; dependencies = CellAddressSet.empty} in
    CellAddressMap.add addr ct (fst acc), CellAddressMap.add addr cell_value (snd acc)
  ) (CellAddressMap.empty, CellAddressMap.empty) saved_state.sheets_text in
  let config_text, config_env =
    let res, data' = 
      let def_expr = def_expr_of_string (fst saved_state.config_text) in
      eval_def_expr def_expr {Start_env.start_data with max_cost = saved_state.max_cost * 10}
    in
    let res = match res with
    | Ok (Some v) -> Ok v
    | Ok (None) -> Ok VNone
    | Error e -> Error e in
    let env = data'.env in
    match snd saved_state.config_text with
    | None -> Saved (fst saved_state.config_text, res), env
    | Some (last, c) ->
      try
        let def_expr = def_expr_of_string last in
        let res_last, data' = eval_def_expr def_expr {Start_env.start_data with max_cost = saved_state.max_cost * 10} in
        let res_last = match res_last with
        | Ok (Some v) -> Ok v
        | Ok (None) -> Ok VNone
        | Error e -> Error e in
        NotSaved (last, c, Some (res_last, data'.env), fst saved_state.config_text, res), env
      with _ ->
        NotSaved (last, c, None, fst saved_state.config_text, res), env
  in
  let data : data = {
    env = config_env;
    config = config_env;
    sheets = sheets;
    dependency_graph = CellAddressMap.empty;
    cell_address = None;
    cost = 0;
    max_cost = saved_state.max_cost;
    fresh = 0;
  } in
  let data = set_config config_env data |> snd in
  let sheets_formatter = List.fold_left (fun acc (addr, (ct_str, ct_last)) ->
    let ct =
      match ct_last with
      | None -> Parsed ct_str
      | Some last -> NotParsed (last, ct_str)
    in
    let value_opt =
      try
        if ct_str = "" || ct_str = "return default_formatter" then
          None
        else
          let def_expr = def_expr_of_string ct_str in
          let start_data = {data with sheets = CellAddressMap.empty; cost = 0; max_cost = saved_state.max_cost / 10} in
          let def_res, _ = eval_def_expr def_expr start_data in
          match def_res with
          | Ok (Some v) -> Some v
          | Ok (None) -> Some VNone
          | Error _ -> None
      with _ -> None
    in
    CellAddressMap.add addr (ct, value_opt) acc
  ) CellAddressMap.empty saved_state.sheets_formatter in
  {
    config_text = config_text;
    config = config_env;
    sheets_text = sheets_text;
    sheets_formatter = sheets_formatter;
    sheets = data.sheets;
    sheet_names = saved_state.sheet_names;
    dependency_graph = data.dependency_graph;
    max_cost = saved_state.max_cost;
    fresh = 0;
    default_column_width = saved_state.default_column_width;
    default_row_height = saved_state.default_row_height;
    column_widths = List.fold_left (fun acc ((sheet_name, column), width) ->
      StringIntMap.add (sheet_name, column) width acc
    ) StringIntMap.empty saved_state.column_widths;
    row_heights = List.fold_left (fun acc ((sheet_name, row), height) ->
      StringIntMap.add (sheet_name, row) height acc
    ) StringIntMap.empty saved_state.row_heights;
    filename = "Untitled.json"
  }


let save_to_file (state : state) : unit =
  let saved_state = state_to_saved_state state in
  let json = saved_state_to_yojson saved_state in
  let oc = open_out state.filename in
  Yojson.Safe.pretty_to_channel oc json;
  close_out oc


let load_from_file (filename : string) : state =
  let ic = open_in filename in
  let json = Yojson.Safe.from_channel ic in
  close_in ic;
  let saved_state = saved_state_of_yojson json in
  match saved_state with
  | Ok ss -> {(saved_state_to_state ss) with filename = filename}
  | Error err -> failwith ("Error loading state from file: " ^ err)

