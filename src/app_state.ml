open Text_editor

type mode =
| View
| CellDef of bool * editor_state
| Formatter of bool * editor_state
| Config of int * editor_state * editor_state
| SheetsList of int * int * (editor_state option)
| SaveFile of (editor_state * bool) option
| Manual of editor_state

module StringMap = Map.Make(String)

type undo_item = {
  mode : mode;
  service_state : Text_service.state;
  selected_sheet : string;
  selected_cell : (int * int) StringMap.t;
  top_left_cell : (int * int) StringMap.t;
}

type app_state = {
  mode : mode;
  service_state : Text_service.state;
  selected_sheet : string;
  selected_cell : (int * int) StringMap.t;
  top_left_cell : (int * int) StringMap.t;
  width : int;
  height : int;
  bottom_height : int;
  value_box_width : int;
  undo_stack : undo_item list;
  redo_stack : undo_item list;
}


let start_app_state w h = {
    mode = View;
    service_state = Text_service.start_state;
    selected_sheet = "Sheet1";
    selected_cell = StringMap.empty |> StringMap.add "Sheet1" (1,1);
    top_left_cell = StringMap.empty |> StringMap.add "Sheet1" (1,1);
    width = w;
    height = h;
    bottom_height = if h >= 45 then 12 else 8;
    value_box_width = if w >= 90 then 30 else 20;
    undo_stack = [];
    redo_stack = [];
  }


let undo_app_state app_state =
  match app_state.undo_stack with
  | [] -> app_state
  | x::xs ->
      let redo_item = {
        mode = app_state.mode;
        service_state = app_state.service_state;
        selected_sheet = app_state.selected_sheet;
        selected_cell = app_state.selected_cell;
        top_left_cell = app_state.top_left_cell;
      } in
      {
        mode = x.mode;
        service_state = x.service_state;
        selected_sheet = x.selected_sheet;
        selected_cell = x.selected_cell;
        top_left_cell = x.top_left_cell;
        width = app_state.width;
        height = app_state.height;
        bottom_height = app_state.bottom_height;
        value_box_width = app_state.value_box_width;
        undo_stack = xs;
        redo_stack = redo_item :: app_state.redo_stack;
      }


let redo_app_state app_state =
  match app_state.redo_stack with
  | [] -> app_state
  | x::xs ->
      let undo_item = {
        mode = app_state.mode;
        service_state = app_state.service_state;
        selected_sheet = app_state.selected_sheet;
        selected_cell = app_state.selected_cell;
        top_left_cell = app_state.top_left_cell;
      } in
      {
        mode = x.mode;
        service_state = x.service_state;
        selected_sheet = x.selected_sheet;
        selected_cell = x.selected_cell;
        top_left_cell = x.top_left_cell;
        width = app_state.width;
        height = app_state.height;
        bottom_height = app_state.bottom_height;
        value_box_width = app_state.value_box_width;
        undo_stack = undo_item :: app_state.undo_stack;
        redo_stack = xs;
      }


let push_undo mode app_state =
  let undo_item = {
    mode = mode;
    service_state = app_state.service_state;
    selected_sheet = app_state.selected_sheet;
    selected_cell = app_state.selected_cell;
    top_left_cell = app_state.top_left_cell;
  } in
  let undo_stack = if List.length app_state.undo_stack >= 50 then
    List.rev (List.tl (List.rev app_state.undo_stack))
  else
    app_state.undo_stack in
  { 
    app_state with
    undo_stack = undo_item :: undo_stack;
    redo_stack = [];
  }


let next_sheet app_state =
  let curr_sheet = app_state.selected_sheet in 
  let rec get_next lst =
    match lst with
    | [] -> app_state
    | x :: xs -> if x <> curr_sheet then get_next xs else
      (match xs with
      | [] -> app_state
      | y :: _ -> {app_state with selected_sheet = y})
  in get_next app_state.service_state.sheet_names


let prev_sheet app_state =
  let curr_sheet = app_state.selected_sheet in 
  let rec get_next lst =
    match lst with
    | [] -> app_state
    | x :: xs -> if x <> curr_sheet then get_next xs else
      (match xs with
      | [] -> app_state
      | y :: _ -> {app_state with selected_sheet = y})
  in get_next (List.rev app_state.service_state.sheet_names)


let add_sheet sheet_name pos app_state =
  if List.mem sheet_name app_state.service_state.sheet_names then
    app_state
  else
  let new_sheet_names =
    let rec insert lst pos =
      match lst with
      | [] -> [sheet_name]
      | x::xs ->
        if pos=1 then sheet_name :: lst
        else x :: insert xs (pos - 1)
    in insert app_state.service_state.sheet_names pos
  in
  let new_service_state = {app_state.service_state with sheet_names = new_sheet_names} in
  let new_selected = StringMap.add sheet_name (1,1) app_state.selected_cell in
  let new_top_left = StringMap.add sheet_name (1,1) app_state.top_left_cell in
  {app_state with service_state = new_service_state; selected_cell = new_selected; top_left_cell = new_top_left}


let rename_sheet new_name pos app_state =
  if new_name = "" || (List.mem new_name app_state.service_state.sheet_names) then app_state else
  let curr_name = List.nth app_state.service_state.sheet_names (pos - 1) in
  let new_service_state = Text_service.rename_sheet curr_name new_name app_state.service_state in
  let curr_selected = StringMap.find curr_name app_state.selected_cell in
  let new_selected = StringMap.add new_name curr_selected (StringMap.remove curr_name app_state.selected_cell) in
  let curr_top_left = StringMap.find curr_name app_state.top_left_cell in
  let new_top_left = StringMap.add new_name curr_top_left (StringMap.remove curr_name app_state.top_left_cell) in
  let new_selected_sheet = if app_state.selected_sheet = curr_name then new_name else app_state.selected_sheet in
  {app_state with service_state = new_service_state; selected_sheet = new_selected_sheet; selected_cell = new_selected; top_left_cell = new_top_left}


let delete_sheet sheet_name app_state =
  let new_sheet_names, pos =
    let rec delete lst acc pos =
      match lst with
      | [] -> List.rev acc, pos
      | x::xs ->
        if x = sheet_name then
          delete xs acc (-pos)
        else
          delete xs (x :: acc) (if pos > 0 then pos else (pos - 1))
    in delete app_state.service_state.sheet_names [] (-1)
  in
  if pos < 0 then app_state else
  let new_service_state = Text_service.delete_sheet sheet_name app_state.service_state in
  let new_selected = StringMap.remove sheet_name app_state.selected_cell in
  let new_top_left = StringMap.remove sheet_name app_state.top_left_cell in
  let new_selected_sheet =
    if app_state.selected_sheet = sheet_name then
      if List.length new_sheet_names = 0 then "Sheet1"
      else if pos <= List.length new_sheet_names then
        List.nth new_sheet_names (pos - 1)
      else
        List.nth new_sheet_names (List.length new_sheet_names - 1)
    else
      app_state.selected_sheet
  in
  let new_app_state = {app_state with service_state = new_service_state; selected_sheet = new_selected_sheet; selected_cell = new_selected; top_left_cell = new_top_left} in
  if new_sheet_names = [] then
    add_sheet "Sheet1" 1 new_app_state
  else
    new_app_state


let move_selection_up app_state =
  let curr_sheet = app_state.selected_sheet in
  let (col, row) = StringMap.find curr_sheet app_state.selected_cell in
  if row > 1 then
    {app_state with selected_cell = StringMap.add curr_sheet (col, row - 1) app_state.selected_cell}
  else
    app_state


let move_selection_down app_state =
  let curr_sheet = app_state.selected_sheet in
  let (col, row) = StringMap.find curr_sheet app_state.selected_cell in
  {app_state with selected_cell = StringMap.add curr_sheet (col, row + 1) app_state.selected_cell}


let move_selection_left app_state =
  let curr_sheet = app_state.selected_sheet in
  let (col, row) = StringMap.find curr_sheet app_state.selected_cell in
  if col > 1 then
    {app_state with selected_cell = StringMap.add curr_sheet (col - 1, row) app_state.selected_cell}
  else
    app_state


let move_selection_right app_state =
  let curr_sheet = app_state.selected_sheet in
  let (col, row) = StringMap.find curr_sheet app_state.selected_cell in
  {app_state with selected_cell = StringMap.add curr_sheet (col + 1, row) app_state.selected_cell}


let show_definition active app_state =
  let (col, row) = StringMap.find app_state.selected_sheet app_state.selected_cell in
  let cell_text = Text_service.get_cell_definition (app_state.selected_sheet, (col, row)) app_state.service_state in
  let def_text = (match cell_text with 
    | NotParsed (c, _) -> c
    | Parsed (p) -> p)
  in
  {app_state with mode = CellDef (active, Text_editor.text_to_editor def_text)}


let show_formatter active app_state =
  let (col, row) = StringMap.find app_state.selected_sheet app_state.selected_cell in
  let cell_text = Text_service.get_cell_formatter (app_state.selected_sheet, (col, row)) app_state.service_state in
  let fmt_text = (match cell_text with 
    | NotParsed (c, _) -> c
    | Parsed (p) -> p)
  in
  {app_state with mode = Formatter (active, Text_editor.text_to_editor fmt_text)}


let show_sheets_list app_state =
  let curr_sheet_pos = 
    let rec find_pos lst pos =
      match lst with
      | [] -> 0
      | x::xs -> if x = app_state.selected_sheet then pos else find_pos xs (pos + 1)
    in find_pos app_state.service_state.sheet_names 1
  in
  {app_state with mode = SheetsList (1, curr_sheet_pos, None)}

