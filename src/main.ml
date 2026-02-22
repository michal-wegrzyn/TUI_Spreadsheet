open Notty
open Notty_unix
open Print
open Text_editor
open App_state


let image_of_multiline attr s =
  s
  |> String.split_on_char '\n'
  |> List.map (I.string attr)
  |> I.vcat


let singleline_image text width =
  I.string A.empty (String.sub (text ^ (String.make (max (width - String.length text) 0) ' ')) 0 width)


let sheets_footer active app_state =
  let prev_sheets, next_sheets = 
    let rec it lst acc =
      match lst with
      | [] -> acc, []
      | x::xs -> if x = app_state.selected_sheet then (acc, xs) else
        it xs (x :: acc)
    in it app_state.service_state.sheet_names []
  in
  let left_arrow = if prev_sheets = [] then "" else if active then "[<] " else " <  " in
  let right_arrow = if next_sheets = [] then "" else if active then " [>]" else "  > " in
  let max_len = app_state.width / 3 - 5 in
  let curr_sheet = if String.length app_state.selected_sheet <= max_len then app_state.selected_sheet else (String.sub app_state.selected_sheet 0 (max_len - 1)) ^ "…" in
  let width_remaining = app_state.width - String.length left_arrow - String.length right_arrow - (min max_len (String.length curr_sheet)) in
  let image = I.string A.(st underline) curr_sheet in
  let max_len = app_state.width / 5 - 3 in
  let rec build_image image prev next width =
    let image, prev, width = match prev with
    | [] -> image, prev, width
    | x::xs ->
      let x' = if String.length x <= max_len then x else (String.sub x 0 (max_len - 1)) ^ "…" in
      let x'_len = min max_len (String.length x') in
      if x'_len + 3 <= width - (if xs = [] then 0 else 4) - (if next = [] then 0 else 4) then
        (I.hcat [I.string A.empty (x' ^ " | "); image]), xs, (width - x'_len - 3)
      else
        if xs = [] then image, [], width else (I.hcat [I.string A.empty "… | "; image]), [], width - 4
    in
    let image, next, width = match next with
    | [] -> image, [], width
    | x::xs ->
      let x' = if String.length x <= max_len then x else (String.sub x 0 (max_len - 1)) ^ "…" in
      let x'_len = min max_len (String.length x') in
      if x'_len + 3 <= width - (if xs = [] then 0 else 4) - (if prev = [] then 0 else 4) then
        (I.hcat [image; I.string A.empty (" | " ^ x')]), xs, (width - x'_len - 3)
      else
        if xs = [] then image, [], width else (I.hcat [image; I.string A.empty " | …"]), [], width - 4
    in
    if prev = [] && next = [] then I.hcat [image; I.string A.empty (String.make (max 0 width) ' ')] else
     build_image image prev next width
  in
  let image = build_image image prev_sheets next_sheets width_remaining in
  I.hcat [I.string A.empty left_arrow; image; I.string A.empty right_arrow]


let display_sheet app_state width height =
  let height = height - 1 in
  let sheet_name = app_state.selected_sheet in
  let col, row = StringMap.find sheet_name app_state.top_left_cell in
  let scol, srow = StringMap.find sheet_name app_state.selected_cell in
  let cell_content c r = image_of_multiline A.empty (Text_service.get_cell_text (sheet_name, (c, r)) app_state.service_state) in
  let row_heights =
    let rec collect_heights r remaining acc =
      let curr_height = Text_service.get_row_height sheet_name r app_state.service_state in
      if remaining <= curr_height then
        List.rev (curr_height :: acc)
      else
        collect_heights (r + 1) (remaining - curr_height - 1) (curr_height :: acc)
    in
    collect_heights row (height - 2) []
  in
  let last_row_number = row + List.length row_heights - 1 in
  let row_digits = min (String.length (string_of_int last_row_number)) 5 in
  let col_widths =
    let rec collect_widths c remaining acc =
      let curr_width = Text_service.get_column_width sheet_name c app_state.service_state in
      if remaining <= curr_width + 1 then
        List.rev (curr_width :: acc)
      else
        collect_widths (c + 1) (remaining - curr_width - 1) (curr_width :: acc)
    in
    collect_widths col (width - row_digits - 1) []
  in
  let col_name col width = Print.fit_center (Print.column_name col) width in
  let cols_str = 
    let text, _ = List.fold_left (fun acc w ->
      let str = fst acc in
      let col = snd acc in
      let str = str^(col_name col w)^"|" in
      (str, col + 1)
    ) ((String.make row_digits ' ')^"|", col) col_widths
    in String.sub text 0 width
  in
  let col_names_image = I.string A.empty cols_str in
  let line_str, line_str_selected_up, line_str_selected_down =
    let rec build_line c ws acc =
      match ws with
      | [] -> acc
      | w::ws_tail ->
        let (l1, l2, l3) = acc in
        let line_part = (Print.string_repeat w "─") ^ ("┼") in
        let line_part2 = if c = scol - 1 then (Print.string_repeat w "─") ^ ("╆") else
          if c = scol then (Print.string_repeat w "━") ^ ("╅") else line_part in
        let line_part3 = if c = scol - 1 then (Print.string_repeat w "─") ^ ("╄") else
          if c = scol then (Print.string_repeat w "━") ^ ("╃") else line_part in
        build_line (c + 1) ws_tail (l1 ^ line_part, l2 ^ line_part2, l3 ^ line_part3)
    in
    let start = ((Print.string_repeat row_digits "─") ^ ("┼")) in
    let start2 =
      if col = scol then ((Print.string_repeat row_digits "─") ^ ("╆")) else 
      start
    in
    let start3 =
      if col = scol then ((Print.string_repeat row_digits "─") ^ ("╄")) else 
      start
    in
    let (line1, line2, line3) = build_line col col_widths (start, start2, start3) in
    let line1 = String.sub line1 0 (width * 3) in
    let line2 = String.sub line2 0 (width * 3) in
    let line3 = String.sub line3 0 (width * 3) in
    (line1, line2, line3)
  in
  let line_image = I.string A.empty line_str in
  let line_image_up = I.string A.empty line_str_selected_up in
  let line_image_down = I.string A.empty line_str_selected_down in
  let row_image r h =
    let row_number_str = Print.fit_center (string_of_int r) row_digits in
    let row_number_image = I.string A.empty row_number_str in
    let vline_str = String.trim (Print.string_repeat h "│\n") in
    let vline_str_selected = String.trim (Print.string_repeat h "┃\n") in
    let vline_image = image_of_multiline A.empty vline_str in
    let vline_image_selected = image_of_multiline A.empty vline_str_selected in
    let rec build_row c ws acc =
      match (ws) with
      | [] -> I.hcat (List.rev acc)
      | _w::ws_tail ->
        let cell_img = cell_content c r in
        let vline = if (c = scol || c+1 = scol) && r = srow then vline_image_selected else vline_image in
        build_row (c + 1) ws_tail (vline::cell_img :: acc)
    in
    let hline_image = 
      if r = srow then line_image_up
      else if r = srow + 1 then line_image_down
      else line_image
    in
    let vline = if col = scol && r = srow then vline_image_selected else vline_image in
    I.vcat [hline_image; (build_row col col_widths [vline; row_number_image])]
  in
  let selected_type = Text_service.get_cell_type_text (sheet_name, (scol, srow)) app_state.service_state in
  let image = I.vcat (col_names_image::(List.init (List.length row_heights) (fun i -> row_image (row + i) (List.nth row_heights i)))) in
  let image = I.crop ?r:(Some (I.width image - width)) ?b:(Some (I.height image - height)) image in
  let sheet_name = if String.length sheet_name <= width / 3 then sheet_name else (String.sub sheet_name 0 (width/3 - 1)) ^ "…" in
  let bottom_text = sheet_name ^ " : " ^ (Print.column_name scol) ^ (string_of_int srow) ^ " | type: " ^ (selected_type) in
  I.vcat [image; singleline_image bottom_text width]


let move_view app_state height = 
  let (tc, tr) = StringMap.find app_state.selected_sheet app_state.top_left_cell in
  let (sc, sr) = StringMap.find app_state.selected_sheet app_state.selected_cell in
  let tc = min sc tc in
  let tr = min sr tr in
  let smallest_row =
    let rec it row remaining =
      let curr_height = Text_service.get_row_height app_state.selected_sheet row app_state.service_state in
      if remaining <= curr_height || row = 0 then
        row + 1
      else
        it (row - 1) (remaining - curr_height - 1)
      in it sr (height - 3)
  in
  let smallest_col =
    let rec it col remaining =
      let curr_width = Text_service.get_column_width app_state.selected_sheet col app_state.service_state in
      if remaining <= curr_width || col = 0 then
        col + 1
      else
        it (col - 1) (remaining - curr_width - 1)
      in it sc (app_state.width - (min (String.length (string_of_int (sr + height / 2))) 5 + 1))
  in
  let tc = max tc smallest_col in
  let tr = max tr smallest_row in
  {app_state with top_left_cell = StringMap.add app_state.selected_sheet (tc, tr) app_state.top_left_cell}


let handle_resize app_state width height =
  let app_state = {app_state with width = width; height = height} in
  match app_state.mode with
  | View ->
      move_view app_state (height - 2)
  | CellDef (is_active, editor_state) | Formatter (is_active, editor_state) ->
      let view_height = max (height - app_state.bottom_height - 3) 10 in
      let bottom_height = max (height - view_height - 3) 5 in
      let app_state = move_view {app_state with bottom_height = bottom_height} (height - bottom_height - 3) in
      let editor_width = max (width - app_state.value_box_width - 1) 25 in
      let value_box_width = max (width - editor_width - 1) 15 in
      let app_state = {app_state with value_box_width = value_box_width; bottom_height = bottom_height} in
      (match app_state.mode with
      | CellDef _ -> {app_state with mode = CellDef (is_active, move_editor_view editor_state (app_state.width - value_box_width - 1) (bottom_height - 1))}
      | Formatter _ -> {app_state with mode = Formatter (is_active, move_editor_view editor_state (app_state.width - value_box_width - 1) (bottom_height - 1))}
      | _ -> app_state)
  | Config (is_active, editor_state, max_cost_editor)->
      let view_height = max (height - app_state.bottom_height - 3) 10 in
      let bottom_height = max (height - view_height - 3) 5 in
      let moved_editor = move_editor_view editor_state app_state.width (height - 2) in
      let moved_cost_editor = move_editor_view max_cost_editor app_state.width 1 in
    {app_state with bottom_height = bottom_height; mode = Config (is_active, moved_editor, moved_cost_editor)}
  | SheetsList (top_row, selected_row, editor) ->
    let new_top_row =
      if selected_row < top_row then
        selected_row
      else if selected_row >= top_row + (height - 3) then
        selected_row - (height - 3) + 1
      else
        top_row
    in {app_state with mode = SheetsList (new_top_row, selected_row, editor)}
  | SaveFile _ | Manual _ ->
      app_state


let rec loop term app_state =
  if app_state.width < 50 || app_state.height < 25 then
    let warning =
        ["Please resize the terminal";
        "to at least 50x25 characters.";
        "Current size: " ^ string_of_int app_state.width ^ "x" ^ string_of_int app_state.height]
    in
    let img = List.map (fun line -> I.string A.empty (Print.fit_center line app_state.width)) warning |> I.vcat in
    Term.image term img;
    match Term.event term with
    | `Resize (w, h) ->
        loop term {app_state with width = w; height = h}
    | _ ->
        loop term app_state
  else
  let app_state = handle_resize app_state app_state.width app_state.height in
  match app_state.mode with
  | View ->
    let img =
      let navbar = singleline_image ("[F]ile [D]efinition [R]Formatter [S]heets [C]onfig [Q]uit [H]elp") app_state.width in
      I.vcat [navbar; display_sheet app_state app_state.width (app_state.height - 2); sheets_footer true app_state]
    in
    Term.image term img;
    begin
    match Term.event term with
    | `Resize (w, h) ->
        loop term { app_state with width = w; height = h }
    | `Key (`ASCII 'q', _) ->
        ()
    | `Key (`ASCII 'h', _) ->
        loop term {app_state with mode = Manual (text_to_editor User_manual.user_manual)}
    | `Key (`ASCII 'f', _) ->
        loop term ( { app_state with mode = SaveFile None } )
    | `Key (`ASCII 'c', _) ->
        let curr_text, curr_max_cost =
          match app_state.service_state.config_text with
          | Saved (t, _) -> t, app_state.service_state.max_cost
          | NotSaved (t, c, _, _, _) -> t, c
        in
        loop term {app_state with mode = Config (0, text_to_editor curr_text, text_to_editor (string_of_int curr_max_cost))}
    | `Key (`ASCII 's', _) ->
        loop term (show_sheets_list app_state)
    | `Key (`ASCII 'd', _) ->
        loop term (show_definition false app_state)
    | `Key (`ASCII 'i', _) ->
        loop term (show_definition true app_state)
    | `Key (`ASCII '=', _) ->
        let app_state = show_definition true app_state in
        let editor_state = match app_state.mode with
          | CellDef (_is_active, editor_state) -> editor_state
          | _ -> failwith "Unexpected mode"
        in
        let new_editor_state = if editor_state = text_to_editor ""
          then {(text_to_editor "=") with cursor_col = 2}
          else editor_state in
        loop term {app_state with mode = CellDef (true, new_editor_state)}
    | `Key (`ASCII 'r', _) ->
        loop term (show_formatter false app_state)
    | `Key (`ASCII ',', _) | `Key (`ASCII '<', _) ->
        loop term (prev_sheet app_state)
    | `Key (`ASCII '.', _) | `Key (`ASCII '>', _) ->
        loop term (next_sheet app_state)
    | `Key (`ASCII 'z', _) ->
        let new_app_state = undo_app_state app_state in
        loop term new_app_state
    | `Key (`ASCII 'y', _) ->
        let new_app_state = redo_app_state app_state in
        loop term new_app_state
    | `Key (`Arrow `Up, []) ->
        loop term (move_selection_up app_state)
    | `Key (`Arrow `Down, []) | `Key (`Enter, _) ->
        loop term (move_selection_down app_state)
    | `Key (`Arrow `Left, []) ->
        loop term (move_selection_left app_state)
    | `Key (`Arrow `Right, []) | `Key (`Tab, _) ->
        loop term (move_selection_right app_state)
    | `Key (`Arrow `Up, [`Shift]) ->
        let (_, r) = StringMap.find app_state.selected_sheet app_state.selected_cell in
        let new_service_state = Text_service.modify_row_height app_state.selected_sheet r (-1) app_state.service_state in
        loop term {app_state with service_state = new_service_state}
    | `Key (`Arrow `Down, [`Shift]) ->
        let (_, r) = StringMap.find app_state.selected_sheet app_state.selected_cell in
        let new_service_state = Text_service.modify_row_height app_state.selected_sheet r 1 app_state.service_state in
        loop term {app_state with service_state = new_service_state}
    | `Key (`Arrow `Left, [`Shift]) ->
        let (c, _) = StringMap.find app_state.selected_sheet app_state.selected_cell in
        let new_service_state = Text_service.modify_column_width app_state.selected_sheet c (-1) app_state.service_state in
        loop term {app_state with service_state = new_service_state}
    | `Key (`Arrow `Right, [`Shift]) ->
        let (c, _) = StringMap.find app_state.selected_sheet app_state.selected_cell in
        let new_service_state = Text_service.modify_column_width app_state.selected_sheet c 1 app_state.service_state in
        loop term {app_state with service_state = new_service_state}
    | _ ->
        loop term app_state
    end


  | CellDef (is_active, editor_state) ->
    let scol, srow = StringMap.find app_state.selected_sheet app_state.selected_cell in
    let message, last_parsed = if is_active then "", None else
      match Text_service.get_cell_definition (app_state.selected_sheet, (scol, srow)) app_state.service_state with
      | NotParsed (_, p) -> " (Parsing failed)", Some p
      | Parsed (_) -> "", None
    in
    let sheets_height = app_state.height - app_state.bottom_height - 3 in
    let sheets_image = display_sheet app_state app_state.width sheets_height in
    let navbar = singleline_image ("[Esc]ape"^(if is_active then " (and evaluate)" else
      " [I]nsert [R]Formatter [S]heets"^(if last_parsed <> None then " [B]ack to last parsed" else ""))) app_state.width in
    let hline = I.string A.empty (Print.string_repeat app_state.width "─") in
    let value_box_text = Text_service.get_cell_box (app_state.selected_sheet, (scol, srow)) app_state.value_box_width app_state.bottom_height app_state.service_state in
    let value_box_image = image_of_multiline A.empty value_box_text in
    let vline = image_of_multiline A.empty (String.trim (Print.string_repeat app_state.bottom_height "│\n")) in
    let editor_title = singleline_image ("Definition:" ^ message) (app_state.width - app_state.value_box_width - 1) in
    let editor_image = display_editor editor_state (app_state.width - app_state.value_box_width - 1) (app_state.bottom_height-1) is_active in
    let bottom_image = I.hcat [value_box_image; vline; I.vcat [editor_title; editor_image]] in
    let sheets_footer_image = sheets_footer (not is_active) app_state in
    let img = I.vcat [navbar; sheets_image; hline; bottom_image; sheets_footer_image] in
    Term.image term img;
    if not is_active then
    begin
    match Term.event term with
    | `Resize (w, h) ->
        loop term { app_state with width = w; height = h }
    | `Key (`Escape, _) | `Key (`ASCII 'd', _) ->
        loop term {app_state with mode = View}
    | `Key (`ASCII 'i', _) ->
        loop term {app_state with mode = CellDef (true, editor_state)}
    | `Key (`ASCII 's', _) ->
        loop term (show_sheets_list app_state)
    | `Key (`ASCII '=', _) ->
        let new_editor_state = if editor_state = text_to_editor ""
          then {(text_to_editor "=") with cursor_col = 2}
          else editor_state in
        loop term {app_state with mode = CellDef (true, new_editor_state)}
    | `Key (`ASCII 'b', _) ->
        if last_parsed = None then
          loop term app_state
        else
        let (c, r) = StringMap.find app_state.selected_sheet app_state.selected_cell in
        let new_text_service = Text_service.set_cell (app_state.selected_sheet, (c, r)) (Option.get last_parsed) app_state.service_state in
        let app_state = push_undo app_state.mode app_state in
        let app_state = show_definition false {app_state with service_state = new_text_service} in
        loop term app_state
    | `Key (`ASCII 'r', _) ->
        loop term (show_formatter false app_state)
    | `Key (`ASCII ',', _) | `Key (`ASCII '<', _) ->
        loop term (app_state |> prev_sheet |> show_definition false)
    | `Key (`ASCII '.', _) | `Key (`ASCII '>', _) ->
        loop term (app_state |> next_sheet |> show_definition false)
    | `Key (`ASCII 'z', _) ->
        let new_app_state = undo_app_state app_state in
        loop term new_app_state
    | `Key (`ASCII 'y', _) ->
        let new_app_state = redo_app_state app_state in
        loop term new_app_state
    | `Key (`Arrow `Up, []) ->
        loop term (app_state |> move_selection_up |> show_definition false)
    | `Key (`Arrow `Down, []) | `Key (`Enter, _) ->
        loop term (app_state |> move_selection_down |> show_definition false)
    | `Key (`Arrow `Left, []) ->
        loop term (app_state |> move_selection_left |> show_definition false)
    | `Key (`Arrow `Right, []) | `Key (`Tab, _) ->
        loop term (app_state |> move_selection_right |> show_definition false)
    | `Key (`Arrow `Up, [`Ctrl]) ->
        loop term {app_state with bottom_height = app_state.bottom_height + 1}
    | `Key (`Arrow `Down, [`Ctrl]) ->
        let new_height = max 5 (app_state.bottom_height - 1) in
        loop term {app_state with bottom_height = new_height}
    | `Key (`Arrow `Left, [`Ctrl]) ->
        let new_width = max 10 (app_state.value_box_width - 1) in
        loop term {app_state with value_box_width = new_width}
    | `Key (`Arrow `Right, [`Ctrl]) ->
        loop term {app_state with value_box_width = app_state.value_box_width + 1}
    | `Key (`Page `Up, _) ->
        let new_editor_state = move_editor_up_by editor_state (app_state.bottom_height - 2) in
        loop term {app_state with mode = CellDef (false, new_editor_state)}
    | `Key (`Page `Down, _) ->
        let new_editor_state = move_editor_down_by editor_state (app_state.bottom_height - 2) in
        loop term {app_state with mode = CellDef (false, new_editor_state)}
    | `Key (`Arrow `Up, [`Shift]) ->
        let (_, r) = StringMap.find app_state.selected_sheet app_state.selected_cell in
        let new_service_state = Text_service.modify_row_height app_state.selected_sheet r (-1) app_state.service_state in
        loop term {app_state with service_state = new_service_state}
    | `Key (`Arrow `Down, [`Shift]) ->
        let (_, r) = StringMap.find app_state.selected_sheet app_state.selected_cell in
        let new_service_state = Text_service.modify_row_height app_state.selected_sheet r 1 app_state.service_state in
        loop term {app_state with service_state = new_service_state}
    | `Key (`Arrow `Left, [`Shift]) ->
        let (c, _) = StringMap.find app_state.selected_sheet app_state.selected_cell in
        let new_service_state = Text_service.modify_column_width app_state.selected_sheet c (-1) app_state.service_state in
        loop term {app_state with service_state = new_service_state}
    | `Key (`Arrow `Right, [`Shift]) ->
        let (c, _) = StringMap.find app_state.selected_sheet app_state.selected_cell in
        let new_service_state = Text_service.modify_column_width app_state.selected_sheet c 1 app_state.service_state in
        loop term {app_state with service_state = new_service_state}
    | _ ->
        loop term app_state
    end
  else
    begin
    match Term.event term with 
    | `Resize (w, h) ->
        loop term { app_state with width = w; height = h }
    | `Key (`Escape, _) -> 
        let editor_title = singleline_image "Definition: [Computing]" (app_state.width - app_state.value_box_width - 1) in
        let bottom_image = I.hcat [value_box_image; vline; I.vcat [editor_title; editor_image]] in
        let img = I.vcat [navbar; sheets_image; hline; bottom_image; sheets_footer_image] in
        Term.image term img;
        let new_text = text_zipper_to_text editor_state.text in
        let (c, r) = StringMap.find app_state.selected_sheet app_state.selected_cell in
        let prev_text = match Text_service.get_cell_definition (app_state.selected_sheet, (c, r)) app_state.service_state with
          | NotParsed (t, _) -> t
          | Parsed (p) -> p
        in
        let app_state = 
          if new_text = prev_text then
            app_state
          else
            push_undo (CellDef(false, text_to_editor prev_text)) app_state
        in
        let new_service_state =
          Text_service.set_cell (app_state.selected_sheet, (c, r)) new_text app_state.service_state
        in
        loop term {app_state with mode = CellDef (false, editor_state); service_state = new_service_state}
    | `Key (`Page `Up, _) ->
        let new_editor_state = move_editor_up_by editor_state (app_state.bottom_height - 2) in
        loop term {app_state with mode = CellDef (true, new_editor_state)}
    | `Key (`Page `Down, _) ->
        let new_editor_state = move_editor_down_by editor_state (app_state.bottom_height - 2) in
        loop term {app_state with mode = CellDef (true, new_editor_state)}
    | `Key (`Arrow `Up, [`Ctrl]) ->
        loop term {app_state with bottom_height = app_state.bottom_height + 1}
    | `Key (`Arrow `Down, [`Ctrl]) ->
        let new_height = max 5 (app_state.bottom_height - 1) in
        loop term {app_state with bottom_height = new_height}
    | `Key (`Arrow `Left, [`Ctrl]) ->
        let new_width = max 10 (app_state.value_box_width - 1) in
        loop term {app_state with value_box_width = new_width}
    | `Key (`Arrow `Right, [`Ctrl]) ->
        loop term {app_state with value_box_width = app_state.value_box_width + 1}
    | event ->
        let new_editor_state = handle_editor_input editor_state event in
        loop term {app_state with mode = CellDef (true, new_editor_state)}
    end

  
  | Formatter (is_active, editor_state) ->
    let scol, srow = StringMap.find app_state.selected_sheet app_state.selected_cell in
    let message, last_parsed = if is_active then "", None else
      match Text_service.get_cell_formatter (app_state.selected_sheet, (scol, srow)) app_state.service_state with
      | NotParsed (_, p) -> " (Parsing failed)", Some p
      | Parsed (_) -> "", None
    in
    let sheets_height = app_state.height - app_state.bottom_height - 3 in
    let sheets_image = display_sheet app_state app_state.width sheets_height in
    let navbar = singleline_image ("[Esc]ape"^(if is_active then " (and evaluate)" else
      " [I]nsert [D]efinition [S]heets"^(if last_parsed <> None then " [B]ack to last parsed" else ""))) app_state.width in
    let hline = I.string A.empty (Print.string_repeat app_state.width "─") in
    let value_box_text = Text_service.get_cell_box (app_state.selected_sheet, (scol, srow)) app_state.value_box_width app_state.bottom_height app_state.service_state in
    let value_box_image = image_of_multiline A.empty value_box_text in
    let vline = image_of_multiline A.empty (String.trim (Print.string_repeat app_state.bottom_height "│\n")) in
    let editor_title = singleline_image ("Formatter:" ^ message) (app_state.width - app_state.value_box_width - 1) in
    let editor_image = display_editor editor_state (app_state.width - app_state.value_box_width - 1) (app_state.bottom_height-1) is_active in
    let bottom_image = I.hcat [value_box_image; vline; I.vcat [editor_title; editor_image]] in
    let sheets_footer_image = sheets_footer (not is_active) app_state in
    let img = I.vcat [navbar; sheets_image; hline; bottom_image; sheets_footer_image] in
    Term.image term img;
    if not is_active then
    begin
    match Term.event term with
    | `Resize (w, h) ->
        loop term { app_state with width = w; height = h }
    | `Key (`Escape, _) | `Key (`ASCII 'r', _) ->
        loop term {app_state with mode = View}
    | `Key (`ASCII 'i', _) ->
        loop term (show_formatter true app_state)
    | `Key (`ASCII 'b', _) ->
        if last_parsed = None then
          loop term app_state
        else
        let (c, r) = StringMap.find app_state.selected_sheet app_state.selected_cell in
        let new_text_service = Text_service.set_cell_formatter (app_state.selected_sheet, (c, r)) (Option.get last_parsed) app_state.service_state in
        let app_state = push_undo app_state.mode app_state in
        let app_state = show_formatter false {app_state with service_state = new_text_service} in
        loop term app_state
    | `Key (`ASCII 'd', _) ->
        loop term (show_definition false app_state)
    | `Key (`ASCII 's', _) ->
        loop term (show_sheets_list app_state)
    | `Key (`ASCII ',', _) | `Key (`ASCII '<', _) ->
        loop term (app_state |> prev_sheet |> show_formatter false)
    | `Key (`ASCII '.', _) | `Key (`ASCII '>', _) ->
        loop term (app_state |> next_sheet |> show_formatter false)
    | `Key (`ASCII 'z', _) ->
        let new_app_state = undo_app_state app_state in
        loop term new_app_state
    | `Key (`ASCII 'y', _) ->
        let new_app_state = redo_app_state app_state in
        loop term new_app_state
    | `Key (`Arrow `Up, []) ->
        loop term (app_state |> move_selection_up |> show_formatter false)
    | `Key (`Arrow `Down, []) | `Key (`Enter, _) ->
        loop term (app_state |> move_selection_down |> show_formatter false)
    | `Key (`Arrow `Left, []) ->
        loop term (app_state |> move_selection_left |> show_formatter false)
    | `Key (`Arrow `Right, []) | `Key (`Tab, _) ->
        loop term (app_state |> move_selection_right |> show_formatter false)
    | `Key (`Arrow `Up, [`Ctrl]) ->
        loop term {app_state with bottom_height = app_state.bottom_height + 1}
    | `Key (`Page `Up, _) ->
        let new_editor_state = move_editor_up_by editor_state (app_state.bottom_height - 2) in
        loop term {app_state with mode = Formatter (false, new_editor_state)}
    | `Key (`Page `Down, _) ->
        let new_editor_state = move_editor_down_by editor_state (app_state.bottom_height - 2) in
        loop term {app_state with mode = Formatter (false, new_editor_state)}
    | `Key (`Arrow `Down, [`Ctrl]) ->
        let new_height = max 5 (app_state.bottom_height - 1) in
        loop term {app_state with bottom_height = new_height}
    | `Key (`Arrow `Left, [`Ctrl]) ->
        let new_width = max 10 (app_state.value_box_width - 1) in
        loop term {app_state with value_box_width = new_width}
    | `Key (`Arrow `Right, [`Ctrl]) ->
        loop term {app_state with value_box_width = app_state.value_box_width + 1}
    | `Key (`Arrow `Up, [`Shift]) ->
        let (_, r) = StringMap.find app_state.selected_sheet app_state.selected_cell in
        let new_service_state = Text_service.modify_row_height app_state.selected_sheet r (-1) app_state.service_state in
        loop term {app_state with service_state = new_service_state}
    | `Key (`Arrow `Down, [`Shift]) ->
        let (_, r) = StringMap.find app_state.selected_sheet app_state.selected_cell in
        let new_service_state = Text_service.modify_row_height app_state.selected_sheet r 1 app_state.service_state in
        loop term {app_state with service_state = new_service_state}
    | `Key (`Arrow `Left, [`Shift]) ->
        let (c, _) = StringMap.find app_state.selected_sheet app_state.selected_cell in
        let new_service_state = Text_service.modify_column_width app_state.selected_sheet c (-1) app_state.service_state in
        loop term {app_state with service_state = new_service_state}
    | `Key (`Arrow `Right, [`Shift]) ->
        let (c, _) = StringMap.find app_state.selected_sheet app_state.selected_cell in
        let new_service_state = Text_service.modify_column_width app_state.selected_sheet c 1 app_state.service_state in
        loop term {app_state with service_state = new_service_state}
    | _ ->
        loop term app_state
    end
  else
    begin
    match Term.event term with 
    | `Resize (w, h) ->
        loop term { app_state with width = w; height = h }
    | `Key (`Escape, _) -> 
        let editor_title = singleline_image "Formatter: [Computing]" (app_state.width - app_state.value_box_width - 1) in
        let bottom_image = I.hcat [value_box_image; vline; I.vcat [editor_title; editor_image]] in
        let img = I.vcat [navbar; sheets_image; hline; bottom_image; sheets_footer_image] in
        Term.image term img;
        let (c, r) = StringMap.find app_state.selected_sheet app_state.selected_cell in
        let new_text = text_zipper_to_text editor_state.text in
        let new_service_state =
          Text_service.set_cell_formatter (app_state.selected_sheet, (c, r)) new_text app_state.service_state
        in
        let app_state = 
          let prev_formatter = match Text_service.get_cell_formatter (app_state.selected_sheet, (c, r)) app_state.service_state with
            | NotParsed (t, _) -> t
            | Parsed (p) -> p
          in
          if new_text = prev_formatter then
            app_state
          else
            push_undo (Formatter(false, text_to_editor prev_formatter)) app_state
        in
        loop term {app_state with mode = Formatter (false, editor_state); service_state = new_service_state}
    | `Key (`Page `Up, _) ->
        let new_editor_state = move_editor_up_by editor_state (app_state.bottom_height - 2) in
        loop term {app_state with mode = Formatter (true, new_editor_state)}
    | `Key (`Page `Down, _) ->
        let new_editor_state = move_editor_down_by editor_state (app_state.bottom_height - 2) in
        loop term {app_state with mode = Formatter (true, new_editor_state)}
    | `Key (`Arrow `Up, [`Ctrl]) ->
        loop term {app_state with bottom_height = app_state.bottom_height + 1}
    | `Key (`Arrow `Down, [`Ctrl]) ->
        let new_height = max 5 (app_state.bottom_height - 1) in
        loop term {app_state with bottom_height = new_height}
    | `Key (`Arrow `Left, [`Ctrl]) ->
        let new_width = max 10 (app_state.value_box_width - 1) in
        loop term {app_state with value_box_width = new_width}
    | `Key (`Arrow `Right, [`Ctrl]) ->
        loop term {app_state with value_box_width = app_state.value_box_width + 1}
    | event ->
        let new_editor_state = handle_editor_input editor_state event in
        loop term {app_state with mode = Formatter (true, new_editor_state)}
    end
  

  | Config (active, editor_state, max_cost_editor) ->
    let is_saved = (match app_state.service_state.config_text with Saved _ -> true | NotSaved _ -> false) in
    let is_savable = (match app_state.service_state.config_text with NotSaved (_, _, Some _, _, _) -> true | _ -> false) in
    let save_text = (if is_savable then " [R]ecompute sheets" else "") ^ (if is_saved then "" else " [B]ack to saved") in
    let navbar = singleline_image ("[Esc]ape"^(if active <> 0 then " (and evaluate)" else " [I]nsert"^save_text)) app_state.width in
    let hline = I.string A.empty (Print.string_repeat app_state.width "─") in
    let editor_height = if active = 1 then app_state.height - 2 else app_state.height - app_state.bottom_height - 3 in
    let editor_image = display_editor editor_state app_state.width editor_height (active=1) in
    let image = if active = 1 then I.vcat [navbar; hline; editor_image] else
      let bottom_image = match app_state.service_state.config_text with 
      | Saved (_, Ok v) ->
          let message = singleline_image "Configuration saved, evaluated to value:" app_state.width in
          let value_box_image = image_of_multiline A.empty (string_to_box (string_of_value v) app_state.width (app_state.bottom_height - 2)) in
          I.vcat [message; value_box_image]
      | Saved (_, Error e) ->
          let message = singleline_image "Configuration saved, evaluation ended with error:" app_state.width in
          let value_box_image = image_of_multiline A.empty (string_to_box (string_of_value e) app_state.width (app_state.bottom_height - 2)) in
          I.vcat [message; value_box_image]
      | NotSaved (_, _, None, _, _) ->
          image_of_multiline A.empty (string_to_box "Parser error" app_state.width app_state.bottom_height)
      | NotSaved (_, _, Some (Ok v, _), _, _) ->
          let message = singleline_image "Evaluated to value:" app_state.width in
          let value_box_image = image_of_multiline A.empty (string_to_box (string_of_value v) app_state.width (app_state.bottom_height - 2)) in
          I.vcat [message; value_box_image]
      | NotSaved (_, _, Some (Error e, _), _, _) ->
          let message = singleline_image "Evaluation ended with error:" app_state.width in
          let value_box_image = image_of_multiline A.empty (string_to_box (string_of_value e) app_state.width (app_state.bottom_height - 2)) in
          I.vcat [message; value_box_image]
      in
      let cost_image =
        let cost_text = I.string A.empty "[M]ax computation cost: " in
        let editor_image = display_editor max_cost_editor 10 1 (active = 2) in
        let cost_info = match app_state.service_state.config_text with
          | NotSaved (_, cost, _, _, _) when cost <> app_state.service_state.max_cost ->
             I.string A.empty (" (saved cost: " ^ string_of_int app_state.service_state.max_cost ^ ")")
          | _ when active = 2 ->
             I.string A.empty (" (saved cost: " ^ string_of_int app_state.service_state.max_cost ^ ")")
          | _ -> I.empty
        in
        I.hcat [cost_text; editor_image; cost_info]
      in
      I.vcat [navbar; hline; editor_image; hline; bottom_image; cost_image] in
    Term.image term image;
    if active = 0 then
    begin
    match Term.event term with
    | `Resize (w, h) ->
        loop term {app_state with width = w; height = h }
    | `Key (`Escape, _) ->
        loop term {app_state with mode = View}
    | `Key (`ASCII 'i', _) ->
        loop term {app_state with mode = Config (1, editor_state, max_cost_editor)}
    | `Key (`ASCII 'r', _) when is_savable ->
        let new_service_state = Text_service.save_config app_state.service_state in
        loop term {app_state with service_state = new_service_state}
    | `Key (`ASCII 'b', _) when not is_saved ->
        let saved_text = match app_state.service_state.config_text with Saved (t, _) | NotSaved (_, _, _, t, _) -> t in
        let new_editor_state = text_to_editor saved_text in
        let new_service_state = Text_service.rollback_config app_state.service_state in
        loop term {app_state with mode = Config (0, new_editor_state, max_cost_editor); service_state = new_service_state}
    | `Key (`ASCII 'm', _) ->
        loop term {app_state with mode = Config (2, editor_state, max_cost_editor)}
    | `Key (`Arrow `Up, []) | `Key (`Arrow `Up, [`Shift]) ->
        let new_editor_state = move_editor_up_by editor_state 1 in
        loop term {app_state with mode = Config (0, new_editor_state, max_cost_editor)}
    | `Key (`Arrow `Down, []) | `Key (`Arrow `Down, [`Shift]) ->
        let new_editor_state = move_editor_down_by editor_state 1 in
        loop term {app_state with mode = Config (0, new_editor_state, max_cost_editor)}
    | `Key (`Arrow `Left, []) | `Key (`Arrow `Left, [`Shift]) ->
        let new_editor_state = move_editor_left editor_state in
        loop term {app_state with mode = Config (0, new_editor_state, max_cost_editor)}
    | `Key (`Arrow `Right, []) | `Key (`Arrow `Right, [`Shift]) ->
        let new_editor_state = move_editor_right editor_state in
        loop term {app_state with mode = Config (0, new_editor_state, max_cost_editor)}
    | `Key (`Page `Up, _) ->
        let new_editor_state = move_editor_up_by editor_state (app_state.height - app_state.bottom_height - 4) in
        loop term {app_state with mode = Config (0, new_editor_state, max_cost_editor)}
    | `Key (`Page `Down, _) ->
        let new_editor_state = move_editor_down_by editor_state (app_state.height - app_state.bottom_height - 4) in
        loop term {app_state with mode = Config (0, new_editor_state, max_cost_editor)}
    | `Key (`Arrow `Up, [`Ctrl]) ->
        loop term {app_state with bottom_height = app_state.bottom_height + 1}
    | `Key (`Arrow `Down, [`Ctrl]) ->
        let new_height = max 5 (app_state.bottom_height - 1) in
        loop term {app_state with bottom_height = new_height}
    | `Key (`ASCII 'z', _) ->
        let new_app_state = undo_app_state app_state in
        loop term new_app_state
    | `Key (`ASCII 'y', _) ->
        let new_app_state = redo_app_state app_state in
        loop term new_app_state
    | _ ->
        loop term app_state
    end
  else if active = 1 then
    begin
    match Term.event term with 
    | `Resize (w, h) ->
        loop term {app_state with width = w; height = h }
    | `Key (`Escape, _) -> 
        let editor_title = singleline_image "Configuration: [Computing]" app_state.width in
        let editor_image = display_editor editor_state app_state.width (app_state.height - 2) false in
        let img = I.vcat [editor_title; hline; editor_image] in
        Term.image term img;
        let new_text = text_zipper_to_text editor_state.text in
        let curr_max_cost = match app_state.service_state.config_text with 
          | Saved (_, _) -> app_state.service_state.max_cost
          | NotSaved (_, cost, _, _, _) -> cost
        in
        let new_service_state =
          Text_service.set_config_text new_text curr_max_cost app_state.service_state
        in
        let prev_text = match app_state.service_state.config_text with Saved (t, _) | NotSaved (t, _, _, _, _) -> t in
        let app_state = 
          if new_text = prev_text then
            app_state
          else
            push_undo (Config(0, text_to_editor prev_text, max_cost_editor)) app_state
        in
        loop term {app_state with mode = Config (0, editor_state, max_cost_editor); service_state = new_service_state}
    | `Key (`Page `Up, _) ->
        let new_editor_state = move_editor_up_by editor_state (app_state.height - app_state.bottom_height - 4) in
        loop term {app_state with mode = Config (1, new_editor_state, max_cost_editor)}
    | `Key (`Page `Down, _) ->
        let new_editor_state = move_editor_down_by editor_state (app_state.height - app_state.bottom_height - 4) in
        loop term {app_state with mode = Config (1, new_editor_state, max_cost_editor)}
    | event ->
        let new_editor_state = handle_editor_input editor_state event in
        loop term {app_state with mode = Config (1, new_editor_state, max_cost_editor)}
    end
  else if active = 2 then
    begin
    let event = Term.event term in
    match event with
    | `Resize (w, h) ->
        loop term {app_state with width = w; height = h }
    | `Key (`Escape, _) | `Key (`Enter, _) -> 
        let editor_title = singleline_image "Configuration: [Computing]" app_state.width in
        let editor_image = display_editor editor_state app_state.width (app_state.height - 2) false in
        let img = I.vcat [editor_title; hline; editor_image] in
        Term.image term img;
        let new_max_cost = try max (min (int_of_string (text_zipper_to_text max_cost_editor.text)) 2000000000) 10000 with _ -> 500000 in
        let new_service_state =
          Text_service.set_config_text (text_zipper_to_text editor_state.text) new_max_cost app_state.service_state
        in
        let prev_max_cost = match app_state.service_state.config_text with
          | Saved (_, _) -> app_state.service_state.max_cost
          | NotSaved (_, cost, _, _, _) -> cost
        in
        let app_state = 
          if new_max_cost = prev_max_cost then
            app_state
          else
            push_undo (Config(0, editor_state, text_to_editor (string_of_int prev_max_cost))) app_state
        in
        loop term {app_state with mode = Config (0, editor_state, text_to_editor (string_of_int new_max_cost)); service_state = new_service_state}
    | `Key (`ASCII c, _) when '0' <= c && c <= '9' ->
        let new_max_cost_editor = handle_editor_input max_cost_editor event in
        loop term {app_state with mode = Config (2, editor_state, new_max_cost_editor)}
    | `Key (`Backspace, _) | `Key (`Delete, _) ->
        let new_max_cost_editor = handle_editor_input max_cost_editor event in
        loop term {app_state with mode = Config (2, editor_state, new_max_cost_editor)}
    | _ ->
        loop term app_state
    end


  | SheetsList (top_row, selected_row, editor) ->
    let navbar_text = if editor = None then "[Esc]ape [A]dd [E]dit [D]elete [Enter] [Shift Up/Down]" else "[Esc]ape and set new name" in
    let navbar = singleline_image navbar_text app_state.width in
    let hline = I.string A.empty (Print.string_repeat app_state.width "─") in
    let sheets_names = app_state.service_state.sheet_names in
    let sheets_list_image =
      let rec build_image names row acc =
        match names with
        | [] -> I.vcat (List.rev acc)
        | x::xs ->
            let short_name = if String.length x > app_state.width - 2 then String.sub x 0 (app_state.width - 3) ^ "…" else x^(String.make (app_state.width - String.length x - 2) ' ') in
            let line_image =
              if row = selected_row then
                let arrow = I.string A.(st bold) "> " in
                match editor with
                | Some editor_state ->
                    let editor_image = display_editor editor_state (app_state.width-2) 1 true in
                    I.hcat [arrow; editor_image]
                | None ->
                    I.hcat [arrow; I.string (if x = app_state.selected_sheet then A.(st bold) else A.empty) short_name]
              else
                if x = app_state.selected_sheet then
                  I.string A.(st bold) ("  " ^ short_name)
                else
                I.string A.empty ("  " ^ short_name)
            in
            build_image xs (row + 1) (line_image :: acc)
      in
      let visible_names = List.filteri (fun i _ -> i + 1 >= top_row && i + 1 < top_row + app_state.height - 2) sheets_names in
      build_image visible_names top_row []
    in
    let img = I.vcat [navbar; hline; sheets_list_image] in
    Term.image term img;
    if editor = None then
    begin
    match Term.event term with
    | `Resize (w, h) ->
        loop term { app_state with width = w; height = h }
    | `Key (`Escape, _) ->
        loop term {app_state with mode = View}
    | `Key (`ASCII 'a', _) ->
        let app_state = push_undo (app_state.mode) app_state in
        let new_sheet_name = 
          let rec find_name n =
            let name = "Sheet" ^ string_of_int n in
            if List.mem name app_state.service_state.sheet_names then
              find_name (n + 1)
            else
              name
          in find_name 1
        in
        let new_top_row = if selected_row = top_row + app_state.height - 3 then top_row + 1 else top_row in
        loop term (add_sheet new_sheet_name (selected_row+1) {app_state with mode = SheetsList (new_top_row, selected_row + 1, Some (text_to_editor new_sheet_name))})
    | `Key (`ASCII 'e', _) ->
        let curr_sheet_name = List.nth app_state.service_state.sheet_names (selected_row - 1) in
        let editor_state = text_to_editor curr_sheet_name in
        loop term {app_state with mode = SheetsList (top_row, selected_row, Some editor_state)}
    | `Key (`ASCII 'd', _) ->
        let app_state = push_undo (app_state.mode) app_state in
        let sheet_to_delete = List.nth app_state.service_state.sheet_names (selected_row - 1) in
        let new_app_state = delete_sheet sheet_to_delete app_state in
        let new_selected_row = if selected_row > List.length new_app_state.service_state.sheet_names then selected_row - 1 else selected_row in
        loop term {new_app_state with mode = SheetsList (top_row, new_selected_row, None)}
    | `Key (`ASCII 'z', _) ->
        let new_app_state = undo_app_state app_state in
        loop term new_app_state
    | `Key (`ASCII 'y', _) ->
        let new_app_state = redo_app_state app_state in
        loop term new_app_state
    | `Key (`Enter, []) ->
        let sheet_name = List.nth app_state.service_state.sheet_names (selected_row - 1) in
        loop term {app_state with selected_sheet = sheet_name; mode = View}
    | `Key (`Arrow `Up, []) ->
        let new_top_row = if selected_row > 1 && selected_row = top_row then top_row - 1 else top_row in
        let new_selected_row = if selected_row > 1 then selected_row - 1 else selected_row in
        loop term {app_state with mode = SheetsList (new_top_row, new_selected_row, editor)}
    | `Key (`Arrow `Down, []) ->
        let sheets_count = List.length app_state.service_state.sheet_names in
        let new_top_row = if selected_row < sheets_count && selected_row = top_row + app_state.height - 3 then top_row + 1 else top_row in
        let new_selected_row = if selected_row < sheets_count then selected_row + 1 else selected_row in
        loop term {app_state with mode = SheetsList (new_top_row, new_selected_row, editor)}
    | `Key (`Arrow `Up, [`Shift]) ->
        if selected_row > 1 then
          let new_sheet_names = 
            let rec swap lst pos =
              match lst with
              | [] -> []
              | x::y::xs ->
                  if pos = 2 then y :: x :: xs
                  else x :: swap (y::xs) (pos - 1)
              | x::xs -> x :: xs
            in swap app_state.service_state.sheet_names selected_row
          in
          let new_service_state = {app_state.service_state with sheet_names = new_sheet_names} in
          let new_top_row = if selected_row = top_row then top_row - 1 else top_row in
          let new_selected_row = selected_row - 1 in
          loop term {app_state with service_state = new_service_state; mode = SheetsList (new_top_row, new_selected_row, editor)}
        else
          loop term app_state
    | `Key (`Arrow `Down, [`Shift]) ->
        let sheets_count = List.length app_state.service_state.sheet_names in
        if selected_row < sheets_count then
          let new_sheet_names = 
            let rec swap lst pos =
              match lst with
              | [] -> []
              | x::y::xs ->
                  if pos = 1 then y :: x :: xs
                  else x :: swap (y::xs) (pos - 1)
              | x::xs -> x :: xs
            in swap app_state.service_state.sheet_names selected_row
          in
          let new_service_state = {app_state.service_state with sheet_names = new_sheet_names} in
          let new_top_row = if selected_row = top_row + app_state.height - 3 then top_row + 1 else top_row in
          let new_selected_row = selected_row + 1 in
          loop term {app_state with service_state = new_service_state; mode = SheetsList (new_top_row, new_selected_row, editor)}
        else
          loop term app_state
    | _ ->
        loop term app_state
    end
    else
    begin
    match Term.event term with 
    | `Resize (w, h) ->
        loop term { app_state with width = w; height = h }
    | `Key (`Escape, _) | `Key (`Enter, _) | `Key (`Tab, _) -> 
        let editor_state = Option.get editor in
        let new_name = text_zipper_to_text editor_state.text in
        let app_state = if new_name = "" || (List.mem new_name app_state.service_state.sheet_names) then app_state else push_undo (SheetsList (top_row, selected_row, None)) app_state in
        let new_app_state = rename_sheet new_name selected_row app_state in
        loop term {new_app_state with mode = SheetsList (top_row, selected_row, None)}
    | event ->
        let editor_state = Option.get editor in
        let new_editor_state = handle_editor_input editor_state event in
        loop term {app_state with mode = SheetsList (top_row, selected_row, Some new_editor_state)}
    end
  
    
  | SaveFile editor_opt ->
    let navbar_text = match editor_opt with 
        | None -> "[Esc]ape [S]ave [E]dit [N]ew [O]pen"
        | Some (_, true) -> "[Esc]ape and set file name [Enter] Save"
        | Some (_, false) -> "[Esc]ape [Enter]" in
    let navbar = singleline_image navbar_text app_state.width in
    let hline = I.string A.empty (Print.string_repeat app_state.width "─") in
    let save_as = singleline_image "Save as:" app_state.width in
    let save_as_image = match editor_opt with
      | Some (editor_state, true) -> display_editor editor_state app_state.width 1 true
      | _ -> singleline_image app_state.service_state.filename app_state.width
    in
    let open_image = match editor_opt with 
        | Some(editor_state, false) ->
          let open_image = singleline_image "Enter the name of file to open:" app_state.width in
          let editor_image = display_editor editor_state app_state.width 1 true in
          I.vcat [open_image; editor_image]
        | _ -> I.empty
    in
    let img = I.vcat [navbar; hline; save_as; save_as_image; open_image] in
    Term.image term img;
    if editor_opt = None then
    begin
    match Term.event term with
    | `Resize (w, h) ->
        loop term { app_state with width = w; height = h }
    | `Key (`Escape, _) ->
        loop term {app_state with mode = View}
    | `Key (`ASCII 's', _) ->
        (try
            Text_service.save_to_file app_state.service_state;
            loop term {app_state with mode = View}
        with _ ->
            loop term {app_state with mode = SaveFile (Some (text_to_editor app_state.service_state.filename, true))})
    | `Key (`ASCII 'e', _) ->
        let editor_state = text_to_editor app_state.service_state.filename in
        loop term {app_state with mode = SaveFile (Some (editor_state, true))}
    | `Key (`ASCII 'n', _) ->
        loop term (start_app_state app_state.width app_state.height)
    | `Key (`ASCII 'o', _) ->
        let editor_state = text_to_editor ".json" in
        loop term {app_state with mode = SaveFile (Some (editor_state, false))}
    | _ ->
        loop term app_state
    end
    else
    begin
    let (editor_state, is_saving) = Option.get editor_opt in
    match Term.event term with 
    | `Resize (w, h) ->
        loop term { app_state with width = w; height = h }
    | `Key (`Escape, _) -> 
        if is_saving then
          let new_filename = text_zipper_to_text editor_state.text in
          let new_service_state = {app_state.service_state with filename = new_filename} in
          loop term {app_state with mode = SaveFile None; service_state = new_service_state}
        else
          loop term {app_state with mode = SaveFile None}
    | `Key (`Enter, _) | `Key (`Tab, _) -> 
        let new_filename = text_zipper_to_text editor_state.text in
        if is_saving then
          (try
            let new_service_state = {app_state.service_state with filename = new_filename} in
            let app_state = {app_state with service_state = new_service_state} in
            Text_service.save_to_file app_state.service_state;
            loop term {app_state with mode = View}
          with _ ->
            loop term app_state)
        else
          (try
            let new_service_state = Text_service.load_from_file new_filename in
            let sheeet_names = new_service_state.sheet_names in
            let selected_sheet = List.hd sheeet_names in
            let selected_cell = List.fold_left (fun acc name -> StringMap.add name (1,1) acc) StringMap.empty sheeet_names in
            let top_left_cell = List.fold_left (fun acc name -> StringMap.add name (1,1) acc) StringMap.empty sheeet_names in
            loop term {app_state with mode = View; service_state = new_service_state; selected_sheet = selected_sheet;
                selected_cell = selected_cell; top_left_cell = top_left_cell; undo_stack = []; redo_stack = []}
          with _ ->
            loop term app_state)
    | event ->
        let new_editor_state = handle_editor_input editor_state event in
        loop term {app_state with mode = SaveFile (Some (new_editor_state, is_saving))}
    end
  

  | Manual editor_state ->
    let navbar = singleline_image "[Esc]ape [Arrows] [Page Up/Down] [0/1/2/3/4] Move to section" app_state.width in
    let hline = I.string A.empty (Print.string_repeat app_state.width "─") in
    let editor_image = display_editor editor_state app_state.width (app_state.height - 2) false in
    let img = I.vcat [navbar; hline; editor_image] in
    Term.image term img;
    match Term.event term with
    | `Resize (w, h) ->
        loop term { app_state with width = w; height = h }
    | `Key (`Escape, _) ->
        loop term {app_state with mode = View}
    | `Key (`Arrow `Up, []) ->
        if editor_state.top_line = 1 then loop term app_state else
        let new_top_line = max 1 (editor_state.top_line - 1) in
        let new_text_zipper = text_zipper_move_up editor_state.text in
        let new_editor_state =  {editor_state with top_line = new_top_line; cursor_col = new_top_line; text = new_text_zipper} in
        loop term {app_state with mode = Manual new_editor_state}
    | `Key (`Arrow `Down, []) ->
        if editor_state.top_line + app_state.height - 2 >= User_manual.manual_length then loop term app_state else
        let new_top_line = editor_state.top_line + 1 in
        let new_text_zipper = text_zipper_move_down editor_state.text in
        let new_editor_state = {editor_state with top_line = new_top_line; cursor_col = new_top_line; text = new_text_zipper} in
        loop term {app_state with mode = Manual new_editor_state}
    | `Key (`Arrow `Left, []) ->
        let new_left_col = max 1 (editor_state.left_col - 1) in
        let new_editor_state = {editor_state with left_col = new_left_col; cursor_col = new_left_col} in
        loop term {app_state with mode = Manual new_editor_state}
    | `Key (`Arrow `Right, []) ->
        let new_left_col = min (editor_state.left_col + 1) 80 in
        let new_editor_state = {editor_state with left_col = new_left_col; cursor_col = new_left_col} in
        loop term {app_state with mode = Manual new_editor_state}
    | `Key (`Page `Up, []) ->
        let new_editor_state = move_editor_up_by editor_state (app_state.height - 3) in
        loop term {app_state with mode = Manual new_editor_state}
    | `Key (`Page `Down, []) ->
        let new_editor_state = move_editor_down_by editor_state (app_state.height - 3) in
        loop term {app_state with mode = Manual new_editor_state}
    | `Key (`ASCII c, []) when '0' <= c && c <= '4' ->
        let section = Char.code c - Char.code '0' in
        let move_by = (List.nth User_manual.section_start_lines section) - editor_state.top_line in
        let new_editor_state = if move_by > 0 then move_editor_down_by editor_state move_by else move_editor_up_by editor_state (-move_by) in
        loop term {app_state with mode = Manual new_editor_state}
    | _ ->
        loop term app_state


let () =
  let term = Term.create () in
  let (w, h) = Term.size term in
  loop term (start_app_state w h);
  Term.release term

