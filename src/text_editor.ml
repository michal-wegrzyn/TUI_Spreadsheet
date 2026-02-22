open Notty


type text_zipper = {
  before : string list;
  current : string;
  after : string list;
}


type editor_state = {
  cursor_line : int;
  cursor_col : int;
  top_line : int;
  left_col : int;
  text : text_zipper;
}


let text_to_zipper text =
  let lines = String.split_on_char '\n' text in
  match lines with
  | [] -> {before = []; current = ""; after = []}
  | x::xs -> {before = []; current = x; after = xs}


let text_zipper_to_text zipper =
  String.concat "\n" (List.rev_append zipper.before (zipper.current :: zipper.after))


let text_zipper_move_up zipper =
  match zipper.before with
  | [] -> zipper
  | x::xs -> {before = xs; current = x; after = zipper.current :: zipper.after}


let text_zipper_move_down zipper =
  match zipper.after with
  | [] -> zipper
  | x::xs -> {before = zipper.current :: zipper.before; current = x; after = xs}


let move_editor_up_by editor_state n =
  let rec move_up_by zipper n =
    if n <= 0 then zipper
    else move_up_by (text_zipper_move_up zipper) (n - 1)
  in
  let new_text = move_up_by editor_state.text n in
  let new_cursor_line = max 1 (editor_state.cursor_line - n) in
  let new_cursor_line = if new_text.before = [] then 1 else new_cursor_line in
  let new_top_line = max 1 (editor_state.top_line - n) in
  {editor_state with text = new_text; cursor_line = new_cursor_line; top_line = new_top_line}


let move_editor_down_by editor_state n =
  let rec move_down_by zipper n =
    if n <= 0 || zipper.after = [] then zipper, n
    else move_down_by (text_zipper_move_down zipper) (n - 1)
  in
  let new_text, not_moved = move_down_by editor_state.text n in
  let moved = n - not_moved in
  let new_cursor_line = editor_state.cursor_line + moved in
  let new_top_line = editor_state.top_line + moved in
  {editor_state with text = new_text; cursor_line = new_cursor_line; top_line = new_top_line}


let move_editor_left editor_state =
  if editor_state.left_col = 1 then editor_state
  else {editor_state with left_col = editor_state.left_col - 1; cursor_col = max 1 (editor_state.cursor_col - 1)}


let move_editor_right editor_state =
  {editor_state with left_col = editor_state.left_col + 1; cursor_col = editor_state.cursor_col + 1}


let text_to_editor text =
  {
    cursor_line = 1;
    cursor_col = 1;
    top_line = 1;
    left_col = 1;
    text = text_to_zipper text;
  }


let move_editor_view editor_state width height =
  let cursor_line = editor_state.cursor_line in
  let cursor_col = editor_state.cursor_col in
  let top_line =
    if cursor_line < editor_state.top_line then
      cursor_line
    else if cursor_line >= editor_state.top_line + height then
      cursor_line - height + 1
    else
      editor_state.top_line
  in
  let left_col =
    if cursor_col < editor_state.left_col then
      cursor_col
    else if cursor_col >= editor_state.left_col + width then
      cursor_col - width + 1
    else
      editor_state.left_col
  in
  {editor_state with top_line = top_line; left_col = left_col}


let display_editor editor_state width height is_active =
  let raw_lines_before = editor_state.cursor_line - editor_state.top_line in
  let num_of_lines_before = max 0 (min (height - 1) raw_lines_before) in
  let num_of_lines_after = max 0 (height - 1 - num_of_lines_before) in
  let lines_before =
    let rec take n lst acc =
      match (n, lst) with
      | (n, _) when n <= 0 -> acc
      | (n, []) -> take (n - 1) [] ("" :: acc)
      | (n, x::xs) -> take (n - 1) xs (x :: acc)
    in
    take num_of_lines_before editor_state.text.before []
  in
  let lines_after =
    let rec take n lst =
      match (n, lst) with
      | (n, _) when n <= 0 -> []
      | (n, []) -> "" :: take (n - 1) []
      | (n, x::xs) -> x :: take (n - 1) xs
    in
    take num_of_lines_after editor_state.text.after
  in
  let padding = String.make (editor_state.left_col + width) ' ' in
  let before_image = 
    lines_before
    |> List.map (fun line -> I.string A.empty (String.sub (line ^ padding) (editor_state.left_col-1) width))
    |> I.vcat
  in
  let current_image =
    let current_line = String.sub (editor_state.text.current ^ padding) (editor_state.left_col-1) width in
    let cursor_col = min (String.length editor_state.text.current + 1) editor_state.cursor_col in
    if cursor_col < editor_state.left_col || cursor_col >= editor_state.left_col + width || not is_active then
      I.string A.empty current_line
    else
    I.hcat [
      I.string A.empty (String.sub current_line 0 (cursor_col - editor_state.left_col));
      I.string A.(st reverse) (String.sub current_line (cursor_col - editor_state.left_col) 1);
      I.string A.empty (String.sub current_line (cursor_col - editor_state.left_col + 1) (width - cursor_col + editor_state.left_col - 1));
    ]
  in
  let after_image = 
    lines_after
    |> List.map (fun line -> I.string A.empty (String.sub (line ^ padding) (editor_state.left_col-1) width))
    |> I.vcat
  in
  I.vcat [before_image; current_image; after_image]


let handle_editor_input editor_state event =
  match event with
  | `Key (`Arrow `Up, []) ->
      let new_text = text_zipper_move_up editor_state.text in
      let new_cursor_line = max 1 (editor_state.cursor_line - 1) in
      let new_top_line =
        if new_cursor_line < editor_state.top_line then
          max 1 (editor_state.top_line - 1)
        else
          editor_state.top_line
      in
      {editor_state with text = new_text; cursor_line = new_cursor_line; top_line = new_top_line}
  | `Key (`Arrow `Down, []) ->
      let new_text = text_zipper_move_down editor_state.text in
      let new_cursor_line = if editor_state.text.after <> [] then editor_state.cursor_line + 1 else editor_state.cursor_line in
      {editor_state with text = new_text; cursor_line = new_cursor_line}
  | `Key (`Arrow `Left, []) ->
      let cursor_col = min editor_state.cursor_col (String.length editor_state.text.current + 1) in
      if cursor_col > 1 then
        {editor_state with cursor_col = cursor_col - 1}
      else
        (match editor_state.text.before with
        | [] -> editor_state
        | x::xs ->
            let new_text = {before = xs; current = x; after = editor_state.text.current :: editor_state.text.after} in
            let new_cursor_line = editor_state.cursor_line - 1 in
            let new_top_line =
              if new_cursor_line < editor_state.top_line then
                max 1 (editor_state.top_line - 1)
              else
                editor_state.top_line
            in
            let new_cursor_col = String.length x + 1 in
            {editor_state with text = new_text; cursor_line = new_cursor_line; top_line = new_top_line; cursor_col = new_cursor_col})
  | `Key (`Arrow `Right, []) ->
      let cursor_col = min editor_state.cursor_col (String.length editor_state.text.current + 1) in
      if cursor_col <= String.length editor_state.text.current then
        {editor_state with cursor_col = cursor_col + 1}
      else
        (match editor_state.text.after with
        | [] -> editor_state
        | x::xs ->
            let new_text = {before = editor_state.text.current :: editor_state.text.before; current = x; after = xs} in
            let new_cursor_line = editor_state.cursor_line + 1 in
            {editor_state with text = new_text; cursor_line = new_cursor_line; cursor_col = 1})
  | `Key (`Arrow `Up, [`Shift]) ->
      let text = editor_state.text in
      (match text.before with
      | [] -> editor_state
      | x::xs ->
          let new_text = {before = xs; current = text.current; after = x::text.after} in
          let new_selected_line = editor_state.cursor_line - 1 in
          let new_top_line = if new_selected_line < editor_state.top_line then new_selected_line else editor_state.top_line in
          {editor_state with text = new_text; cursor_line = new_selected_line; top_line = new_top_line})
  | `Key (`Arrow `Down, [`Shift]) ->
      let text = editor_state.text in
      (match text.after with
      | [] -> editor_state
      | x::xs ->
          let new_text = {before = x::text.before; current = text.current; after = xs} in
          let new_selected_line = editor_state.cursor_line + 1 in
          {editor_state with text = new_text; cursor_line = new_selected_line})
  | `Key (`Arrow `Left, [`Shift]) ->
      move_editor_left editor_state
  | `Key (`Arrow `Right, [`Shift]) ->
      move_editor_right editor_state
  | `Key (`ASCII c, []) ->
      let current_line = editor_state.text.current in
      let cursor_col = min editor_state.cursor_col (String.length current_line + 1) in
      let new_current_line = 
        (String.sub current_line 0 (cursor_col - 1)) ^ (String.make 1 c) ^ (String.sub current_line (cursor_col - 1) (String.length current_line - cursor_col + 1))
      in
      let new_text = {editor_state.text with current = new_current_line} in
      {editor_state with text = new_text; cursor_col = cursor_col + 1}
  | `Key (`Backspace, []) ->
      let current_line = editor_state.text.current in
      let cursor_col = min editor_state.cursor_col (String.length current_line + 1) in
      if cursor_col > 1 then
        let new_current_line = 
          (String.sub current_line 0 (cursor_col - 2)) ^ (String.sub current_line (cursor_col - 1) (String.length current_line - cursor_col + 1))
        in
        let new_text = {editor_state.text with current = new_current_line} in
        {editor_state with text = new_text; cursor_col = cursor_col - 1}
      else
        (match editor_state.text.before with
        | [] -> editor_state
        | x::xs ->
            let new_current_line = x ^ current_line in
            let new_text = {before = xs; current = new_current_line; after = editor_state.text.after} in
            let new_cursor_line = editor_state.cursor_line - 1 in
            let new_top_line =
              if new_cursor_line < editor_state.top_line then
                max 1 (editor_state.top_line - 1)
              else
                editor_state.top_line
            in
            let new_cursor_col = String.length x + 1 in
            {editor_state with text = new_text; cursor_line = new_cursor_line; top_line = new_top_line; cursor_col = new_cursor_col})
  | `Key (`Delete, []) ->
      let current_line = editor_state.text.current in
      let cursor_col = min editor_state.cursor_col (String.length current_line + 1) in
      if cursor_col <= String.length current_line then
        let new_current_line = 
          (String.sub current_line 0 (cursor_col - 1)) ^ (String.sub current_line cursor_col (String.length current_line - cursor_col))
        in
        let new_text = {editor_state.text with current = new_current_line} in
        {editor_state with text = new_text}
      else
        (match editor_state.text.after with
        | [] -> editor_state
        | x::xs ->
            let new_current_line = current_line ^ x in
            let new_text = {before = editor_state.text.before; current = new_current_line; after = xs} in
            {editor_state with text = new_text})
  | `Key (`Tab, []) ->
      let current_line = editor_state.text.current in
      let cursor_col = min editor_state.cursor_col (String.length current_line + 1) in
      let new_current_line = "    " ^ current_line in
      let new_text = {editor_state.text with current = new_current_line} in
      {editor_state with text = new_text; cursor_col = cursor_col + 4}
  | `Key (`Tab, [`Shift]) ->
      let current_line = editor_state.text.current in
      let cursor_col = min editor_state.cursor_col (String.length current_line + 1) in
      let starts_with_indent = String.starts_with ~prefix:"    " current_line in
      let new_current_line = if starts_with_indent then String.sub current_line 4 (String.length current_line - 4) else current_line in
      let new_text = {editor_state.text with current = new_current_line} in
      {editor_state with text = new_text; cursor_col = if starts_with_indent then max 1 (cursor_col - 4) else cursor_col}
  | `Key (`Enter, []) ->
      let current_line = editor_state.text.current in
      let current_indent =
        let rec count_indent s acc =
          match s with
          | ' '::' '::' '::' '::rest -> count_indent rest (acc + 1)
          | _ -> acc
        in count_indent (List.of_seq (String.to_seq current_line)) 0
      in
      let cursor_col = min editor_state.cursor_col (String.length current_line + 1) in
      let new_indent_text = if (cursor_col = String.length current_line + 1) && (current_line <> "") then
        if current_line.[String.length current_line - 1] = ':' then
          String.make ((current_indent + 1) * 4) ' '
        else
          String.make (current_indent * 4) ' '
        else
          String.make (current_indent* 4) ' '
        in
      let new_current_line = String.sub current_line 0 (cursor_col - 1) in
      let new_next_line = new_indent_text ^ (String.sub current_line (cursor_col - 1) (String.length current_line - cursor_col + 1)) in
      let new_text = {before = new_current_line :: editor_state.text.before; current = new_next_line; after = editor_state.text.after} in
      let new_cursor_line = editor_state.cursor_line + 1 in
      {editor_state with text = new_text; cursor_line = new_cursor_line; cursor_col = String.length new_indent_text + 1}
  | _ -> editor_state
