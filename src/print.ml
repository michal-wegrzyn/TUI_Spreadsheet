open Sheet_types.Prelude


let rec string_of_value v =
  match v with
  | VInt n -> string_of_int n
  | VFloat f -> string_of_float f
  | VChar c -> String.make 1 c
  | VBool b -> if b then "True" else "False"
  | VNone -> "None"
  | VList lst ->
      let lst = SekP.to_list lst in
      if List.for_all (function VChar _ -> true | _ -> false) lst && lst <> [] then
      String.concat "" (List.map (function VChar c -> String.make 1 c | _ -> "") lst)
    else
     "[" ^ String.concat ", " (List.map string_of_value lst) ^ "]"
  | VMatrix (mat, row_len) ->
    if row_len = 0 then "[||]"
    else
    let row_cnt = (SekP.length mat) / row_len in
    "[|" ^ String.concat ", " (List.init row_cnt (fun i -> 
      let row = SekP.sub mat (i * row_len) row_len |> SekP.to_list in
      "[" ^ String.concat ", " (List.map string_of_value row) ^ "]"
    )) ^ "|]"
  | VDict d -> let lst = Sheet_types.ValueMap.bindings d in
     "{" ^ String.concat ", " (List.map (fun (k, v) -> string_of_value k ^ " : " ^ string_of_value v) lst) ^ "}"
  | VSet s -> "{" ^ String.concat ", " (List.map string_of_value (Sheet_types.ValueSet.elements s)) ^ "}"
  | VDef _ -> "function"
  | VBuiltin _ -> "function"
  | VExtend _ -> "function"
  | VCustom _ -> "custom"
  | VTimeLimitExceededError -> "TimeLimitExceededError"
  | VDivisionByZeroError -> "DivisionByZeroError"
  | VInvalidArgumentsError -> "InvalidArgumentsError"
  | VIndexError -> "IndexError"
  | VBreak -> "Break"
  | VContinue -> "Continue"


let rec string_of_value_limitted v limit =
  let text =
    match v with
    | VList lst ->
        let lst = SekP.to_list lst in
        let lst =
          if List.length lst > limit then
            List.take limit lst
          else
            lst
        in
        if List.for_all (function VChar _ -> true | _ -> false) lst && lst <> [] then
          String.concat "" (List.map (function VChar c -> String.make 1 c | _ -> "") (List.take limit lst))
        else
          let rec aux lst limit acc =
            match lst, limit with
            | [], _ -> List.rev acc
            | _, n when n <= 0 -> List.rev acc
            | x::xs, n ->
              let str = string_of_value_limitted x limit in
              aux xs (n - (String.length str)-(if acc = [] then 0 else 2)) (str :: acc)
          in
        "[" ^ String.concat ", " (aux lst (limit-1) []) ^ "]"
    | VMatrix (mat, row_len) ->
      if row_len = 0 then "[||]"
      else
      let row_cnt = (SekP.length mat) / row_len in
      let rec aux i limit acc =
        if i >= row_cnt || limit <= 0 then
          List.rev acc
        else
          let row = SekP.sub mat (i * row_len) row_len |> SekP.to_list in
          let rec row_aux row limit acc =
            match row, limit with
            | [], _ -> List.rev acc
            | _, n when n <= 0 -> List.rev acc
            | x::xs, n ->
              let str = string_of_value_limitted x limit in
              row_aux xs (n - (String.length str)-(if acc = [] then 0 else 2)) (str :: acc)
          in
          let row_str = "[" ^ String.concat ", " (row_aux row (limit-1) []) ^ "]" in
          aux (i + 1) (limit - (String.length row_str) - (if acc = [] then 0 else 2)) (row_str :: acc)
      in
      let rows = aux 0 (limit-1) [] in
      "[|" ^ String.concat ", " rows ^ "|]"
    | VDict d -> let lst = Sheet_types.ValueMap.bindings d in
      let rec aux lst limit acc =
        match lst, limit with
        | [], _ -> List.rev acc
        | _, n when n <= 0 -> List.rev acc
        | (k, v)::xs, _ ->
          let key_str = string_of_value_limitted k limit in
          let val_str = string_of_value_limitted v (limit - (String.length key_str) - 3) in
          let pair_str = key_str ^ " : " ^ val_str in
          aux xs (limit - (String.length pair_str) - (if acc = [] then 0 else 2)) (pair_str :: acc)
      in
      "{" ^ String.concat ", " (aux lst (limit-1) []) ^ "}"
    | VSet s ->
      let rec aux lst limit acc =
        match lst, limit with
        | [], _ -> List.rev acc
        | _, n when n <= 0 -> List.rev acc
        | x::xs, _ ->
          let str = string_of_value_limitted x limit in
          aux xs (limit - (String.length str) - (if acc = [] then 0 else 2)) (str :: acc)
      in
      "{" ^ String.concat ", " (aux (Sheet_types.ValueSet.elements s) (limit-1) []) ^ "}"
    | _ -> string_of_value v
  in
  if String.length text > limit then String.sub text 0 limit else text


let string_to_box s width height =
  let len = String.length s in

  let rec build_lines i col current acc =
    if i = len then
      List.rev (current :: acc)
    else
      match s.[i] with
      | '\n' ->
          build_lines (i + 1) 0 "" (current :: acc)
      | c ->
          if col = width then
            build_lines i 0 "" (current :: acc)
          else
            build_lines (i + 1) (col + 1) (current ^ String.make 1 c) acc
  in

  let raw_lines = build_lines 0 0 "" [] in

  let normalize_line line =
    let l = String.length line in
    if l < width then
      line ^ String.make (width - l) ' '
    else
      String.sub line 0 width
  in

  let rec normalize_height lines acc n =
    match lines, n with
    | _, 0 -> List.rev acc
    | [], n ->
        normalize_height [] (normalize_line "" :: acc) (n - 1)
    | l :: ls, n ->
        normalize_height ls (normalize_line l :: acc) (n - 1)
  in

  normalize_height raw_lines [] height
  |> String.concat "\n"


let column_name col =
  let rec aux n =
    if n < 0 then ""
    else
      let rem = n mod 26 in
      let ch = Char.chr (rem + Char.code 'A') in
      aux (n / 26 - 1) ^ String.make 1 ch
  in
  aux (col - 1)


let fit_center text width =
  let len = String.length text in
  if len > width then
    "…" ^ String.sub text (len - width + 1) (width-1)
  else
    let total_padding = width - len in
    let right_padding = total_padding / 2 in
    let left_padding = total_padding - right_padding in
    String.make left_padding ' ' ^ text ^ String.make right_padding ' '


let string_repeat n s =
  if n <= 0 then ""
  else String.concat "" (List.init n (fun _ -> s))

