open Spreadsheet
open Sheet_types.Prelude


let increase_cost (c : int) : unit comp =
  let open Computation in
  fun data ->
    let new_cost = data.cost + c in
    if new_cost > data.max_cost then
      (Error VTimeLimitExceededError, data)
    else
      (Ok (), {data with cost = new_cost})


let plus = VBuiltin ((fun args ->
  match args with
  | [VInt a; VInt b] -> return (VInt (a + b))
  | [VFloat a; VFloat b] -> return (VFloat (a +. b))
  | [VInt a; VFloat b] -> return (VFloat (float_of_int a +. b))
  | [VFloat a; VInt b] -> return (VFloat (a +. float_of_int b))
  | [VList a; VList b] -> return (VList (SekP.concat a b))
  | [VInt a; VBool b] | [VBool b; VInt a] -> return (VInt (a + (if b then 1 else 0)))
  | [VFloat a; VBool b] | [VBool b; VFloat a] -> return (VFloat (a +. (if b then 1.0 else 0.0)))
  | [VSet a; VSet b] ->
    let* _ = increase_cost (ValueSet.cardinal a + ValueSet.cardinal b) in
    return (VSet (ValueSet.union a b))
  | [VDict a; VDict b] ->
    let* _ = increase_cost (ValueMap.cardinal a + ValueMap.cardinal b) in
    return (VDict (ValueMap.union (fun _ _ v -> Some v) a b))
  | _ -> fail VInvalidArgumentsError
), -1)


let minus = VBuiltin ((fun args ->
  match args with
  | [VInt a; VInt b] -> return (VInt (a - b))
  | [VFloat a; VFloat b] -> return (VFloat (a -. b))
  | [VInt a; VFloat b] -> return (VFloat (float_of_int a -. b))
  | [VFloat a; VInt b] -> return (VFloat (a -. float_of_int b))
  | [VSet a; VSet b] ->
    let* _ = increase_cost (ValueSet.cardinal a + ValueSet.cardinal b) in
    return (VSet (ValueSet.diff a b))
  | _ -> fail VInvalidArgumentsError
), -2)


let mult = VBuiltin ((fun args ->
  match args with
  | [VInt a; VInt b] -> return (VInt (a * b))
  | [VFloat a; VFloat b] -> return (VFloat (a *. b))
  | [VInt a; VFloat b] -> return (VFloat (float_of_int a *. b))
  | [VFloat a; VInt b] -> return (VFloat (a *. float_of_int b))
  | [VList a; VInt b] | [VInt b; VList a] -> 
    let* _ = increase_cost b in
    let repeat s n =
      if n <= 0 then SekP.make VNone 0 VNone
      else
        let rec loop acc cur n =
          if n = 0 then acc
          else
            let acc = if n land 1 = 1 then SekP.concat acc cur else acc in
            let cur = SekP.concat cur cur in
            loop acc cur (n lsr 1)
        in
        loop (SekP.make VNone 0 VNone) s n
    in return (VList (repeat a b))
  | _ -> fail VInvalidArgumentsError
), -3)


let pow = VBuiltin ((fun args ->
  match args with
  | [VInt a; VInt b] -> return (VFloat (float_of_int a ** float_of_int b))
  | [VFloat a; VFloat b] -> return (VFloat (a ** b))
  | [VInt a; VFloat b] -> return (VFloat (float_of_int a ** b))
  | [VFloat a; VInt b] -> return (VFloat (a ** float_of_int b))
  | _ -> fail VInvalidArgumentsError
), -4)


let divide = VBuiltin ((fun args ->
  match args with
  | [VInt a; VInt b] ->
      if b = 0 then fail VDivisionByZeroError
      else return (VFloat (float_of_int a /. float_of_int b))
  | [VFloat a; VFloat b] ->
      if b = 0.0 then fail VDivisionByZeroError
      else return (VFloat (a /. b))
  | [VInt a; VFloat b] ->
      if b = 0.0 then fail VDivisionByZeroError
      else return (VFloat (float_of_int a /. b))
  | [VFloat a; VInt b] ->
      if b = 0 then fail VDivisionByZeroError
      else return (VFloat (a /. float_of_int b))
  | _ -> fail VInvalidArgumentsError
), -5)


let uminus = VBuiltin ((fun args ->
  match args with
  | [VInt a] -> return (VInt (-a))
  | [VFloat a] -> return (VFloat (-.a))
  | _ -> fail VInvalidArgumentsError
), -6)


let uplus = VBuiltin ((fun args ->
  match args with
  | [VInt a] -> return (VInt a)
  | [VFloat a] -> return (VFloat a)
  | _ -> fail VInvalidArgumentsError
), -7)


let rec equals_func args =
  match args with
  | [VInt a; VInt b] -> return (VBool (a = b))
  | [VFloat a; VFloat b] -> return (VBool (a = b))
  | [VInt a; VFloat b] -> return (VBool (float_of_int a = b))
  | [VFloat a; VInt b] -> return (VBool (a = float_of_int b))
  | [VChar a; VChar b] -> return (VBool (a = b))
  | [VBool a; VBool b] -> return (VBool (a = b))
  | [VNone; VNone] -> return (VBool true)
  | [VList a; VList b] ->
    if SekP.length a <> SekP.length b then
      return (VBool false)
    else
      let* _ = increase_cost (SekP.length a) in
      return (VBool (SekP.equal (fun x y -> ValueOrderedType.compare x y = 0) a b))
  | [VMatrix (a, row_len1); VMatrix (b, row_len2)] ->
    if SekP.length a <> SekP.length b || row_len1 <> row_len2 then
      return (VBool false)
    else
      let* _ = increase_cost (SekP.length a / row_len1) in
      return (VBool (SekP.equal (fun x y -> ValueOrderedType.compare x y = 0) a b))
  | [VDict a; VDict b] ->
    let* _ = increase_cost (ValueMap.cardinal a + ValueMap.cardinal b) in
    return (VBool (ValueMap.equal (=) a b))
  | [VSet a; VSet b] ->
    let* _ = increase_cost (ValueSet.cardinal a + ValueSet.cardinal b) in
    return (VBool (ValueSet.equal a b))
  | [VCustom (id1, v1); VCustom (id2, v2)] ->
    if id1 <> id2 then
      return (VBool false)
    else
      let* res = equals_func [v1; v2] in
      return res
  | [VBuiltin (_ , id1); VBuiltin (_, id2)] -> return (VBool (id1 = id2))
  | [VDef (_, _, _, _, _, id1); VDef (_, _, _, _, _, id2)] -> return (VBool (id1 = id2))
  | _ -> return (VBool false)

let equals = VBuiltin (equals_func, -8)

let not_equals = VBuiltin ((fun args ->
  let* res = equals_func args in
  match res with
  | VBool b -> return (VBool (not b))
  | _ -> fail VInvalidArgumentsError
), -9)


let lte_func args =
  match args with
  | [VInt a; VInt b] -> return (VBool (a <= b))
  | [VFloat a; VFloat b] -> return (VBool (a <= b))
  | [VInt a; VFloat b] -> return (VBool (float_of_int a <= b))
  | [VFloat a; VInt b] -> return (VBool (a <= float_of_int b))
  | [VList a; VList b] ->
    let* _ = increase_cost (max (SekP.length a) (SekP.length b)) in
    return (VBool (SekP.compare ValueOrderedType.compare a b <= 0))
  | [VChar a; VChar b] -> return (VBool (a <= b))
  | [VBool a; VBool b] -> return (VBool (a <= b))
  | [VSet a; VSet b] -> 
    let* _ = increase_cost (ValueSet.cardinal a + ValueSet.cardinal b) in
    return (VBool (ValueSet.subset a b))
  | _ -> fail VInvalidArgumentsError

let lte = VBuiltin (lte_func, -10)

let gte = VBuiltin ((fun args -> lte_func (List.rev args)), -11)

let lt = VBuiltin ((fun args ->
  let* res = lte_func args in
  match res with
  | VBool false -> return (VBool false)
  | VBool true ->
      let* eq = equals_func args in
      (match eq with
      | VBool eq -> return (VBool (not eq))
      | _ -> fail VInvalidArgumentsError)
  | _ -> fail VInvalidArgumentsError
), -12)

let gt = VBuiltin ((fun args ->
  let* res = lte_func (List.rev args) in
  match res with
  | VBool false -> return (VBool false)
  | VBool true ->
      let* eq = equals_func args in
      (match eq with
      | VBool eq -> return (VBool (not eq))
      | _ -> fail VInvalidArgumentsError)
  | _ -> fail VInvalidArgumentsError
), -13)


let getitem = VBuiltin ((fun args ->
  match args with
  | [VDict dict; key] ->
      (match ValueMap.find_opt key dict with
      | Some v -> return v
      | None -> fail VInvalidArgumentsError)
  | [VList lst; VInt index] ->
      let len = SekP.length lst in
      let idx = if index < 0 then (len + index) else index in
      if idx < 0 || idx >= len then
        fail VIndexError
      else
        return (SekP.get lst idx)
  | [VMatrix (values, row_len); VInt index] ->
      let len = SekP.length values in
      let row_count = if row_len = 0 then 0 else (len / row_len) in
      if row_len = 0 then
        fail VIndexError
      else
      let idx = if index < 0 then (row_count + index) else index in
      if idx < 0 || idx >= row_count then
        fail VIndexError
      else
        return (SekP.sub values (idx * row_len) row_len |> fun row_values -> VList row_values)
  | [VMatrix (values, row_len); VList indices] ->
      let len = SekP.length values in
      let row_count = if row_len = 0 then 0 else (len / row_len) in
      if row_len = 0 then
        fail VIndexError
      else
      if SekP.length indices <> 2 then
        fail VIndexError
      else
      let ridx = SekP.get indices 0 in
      let cidx = SekP.get indices 1 in
      let row_index = match ridx with
      | VInt n -> if n < 0 then (row_count + n) else n
      | _ -> -1 in
      let col_index = match cidx with
      | VInt n -> if n < 0 then (row_len + n) else n
      | _ -> -1 in
      if row_index < 0 || row_index >= row_count || col_index < 0 || col_index >= row_len then
        fail VIndexError
      else
        return (SekP.get values (row_index * row_len + col_index))
  | _ -> fail VInvalidArgumentsError
), -14)


let setitem = VBuiltin ((fun args ->
  match args with
  | [VDict dict; key; value] ->
      let new_dict = ValueMap.add key value dict in
      return (VDict new_dict)
  | [VList lst; VInt index; value] ->
      let len = SekP.length lst in
      let idx = if index < 0 then (len + index) else index in
      if idx < 0 || idx >= len then
        fail VIndexError
      else
        let new_lst = SekP.set lst idx value in
        return (VList new_lst)
  | [VMatrix (values, row_len); VInt index; VList value] ->
      let len = SekP.length values in
      let row_count = if row_len = 0 then 0 else (len / row_len) in
      if row_len = 0 then
        fail VIndexError
      else
      let idx = if index < 0 then (row_count + index) else index in
      if idx < 0 || idx >= row_count then
        fail VIndexError
      else
        let row_len_value = SekP.length value in
        if row_len_value <> row_len then
          fail VIndexError
        else
          let new_values = SekP.concat (SekP.sub values 0 (idx * row_len)) (SekP.concat value (SekP.sub values ((idx + 1) * row_len) (len - (idx + 1) * row_len))) in
          return (VMatrix (new_values, row_len))
  | [VMatrix (values, row_len); VList indices; value] ->
      let len = SekP.length values in
      let row_count = if row_len = 0 then 0 else (len / row_len) in
      if row_len = 0 then
        fail VIndexError
      else
      if SekP.length indices <> 2 then
        fail VIndexError
      else
      let ridx = SekP.get indices 0 in
      let cidx = SekP.get indices 1 in
      let row_index = match ridx with
      | VInt n -> if n < 0 then (row_count + n) else n
      | _ -> -1 in
      let col_index = match cidx with
      | VInt n -> if n < 0 then (row_len + n) else n
      | _ -> -1 in
      if row_index < 0 || row_index >= row_count || col_index < 0 || col_index >= row_len then
        fail VIndexError
      else
        let new_values = SekP.set values (row_index * row_len + col_index) value in
        return (VMatrix (new_values, row_len))
  | _ -> fail VInvalidArgumentsError
), -15)


let delitem = VBuiltin ((fun args ->
  match args with
  | [VDict dict; key] ->
      let new_dict = ValueMap.remove key dict in
      return (VDict new_dict)
  | [VList lst; VInt index] ->
      let len = SekP.length lst in
      let idx = if index < 0 then (len + index) else index in
      if idx < 0 || idx >= len then
        fail VIndexError
      else
        let front, back = SekP.split lst idx in
        let new_lst = SekP.concat front (snd (SekP.pop Sek.front back)) in
        return (VList new_lst)
  | [VMatrix (values, row_len); VInt index] ->
      let len = SekP.length values in
      let row_count = if row_len = 0 then 0 else (len / row_len) in
      if row_len = 0 then
        fail VIndexError
      else
      let idx = if index < 0 then (row_count + index) else index in
      if idx < 0 || idx >= row_count then
        fail VIndexError
      else
        let front, back = SekP.split values (idx * row_len) in
        let new_values = SekP.concat front (SekP.sub back (row_len) (len - (idx + 1) * row_len)) in
        if SekP.is_empty new_values then
          return (VMatrix (new_values, 0))
        else
          return (VMatrix (new_values, row_len))
  | _ -> fail VInvalidArgumentsError
), -16)


let len = VBuiltin ((fun args ->
  match args with
  | [VList lst] -> return (VInt (SekP.length lst))
  | [VMatrix (values, row_len)] ->
    if row_len = 0 then return (VList (SekP.of_list_segment VNone 2 [VInt 0; VInt 0]))
    else
      return (VList (SekP.of_list_segment VNone 2 [VInt (SekP.length values / row_len); VInt row_len]))
  | [VDict dict] -> return (VInt (ValueMap.cardinal dict))
  | [VSet s] -> return (VInt (ValueSet.cardinal s))
  | _ -> fail VInvalidArgumentsError
), -17)


let default_formatter = VBuiltin ((fun args ->
  match args with
  | [value; VInt width; VInt height] ->
    let text = Print.string_of_value_limitted value ((width+1) * height) in
    return (VList (SekP.of_list VNone (List.map (fun c -> VChar c) (List.init (String.length text) (String.get text))) ))
  | _ -> fail VInvalidArgumentsError
), -18)


let str = VBuiltin ((fun args ->
  match args with
  | [value] ->
    let text = Print.string_of_value value in
    let* _ = increase_cost (String.length text) in
    return (VList (SekP.of_list VNone (List.map (fun c -> VChar c) (List.init (String.length text) (String.get text))) ))
  | _ -> fail VInvalidArgumentsError
), -19)


let toInt = VBuiltin ((fun args ->
  match args with
  | [VInt n] -> return (VInt n)
  | [VFloat f] ->
    let res = int_of_float f in
    if res = 0 && abs_float f >= 1.0 then
      fail VInvalidArgumentsError
    else
    return (VInt res)
  | [VChar c] -> return (VInt (Char.code c))
  | [VBool b] -> return (VInt (if b then 1 else 0))
  | [VList chars] ->
      if SekP.length chars >= 25 then
        fail VInvalidArgumentsError
      else
      let chars = SekP.to_list chars in
      if List.for_all (function VChar _ -> true | _ -> false) chars then
        let s = String.concat "" (List.map (function VChar c -> String.make 1 c | _ -> "") chars) in
        (try
          return (VInt (int_of_string s))
        with _ ->
          fail VInvalidArgumentsError)
      else
        fail VInvalidArgumentsError
  | _ -> fail VInvalidArgumentsError
), -20)


let toFloat = VBuiltin ((fun args ->
  match args with
  | [VFloat f] -> return (VFloat f)
  | [VInt n] -> return (VFloat (float_of_int n))
  | [VChar c] -> return (VFloat (float_of_int (Char.code c)))
  | [VBool b] -> return (VFloat (if b then 1.0 else 0.0))
  | [VList chars] ->
      let chars = SekP.to_list chars in
      if List.for_all (function VChar _ -> true | _ -> false) chars then
        let s = String.concat "" (List.map (function VChar c -> String.make 1 c | _ -> "") chars) in
        (try
          return (VFloat (float_of_string s))
        with _ ->
          fail VInvalidArgumentsError)
      else
        fail VInvalidArgumentsError
  | _ -> fail VInvalidArgumentsError
), -21)


let toBool = VBuiltin ((fun args ->
  match args with
  | [VBool b] -> return (VBool b)
  | [VInt n] -> return (VBool (n <> 0))
  | [VFloat f] -> return (VBool (f <> 0.0))
  | [VNone] -> return (VBool false)
  | [VList lst] -> return (VBool (not (SekP.is_empty lst)))
  | [VMatrix (_, row_len)] -> return (VBool (row_len <> 0))
  | [VDict dict] -> return (VBool (ValueMap.cardinal dict <> 0))
  | [VSet s] -> return (VBool (ValueSet.cardinal s <> 0))
  | _ -> fail VInvalidArgumentsError
), -22)


let toList = VBuiltin ((fun args ->
  match args with
  | [VList _] -> return (List.hd args)
  | [VMatrix (values, _)] -> return (VList values)
  | [VDict dict] ->
    let* _ = increase_cost (ValueMap.cardinal dict) in
    return (VList (SekP.of_list VNone (List.map (fun (k, v) -> VList (SekP.of_list_segment VNone 2 [k; v])) (ValueMap.bindings dict))))
  | [VSet s] -> 
    let* _ = increase_cost (ValueSet.cardinal s) in
    return (VList (SekP.of_list VNone (ValueSet.elements s)))
  | _ -> fail VInvalidArgumentsError
), -23)


let toMatrix = VBuiltin ((fun args ->
  match args with
  | [VMatrix _] -> return (List.hd args)
  | [VList lst] ->
      if SekP.is_empty lst then return (VMatrix (SekP.of_list VNone [],0))
      else
        if not (SekP.for_all (function VList _ -> true | _ -> false) lst) then
          fail VInvalidArgumentsError
        else 
          let* _ = increase_cost (SekP.length lst) in
          SekP.fold_left (fun acc v ->
            let* acc = acc in
            match acc, v with
            | VMatrix (values, row_len), VList row ->
              let row_len' = SekP.length row in
              if row_len = -1 then
                return (VMatrix (SekP.concat values row, row_len'))
              else if row_len = row_len' then
                return (VMatrix (SekP.concat values row, row_len))
              else
                fail VInvalidArgumentsError
            | _, _ -> fail VInvalidArgumentsError
          ) (return (VMatrix (SekP.of_list VNone [], -1))) lst
  | _ -> fail VInvalidArgumentsError
), -24)


let toSet = VBuiltin ((fun args ->
  match args with
  | [VSet s] -> return (VSet s)
  | [] -> return (VSet ValueSet.empty)
  | [VList lst] ->
    let* _ = increase_cost (SekP.length lst) in
    return (VSet (SekP.fold_left (fun acc v -> ValueSet.add v acc) ValueSet.empty lst))
  | _ -> fail VInvalidArgumentsError
), -25)


let toDict = VBuiltin ((fun args ->
  match args with
  | [VDict dict] -> return (VDict dict)
  | [] -> return (VDict ValueMap.empty)
  | [VList lst] ->
      let* _ = increase_cost (SekP.length lst) in
      let rec aux items acc =
        match SekP.pop_opt Sek.front items with
        | None, _ -> return (VDict (List.fold_left (fun m (k, v) -> ValueMap.add k v m) ValueMap.empty acc))
        | Some (VList lst), xs ->
          if SekP.length lst <> 2 then
            fail VInvalidArgumentsError
          else
            let k = SekP.get lst 0 in
            let v = SekP.get lst 1 in
          aux xs ((k, v) :: acc)
        | _ -> fail VInvalidArgumentsError
      in
      aux lst []
  | _ -> fail VInvalidArgumentsError
), -26)


let round = VBuiltin ((fun args ->
  match args with
  | [VFloat f] -> return (VInt (int_of_float (Float.round f)))
  | [VFloat f; VInt n] ->
      let factor = 10.0 ** float_of_int n in
      return (VFloat (Float.round (f *. factor) /. factor))
  | [VInt n] -> return (VInt n)
  | _ -> fail VInvalidArgumentsError
), -27)


let zip =
  VBuiltin ((fun args ->
    let rec all_heads_tails lists =
      match lists with
      | [] -> Some ([], [])
      | [] :: _ -> None
      | (x :: xs) :: rest ->
          match all_heads_tails rest with
          | None -> None
          | Some (heads, tails) ->
              Some (x :: heads, xs :: tails)
    in

    let rec aux lists acc =
      match all_heads_tails lists with
      | None ->
          return (VList (List.rev acc |> SekP.of_list VNone))
      | Some (heads, tails) ->
          let* () = increase_cost (List.length heads) in
          aux tails (VList (SekP.of_list VNone heads) :: acc)
    in

    match args with
    | [] ->
        fail VInvalidArgumentsError
    | _ ->
        let lists = if not (List.for_all (function VList _ -> true | _ -> false) args) then
          []
        else List.map (function
              | VList l -> SekP.to_list l
              | _ -> []
            ) args
        in
        aux lists []
  ), -28)


let mmod = VBuiltin ((fun args ->
  match args with
  | [VInt a; VInt b] ->
      if b = 0 then fail VDivisionByZeroError
      else return (VInt (a mod b))
  | [VFloat a; VFloat b] ->
      if b = 0.0 then fail VDivisionByZeroError
      else return (VFloat (mod_float a b))
  | [VInt a; VFloat b] ->
      if b = 0.0 then fail VDivisionByZeroError
      else return (VFloat (mod_float (float_of_int a) b))
  | [VFloat a; VInt b] ->
      if b = 0 then fail VDivisionByZeroError
      else return (VFloat (mod_float a (float_of_int b)))
  | _ -> fail VInvalidArgumentsError
), -29)


let iin = VBuiltin ((fun args ->
  match args with
  | [item; VList lst] ->
      let* _ = increase_cost (SekP.length lst) in
      return (VBool (SekP.exists (fun v -> ValueOrderedType.compare item v = 0) lst))
  | [item; VMatrix (values, _)] ->
      let* _ = increase_cost (SekP.length values) in
      return (VBool (SekP.exists (fun v -> ValueOrderedType.compare item v = 0) values))
  | [item; VSet s] ->
      return (VBool (ValueSet.mem item s))
  | [item; VDict dict] ->
      return (VBool (ValueMap.mem item dict))
  | _ -> fail VInvalidArgumentsError
), -30)


let rows = VBuiltin ((fun args ->
  match args with
  | [VMatrix (values, row_len)] ->
      if row_len = 0 then return (VList values)
      else 
        let rec aux acc r =
          if r * row_len >= SekP.length values then
            return (VList (List.rev acc |> SekP.of_list VNone))
          else
            let row = SekP.sub values (r * row_len) row_len in
            aux (VList row :: acc) (r + 1)
        in
        let* _ = increase_cost (SekP.length values / row_len) in
        aux [] 0
  | _ -> fail VInvalidArgumentsError
), -31)


let columns = VBuiltin ((fun args ->
  match args with
  | [VMatrix (values, row_len)] ->
      if row_len = 0 then return (VList values)
      else 
        let col_len = SekP.length values / row_len in
        let rec aux acc c =
          if c >= row_len then
            return (VList (List.rev acc |> SekP.of_list VNone))
          else
            let rec get_column r acc_col =
              if r >= col_len then
                SekP.of_list VNone (List.rev acc_col)
              else
                let v = SekP.get values (r * row_len + c) in
                get_column (r + 1) (v :: acc_col)
            in
            let column = get_column 0 [] in
            aux (VList column :: acc) (c + 1)
        in
        let* _ = increase_cost (SekP.length values) in
        aux [] 0
  | _ -> fail VInvalidArgumentsError
), -32)


let transpose = VBuiltin ((fun args ->
  match args with
  | [VMatrix (values, row_len)] ->
      if row_len = 0 then return (List.hd args)
      else 
        let col_len = SekP.length values / row_len in
        let rec aux c acc =
          if c >= row_len then 
            return (VMatrix (acc, col_len))
          else
            let rec get_column r acc_col =
              if r >= col_len then
                SekP.of_list VNone (List.rev acc_col)
              else
                let v = SekP.get values (r * row_len + c) in
                get_column (r + 1) (v :: acc_col)
            in
            let column = get_column 0 [] in
            aux (c + 1) (SekP.concat acc column)
        in
        let* _ = increase_cost (SekP.length values) in
        aux 0 (SekP.of_list VNone [])
  | _ -> fail VInvalidArgumentsError
), -33)


let range = VBuiltin ((fun args ->
  match args with
  | [VInt stop] ->
      if stop <= 0 then
        return (VList (SekP.of_list VNone []))
      else
        let* _ = increase_cost stop in
        let rec aux n acc =
          if n >= stop then
            return (VList (List.rev acc |> SekP.of_list VNone))
          else
            aux (n + 1) (VInt n :: acc)
        in
        aux 0 []
  | [VInt start; VInt stop] ->
      if start >= stop then
        return (VList (SekP.of_list VNone []))
      else
        let* _ = increase_cost (stop - start) in
        let rec aux n acc =
          if n >= stop then
            return (VList (List.rev acc |> SekP.of_list VNone))
          else
            aux (n + 1) (VInt n :: acc)
        in
        aux start []
  | [VInt start; VInt stop; VInt step] ->
      if step = 0 then
        fail VInvalidArgumentsError
      else if (step > 0 && start >= stop) || (step < 0 && start <= stop) then
        return (VList (SekP.of_list VNone []))
      else
        let* _ = increase_cost (abs (stop - start) / abs step) in
        let rec aux n acc =
          if (step > 0 && n >= stop) || (step < 0 && n <= stop) then
            return (VList (List.rev acc |> SekP.of_list VNone))
          else
            aux (n + step) (VInt n :: acc)
        in
        aux start []
  | _ -> fail VInvalidArgumentsError
), -34)


let slice = VBuiltin ((fun args ->
  match args with
  | [VList lst; VInt stop]  ->
      let len = SekP.length lst in
      let e = if stop < 0 then max 0 (len + stop) else min len stop in
      if e <= 0 then
        return (VList (SekP.of_list VNone []))
      else 
        return (VList (SekP.sub lst 0 e))
  | [VList lst; VInt start; VInt stop] | [VList lst; VInt start; VInt stop; VInt 1] ->
      let len = SekP.length lst in
      let s = if start < 0 then max 0 (len + start) else min len start in
      let e = if stop < 0 then max 0 (len + stop) else min len stop in
      if s >= e then
        return (VList (SekP.of_list VNone []))
      else
        return (VList (SekP.sub lst s (e - s)))
  | [VList lst; VInt start; VNone]  ->
      let len = SekP.length lst in
      let s = if start < 0 then max 0 (len + start) else min len start in
      if s >= len then
        return (VList (SekP.of_list VNone []))
      else
        return (VList (SekP.sub lst s (len - s)))
  | [VList lst; VInt start; VInt stop; VInt step]  ->
      let len = SekP.length lst in
      let s = if start < 0 then max 0 (len + start) else min len start in
      let e = if stop < 0 then max 0 (len + stop) else min len stop in
      if step = 0 then
        fail VInvalidArgumentsError
      else
        let rec aux i acc =
          if (step > 0 && i >= e) || (step < 0 && i <= e) then
            let* _ = increase_cost (List.length acc) in
            return (VList (List.rev acc |> SekP.of_list VNone))
          else
            aux (i + step) (SekP.get lst i :: acc)
        in
        aux s []
  | [VList lst; VInt start; VNone; VInt step]  ->
      let len = SekP.length lst in
      let s = if start < 0 then max 0 (len + start) else min len start in
      if step = 0 then
        fail VInvalidArgumentsError
      else
        let rec aux i acc =
          if (step > 0 && i >= len) || (step < 0 && i < 0) then
            let* _ = increase_cost (List.length acc) in
            return (VList (List.rev acc |> SekP.of_list VNone))
          else
            aux (i + step) (SekP.get lst i :: acc)
        in
        aux s []
  | _ -> fail VInvalidArgumentsError
), -35)


let cell = VBuiltin ((fun args ->
  let args = if List.length args = 2 then VNone :: args else args in
  let* args = match args with
    | [VNone; VInt col; VInt row] -> 
      let* curr = get_cell_address in
      (match curr with
      | Some (sheet_name, (_, _)) -> return (sheet_name, col, row)
      | None -> fail VInvalidArgumentsError)
    | [VList name; VInt col; VInt row] -> 
      let sheet_name = name |> SekP.to_list |> List.map (function VChar c -> String.make 1 c | _ -> "") |> String.concat "" in
      let* sheet_name = if sheet_name <> "" then return sheet_name else
        let* curr = get_cell_address in
        (match curr with
        | Some (sheet_name, (_, _)) -> return sheet_name
        | None -> fail VInvalidArgumentsError)
      in 
      return (sheet_name, col, row)
    | _ -> fail VInvalidArgumentsError
  in
  let (sheet_name, col, row) = args in
    let* value = get_cell_value (sheet_name, (col, row)) in
    return value
), -36)


let start_var_env : var_env =
let start_var_env = StringMap.empty in
let start_var_env = StringMap.add "+" plus start_var_env in
let start_var_env = StringMap.add "-" minus start_var_env in
let start_var_env = StringMap.add "*" mult start_var_env in
let start_var_env = StringMap.add "/" divide start_var_env in
let start_var_env = StringMap.add "%" mmod start_var_env in
let start_var_env = StringMap.add "**" pow start_var_env in
let start_var_env = StringMap.add "__neg__" uminus start_var_env in
let start_var_env = StringMap.add "__pos__" uplus start_var_env in
let start_var_env = StringMap.add "==" equals start_var_env in
let start_var_env = StringMap.add "!=" not_equals start_var_env in
let start_var_env = StringMap.add "<=" lte start_var_env in
let start_var_env = StringMap.add ">=" gte start_var_env in
let start_var_env = StringMap.add "<" lt start_var_env in
let start_var_env = StringMap.add ">" gt start_var_env in
let start_var_env = StringMap.add "__getitem__" getitem start_var_env in
let start_var_env = StringMap.add "__setitem__" setitem start_var_env in
let start_var_env = StringMap.add "__delitem__" delitem start_var_env in
let start_var_env = StringMap.add "__in__" iin start_var_env in
let start_var_env = StringMap.add "len" len start_var_env in
let start_var_env = StringMap.add "int" toInt start_var_env in
let start_var_env = StringMap.add "float" toFloat start_var_env in
let start_var_env = StringMap.add "bool" toBool start_var_env in
let start_var_env = StringMap.add "list" toList start_var_env in
let start_var_env = StringMap.add "matrix" toMatrix start_var_env in
let start_var_env = StringMap.add "set" toSet start_var_env in
let start_var_env = StringMap.add "dict" toDict start_var_env in
let start_var_env = StringMap.add "round" round start_var_env in
let start_var_env = StringMap.add "zip" zip start_var_env in
let start_var_env = StringMap.add "default_formatter" default_formatter start_var_env in
let start_var_env = StringMap.add "str" str start_var_env in
let start_var_env = StringMap.add "InvalidArgumentsError" VInvalidArgumentsError start_var_env in
let start_var_env = StringMap.add "DivisionByZeroError" VDivisionByZeroError start_var_env in
let start_var_env = StringMap.add "IndexError" VIndexError start_var_env in
let start_var_env = StringMap.add "rows" rows start_var_env in
let start_var_env = StringMap.add "columns" columns start_var_env in
let start_var_env = StringMap.add "transpose" transpose start_var_env in
let start_var_env = StringMap.add "range" range start_var_env in
let start_var_env = StringMap.add "slice" slice start_var_env in
let start_var_env = StringMap.add "cell" cell start_var_env in
start_var_env


let start_type_env : type_env = 
let start_type_env = StringMap.empty in
let start_type_env = StringMap.add "int" TInt start_type_env in
let start_type_env = StringMap.add "float" TFloat start_type_env in
let start_type_env = StringMap.add "char" TChar start_type_env in
let start_type_env = StringMap.add "bool" TBool start_type_env in
let start_type_env = StringMap.add "None" TNone start_type_env in
let start_type_env = StringMap.add "list" TList start_type_env in
let start_type_env = StringMap.add "set" TSet start_type_env in
let start_type_env = StringMap.add "dict" TDict start_type_env in
let start_type_env = StringMap.add "matrix" TMatrix start_type_env in
let start_type_env = StringMap.add "function" TDef start_type_env in
let start_type_env = StringMap.add "InvalidArgumentsError" TInvalidArgumentsError start_type_env in
let start_type_env = StringMap.add "DivisionByZeroError" TDivisionByZeroError start_type_env in
let start_type_env = StringMap.add "IndexError" TIndexError start_type_env in
start_type_env


let start_env = {vars = start_var_env; types = start_type_env}

let start_data : data = {
  env = start_env;
  config = start_env;
  sheets = CellAddressMap.empty;
  dependency_graph = CellAddressMap.empty;
  cell_address = None;
  cost = 0;
  max_cost = 200000;
  fresh = 0
}

