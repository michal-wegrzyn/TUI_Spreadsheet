open Sheet_types.Prelude


let value_of_str s = VList (SekP.of_list VNone (s |> String.to_seq |> List.of_seq |> List.map (fun c -> VChar c)))


let error_str s = Error (value_of_str s)


let add_cost (d : data) (c : int) : data =
  { d with cost = d.cost + c }


let set_cell_address (d : data) (addr : cell_address) : data =
  { d with cell_address = Some addr; cost = d.cost + 1 }


let add_cell_access (d : data) (addr : cell_address) : data =
    match d.cell_address with
    | None -> d
    | Some cell_addr ->
      if (match CellAddressMap.find_opt cell_addr d.sheets with
          | None -> true
          | Some cell -> cell.value <> Computing) then d else
      let accessed =
        match CellAddressMap.find_opt addr d.dependency_graph with
        | None -> CellAddressSet.empty
        | Some s -> s
      in
      let new_dep_graph = CellAddressMap.add addr (CellAddressSet.add cell_addr accessed) d.dependency_graph in
      let dependencies =
        match CellAddressMap.find_opt cell_addr d.sheets with
        | None -> CellAddressSet.empty
        | Some cell -> cell.dependencies
      in
      let new_dependencies = CellAddressSet.add addr dependencies in
      let new_cell =
        match CellAddressMap.find_opt cell_addr d.sheets with
        | None -> {value = Computed VNone; definition = Return NNone; dependencies = new_dependencies}
        | Some cell -> {cell with dependencies = new_dependencies}
      in
      let new_sheets = CellAddressMap.add cell_addr new_cell d.sheets in
      { d with sheets = new_sheets; dependency_graph = new_dep_graph; cost = d.cost + 1 }


let return x : 'a comp = fun data -> (Ok x, add_cost data 1)


let bind (m : 'a comp) (f: 'a -> 'b comp) : 'b comp =
  fun data ->
    let (res, data') = m data in
    match res with
    | Error s -> (Error s, data')
    | Ok x -> if data'.cost > data'.max_cost then (Error VTimeLimitExceededError, data') else f x (add_cost data' 1)


let (let*) = bind


let get_cell_address : cell_address option comp =
  fun data ->
    (Ok data.cell_address, add_cost data 1)


let lookup_var (var_name: string) : value comp =
  fun data ->
    match StringMap.find_opt var_name data.env.vars with
    | None -> (error_str ("Undefined variable: " ^ var_name), data)
    | Some v -> (Ok v, add_cost data 1)


let lookup_var_opt (var_name: string) : value option comp =
  fun data ->
    let var_env = data.env.vars in
    match StringMap.find_opt var_name var_env with
    | None -> (Ok None, add_cost data 1)
    | Some v -> (Ok (Some v), add_cost data 1)


let add_to_env (var_name: string) (v: value) : unit comp =
  fun data ->
    if var_name = "_" then (Ok (), add_cost data 1) else
    let new_var_env = StringMap.add var_name v data.env.vars in
    (Ok (), { data with env = { data.env with vars = new_var_env }; cost = data.cost + 1 })


let delete_from_env (var_name: string) : unit comp =
  fun data ->
    let new_var_env = StringMap.remove var_name data.env.vars in
    (Ok (), { data with env = { data.env with vars = new_var_env }; cost = data.cost + 1 })


let lookup_type (type_name: string) : value_type comp =
  fun data ->
    match StringMap.find_opt type_name data.env.types with
    | None -> (error_str ("Undefined type: " ^ type_name), data)
    | Some t -> (Ok t, add_cost data 1)


let add_type_to_env (type_name: string) (t: value_type) : unit comp =
  fun data ->
    if type_name = "_" then (Ok (), add_cost data 1) else
    let new_type_env = StringMap.add type_name t data.env.types in
    (Ok (), { data with env = { data.env with types = new_type_env }; cost = data.cost + 1 })


let type_of_value (v: value) : value_type =
  match v with
  | VInt _ -> TInt
  | VFloat _ -> TFloat
  | VChar _ -> TChar
  | VBool _ -> TBool
  | VNone -> TNone
  | VList _ -> TList
  | VMatrix _ -> TMatrix
  | VDict _ -> TDict
  | VSet _ -> TSet
  | VDef _ -> TDef
  | VBuiltin _ -> TDef
  | VExtend _ -> TDef
  | VCustom (id, _) -> TCustom id
  | VTimeLimitExceededError -> TTimeLimitExceededError
  | VDivisionByZeroError -> TDivisionByZeroError
  | VInvalidArgumentsError -> TInvalidArgumentsError
  | VIndexError -> TIndexError
  | VBreak -> TBreak
  | VContinue -> TContinue


let get_env : var_env comp =
  fun data ->
    (Ok data.env.vars, add_cost data 1)


let get_type_env : type_env comp =
  fun data ->
    (Ok data.env.types, add_cost data 1)


let fresh_id : int comp =
  fun data ->
    (Ok data.fresh, { data with cost = data.cost + 1; fresh = data.fresh + 1 })


let set_env (env: var_env) : unit comp =
  fun data ->
    (Ok (), { data with env = { data.env with vars = env }; cost = data.cost + 1 })


let set_type_env (type_env: type_env) : unit comp =
  fun data ->
    (Ok (), { data with env = { data.env with types = type_env }; cost = data.cost + 1 })


let fail s : 'a comp = fun data -> (Error s, data)


let failstr s : 'a comp = fun data -> (error_str s, data)


let catch (m : 'a comp) (f: error -> 'a comp) : 'a comp =
  fun data ->
    let (res, data') = m data in
    match res with
    | Error s -> f s data'
    | Ok x -> (Ok x,  data')


let bind_catch (m : 'a comp) (f: 'a -> 'b comp) (h: error -> 'b comp) : 'b comp =
  fun data ->
    let (res, data') = m data in
    match res with
    | Error s -> h s data'
    | Ok x -> if data'.cost > data'.max_cost then (Error VTimeLimitExceededError, data') else f x (add_cost data' 1)


let finally (m : 'a comp) (finalizer : 'c comp) : 'b comp =
  fun data ->
    let (_, data') = m data in
    finalizer data'


let finally_do (m : 'a comp) (finalizer : unit comp) : 'b comp =
  fun data ->
    let (res, data') = m data in
    (res, snd (finalizer data'))


let temp_var (var_name : string) (m : 'a comp) : 'a comp =
  let* curr_value = lookup_var_opt var_name in
  finally_do m (match curr_value with
    | Some v -> add_to_env var_name v
    | None -> delete_from_env var_name)


let rec setNotComputedYet : accessed_cells comp =
  fun data ->
    match data.cell_address with
    | None -> (Ok CellAddressSet.empty, data)
    | Some addr ->
      let data = (match CellAddressMap.find_opt addr data.sheets with
        | None -> data
        | Some cell ->
          if cell.value = NotComputedYet then
            data
          else
          let new_dependency_graph = CellAddressSet.fold
            (fun dep_addr acc ->
              match CellAddressMap.find_opt dep_addr acc with
              | None -> acc
              | Some s ->
                let new_s = CellAddressSet.remove addr s in
                if CellAddressSet.is_empty new_s then
                  CellAddressMap.remove dep_addr acc
                else
                CellAddressMap.add dep_addr new_s acc)
            (match CellAddressMap.find_opt addr data.dependency_graph with
            | None -> CellAddressSet.empty
            | Some s -> s)
            data.dependency_graph in
          let new_cell = {cell with value = NotComputedYet; dependencies = CellAddressSet.empty} in
          let new_sheets = CellAddressMap.add addr new_cell data.sheets in
        { data with sheets = new_sheets; dependency_graph = new_dependency_graph }) in
        let dependents =
          match CellAddressMap.find_opt addr data.dependency_graph with
          | None -> CellAddressSet.empty
          | Some s -> s
        in
        let rec aux dependents data_acc cells =
          match CellAddressSet.choose_opt dependents with
          | None -> (Ok cells, set_cell_address data_acc addr)
          | Some dep_addr ->
            let remaining = CellAddressSet.remove dep_addr dependents in
            let (res, data_next) = setNotComputedYet (set_cell_address data_acc dep_addr) in
            match res with
            | Error e -> (Error e, data_next)
            | Ok cells' -> aux remaining data_next (CellAddressSet.union cells cells')
        in
        aux dependents data (CellAddressSet.singleton addr)


let rec get_cell_value (addr: cell_address) : value comp =
  fun data ->
    if data.cost > data.max_cost then
      (Error VTimeLimitExceededError, data)
    else
    match CellAddressMap.find_opt addr data.sheets with
    | None -> (Ok VNone, add_cell_access data addr)
    | Some cell ->
        match cell.value with
        | Computed v -> (Ok v, add_cell_access data addr)
        | Computing -> (error_str "Cyclic dependency", add_cell_access data addr)
        | NotComputedYet ->
            let new_cell = {cell with value = Computing} in
            let new_sheets = CellAddressMap.add addr new_cell data.sheets in
            let data' = { data with sheets = new_sheets; cell_address = Some addr; env = data.config } in
            let (res, data'') = eval_def_expr cell.definition data' in
          (match res with
          | Error e ->
            let new_cell = {cell with value = ComputingError e} in
            let new_sheets = CellAddressMap.add addr new_cell data''.sheets in
            let new_data = { data with sheets = new_sheets; dependency_graph = data''.dependency_graph; cost = data.cost + 1; fresh = data''.fresh } in
            (match e with
            | VTimeLimitExceededError -> (error_str "Time limit exceeded in accessed cell", add_cell_access new_data addr)
            | _ -> (Error e, add_cell_access new_data addr))
          | Ok None ->
            let new_cell = {cell with value = Computed VNone} in
            let new_sheets = CellAddressMap.add addr new_cell data''.sheets in
            let new_data = { data with sheets = new_sheets; dependency_graph = data''.dependency_graph; cost = data.cost + 1; fresh = data''.fresh } in
            (Ok VNone, add_cell_access new_data addr)
          | Ok (Some v) ->
            let new_cell = {cell with value = Computed v} in
            let new_sheets = CellAddressMap.add addr new_cell data''.sheets in
            let new_data = { data with sheets = new_sheets; dependency_graph = data''.dependency_graph; cost = data.cost + 1; fresh = data''.fresh } in
            (Ok v, add_cell_access new_data addr))
        | ComputingError e -> (match e with
          | VTimeLimitExceededError -> (error_str "Time limit exceeded in accessed cell", add_cell_access data addr)
          | _ -> (Error e, add_cell_access data addr))


and set_cell_def (def : def_expr) : unit comp =
  let* set_to_not_computed = setNotComputedYet in
  let* addr = 
    fun data ->
      match data.cell_address with
      | None -> (error_str "No cell address set", data)
      | Some addr ->
        let cell = {value = NotComputedYet; definition = def; dependencies = CellAddressSet.empty}
        in
        let new_sheets = if def = Sequence []
          then CellAddressMap.remove addr data.sheets
          else CellAddressMap.add addr cell data.sheets in
        let data' = { data with sheets = new_sheets; cost = data.cost + 1 } in
        (Ok addr, data')
  in
  let set_to_not_computed = CellAddressSet.add addr set_to_not_computed in
  let* _ = CellAddressSet.fold
    (fun addr acc ->
      let* () = acc in
      let* _ = get_cell_value addr in
      return ())
    set_to_not_computed
    (return ())
  in
  return ()


and set_config (env: env) : unit comp =
  let* cells = (fun data ->
    let new_sheets = CellAddressMap.map (fun cell -> {cell with value = NotComputedYet; dependencies = CellAddressSet.empty}) data.sheets in
    let new_data = {env = env; config = env; sheets = new_sheets; dependency_graph = CellAddressMap.empty; cell_address = data.cell_address; cost = data.cost + 1; max_cost = data.max_cost; fresh = data.fresh} in
    (Ok (CellAddressMap.fold (fun addr _ acc -> CellAddressSet.add addr acc) data.sheets CellAddressSet.empty), new_data)) in
  let* _ = CellAddressSet.fold 
    (fun addr acc ->
      let* () = acc in
      finally (get_cell_value addr) (return ()))
    cells
    (return ())
  in
  return ()


and delete_spreadsheet sheet_name : unit comp =
  let* cells = (fun data ->
    let cells_to_delete = CellAddressMap.fold
      (fun addr _ acc ->
        match addr with
        | s, _ when s = sheet_name -> CellAddressSet.add addr acc
        | _ -> acc)
      data.sheets
      CellAddressSet.empty
    in
    (Ok cells_to_delete, data)) in
  let* set_to_not_computed = CellAddressSet.fold
    (fun addr acc ->
      let* curr = acc in
      let* () = (fun data -> (Ok (), set_cell_address data addr)) in
      let* new_cells = setNotComputedYet in
      return (CellAddressSet.union curr new_cells))
    cells
    (return CellAddressSet.empty) in
  let* _ = CellAddressSet.fold
    (fun addr acc ->
      if fst addr = sheet_name then
        return ()
      else
      let* () = acc in
      let* _ = get_cell_value addr in
      return ())
    set_to_not_computed
    (return ()) in
  return ()


and rename_spreadsheet old_name new_name : unit comp =
  let* cells = (fun data ->
    let cells_to_rename = CellAddressMap.fold
      (fun addr _ acc ->
        match addr with
        | s, _ when s = old_name -> CellAddressSet.add addr acc
        | _ -> acc)
      data.sheets
      CellAddressSet.empty
    in
    (Ok cells_to_rename, data)) in
  let* set_to_not_computed = CellAddressSet.fold
    (fun addr acc ->
      let* curr = acc in
      let* () = (fun data -> (Ok (), set_cell_address data addr)) in
      let* new_cells = setNotComputedYet in
      return (CellAddressSet.union curr new_cells))
    cells
    (return CellAddressSet.empty) in
  let* set_to_not_computed = CellAddressSet.fold
    (fun addr acc ->
      let* curr = acc in
      let new_addr = match addr with
        | s, r when s = old_name -> (new_name, r)
        | _ -> addr
      in
      let* () = (fun data ->
        let def = match CellAddressMap.find_opt addr data.sheets with
          | None -> Sequence []
          | Some cell -> cell.definition
        in
        let cell = {value = NotComputedYet; definition = def; dependencies = CellAddressSet.empty} in
        let new_sheets = if def = Sequence [] then CellAddressMap.remove new_addr data.sheets else CellAddressMap.add new_addr cell data.sheets in
        let new_data = { data with sheets = new_sheets} in
        (Ok (), new_data)) in
      return (CellAddressSet.add new_addr curr))
    cells
    (return set_to_not_computed) in
  let* _ = CellAddressSet.fold
    (fun addr acc ->
      let new_addr = match addr with
        | s, r when s = old_name -> (new_name, r)
        | _ -> addr
      in
      let* () = acc in
      let* _ = get_cell_value new_addr in
      return ())
    set_to_not_computed
    (return ()) in
  return ()


and to_bool (v: value) : value comp =
  match v with
  | VBool _ -> return v
  | v -> let* func = lookup_var "bool" in
          let* result = eval_call func [v] in
          match result with
          | VBool _ -> return result
          | _ -> failstr "Conversion to bool did not return a boolean value"


and to_list (v: value) : value comp =
  match v with
  | VList _ -> return v
  | v -> let* func = lookup_var "list" in
          let* result = eval_call func [v] in
          match result with
          | VList _ -> return result
          | _ -> failstr "Conversion to list did not return a list value"


and eval_call (func_value : value) (arg_values : value list) : value comp =
  let rec eval_call2 (curr_func) (arg_values) : value comp =
    match curr_func with
    | VDef (name, args, def_expr, def_env, type_env, _func_id) ->
      if List.length arg_values <> List.length args then
        fail VInvalidArgumentsError
      else
      let* v_env = get_env in
      let* t_env = get_type_env in
      finally_do (
        let* () = set_type_env type_env in
        let* () = set_env def_env in
        let* _ = add_to_env name func_value in
        let* () =
          List.fold_left2
            (fun acc arg_name arg_value ->
              let* () = acc in
              add_to_env arg_name arg_value)
            (return ())
            args
            arg_values
        in
        let* v = eval_def_expr def_expr in
        (match v with
        | None -> return VNone
        | Some ret_value -> return ret_value)
      )
      (
        let* () = set_env v_env in
        set_type_env t_env
      )
    | VBuiltin (f, _func_id) ->
      f arg_values
    | VExtend funcs ->
      let rec aux funcs =
        match funcs with
        | [] -> fail VInvalidArgumentsError
        | f::fs ->
          bind_catch (eval_call2 f arg_values) (fun res -> return res) (
            function
            | VInvalidArgumentsError -> aux fs
            | error -> fail error)
      in
      aux funcs
    | _ ->
      let* call_func = lookup_var "__call__" in
      eval_call2 call_func (curr_func::arg_values)
  in
  eval_call2 func_value arg_values


and eval_def_expr (def: def_expr) : (value option) comp =
  match def with
  | Sequence defs ->
    let rec aux defs =
      match defs with
      | [] -> return None
      | d::ds ->
        let* v = eval_def_expr d in
        (match v with
        | Some _ -> return v
        | None -> aux ds)
    in
    aux defs
  | Assign (var_item, expr) ->
    (let* value = eval_value_expr expr in
    let* getitem = lookup_var "__getitem__" in
    let* setitem = lookup_var "__setitem__" in
    match var_item with
    | VarIdent var_name ->
      let* () = add_to_env var_name value in
      return None
    | VarItem (vi, index_expr) ->
      let* index_to_set = eval_value_expr index_expr in
      let rec list_values vi =
        match vi with
        | VarIdent var_name ->
          let* v = lookup_var var_name in
          return ([(v, VNone)], var_name)
        | VarItem (inner_vi, index_expr) ->
          let* v_curr, var_name = list_values inner_vi in
          let* index_value = eval_value_expr index_expr in
          let* v = eval_call getitem [fst (List.hd v_curr); index_value] in
          return ((v, index_value)::v_curr, var_name)
      in
      let* values, var_name = list_values vi in
      let* values = match values with
      | [] -> failstr "Unexpected error in assign operation"
      | (v, idx)::rest ->
        let* del_v = eval_call setitem [v; index_to_set; value] in
        return ((del_v, idx)::rest)
      in
      let rec set_values values =
        match values with
        | [] -> return ()
        | [(v, _)] -> 
          let* () = add_to_env var_name v in
          return ()
        | (v, idx)::(w, idx2)::rest ->
          let* new_w = eval_call setitem [w; idx; v] in
          set_values ((new_w, idx2)::rest)
      in
      let* () = set_values values in
      return None)
  | If (cond_expr, then_def, else_def) ->
    let* cond_value = eval_value_expr cond_expr in
    let* cond_value = to_bool cond_value in
    begin
    match cond_value with
      | VBool true -> eval_def_expr then_def
      | VBool false -> eval_def_expr else_def
      | _ -> failstr "Condition expression does not evaluate to a boolean"
    end
  | While (cond_expr, body_def) ->
    let rec loop () =
      let* cond_value = eval_value_expr cond_expr in
      let* cond_value = to_bool cond_value in
      match cond_value with
      | VBool true ->
        let* v = eval_def_expr body_def in
        (match v with
        | None -> loop ()
        | _ -> return v)
      | VBool false -> return None
      | _ -> failstr "Condition expression does not evaluate to a boolean"
    in
    loop ()
  | For (var_name, iterable_expr, body_def) ->
    let* iterable_value = eval_value_expr iterable_expr in
    let* iterable_value = to_list iterable_value in
    begin
    match iterable_value with
      | VList lst ->
        let rec loop items =
          if SekP.is_empty items then return None
          else
          let x, xs = SekP.pop Sek.front items in
            let* () = add_to_env var_name x in
            bind_catch (eval_def_expr body_def) 
              (function
                | Some v -> return (Some v)
                | None -> loop xs)
              (function
              | VBreak -> return None
              | VContinue -> loop xs
              | err -> fail err)
        in
        let* res = temp_var var_name (loop lst) in
        return res
      | _ -> failstr "Iterable expression does not evaluate to a list"
    end
  | Def (name, args, body) ->
    let* var_env = get_env in
    let* type_env = get_type_env in
    let* func_id = fresh_id in
    let func_value = VDef (name, args, body, var_env, type_env, func_id) in
    let* () = add_to_env name func_value in
    return None
  | Extend (name, args, body) ->
    let* var_env = get_env in
    let* type_env = get_type_env in
    let* func_id = fresh_id in
    let func_value = VDef (name, args, body, var_env, type_env, func_id) in
    let* curr_func_opt = lookup_var_opt name in
    let value = match curr_func_opt with
    | None -> func_value
    | Some curr_func ->
      match curr_func with
      | VExtend v -> VExtend (func_value :: v)
      | v -> VExtend ([func_value; v])
    in
    let* () = add_to_env name value in
    return None
  | Extended (name, args, body) ->
    let* var_env = get_env in
    let* type_env = get_type_env in
    let* func_id = fresh_id in
    let func_value = VDef (name, args, body, var_env, type_env, func_id) in
    let* curr_func_opt = lookup_var_opt name in
    let value = match curr_func_opt with
    | None -> func_value
    | Some curr_func ->
      match curr_func with
      | VExtend v -> VExtend (v@[func_value])
      | v -> VExtend ([v; func_value])
    in
    let* () = add_to_env name value in
    return None
  | TryCatch (try_def, name, catch_def) ->
    let handler err =
      match err with
      | VBreak -> fail VBreak
      | VContinue -> fail VContinue
      | VTimeLimitExceededError -> fail VTimeLimitExceededError
      | _ ->
        temp_var name (
          let* () = add_to_env name err in
          eval_def_expr catch_def
        )
    in
    catch (eval_def_expr try_def) handler
  | Return value_expr ->
    let* v = eval_value_expr value_expr in
    return (Some v)
  | NewType (type_name, constructor_name, destructor_name) ->
    let* type_id = fresh_id in
    let* constructor_id = fresh_id in
    let* destructor_id = fresh_id in
    let constructor = VBuiltin ((fun args ->
      match args with
      | [v] -> return (VCustom (type_id, v))
      | _ -> fail VInvalidArgumentsError
    ), constructor_id) in
    let destructor = VBuiltin ((fun args ->
      match args with
      | [VCustom (id, v)] when id = type_id -> return v
      | [ _ ] -> fail VInvalidArgumentsError
      | _ -> fail VInvalidArgumentsError
    ), destructor_id) in
    let* () = add_to_env constructor_name constructor in
    let* () = add_to_env destructor_name destructor in
    let* () = add_type_to_env type_name (TCustom type_id) in
    return None
  | NewTypeSingleton (type_name, constructor_name) ->
    let* type_id = fresh_id in
    let constructor = VCustom (type_id, VNone) in
    let* () = add_to_env constructor_name constructor in
    let* () = add_type_to_env type_name (TCustom type_id) in
    return None
  | Del var_item ->
    (let* delitem = lookup_var "__delitem__" in
    let* getitem = lookup_var "__getitem__" in
    let* setitem = lookup_var "__setitem__" in
    match var_item with
    | VarIdent var_name ->
      let* () = delete_from_env var_name in
      return None
    | VarItem (vi, index_expr) ->
      let* index_to_delete = eval_value_expr index_expr in
      let rec list_values vi =
        match vi with
        | VarIdent var_name ->
          let* v = lookup_var var_name in
          return ([(v, VNone)], var_name)
        | VarItem (inner_vi, index_expr) ->
          let* v_curr, var_name = list_values inner_vi in
          let* index_value = eval_value_expr index_expr in
          let* v = eval_call getitem [fst (List.hd v_curr); index_value] in
          return ((v, index_value)::v_curr, var_name)
      in
      let* values, var_name = list_values vi in
      let* values = match values with
      | [] -> failstr "Unexpected error in delete operation"
      | (v, idx)::rest ->
        let* del_v = eval_call delitem [v; index_to_delete] in
        return ((del_v, idx)::rest)
      in
      let rec set_values values =
        match values with
        | [] -> return ()
        | [(v, _)] -> 
          let* () = add_to_env var_name v in
          return ()
        | (v, idx)::(w, idx2)::rest ->
          let* new_w = eval_call setitem [w; idx; v] in
          set_values ((new_w, idx2)::rest)
      in
      let* () = set_values values in
      return None)
  | Raise value_expr ->
    let* v = eval_value_expr value_expr in
    fail v
  | Break ->
    fail VBreak
  | Continue ->
    fail VContinue


and eval_value_expr (expr: value_expr) : value comp =
  match expr with
  | Int n -> return (VInt n)
  | Float f -> return (VFloat f)
  | Char c -> return (VChar c)
  | Bool b -> return (VBool b)
  | NNone -> return VNone
  | List exprs ->
    let rec aux exprs acc =
      match exprs with
      | [] -> return (VList (List.rev acc |> SekP.of_list VNone))
      | e::es ->
        let* v = eval_value_expr e in
        let* vs = aux es (v::acc) in
        return vs
    in
    aux exprs []
  | Matrix rows ->
    if rows = [] then
      return (VMatrix (SekP.of_list VNone [], 0))
    else
    let rec aux_rows rows acc =
      match rows with
      | [] -> return (VMatrix (fst acc, snd acc))
      | r::rs ->
        let* r_value = eval_value_expr r in
        match r_value with
        | VList row_values ->
          let row_len = SekP.length row_values in
          if (snd acc <> -1) && row_len <> (snd acc) then
            failstr "Matrix rows have inconsistent lengths"
          else
          let* matrix_values = aux_rows rs (SekP.concat (fst acc) row_values, row_len) in
          return matrix_values
        | _ -> failstr "Matrix row does not evaluate to a list"
    in
    aux_rows rows (SekP.of_list VNone [], -1)
  | Dict pairs ->
    let rec aux pairs acc =
      match pairs with
      | [] -> return (VDict (List.fold_left (fun m (k, v) -> ValueMap.add k v m) ValueMap.empty (List.rev acc)))
      | (k_expr, v_expr)::ps ->
        let* k = eval_value_expr k_expr in
        let* v = eval_value_expr v_expr in
        let* vs = aux ps ((k, v)::acc) in
        return vs
    in
    aux pairs []
  | SSet exprs ->
    let rec aux exprs acc =
      match exprs with
      | [] -> return (VSet (List.fold_left (fun s v -> ValueSet.add v s) ValueSet.empty (List.rev acc)))
      | e::es ->
        let* v = eval_value_expr e in
        let* vs = aux es (v::acc) in
        return vs
    in
    aux exprs []
  | If (cond_expr, then_expr, else_expr) ->
    let* cond_value = eval_value_expr cond_expr in
    let* cond_value = to_bool cond_value in
    begin
    match cond_value with
      | VBool true -> eval_value_expr then_expr
      | VBool false -> eval_value_expr else_expr
      | _ -> failstr "Condition expression does not evaluate to a boolean"
    end
  | Let (var_name, value_expr, in_expr) ->
    let* value = eval_value_expr value_expr in
    temp_var var_name (
      let* () = add_to_env var_name value in
      let* result = eval_value_expr in_expr in
      return result)
  | Var var_name -> lookup_var var_name
  | Call (func, arg_exprs) ->
    let* func_value = eval_value_expr func in
    let* eval_exprs = 
      let rec aux exprs acc =
        match exprs with
        | [] -> return (List.rev acc)
        | e::es ->
          let* v = eval_value_expr e in
          let* vs = aux es (v::acc) in
          return vs
      in
      aux arg_exprs []
    in
    eval_call func_value eval_exprs
  | Or (expr1, expr2) ->
    let* v1 = eval_value_expr expr1 in
    begin
    match v1 with
      | VBool true -> return (VBool true)
      | VBool false -> eval_value_expr expr2
      | _ -> failstr "First operand of 'or' does not evaluate to a boolean"
    end
  | And (expr1, expr2) ->
    let* v1 = eval_value_expr expr1 in
    begin
    match v1 with
      | VBool false -> return (VBool false)
      | VBool true -> eval_value_expr expr2
      | _ -> failstr "First operand of 'and' does not evaluate to a boolean"
    end
  | Not expr ->
    let* v = eval_value_expr expr in
    let* v = to_bool v in
    begin
    match v with
      | VBool b -> return (VBool (not b))
      | _ -> failstr "Operand of 'not' does not evaluate to a boolean"
    end
  | ListComprehension (value_expr, var_name, iterable_expr, if_expr) ->
    let* iterable_value = eval_value_expr iterable_expr in
    let* iterable_value = to_list iterable_value in
    begin
    match iterable_value with
      | VList lst ->
        let rec aux items acc =
          match items with
          | [] -> return (VList (List.rev acc |> SekP.of_list VNone))
          | x::xs ->
            let* () = add_to_env var_name x in
            let* cond_value = eval_value_expr if_expr in
            let* cond_value = to_bool cond_value in
            match cond_value with
            | VBool true ->
              let* value = eval_value_expr value_expr in
              aux xs (value :: acc)
            | VBool false ->
              aux xs acc
            | _ -> failstr "If expr does not evaluate to a boolean"
        in
        let* result = temp_var var_name (aux ( SekP.to_list lst) []) in
        return result
      | _ -> failstr "Iterable expression does not evaluate to a list"
    end
  | MatrixComprehension (value_expr, var_name, iterable_expr, if_expr) ->
    let* iterable_value = eval_value_expr iterable_expr in
    let* iterable_value = to_list iterable_value in
    begin
    match iterable_value with
      | VList lst ->
        let rec aux items acc =
          match SekP.pop_opt Sek.front items with
          | None, _ -> return (VMatrix (fst acc, snd acc))
          | Some x, xs ->
            let* () = add_to_env var_name x in
            let* cond_value = eval_value_expr if_expr in
            let* cond_value = to_bool cond_value in
            match cond_value with
            | VBool true ->
              let* value = eval_value_expr value_expr in
              (match value with
              | VList row ->
                let row_len = SekP.length row in
                if snd acc <> -1 && row_len <> snd acc then
                  failstr "Matrix comprehension rows have inconsistent lengths"
                else
                aux xs (SekP.concat (fst acc) row, row_len)
              | _ -> failstr "Value expression does not evaluate to a list")
            | VBool false ->
              aux xs acc
            | _ -> failstr "If expr does not evaluate to a boolean"
        in
        let* result = temp_var var_name (aux lst (SekP.of_list VNone [], -1)) in
        return result
      | _ -> failstr "Iterable expression does not evaluate to a list"
    end
  | DictComprehension (key_expr, value_expr, var_name, iterable_expr, if_expr) ->
    let* iterable_value = eval_value_expr iterable_expr in
    let* iterable_value = to_list iterable_value in
    begin
    match iterable_value with
      | VList lst ->
        let rec aux items acc =
          match items with
          | [] -> return (VDict (List.fold_left (fun m (k, v) -> ValueMap.add k v m) ValueMap.empty (List.rev acc)))
          | x::xs ->
            let* () = add_to_env var_name x in
            let* cond_value = eval_value_expr if_expr in
            let* cond_value = to_bool cond_value in
            match cond_value with
            | VBool true ->
              let* k = eval_value_expr key_expr in
              let* v = eval_value_expr value_expr in
              aux xs ((k, v) :: acc)
            | VBool false ->
              aux xs acc
            | _ -> failstr "If expr does not evaluate to a boolean"
        in
        let* result = temp_var var_name (aux (SekP.to_list lst) []) in
        return result
      | _ -> failstr "Iterable expression does not evaluate to a list"
    end
  | SSetComprehension (value_expr, var_name, iterable_expr, if_expr) ->
    let* iterable_value = eval_value_expr iterable_expr in
    let* iterable_value = to_list iterable_value in
    begin
    match iterable_value with
      | VList lst ->
        let rec aux items acc =
          match items with
          | [] -> return (VSet (List.fold_left (fun s v -> ValueSet.add v s) ValueSet.empty (List.rev acc)))
          | x::xs ->
            let* () = add_to_env var_name x in
            let* cond_value = eval_value_expr if_expr in
            let* cond_value = to_bool cond_value in
            match cond_value with
            | VBool true ->
              let* value = eval_value_expr value_expr in
              aux xs (value :: acc)
            | VBool false ->
              aux xs acc
            | _ -> failstr "If expr does not evaluate to a boolean"
        in
        let* result = temp_var var_name (aux ( SekP.to_list lst) []) in
        return result
      | _ -> failstr "Iterable expression does not evaluate to a list"
    end
  | IsOfType (value_expr, type_name) ->
    let* v = eval_value_expr value_expr in
    let* t = lookup_type type_name in
    let v_type = type_of_value v in
    if v_type = t then
      return (VBool true)
    else
      return (VBool false)
  | CellAddress addr ->
    if fst addr = "" then
    let* curr_addr = get_cell_address in
    match curr_addr with
    | None -> failstr "Unknown sheet name"
    | Some a -> let addr' = (fst a, snd addr) in get_cell_value addr'
    else
    get_cell_value addr
  | CellAddressList (addr1, addr2) ->
    let (sheet_name, (col1, row1)) = addr1 in
    let (sheet_name2, (col2, row2)) = addr2 in
    if sheet_name <> sheet_name2 && sheet_name2 <> "" then
      failstr "CellAddressList: different sheet names"
    else
    let* sheet_name = (if sheet_name = "" then
      let* curr_addr = get_cell_address in
      match curr_addr with
      | None -> failstr "Unknown sheet name"
      | Some a -> return (fst a)
    else
      return sheet_name) in
    if row1 <> row2 && col1 <> col2 then
      failstr "CellAddressList: invalid range (must be in same row or same column)"
    else
    if row1 > row2 || col1 > col2 then
      failstr "CellAddressList: invalid range (start must be before end)"
    else
    let rec aux r c acc =
      if r = row2 && c = col2 then
        let* v = get_cell_value (sheet_name, (c, r)) in
        return (VList (List.rev (v :: acc) |> SekP.of_list VNone))
      else if c < col2 then
        let* v = get_cell_value (sheet_name, (c, r)) in
        aux row1 (c + 1) (v :: acc)
      else
        let* v = get_cell_value (sheet_name, (c, r)) in
        aux (r + 1) col1 (v :: acc)
    in
    aux row1 col1 []
  | CellAddressMatrix (addr1, addr2) ->
    let (sheet_name, (col1, row1)) = addr1 in
    let (sheet_name2, (col2, row2)) = addr2 in
    if sheet_name <> sheet_name2 && sheet_name2 <> "" then
      failstr "CellAddressMatrix: different sheet names"
    else
    let* sheet_name = (if sheet_name = "" then
      let* curr_addr = get_cell_address in
      match curr_addr with
      | None -> failstr "Unknown sheet name"
      | Some a -> return (fst a)
    else
      return sheet_name) in
    if row1 > row2 || col1 > col2 then
      failstr "CellAddressMatrix: invalid range (start must be before end)"
    else
    let rec aux_rows r acc_rows =
      if r > row2 then
        return (VMatrix (List.rev acc_rows |> List.concat |> SekP.of_list VNone, col2 - col1 + 1))
      else
        let rec aux_cols c acc_cols =
          if c > col2 then
            return (List.rev acc_cols)
          else
            let* v = get_cell_value (sheet_name, (c, r)) in
            aux_cols (c + 1) (v :: acc_cols)
        in
        let* row_values = aux_cols col1 [] in
        let* matrix_values = aux_rows (r + 1) (row_values :: acc_rows) in
        return matrix_values
    in
    aux_rows row1 []
  | Lambda (args, body) ->
    let* v_env = get_env in
    let* t_env = get_type_env in
    let* func_id = fresh_id in
    let func_value = VDef ("_", args, Return body, v_env, t_env, func_id) in
    return func_value

