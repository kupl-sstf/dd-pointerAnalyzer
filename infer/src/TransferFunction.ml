open! IStd
open! Program
open! Domain
module F = Format

let rec eval : Context.t -> Exp.t -> Procname.t -> State.t -> Val.t 
=fun ctx e pname s -> 
  match e with
  | Var id -> State.find (Loc.from_id ctx id pname) s
  | Lvar pv -> Val.from_loc (Loc.from_pvar ctx pv)
  | BinOp (PlusPI, e1, _) 
  | BinOp (MinusPI, e1, _) -> eval ctx e1 pname s
  | Const (Cint lit) when IntLit.iszero lit -> Val.from_loc Loc.Null
  | Const (Cfun pn) -> Val.from_procs (ProcSet.singleton pn)
  | Cast (_, e) -> eval ctx e pname s
  | Lfield (e, f, _) -> Val.from_locs (LocSet.map (Loc.add_field f) (eval_locs ctx e pname s))
  | Lindex (e1, _) -> Val.from_locs (eval_locs ctx e1 pname s) 
  | _ -> Val.bottom

and eval_locs : Context.t -> Exp.t -> Procname.t -> State.t -> LocSet.t
=fun ctx e pname s -> Val.locs_of (eval ctx e pname s)

and eval_procs : Context.t -> Exp.t -> Procname.t -> State.t -> ProcSet.t
=fun ctx e pname s -> Val.procs_of (eval ctx e pname s)

let transfer_load ctx id e _ pname s = 
  let l = Loc.from_id ctx id pname in
  let v = State.find_set (eval_locs ctx e pname s) s in
    State.weak_update l v s

let transfer_store ctx  e1 _ e2 pname s = 
  let locs = eval_locs ctx e1 pname s in
  let v = eval ctx e2 pname s in
    State.weak_update_set locs v s 

let get_callees : Context.t -> Sil.instr -> Procname.t -> State.t -> ProcSet.t
=fun ctx instr pname state ->
  match instr with
  | Call (_, f, _, _, {cf_virtual}) when not cf_virtual -> eval_procs ctx f pname state 
  | _ -> ProcSet.empty

let bind_actual ctx pname caller callee_ctx (name, _) actual state = 
  let loc_formal = Loc.from_pvar callee_ctx (Pvar.mk name pname) in
  let val_actual = eval ctx actual caller state in
    State.weak_update loc_formal val_actual state

let get_ret_value ctx callee loc invo_instr state = 
  match Procname.get_method callee with
  | "malloc" -> Val.from_loc (Loc.from_invosite ctx invo_instr)
  | "realloc" -> Val.from_loc (Loc.from_invosite ctx invo_instr)
  | _ -> State.find loc state 

let bind_ret ctx callee caller callee_ctx (ret_id, _) invo_instr state = 
  let ret_pvar = Pvar.get_ret_pvar callee in
  let loc_ret_pvar = Loc.from_pvar callee_ctx ret_pvar in
  let ret_value = get_ret_value ctx callee loc_ret_pvar invo_instr state in
    State.weak_update (Loc.from_id ctx ret_id caller) ret_value state

let transfer_call caller_ctx (callee, callee_ctx) caller (ret_id, ret_typ) actuals invo_instr state = 
  match Program.get_pdesc callee with
  | None -> state
  | Some pd ->
    let formals = Procdesc.get_formals pd in
    let state = State.add_callee invo_instr callee callee_ctx state in
    let state = 
      try 
        List.fold2_exn
          ~f:(fun acc (name, typ) (actual, _) ->
              bind_actual caller_ctx callee caller callee_ctx (name, typ) actual acc)
          ~init:state formals actuals
      with _ -> state in
      bind_ret caller_ctx callee caller callee_ctx (ret_id, ret_typ) invo_instr state

let apply_tunneling : Procname.t -> Procname.t -> bool
=fun p1 p2 -> 
  if not Config.pointer_apply_tunneling then false
  else
    let m1 = Procname.get_method p1 in
    let m2 = Procname.get_method p2 in
      if String.length m1 < String.length m2 then true else false

let transfer : Procname.t -> Context.t -> Sil.instr -> State.t -> State.t
=fun pname ctx instr state ->
  match instr with
  | Load {id; e; typ} when Typ.is_pointer typ -> transfer_load ctx id e typ pname state
  | Store {e1; typ; e2} when Typ.is_pointer typ -> transfer_store ctx e1 typ e2 pname state
  | Call (ret, _, actuals, _, _) as i -> 
    let callees = get_callees ctx i pname state in
      ProcSet.fold 
        (fun callee -> 
          let callee_ctx = if apply_tunneling pname callee then ctx else Context.append (InvocationSite.from_instr i) ctx in
            transfer_call ctx (callee, callee_ctx) pname ret actuals i) callees state
  | _ -> state
