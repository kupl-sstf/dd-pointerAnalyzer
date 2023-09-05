open! IStd
open! Domain
open! Program

let pvar_equal x y = String.equal (Pvar.to_string x) (Pvar.to_string y)

module AliasSet = struct
  type t = (Pvar.t * Pvar.t) list
  let empty = []
  let eq_alias (a,b) (c,d) = pvar_equal a c && pvar_equal b d
  let add x y t = 
    if pvar_equal x y || 
       List.mem ~equal:eq_alias t (x,y) || 
       List.mem ~equal:eq_alias t (y,x) 
    then t 
    else (x,y)::t
  let sizeof = List.length
  let to_string t = Vocab.string_of_list t (fun (x,y) -> "(" ^ Pvar.to_string x ^ "," ^ Pvar.to_string y ^ ")" )
end

module Aliases = struct
  type t = AliasSet.t PnameMap.t

  let check_alias v1 v2 ctxset state = 
    CtxSet.exists (fun ctx ->
      not (LocSet.is_empty 
        (LocSet.inter
          (Val.locs_of (State.find (Loc.from_pvar ctx v1) state))
          (Val.locs_of (State.find (Loc.from_pvar ctx v2) state))))
    ) ctxset 
  
  let compute_proc : Procname.t -> CtxSet.t -> State.t -> AliasSet.t
  =fun proc ctxset state -> 
    let pvars = State.get_pvars_of_proc proc state in
      VarSet.fold (fun v1 acc ->
        VarSet.fold (fun v2 acc ->
          if not (pvar_equal v1 v2) && check_alias v1 v2 ctxset state 
          then AliasSet.add v1 v2 acc
          else acc
        ) pvars acc
      ) pvars AliasSet.empty
  
  let compute : State.t -> t
  =fun state -> 
    PnameMap.fold (fun proc ctxset ->
      PnameMap.add proc (compute_proc proc ctxset state)
    ) state.reachable PnameMap.empty

  let num_of_aliases t = PnameMap.fold (fun _ aset n -> AliasSet.sizeof aset + n ) t 0
  let avg_num_of_aliases t = 
    (float_of_int (num_of_aliases t)) /. 
    (float_of_int (PnameMap.fold (fun _ _ c -> c+1) t 0))

  let to_string aliases = 
    PnameMap.fold (fun p a s -> 
      Procname.to_string p ^ " |-> " ^ AliasSet.to_string a ^ "\n" ^ s
    ) aliases ""

end

module Pointsto = struct
  type t = LocSet.t PnameMap.t

  let get_pointsto pvar ctxset state = 
    CtxSet.fold (fun ctx -> 
      let pointsto = Val.locs_of (State.find (Loc.from_pvar ctx pvar) state) in
      let pointsto_ci = LocSet.remove_ctxs pointsto in
        LocSet.union pointsto_ci
    ) ctxset LocSet.empty
  
  let compute_proc : Procname.t -> CtxSet.t -> State.t -> LocSet.t
  =fun proc ctxset state -> 
    let pvars = State.get_pvars_of_proc proc state in
      VarSet.fold (fun pvar -> LocSet.union (get_pointsto pvar ctxset state)) pvars LocSet.empty
  
  let compute : State.t -> t
  =fun state -> 
    PnameMap.fold (fun proc ctxset ->
      let locset = compute_proc proc ctxset state in
        PnameMap.add proc locset 
    ) state.reachable PnameMap.empty

  let num_of_pointsto t = PnameMap.fold (fun _ set n -> LocSet.cardinal set + n ) t 0

end

module NullDeref = struct
  type t = PInstrSet.t

  let compute_load pname ctx _ e _ state = 
    LocSet.mem Loc.Null (TransferFunction.eval_locs ctx e pname state) 
  
  let compute : State.t -> PInstr.t list -> t
  =fun state pinstrs -> 
    List.fold pinstrs ~init:PInstrSet.empty 
      ~f:(fun acc pinstr -> 
        let pname = Procdesc.get_proc_name (PInstr.get_procdesc pinstr) in
        let contexts = State.find_reachable pname state in
        let instr = Program.PInstr.get_instr pinstr in
          match instr with
          | Sil.Load {id; e; typ} -> 
            CtxSet.fold (fun ctx acc -> 
              if (compute_load pname ctx id e typ state) then PInstrSet.add pinstr acc
              else acc) contexts acc
          | _ -> acc
    )
   
  let num_of_nullderefs : t -> int
  =fun t -> PInstrSet.cardinal t
end

let string_of_stat s instrs =
  let aliases = Aliases.compute s in
  let pointsto = Pointsto.compute s in
  let nullderef = NullDeref.compute s instrs in
  F.asprintf "---- Clients ----\n" ^ 
  F.asprintf "#Aliases    : %d\n" (Aliases.num_of_aliases aliases) ^
  F.asprintf "#Pointsto   : %d\n" (Pointsto.num_of_pointsto pointsto) ^
  F.asprintf "#NullDeref  : %d\n" (NullDeref.num_of_nullderefs nullderef)

