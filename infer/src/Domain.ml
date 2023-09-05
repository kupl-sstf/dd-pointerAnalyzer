open! IStd
open! Vocab
module F = Format

module InvocationSite = struct
  type t = Sil.instr
  [@@deriving compare]
  let from_instr i = i
  let pp fmt i = Location.pp fmt (Sil.location_of_instr i)
  let to_string i = 
    let loc = Sil.location_of_instr i in
      SourceFile.to_string loc.file ^ Location.to_string loc
end

let maxK = Config.pointer_call_context_depth
let maxH = Config.pointer_heap_context_depth
let maxF = Config.pointer_max_access_path
 
module Context = struct
  type t = InvocationSite.t list
  [@@deriving compare]
  let empty = []
  let append invo ctx = invo::ctx |> take maxK
  let to_string ctx = Vocab.string_of_list ctx InvocationSite.to_string
end

module Loc = struct
  type t = 
  | Id of Context.t * Ident.t * Procname.t
  | Var of Context.t * Pvar.t
  | AllocSite of Context.t * InvocationSite.t
  | Field of t * Fieldname.t list
  | Null
  [@@deriving compare]

  let from_id ctx id pn = Id (ctx, id, pn)
  let from_pvar ctx pv = if Pvar.is_global pv then Var (Context.empty, pv) else Var (ctx, pv) 
  let from_invosite ctx l = AllocSite (take maxH ctx, l)

  let add_field f l = 
    match l with
    | Field (base, flds) -> Field (base, f::flds |> take maxF)
    | _ -> Field (l, [f])

  let rec remove_ctxs l = 
    match l with
    | Id (_,id,pn) -> Id (Context.empty,id,pn)
    | Var (_,pv) -> Var (Context.empty,pv)
    | AllocSite (_,is) -> AllocSite (Context.empty,is)
    | Field (l', fields) -> Field (remove_ctxs l', fields)
    | _ -> l

  let compare x y =
   match (x, y) with
   | Var (_,v1), Var (_,v2) when Pvar.is_global v1 && Pvar.is_global v2 ->
       if String.equal (Pvar.to_string v1) (Pvar.to_string v2) then 0 else compare x y
   | _ -> compare x y

  let rec pp fmt loc = 
    match loc with
    | Id (_, id, pn) -> F.fprintf fmt "%a %a" Ident.pp id Procname.pp pn
    | Var (_, v) -> F.fprintf fmt "%a" Mangled.pp (Pvar.get_name v)
    | AllocSite (_, a) -> InvocationSite.pp fmt a
    | Field (l, _) -> pp fmt l
    | Null -> F.fprintf fmt "null"

  let rec to_string loc = 
    match loc with
    | Id (ctx, id, pn) -> "[" ^ Context.to_string ctx ^ Procname.to_string pn ^ "," ^ Ident.to_string id ^ "]"
    | Var (ctx, pvar) -> 
      let decl_func_name = 
        match Pvar.get_declaring_function pvar with
        | Some pn -> Procname.to_string pn 
        | None -> "global" in
        "(" ^ Context.to_string ctx ^ decl_func_name ^ "," ^ Pvar.to_string pvar ^ ")"
    | AllocSite (ctx, a) -> Context.to_string ctx ^ "@" ^ InvocationSite.to_string a
    | Field (l, fs) -> to_string l ^ string_of_list (reverse fs) Fieldname.to_string
    | Null -> "Null"
end


module LocSet = struct
  include AbstractDomain.FiniteSet (Loc)
  let to_string locs = Vocab.string_of_set fold locs Loc.to_string
  let remove_ctxs ls = map Loc.remove_ctxs ls
end

module ProcSet = struct
  include AbstractDomain.FiniteSet (Procname)
  let to_string procs = Vocab.string_of_set fold procs Procname.to_string
end

module Val = struct
  type t = LocSet.t * ProcSet.t
  let bottom = (LocSet.empty, ProcSet.empty)
  let locs_of (locs,_) = locs
  let procs_of (_,procs) = procs
  let from_locs locs = (locs, ProcSet.empty)
  let from_loc loc = (LocSet.singleton loc, ProcSet.empty)
  let from_procs procs = (LocSet.empty, procs)
  let join (l1,p1) (l2,p2) = (LocSet.join l1 l2, ProcSet.join p1 p2)
  let order (l1,p1) (l2,p2) = LocSet.subset l1 l2 && ProcSet.subset p1 p2
  let pp fmt (locs, procs) = F.fprintf fmt "(%a, %a)\n" LocSet.pp locs ProcSet.pp procs
  let to_string (locs, procs) = LocSet.to_string locs ^ " " ^ ProcSet.to_string procs
  let remove_ctxs (ls,ps) = (LocSet.remove_ctxs ls, ps)
end

module CtxSet = Caml.Set.Make(Context)
module LocMap = Caml.Map.Make(Loc)
module VarSet = Caml.Set.Make(Pvar)
module PnameMap = Caml.Map.Make(Procname)
module LocationMap = Caml.Map.Make(InvocationSite)

module Memory = struct
  type t = Val.t LocMap.t
  let bottom = LocMap.empty
  let find l m = try LocMap.find l m with _ -> Val.bottom
  let find_set ls m = LocSet.fold (fun l -> Val.join (find l m)) ls Val.bottom
  let strong_update l v m = LocMap.add l v m
  let weak_update l v m = LocMap.add l (Val.join v (find l m)) m
  let weak_update_set ls v m = LocSet.fold (fun l -> weak_update l v) ls m
  let order m1 m2 = LocMap.for_all (fun l v1 -> Val.order v1 (find l m2)) m1
  let sizeof m = LocMap.fold (fun _ _ n -> n+1) m 0
  let points_to_set mem = LocMap.fold (fun _ v acc -> LocSet.cardinal (Val.locs_of v) + acc) mem 0

  let remove_ctxs m = 
    LocMap.fold (fun l v ->
      let l_ci = Loc.remove_ctxs l in 
      let v_ci = Val.remove_ctxs v in
        weak_update l_ci v_ci
    ) m bottom

  let to_string m = 
    LocMap.fold (fun l v s -> 
      Loc.to_string l ^ " |-> " ^ Val.to_string v ^ "\n" ^ s
    ) m ""
end

module Reachable = struct
  type t = CtxSet.t PnameMap.t
  let empty = PnameMap.empty
  let find p r = try PnameMap.find p r with _ -> CtxSet.empty
  let add p ctx r = PnameMap.add p (CtxSet.add ctx (find p r)) r
  let order r1 r2 = PnameMap.for_all (fun p ctx1 -> CtxSet.subset ctx1 (find p r2)) r1
  let num_of_procs r = PnameMap.fold (fun _ _ n -> n+1) r 0
  let num_of_ctx r = PnameMap.fold (fun _ ctx n -> n+(CtxSet.cardinal ctx)) r 0
end

module Callgraph = struct
  type t = ProcSet.t LocationMap.t
  let empty = LocationMap.empty
  let find l g = try LocationMap.find l g with _ -> ProcSet.empty
  let add l p g = LocationMap.add l (ProcSet.add p (find l g)) g
  let order g1 g2 = LocationMap.for_all (fun label set -> ProcSet.subset set (find label g2)) g1
  let count_calledges cg = LocationMap.fold (fun _ callees acc -> ProcSet.cardinal callees + acc) cg 0
  let count_callsites cg = LocationMap.fold (fun _ _ acc -> acc + 1) cg 0
  let to_string g = 
    LocationMap.fold (fun l v s -> 
      InvocationSite.to_string l ^ " |-> " ^ ProcSet.to_string v ^ "\n" ^ s
    ) g ""
end

module State = struct
  type t = 
    { mem: Memory.t
    ; reachable: Reachable.t
    ; callgraph: Callgraph.t }

  let bottom = 
    { mem = Memory.bottom
    ; reachable = Reachable.empty
    ; callgraph = Callgraph.empty }

  let get_memory s = s.mem
  let get_reachable s = s.reachable
  let get_callgraph s = s.callgraph

  let find l s = Memory.find l s.mem
  let find_set ls s = Memory.find_set ls s.mem
  let strong_update l v s = { s with mem = Memory.strong_update l v s.mem }
  let weak_update l v s = { s with mem = Memory.weak_update l v s.mem }
  let weak_update_set ls v s = { s with mem = Memory.weak_update_set ls v s.mem }

  let find_reachable p s = Reachable.find p s.reachable
  let add_reachable p ctx s = { s with reachable = Reachable.add p ctx s.reachable }

  let find_cg l s = Callgraph.find l s.callgraph
  let add_cg l p s = { s with callgraph = Callgraph.add l p s.callgraph }

  let add_callee invosite callee ctx s = s |> add_reachable callee ctx |> add_cg invosite callee

  let order s1 s2 = 
    Memory.order s1.mem s2.mem &&
    Reachable.order s1.reachable s2.reachable &&
    Callgraph.order s1.callgraph s2.callgraph

  let get_pvars_of_proc pname state = 
    LocMap.fold (fun loc _ acc -> 
      match loc with
      | Var (_, pv) ->
        begin
          match Pvar.get_declaring_function pv with
          | Some pn -> if Procname.equal pn pname then VarSet.add pv acc else acc
          | _ -> acc
        end
      | _ -> acc
    ) state.mem VarSet.empty

  let string_of_stat s = 
    let ci_mem = Memory.remove_ctxs s.mem in
    F.asprintf "---- State Statistics ----\n" ^ 
    F.asprintf "Reachable  : procs %d, contexts %d\n" (Reachable.num_of_procs s.reachable) 
                                                      (Reachable.num_of_ctx s.reachable) ^
    F.asprintf "Points-to  : locs %d, set %d, average %.2f\n" 
        (Memory.sizeof s.mem) 
        (Memory.points_to_set s.mem) 
        (float_of_int (Memory.points_to_set s.mem) /. float_of_int (Memory.sizeof s.mem)) ^
    F.asprintf "Points-to(ci): locs %d, set %d, average %.2f\n" 
        (Memory.sizeof ci_mem) 
        (Memory.points_to_set ci_mem) 
        (float_of_int (Memory.points_to_set ci_mem) /. float_of_int (Memory.sizeof ci_mem)) ^
    F.asprintf "CallGraph  : edges %d, callsites %d, average %.2f\n" 
        (Callgraph.count_calledges s.callgraph) 
        (Callgraph.count_callsites s.callgraph) 
        (float_of_int (Callgraph.count_calledges s.callgraph) /. float_of_int (Callgraph.count_callsites s.callgraph))
end
