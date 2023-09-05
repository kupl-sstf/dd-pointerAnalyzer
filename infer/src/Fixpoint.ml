open! IStd
open! Domain
open! Program

let transfer state pinstr =
  let pname = Procdesc.get_proc_name (PInstr.get_procdesc pinstr) in
  let contexts = State.find_reachable pname state in
  let instr = Program.PInstr.get_instr pinstr in
    CtxSet.fold (fun ctx state ->
      TransferFunction.transfer pname ctx instr state) contexts state

let print_log n state elapsed =       
  print_endline (F.asprintf "Fixpoint computation %4d, reach: %6d, memory: %8d, time: %6.2f" n 
                      (Reachable.num_of_procs (State.get_reachable state)) 
                      (Memory.sizeof (State.get_memory state)) elapsed)
 
let perform instrs entries = 
  let init_state = List.fold entries ~init:State.bottom 
    ~f:(fun state entry -> State.add_reachable entry Context.empty state) in
  let rec iter state n = 
    let t0 = Unix.gettimeofday () in
    let new_state = List.fold instrs ~init:state ~f:transfer in
    let fix_reached = State.order new_state state in
    let elapsed = Unix.gettimeofday () -. t0 in
      if fix_reached then state
      else (print_log n state elapsed; iter new_state (n+1)) in
  iter init_state 1
