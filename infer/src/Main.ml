open! IStd
open! Domain
open! Vocab
module F = Format
module L = Logging

let pp_typ = Typ.pp_full

let collect_proc : SourceFile.t -> Procname.t -> Program.PInstr.t list
=fun file proc_name ->
  match Procdesc.load proc_name with
  | Some pd -> 
    let nodes = Procdesc.get_nodes pd in
      List.fold nodes ~init:[]
        ~f:(fun acc node -> 
          (List.map ~f:(fun instr -> (file, pd, instr))
            (node 
             |> Procdesc.Node.get_instrs 
             |> Instrs.get_underlying_not_reversed 
             |> Array.to_list)) @ acc)
  | None -> []
 
let collect_file : SourceFile.t -> Program.PInstr.t list
=fun file ->
  let proc_names = SourceFiles.proc_names_of_source file in
    List.fold proc_names ~init:[]
      ~f:(fun acc proc_name -> collect_proc file proc_name @ acc) 

let collect_instrs : SourceFile.t list -> Program.PInstr.t list
=fun files -> List.fold ~init:[] ~f:(fun acc file -> collect_file file @ acc) files

let filter_instrs : Program.PInstr.t list -> Program.PInstr.t list
=fun instrs ->
  List.filter ~f:(fun i ->
    match Program.PInstr.get_instr i with
    | Sil.Load  {typ} 
    | Sil.Store {typ} -> Typ.is_pointer typ
    | Sil.Call _ -> true
    | _ -> false
  ) instrs

let collect_pdescs : SourceFile.t list -> (Procname.t * Procdesc.t) list
=fun files ->
  List.fold ~init:[] 
    ~f:(fun acc file -> 
      let proc_names = SourceFiles.proc_names_of_source file in
      List.fold proc_names ~init:acc
      ~f:(fun acc proc_name -> 
        match Procdesc.load proc_name with
        | Some pd -> (proc_name, pd)::acc
        | None -> acc)
    ) files

let add_pdescs : SourceFile.t list -> unit
=fun files ->
  let pdescs = collect_pdescs files in
    List.iter pdescs ~f:(fun (pname, pdesc) -> Program.add_pdesc pname pdesc) 

let get_entry_proc : SourceFile.t list -> Procname.t list
=fun files ->
  List.fold ~init:[] ~f:(fun acc file ->
    List.filter ~f:(fun proc_name -> String.equal (Procname.to_string proc_name) "main")
      (SourceFiles.proc_names_of_source file) @ acc
  ) files

let print_header () = 
  print_endline "Pointer analysis begins...";
  print_endline (F.asprintf "maxK: %d, maxH: %d, maxF: %d, tunneling: %b" 
    Domain.maxK Domain.maxH Domain.maxF Config.pointer_apply_tunneling)

let print_entry_info entries = 
  print_string (string_of_int (List.length entries) ^ " entries found: "); 
  print_endline (Vocab.string_of_list entries Procname.to_string)
 
let main () = 
  let _ = print_header () in
  let source_files = SourceFiles.get_all ~filter:(fun _ -> true) () in
  let instrs = (filter_instrs >>> collect_instrs) source_files in
  let entries = get_entry_proc source_files in
  let _ = print_entry_info entries in
  let _ = add_pdescs source_files in
  let start_t = Unix.gettimeofday () in
  let s = Fixpoint.perform instrs entries in
  let stop_t = Unix.gettimeofday () in
    Printf.printf "Execution time: %f\n%!" (stop_t -. start_t);
    print_endline "Pointer analysis finishes...";
    print_endline (State.string_of_stat s);
    print_endline (Clients.string_of_stat s instrs)
