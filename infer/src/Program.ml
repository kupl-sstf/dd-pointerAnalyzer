open! IStd
open! Sil
module F = Format
module L = Logging

module PInstr = struct
  type t = SourceFile.t * Procdesc.t * Sil.instr
  let create file pd instr = (file, pd, instr)
  let get_procdesc (_, pd, _) = pd
  let get_instr (_, _, instr) = instr
  let to_string (file, pd, instr) =
    let pname = Procname.to_string (Procdesc.get_proc_name pd) in
    let fname = SourceFile.to_string file in
    let itype = 
      match instr with 
      | Load _ -> "Load" 
      | Store _ -> "Store" 
      | Prune _ -> "Prune" 
      | Call _ -> "Call" 
      | Metadata _ -> "Meta" in
      F.asprintf "%s %s %s %a" fname pname itype (Sil.pp_instr ~print_types:true Pp.text) instr
  let compare = Stdlib.compare
end

module PnameMap = Caml.Map.Make(Procname)
module PInstrSet = Caml.Set.Make(PInstr)

let pdescs = ref PnameMap.empty

let add_pdesc pname pdesc = pdescs := PnameMap.add pname pdesc !pdescs

let get_pdesc pn = 
  try Some (PnameMap.find pn !pdescs)
  with _ -> 
    match Procdesc.load pn with
    | Some pdesc -> add_pdesc pn pdesc; Some pdesc
    | None -> None
