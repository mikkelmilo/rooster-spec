open Ltac_plugin
open Extraction_plugin
open Sys
open Pp
open Util
open Names
open ModPath
open Namegen
open Nameops
open Libnames
open Globnames
open Table
open Miniml
open Mlutil
open Common
open Printf

(* open Dolmen *)

(* module P = Dolmen.Smtlib.Make(Dolmen_std.ParseLocation)(Dolmen_std.Id)(Dolmen_std.Term)(Dolmen_std.Statement) *)
(* let d = Dolmen.Statement.Exit *)

(* 
(* ------------------------------- *)
(* Parsing & pretty printing of .smt2 file *)
(* ------------------------------- *)
let rec pp_smt2_file file = 
  let stms = P.parse_file file
  in List.iter pp_stm stms

and pp_stm (stm : P.statement) : unit =
  Statement.print Format.std_formatter stm
   *)


(** Example: simple, no-op tactic + print *)
module PV = Proofview

let term_size = ref 5
let test_size = ref 14
let should_prune = ref true

let mk_temp_file tmp_filename =
  if Sys.file_exists tmp_filename
  then 
    (
      Sys.remove tmp_filename ;
      Sys.command ("touch " ^ tmp_filename)
    )
  else Sys.command ("touch " ^ tmp_filename)


let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (Bytes.unsafe_to_string s)
  
let read_and_print_file filename =
  if Sys.file_exists filename
  then Feedback.msg_info (str (load_file filename))
  else CErrors.user_err (str "failed to find file")

let stripchars s cs =
  let len = String.length s in
  let res = Bytes.create len in
  let rec aux i j =
    if i >= len
    then Bytes.to_string (Bytes.sub res 0 j)
    else if String.contains cs s.[i] then
      aux (succ i) (j)
    else begin
      Bytes.set res j s.[i];
      aux (succ i) (succ j)
    end
  in aux 0 0

type fnamesMap = (Libnames.qualid * string) list
let make_unique_fnames (fnames : Libnames.qualid list) : fnamesMap =
  List.map (fun fn -> 
    let basename_str = Id.to_string (Libnames.qualid_basename fn) in
    let no_underscore s = stripchars s "_" in
    let uniq_prefix = "zz" in
    (fn, uniq_prefix ^ no_underscore basename_str)
  ) fnames

let get_mapped_name (fnames_map : fnamesMap) (id : Libnames.qualid) =
  match List.find_opt (fun (id_, f) -> id_ = id) fnames_map with
  | Some (_, f) -> Some f
  | None -> None

let get_original_name (fnames_map : fnamesMap) (mapped_name : string) =
  match List.find_opt (fun (_, f) -> f = mapped_name) fnames_map with
  | Some (id, _) -> Some id
  | None -> None

let replace input output =
  Str.global_replace (Str.regexp_string input) output

let replace_names (fnames_map : fnamesMap) file =
  let filestr = load_file file in
  let filestr_replaced = List.fold_left (fun fileacc (id, n) -> 
    replace n (Libnames.string_of_qualid id) fileacc
  ) filestr fnames_map in
  let oc = open_out file in
  (Printf.fprintf oc "%s" filestr_replaced;
  close_out oc)


let run_quickspec (hs_filename : string) (fnames : Libnames.qualid list) =
  if Sys.file_exists (hs_filename ^ ".hs")
  then 
    
    let fnames_map = make_unique_fnames fnames in
    let include_fnames_arg = List.fold_left (fun acc id -> "-i " ^ Id.to_string (Libnames.qualid_basename id) ^ " " ^ acc) "" fnames in
    let smtfilename = hs_filename ^ ".smt2" in
    let smt_qs_res_filename = "qs_res_" ^ smtfilename in
    let mk_smtfile () = Sys.command ("touch " ^ smtfilename) in
    let mk_smtresfile () = Sys.command ("touch " ^ smt_qs_res_filename) in
    let rename_names_in_hs_file () =
      let file_str = load_file (hs_filename ^ ".hs") in
      let file_str_replaced = List.fold_left (fun file_acc (id, mapped_name) ->
        let id_str = Id.to_string (Libnames.qualid_basename id) in
        replace (id_str ^ " ") (mapped_name ^ " ") file_acc (* the ^ " " ensures no other names with same prefix are changed*)
      ) file_str fnames_map in
      let oc = open_out (hs_filename ^ ".hs") in
      (Printf.fprintf oc "%s" file_str_replaced;
      close_out oc) 
    in
    
    
    let run_tipghc () = Sys.command ("tip-ghc " ^ hs_filename ^ ".hs > " ^ smtfilename) in
    let prune_str = if !should_prune then " --prune " else " " in
    let run_tipspec () = let cmdstr = "tip-spec" ^ prune_str ^ "--size " ^ string_of_int !term_size 
                                     ^ " --test-size " ^ string_of_int !test_size ^ " " 
                                     ^ include_fnames_arg 
                                     ^ smtfilename 
                                     ^ " > " ^ smt_qs_res_filename in
                            (Feedback.msg_info (str ("executing command: " ^ cmdstr))
                            ; Sys.command cmdstr) in
    let run_tipcoq () = Sys.command ("tip --coq " ^ smt_qs_res_filename ^ " > .temp") in
    if (mk_smtfile() = 0 && mk_smtresfile () = 0) 
      then let _ = ()(* rename_names_in_hs_file ()*)
      in if run_tipghc () = 0
        then 
          if run_tipspec () = 0
          then if mk_temp_file ".temp" = 0 then 
            if run_tipcoq () = 0 then
              (read_and_print_file ".temp")
            else CErrors.user_err (str "failed to make temp file")
          else CErrors.user_err (str "failed tip --coq")
        else CErrors.user_err (str "Error")
      else CErrors.user_err (str "Error")
  else CErrors.user_err (str "Failed to find file " ++ str hs_filename ++ str ".hs" ++ str " generated by extraction") 
