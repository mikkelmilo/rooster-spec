open Pcoq.Prim

DECLARE PLUGIN "coq_spec"

open Ltac_plugin
open Extraction_plugin
open Genarg
open Stdarg
open Pp
open Names
open Table
open Extract_env



let pr_mlname _ _ _ s = spc () ++ qs s

let _ = extraction_language Haskell


VERNAC COMMAND EXTEND Config CLASSIFIED AS QUERY
| ["MaxTermSize" int(x)] -> [Ce_api.term_size := x]
| ["MaxTestSize" int(x)] -> [Ce_api.test_size := x]
END


VERNAC COMMAND EXTEND DiscoverLemmas CLASSIFIED AS QUERY
(* Extraction in the Coq toplevel *)
| [ "MyExtraction" global(x) ] -> [ simple_extraction x ]
(* | [ "Recursive" "Extraction" ne_global_list(l) ] -> [ full_extraction None l ] *)

(* Monolithic extraction to a file *)
| [ "DiscoverLemmas" string(f) ne_global_list(l) ]
  -> [ full_extraction (Some f) l ; Ce_api.run_quickspec f l]
(* 
(* Extraction to a temporary file and OCaml compilation *)
| [ "Extraction" "TestCompile" ne_global_list(l) ]
  -> [ extract_and_compile l ] *)
END
