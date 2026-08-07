open Aws.BaseTypes

type t =
  | Amd_sev_snp
  | Nested_virtualization

let str_to_t =
  [ "nested-virtualization", Nested_virtualization; "amd-sev-snp", Amd_sev_snp ]

let t_to_str =
  [ Nested_virtualization, "nested-virtualization"; Amd_sev_snp, "amd-sev-snp" ]

let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
let make v () = v

let parse xml =
  Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

let to_query v =
  Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
