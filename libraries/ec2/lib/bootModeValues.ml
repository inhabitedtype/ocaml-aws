open Aws.BaseTypes

type t =
  | Legacy_bios
  | Uefi
  | Uefi_preferred

let str_to_t =
  [ "uefi-preferred", Uefi_preferred; "uefi", Uefi; "legacy-bios", Legacy_bios ]

let t_to_str =
  [ Uefi_preferred, "uefi-preferred"; Uefi, "uefi"; Legacy_bios, "legacy-bios" ]

let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
let make v () = v

let parse xml =
  Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

let to_query v =
  Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
