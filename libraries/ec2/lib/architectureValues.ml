open Aws.BaseTypes

type t =
  | I386
  | X86_64
  | Arm64
  | X86_64_mac
  | Arm64_mac

let str_to_t =
  [ "arm64_mac", Arm64_mac
  ; "x86_64_mac", X86_64_mac
  ; "arm64", Arm64
  ; "x86_64", X86_64
  ; "i386", I386
  ]

let t_to_str =
  [ Arm64_mac, "arm64_mac"
  ; X86_64_mac, "x86_64_mac"
  ; Arm64, "arm64"
  ; X86_64, "x86_64"
  ; I386, "i386"
  ]

let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
let make v () = v

let parse xml =
  Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

let to_query v =
  Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
