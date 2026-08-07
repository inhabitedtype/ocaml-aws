open Aws.BaseTypes

type t =
  | Ipv4
  | Dualstack
  | Ipv6
  | Service_defined

let str_to_t =
  [ "service-defined", Service_defined
  ; "ipv6", Ipv6
  ; "dualstack", Dualstack
  ; "ipv4", Ipv4
  ]

let t_to_str =
  [ Service_defined, "service-defined"
  ; Ipv6, "ipv6"
  ; Dualstack, "dualstack"
  ; Ipv4, "ipv4"
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
