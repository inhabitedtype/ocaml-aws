open Aws.BaseTypes

type t =
  | Packet_length
  | Description
  | Virtual_network_id

let str_to_t =
  [ "virtual-network-id", Virtual_network_id
  ; "description", Description
  ; "packet-length", Packet_length
  ]

let t_to_str =
  [ Virtual_network_id, "virtual-network-id"
  ; Description, "description"
  ; Packet_length, "packet-length"
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
