open Aws.BaseTypes

type t =
  | Source_attachment_owner
  | Destination_attachment_owner
  | Transit_gateway_owner

let str_to_t =
  [ "transit-gateway-owner", Transit_gateway_owner
  ; "destination-attachment-owner", Destination_attachment_owner
  ; "source-attachment-owner", Source_attachment_owner
  ]

let t_to_str =
  [ Transit_gateway_owner, "transit-gateway-owner"
  ; Destination_attachment_owner, "destination-attachment-owner"
  ; Source_attachment_owner, "source-attachment-owner"
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
