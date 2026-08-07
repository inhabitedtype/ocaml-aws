open Aws.BaseTypes

type t =
  { primary : Boolean.t option
  ; private_ip_address : String.t option
  }

let make ?primary ?private_ip_address () = { primary; private_ip_address }

let parse xml =
  Some
    { primary = Aws.Util.option_bind (Aws.Xml.member "primary" xml) Boolean.parse
    ; private_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "privateIpAddress" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.private_ip_address (fun f ->
             Aws.Query.Pair ("PrivateIpAddress", String.to_query f))
       ; Aws.Util.option_map v.primary (fun f ->
             Aws.Query.Pair ("Primary", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.private_ip_address (fun f ->
             "privateIpAddress", String.to_json f)
       ; Aws.Util.option_map v.primary (fun f -> "primary", Boolean.to_json f)
       ])

let of_json j =
  { primary = Aws.Util.option_map (Aws.Json.lookup j "primary") Boolean.of_json
  ; private_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "privateIpAddress") String.of_json
  }
