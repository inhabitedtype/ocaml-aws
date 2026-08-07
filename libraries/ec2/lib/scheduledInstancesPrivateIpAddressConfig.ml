open Aws.BaseTypes

type t =
  { primary : Boolean.t option
  ; private_ip_address : String.t option
  }

let make ?primary ?private_ip_address () = { primary; private_ip_address }

let parse xml =
  Some
    { primary = Aws.Util.option_bind (Aws.Xml.member "Primary" xml) Boolean.parse
    ; private_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "PrivateIpAddress" xml) String.parse
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
             "PrivateIpAddress", String.to_json f)
       ; Aws.Util.option_map v.primary (fun f -> "Primary", Boolean.to_json f)
       ])

let of_json j =
  { primary = Aws.Util.option_map (Aws.Json.lookup j "Primary") Boolean.of_json
  ; private_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "PrivateIpAddress") String.of_json
  }
