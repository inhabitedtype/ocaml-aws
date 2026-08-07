open Aws.BaseTypes

type t = { private_ip_address : String.t option }

let make ?private_ip_address () = { private_ip_address }

let parse xml =
  Some
    { private_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "PrivateIpAddress" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.private_ip_address (fun f ->
             Aws.Query.Pair ("PrivateIpAddress", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.private_ip_address (fun f ->
             "PrivateIpAddress", String.to_json f)
       ])

let of_json j =
  { private_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "PrivateIpAddress") String.of_json
  }
