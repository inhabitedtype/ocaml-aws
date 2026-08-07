open Aws.BaseTypes

type t = { private_ip_address : String.t }

let make ~private_ip_address () = { private_ip_address }

let parse xml =
  Some
    { private_ip_address =
        Aws.Xml.required
          "PrivateIpAddress"
          (Aws.Util.option_bind (Aws.Xml.member "PrivateIpAddress" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("PrivateIpAddress", String.to_query v.private_ip_address))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("PrivateIpAddress", String.to_json v.private_ip_address) ])

let of_json j =
  { private_ip_address =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PrivateIpAddress"))
  }
