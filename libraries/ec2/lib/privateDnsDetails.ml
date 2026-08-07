open Aws.BaseTypes

type t = { private_dns_name : String.t option }

let make ?private_dns_name () = { private_dns_name }

let parse xml =
  Some
    { private_dns_name =
        Aws.Util.option_bind (Aws.Xml.member "privateDnsName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.private_dns_name (fun f ->
             Aws.Query.Pair ("PrivateDnsName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.private_dns_name (fun f ->
             "privateDnsName", String.to_json f)
       ])

let of_json j =
  { private_dns_name =
      Aws.Util.option_map (Aws.Json.lookup j "privateDnsName") String.of_json
  }
