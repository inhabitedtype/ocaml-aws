open Aws.BaseTypes

type t = { ipv6_address : String.t option }

let make ?ipv6_address () = { ipv6_address }

let parse xml =
  Some
    { ipv6_address = Aws.Util.option_bind (Aws.Xml.member "Ipv6Address" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipv6_address (fun f ->
             Aws.Query.Pair ("Ipv6Address", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipv6_address (fun f -> "Ipv6Address", String.to_json f) ])

let of_json j =
  { ipv6_address = Aws.Util.option_map (Aws.Json.lookup j "Ipv6Address") String.of_json }
