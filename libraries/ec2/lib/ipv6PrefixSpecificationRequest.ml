open Aws.BaseTypes

type t = { ipv6_prefix : String.t option }

let make ?ipv6_prefix () = { ipv6_prefix }

let parse xml =
  Some
    { ipv6_prefix = Aws.Util.option_bind (Aws.Xml.member "Ipv6Prefix" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipv6_prefix (fun f ->
             Aws.Query.Pair ("Ipv6Prefix", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipv6_prefix (fun f -> "Ipv6Prefix", String.to_json f) ])

let of_json j =
  { ipv6_prefix = Aws.Util.option_map (Aws.Json.lookup j "Ipv6Prefix") String.of_json }
