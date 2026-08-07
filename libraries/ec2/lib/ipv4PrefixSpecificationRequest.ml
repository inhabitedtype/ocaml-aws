open Aws.BaseTypes

type t = { ipv4_prefix : String.t option }

let make ?ipv4_prefix () = { ipv4_prefix }

let parse xml =
  Some
    { ipv4_prefix = Aws.Util.option_bind (Aws.Xml.member "Ipv4Prefix" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipv4_prefix (fun f ->
             Aws.Query.Pair ("Ipv4Prefix", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipv4_prefix (fun f -> "Ipv4Prefix", String.to_json f) ])

let of_json j =
  { ipv4_prefix = Aws.Util.option_map (Aws.Json.lookup j "Ipv4Prefix") String.of_json }
