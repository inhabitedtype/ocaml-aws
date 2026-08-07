open Aws.BaseTypes

type t = { cidr : String.t option }

let make ?cidr () = { cidr }

let parse xml =
  Some { cidr = Aws.Util.option_bind (Aws.Xml.member "poolCidrBlock" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cidr (fun f ->
             Aws.Query.Pair ("PoolCidrBlock", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cidr (fun f -> "poolCidrBlock", String.to_json f) ])

let of_json j =
  { cidr = Aws.Util.option_map (Aws.Json.lookup j "poolCidrBlock") String.of_json }
