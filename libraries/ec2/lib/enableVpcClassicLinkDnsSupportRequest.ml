open Aws.BaseTypes

type t = { vpc_id : String.t option }

let make ?vpc_id () = { vpc_id }

let parse xml =
  Some { vpc_id = Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_id (fun f -> "VpcId", String.to_json f) ])

let of_json j =
  { vpc_id = Aws.Util.option_map (Aws.Json.lookup j "VpcId") String.of_json }
