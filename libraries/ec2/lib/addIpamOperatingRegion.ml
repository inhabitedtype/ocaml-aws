open Aws.BaseTypes

type t = { region_name : String.t option }

let make ?region_name () = { region_name }

let parse xml =
  Some
    { region_name = Aws.Util.option_bind (Aws.Xml.member "RegionName" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.region_name (fun f ->
             Aws.Query.Pair ("RegionName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.region_name (fun f -> "RegionName", String.to_json f) ])

let of_json j =
  { region_name = Aws.Util.option_map (Aws.Json.lookup j "RegionName") String.of_json }
