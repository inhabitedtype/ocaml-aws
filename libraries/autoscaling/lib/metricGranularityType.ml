open Aws.BaseTypes

type t = { granularity : String.t option }

let make ?granularity () = { granularity }

let parse xml =
  Some
    { granularity = Aws.Util.option_bind (Aws.Xml.member "Granularity" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.granularity (fun f ->
             Aws.Query.Pair ("Granularity", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.granularity (fun f -> "Granularity", String.to_json f) ])

let of_json j =
  { granularity = Aws.Util.option_map (Aws.Json.lookup j "Granularity") String.of_json }
