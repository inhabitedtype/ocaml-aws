open Aws.BaseTypes

type t = { data : String.t option }

let make ?data () = { data }

let parse xml =
  Some { data = Aws.Util.option_bind (Aws.Xml.member "data" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.data (fun f -> Aws.Query.Pair ("Data", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.data (fun f -> "data", String.to_json f) ])

let of_json j = { data = Aws.Util.option_map (Aws.Json.lookup j "data") String.of_json }
