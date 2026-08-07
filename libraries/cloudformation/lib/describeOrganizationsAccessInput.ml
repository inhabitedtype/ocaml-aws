type t = { call_as : CallAs.t option }

let make ?call_as () = { call_as }

let parse xml =
  Some { call_as = Aws.Util.option_bind (Aws.Xml.member "CallAs" xml) CallAs.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.call_as (fun f ->
             Aws.Query.Pair ("CallAs", CallAs.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.call_as (fun f -> "CallAs", CallAs.to_json f) ])

let of_json j =
  { call_as = Aws.Util.option_map (Aws.Json.lookup j "CallAs") CallAs.of_json }
