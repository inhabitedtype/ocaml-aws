type t = { activity : Activity.t option }

let make ?activity () = { activity }

let parse xml =
  Some { activity = Aws.Util.option_bind (Aws.Xml.member "Activity" xml) Activity.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.activity (fun f ->
             Aws.Query.Pair ("Activity", Activity.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.activity (fun f -> "Activity", Activity.to_json f) ])

let of_json j =
  { activity = Aws.Util.option_map (Aws.Json.lookup j "Activity") Activity.of_json }
