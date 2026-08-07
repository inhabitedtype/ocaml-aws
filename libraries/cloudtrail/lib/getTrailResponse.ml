type t = { trail : Trail.t option }

let make ?trail () = { trail }

let parse xml =
  Some { trail = Aws.Util.option_bind (Aws.Xml.member "Trail" xml) Trail.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.trail (fun f -> Aws.Query.Pair ("Trail", Trail.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.trail (fun f -> "Trail", Trail.to_json f) ])

let of_json j = { trail = Aws.Util.option_map (Aws.Json.lookup j "Trail") Trail.of_json }
