type t = { snapshot : Snapshot.t option }

let make ?snapshot () = { snapshot }

let parse xml =
  Some { snapshot = Aws.Util.option_bind (Aws.Xml.member "Snapshot" xml) Snapshot.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.snapshot (fun f ->
             Aws.Query.Pair ("Snapshot", Snapshot.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.snapshot (fun f -> "Snapshot", Snapshot.to_json f) ])

let of_json j =
  { snapshot = Aws.Util.option_map (Aws.Json.lookup j "Snapshot") Snapshot.of_json }
