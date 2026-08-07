type t = { retention_triggers : RetentionTriggers.t option }

let make ?retention_triggers () = { retention_triggers }

let parse xml =
  Some
    { retention_triggers =
        Aws.Util.option_bind
          (Aws.Xml.member "RetentionTriggers" xml)
          RetentionTriggers.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.retention_triggers (fun f ->
             Aws.Query.Pair ("RetentionTriggers", RetentionTriggers.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.retention_triggers (fun f ->
             "RetentionTriggers", RetentionTriggers.to_json f)
       ])

let of_json j =
  { retention_triggers =
      Aws.Util.option_map
        (Aws.Json.lookup j "RetentionTriggers")
        RetentionTriggers.of_json
  }
