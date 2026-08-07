type t = { health_check : HealthCheck.t option }

let make ?health_check () = { health_check }

let parse xml =
  Some
    { health_check =
        Aws.Util.option_bind (Aws.Xml.member "HealthCheck" xml) HealthCheck.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.health_check (fun f ->
             Aws.Query.Pair ("HealthCheck", HealthCheck.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.health_check (fun f ->
             "HealthCheck", HealthCheck.to_json f)
       ])

let of_json j =
  { health_check =
      Aws.Util.option_map (Aws.Json.lookup j "HealthCheck") HealthCheck.of_json
  }
