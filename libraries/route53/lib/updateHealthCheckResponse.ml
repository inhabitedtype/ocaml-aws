type t = { health_check : HealthCheck.t }

let make ~health_check () = { health_check }

let parse xml =
  Some
    { health_check =
        Aws.Xml.required
          "HealthCheck"
          (Aws.Util.option_bind (Aws.Xml.member "HealthCheck" xml) HealthCheck.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("HealthCheck", HealthCheck.to_query v.health_check)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("HealthCheck", HealthCheck.to_json v.health_check) ])

let of_json j =
  { health_check =
      HealthCheck.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheck"))
  }
