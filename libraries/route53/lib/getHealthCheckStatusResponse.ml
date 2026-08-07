type t = { health_check_observations : HealthCheckObservations.t }

let make ~health_check_observations () = { health_check_observations }

let parse xml =
  Some
    { health_check_observations =
        Aws.Xml.required
          "HealthCheckObservations"
          (Aws.Util.option_bind
             (Aws.Xml.member "HealthCheckObservations" xml)
             HealthCheckObservations.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "HealthCheckObservations.member"
              , HealthCheckObservations.to_query v.health_check_observations ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "HealthCheckObservations"
           , HealthCheckObservations.to_json v.health_check_observations )
       ])

let of_json j =
  { health_check_observations =
      HealthCheckObservations.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheckObservations"))
  }
