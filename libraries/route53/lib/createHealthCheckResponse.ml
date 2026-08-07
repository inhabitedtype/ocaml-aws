open Aws.BaseTypes

type t =
  { health_check : HealthCheck.t
  ; location : String.t
  }

let make ~health_check ~location () = { health_check; location }

let parse xml =
  Some
    { health_check =
        Aws.Xml.required
          "HealthCheck"
          (Aws.Util.option_bind (Aws.Xml.member "HealthCheck" xml) HealthCheck.parse)
    ; location =
        Aws.Xml.required
          "Location"
          (Aws.Util.option_bind (Aws.Xml.member "Location" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Location", String.to_query v.location))
       ; Some (Aws.Query.Pair ("HealthCheck", HealthCheck.to_query v.health_check))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Location", String.to_json v.location)
       ; Some ("HealthCheck", HealthCheck.to_json v.health_check)
       ])

let of_json j =
  { health_check =
      HealthCheck.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheck"))
  ; location = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Location"))
  }
