open Aws.BaseTypes

type t = { health_check_count : Long.t }

let make ~health_check_count () = { health_check_count }

let parse xml =
  Some
    { health_check_count =
        Aws.Xml.required
          "HealthCheckCount"
          (Aws.Util.option_bind (Aws.Xml.member "HealthCheckCount" xml) Long.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("HealthCheckCount", Long.to_query v.health_check_count)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("HealthCheckCount", Long.to_json v.health_check_count) ])

let of_json j =
  { health_check_count =
      Long.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheckCount"))
  }
