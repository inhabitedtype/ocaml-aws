open Aws.BaseTypes

type t = { health_check_id : String.t }

let make ~health_check_id () = { health_check_id }

let parse xml =
  Some
    { health_check_id =
        Aws.Xml.required
          "HealthCheckId"
          (Aws.Util.option_bind (Aws.Xml.member "HealthCheckId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("HealthCheckId", String.to_query v.health_check_id)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("HealthCheckId", String.to_json v.health_check_id) ])

let of_json j =
  { health_check_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheckId"))
  }
