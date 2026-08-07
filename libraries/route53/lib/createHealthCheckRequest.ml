open Aws.BaseTypes

type t =
  { caller_reference : String.t
  ; health_check_config : HealthCheckConfig.t
  }

let make ~caller_reference ~health_check_config () =
  { caller_reference; health_check_config }

let parse xml =
  Some
    { caller_reference =
        Aws.Xml.required
          "CallerReference"
          (Aws.Util.option_bind (Aws.Xml.member "CallerReference" xml) String.parse)
    ; health_check_config =
        Aws.Xml.required
          "HealthCheckConfig"
          (Aws.Util.option_bind
             (Aws.Xml.member "HealthCheckConfig" xml)
             HealthCheckConfig.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("HealthCheckConfig", HealthCheckConfig.to_query v.health_check_config))
       ; Some (Aws.Query.Pair ("CallerReference", String.to_query v.caller_reference))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("HealthCheckConfig", HealthCheckConfig.to_json v.health_check_config)
       ; Some ("CallerReference", String.to_json v.caller_reference)
       ])

let of_json j =
  { caller_reference =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CallerReference"))
  ; health_check_config =
      HealthCheckConfig.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheckConfig"))
  }
