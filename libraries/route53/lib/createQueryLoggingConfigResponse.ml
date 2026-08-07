open Aws.BaseTypes

type t =
  { query_logging_config : QueryLoggingConfig.t
  ; location : String.t
  }

let make ~query_logging_config ~location () = { query_logging_config; location }

let parse xml =
  Some
    { query_logging_config =
        Aws.Xml.required
          "QueryLoggingConfig"
          (Aws.Util.option_bind
             (Aws.Xml.member "QueryLoggingConfig" xml)
             QueryLoggingConfig.parse)
    ; location =
        Aws.Xml.required
          "Location"
          (Aws.Util.option_bind (Aws.Xml.member "Location" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Location", String.to_query v.location))
       ; Some
           (Aws.Query.Pair
              ("QueryLoggingConfig", QueryLoggingConfig.to_query v.query_logging_config))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Location", String.to_json v.location)
       ; Some ("QueryLoggingConfig", QueryLoggingConfig.to_json v.query_logging_config)
       ])

let of_json j =
  { query_logging_config =
      QueryLoggingConfig.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "QueryLoggingConfig"))
  ; location = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Location"))
  }
