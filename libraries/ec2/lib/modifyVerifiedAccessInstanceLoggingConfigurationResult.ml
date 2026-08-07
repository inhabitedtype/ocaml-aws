type t = { logging_configuration : VerifiedAccessInstanceLoggingConfiguration.t option }

let make ?logging_configuration () = { logging_configuration }

let parse xml =
  Some
    { logging_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "loggingConfiguration" xml)
          VerifiedAccessInstanceLoggingConfiguration.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.logging_configuration (fun f ->
             Aws.Query.Pair
               ( "LoggingConfiguration"
               , VerifiedAccessInstanceLoggingConfiguration.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.logging_configuration (fun f ->
             "loggingConfiguration", VerifiedAccessInstanceLoggingConfiguration.to_json f)
       ])

let of_json j =
  { logging_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "loggingConfiguration")
        VerifiedAccessInstanceLoggingConfiguration.of_json
  }
