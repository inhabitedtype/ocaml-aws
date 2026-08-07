open Aws.BaseTypes

type t =
  { logging_configurations : VerifiedAccessInstanceLoggingConfigurationList.t
  ; next_token : String.t option
  }

let make ?(logging_configurations = []) ?next_token () =
  { logging_configurations; next_token }

let parse xml =
  Some
    { logging_configurations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "loggingConfigurationSet" xml)
             VerifiedAccessInstanceLoggingConfigurationList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LoggingConfigurationSet"
              , VerifiedAccessInstanceLoggingConfigurationList.to_query
                  v.logging_configurations ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "loggingConfigurationSet"
           , VerifiedAccessInstanceLoggingConfigurationList.to_json
               v.logging_configurations )
       ])

let of_json j =
  { logging_configurations =
      VerifiedAccessInstanceLoggingConfigurationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "loggingConfigurationSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
