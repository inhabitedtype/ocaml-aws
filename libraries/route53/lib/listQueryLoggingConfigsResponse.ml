open Aws.BaseTypes

type t =
  { query_logging_configs : QueryLoggingConfigs.t
  ; next_token : String.t option
  }

let make ~query_logging_configs ?next_token () = { query_logging_configs; next_token }

let parse xml =
  Some
    { query_logging_configs =
        Aws.Xml.required
          "QueryLoggingConfigs"
          (Aws.Util.option_bind
             (Aws.Xml.member "QueryLoggingConfigs" xml)
             QueryLoggingConfigs.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "QueryLoggingConfigs.member"
              , QueryLoggingConfigs.to_query v.query_logging_configs ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("QueryLoggingConfigs", QueryLoggingConfigs.to_json v.query_logging_configs)
       ])

let of_json j =
  { query_logging_configs =
      QueryLoggingConfigs.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "QueryLoggingConfigs"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
