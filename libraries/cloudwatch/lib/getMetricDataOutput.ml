open Aws.BaseTypes

type t =
  { metric_data_results : MetricDataResults.t
  ; next_token : String.t option
  ; messages : MetricDataResultMessages.t
  }

let make ?(metric_data_results = []) ?next_token ?(messages = []) () =
  { metric_data_results; next_token; messages }

let parse xml =
  Some
    { metric_data_results =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "MetricDataResults" xml)
             MetricDataResults.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; messages =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Messages" xml)
             MetricDataResultMessages.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("Messages.member", MetricDataResultMessages.to_query v.messages))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "MetricDataResults.member"
              , MetricDataResults.to_query v.metric_data_results ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Messages", MetricDataResultMessages.to_json v.messages)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("MetricDataResults", MetricDataResults.to_json v.metric_data_results)
       ])

let of_json j =
  { metric_data_results =
      MetricDataResults.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricDataResults"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; messages =
      MetricDataResultMessages.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Messages"))
  }
