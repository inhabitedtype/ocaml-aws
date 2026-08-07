open Aws.BaseTypes

type t =
  { metric_data_results : MetricDataResultSet.t
  ; next_token : String.t option
  }

let make ?(metric_data_results = []) ?next_token () = { metric_data_results; next_token }

let parse xml =
  Some
    { metric_data_results =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "metricDataResultSet" xml)
             MetricDataResultSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("MetricDataResultSet", MetricDataResultSet.to_query v.metric_data_results))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("metricDataResultSet", MetricDataResultSet.to_json v.metric_data_results)
       ])

let of_json j =
  { metric_data_results =
      MetricDataResultSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "metricDataResultSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
