open Aws.BaseTypes

type t =
  { metric_dimension_results : MetricDimensionResultSet.t
  ; next_token : String.t option
  }

let make ?(metric_dimension_results = []) ?next_token () =
  { metric_dimension_results; next_token }

let parse xml =
  Some
    { metric_dimension_results =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "metricDimensionResultSet" xml)
             MetricDimensionResultSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "MetricDimensionResultSet"
              , MetricDimensionResultSet.to_query v.metric_dimension_results ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "metricDimensionResultSet"
           , MetricDimensionResultSet.to_json v.metric_dimension_results )
       ])

let of_json j =
  { metric_dimension_results =
      MetricDimensionResultSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "metricDimensionResultSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
