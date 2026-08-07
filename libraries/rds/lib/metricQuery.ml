type t = { performance_insights_metric_query : PerformanceInsightsMetricQuery.t option }

let make ?performance_insights_metric_query () = { performance_insights_metric_query }

let parse xml =
  Some
    { performance_insights_metric_query =
        Aws.Util.option_bind
          (Aws.Xml.member "PerformanceInsightsMetricQuery" xml)
          PerformanceInsightsMetricQuery.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.performance_insights_metric_query (fun f ->
             Aws.Query.Pair
               ( "PerformanceInsightsMetricQuery"
               , PerformanceInsightsMetricQuery.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.performance_insights_metric_query (fun f ->
             "PerformanceInsightsMetricQuery", PerformanceInsightsMetricQuery.to_json f)
       ])

let of_json j =
  { performance_insights_metric_query =
      Aws.Util.option_map
        (Aws.Json.lookup j "PerformanceInsightsMetricQuery")
        PerformanceInsightsMetricQuery.of_json
  }
