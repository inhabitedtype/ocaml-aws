open Aws.BaseTypes

type t =
  { key_labels : InsightRuleContributorKeyLabels.t
  ; aggregation_statistic : String.t option
  ; aggregate_value : Double.t option
  ; approximate_unique_count : Long.t option
  ; contributors : InsightRuleContributors.t
  ; metric_datapoints : InsightRuleMetricDatapoints.t
  }

let make
    ?(key_labels = [])
    ?aggregation_statistic
    ?aggregate_value
    ?approximate_unique_count
    ?(contributors = [])
    ?(metric_datapoints = [])
    () =
  { key_labels
  ; aggregation_statistic
  ; aggregate_value
  ; approximate_unique_count
  ; contributors
  ; metric_datapoints
  }

let parse xml =
  Some
    { key_labels =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "KeyLabels" xml)
             InsightRuleContributorKeyLabels.parse)
    ; aggregation_statistic =
        Aws.Util.option_bind (Aws.Xml.member "AggregationStatistic" xml) String.parse
    ; aggregate_value =
        Aws.Util.option_bind (Aws.Xml.member "AggregateValue" xml) Double.parse
    ; approximate_unique_count =
        Aws.Util.option_bind (Aws.Xml.member "ApproximateUniqueCount" xml) Long.parse
    ; contributors =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Contributors" xml)
             InsightRuleContributors.parse)
    ; metric_datapoints =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "MetricDatapoints" xml)
             InsightRuleMetricDatapoints.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "MetricDatapoints.member"
              , InsightRuleMetricDatapoints.to_query v.metric_datapoints ))
       ; Some
           (Aws.Query.Pair
              ("Contributors.member", InsightRuleContributors.to_query v.contributors))
       ; Aws.Util.option_map v.approximate_unique_count (fun f ->
             Aws.Query.Pair ("ApproximateUniqueCount", Long.to_query f))
       ; Aws.Util.option_map v.aggregate_value (fun f ->
             Aws.Query.Pair ("AggregateValue", Double.to_query f))
       ; Aws.Util.option_map v.aggregation_statistic (fun f ->
             Aws.Query.Pair ("AggregationStatistic", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("KeyLabels.member", InsightRuleContributorKeyLabels.to_query v.key_labels))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("MetricDatapoints", InsightRuleMetricDatapoints.to_json v.metric_datapoints)
       ; Some ("Contributors", InsightRuleContributors.to_json v.contributors)
       ; Aws.Util.option_map v.approximate_unique_count (fun f ->
             "ApproximateUniqueCount", Long.to_json f)
       ; Aws.Util.option_map v.aggregate_value (fun f ->
             "AggregateValue", Double.to_json f)
       ; Aws.Util.option_map v.aggregation_statistic (fun f ->
             "AggregationStatistic", String.to_json f)
       ; Some ("KeyLabels", InsightRuleContributorKeyLabels.to_json v.key_labels)
       ])

let of_json j =
  { key_labels =
      InsightRuleContributorKeyLabels.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyLabels"))
  ; aggregation_statistic =
      Aws.Util.option_map (Aws.Json.lookup j "AggregationStatistic") String.of_json
  ; aggregate_value =
      Aws.Util.option_map (Aws.Json.lookup j "AggregateValue") Double.of_json
  ; approximate_unique_count =
      Aws.Util.option_map (Aws.Json.lookup j "ApproximateUniqueCount") Long.of_json
  ; contributors =
      InsightRuleContributors.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Contributors"))
  ; metric_datapoints =
      InsightRuleMetricDatapoints.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricDatapoints"))
  }
