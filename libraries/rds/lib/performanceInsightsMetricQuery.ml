open Aws.BaseTypes

type t =
  { group_by : PerformanceInsightsMetricDimensionGroup.t option
  ; metric : String.t option
  }

let make ?group_by ?metric () = { group_by; metric }

let parse xml =
  Some
    { group_by =
        Aws.Util.option_bind
          (Aws.Xml.member "GroupBy" xml)
          PerformanceInsightsMetricDimensionGroup.parse
    ; metric = Aws.Util.option_bind (Aws.Xml.member "Metric" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.metric (fun f ->
             Aws.Query.Pair ("Metric", String.to_query f))
       ; Aws.Util.option_map v.group_by (fun f ->
             Aws.Query.Pair ("GroupBy", PerformanceInsightsMetricDimensionGroup.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.metric (fun f -> "Metric", String.to_json f)
       ; Aws.Util.option_map v.group_by (fun f ->
             "GroupBy", PerformanceInsightsMetricDimensionGroup.to_json f)
       ])

let of_json j =
  { group_by =
      Aws.Util.option_map
        (Aws.Json.lookup j "GroupBy")
        PerformanceInsightsMetricDimensionGroup.of_json
  ; metric = Aws.Util.option_map (Aws.Json.lookup j "Metric") String.of_json
  }
