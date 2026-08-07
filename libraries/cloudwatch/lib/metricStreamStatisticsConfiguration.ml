type t =
  { include_metrics : MetricStreamStatisticsIncludeMetrics.t
  ; additional_statistics : MetricStreamStatisticsAdditionalStatistics.t
  }

let make ~include_metrics ~additional_statistics () =
  { include_metrics; additional_statistics }

let parse xml =
  Some
    { include_metrics =
        Aws.Xml.required
          "IncludeMetrics"
          (Aws.Util.option_bind
             (Aws.Xml.member "IncludeMetrics" xml)
             MetricStreamStatisticsIncludeMetrics.parse)
    ; additional_statistics =
        Aws.Xml.required
          "AdditionalStatistics"
          (Aws.Util.option_bind
             (Aws.Xml.member "AdditionalStatistics" xml)
             MetricStreamStatisticsAdditionalStatistics.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "AdditionalStatistics.member"
              , MetricStreamStatisticsAdditionalStatistics.to_query
                  v.additional_statistics ))
       ; Some
           (Aws.Query.Pair
              ( "IncludeMetrics.member"
              , MetricStreamStatisticsIncludeMetrics.to_query v.include_metrics ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "AdditionalStatistics"
           , MetricStreamStatisticsAdditionalStatistics.to_json v.additional_statistics )
       ; Some
           ( "IncludeMetrics"
           , MetricStreamStatisticsIncludeMetrics.to_json v.include_metrics )
       ])

let of_json j =
  { include_metrics =
      MetricStreamStatisticsIncludeMetrics.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "IncludeMetrics"))
  ; additional_statistics =
      MetricStreamStatisticsAdditionalStatistics.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AdditionalStatistics"))
  }
