open Aws.BaseTypes

type t =
  { excluded_time_ranges : AnomalyDetectorExcludedTimeRanges.t
  ; metric_timezone : String.t option
  }

let make ?(excluded_time_ranges = []) ?metric_timezone () =
  { excluded_time_ranges; metric_timezone }

let parse xml =
  Some
    { excluded_time_ranges =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ExcludedTimeRanges" xml)
             AnomalyDetectorExcludedTimeRanges.parse)
    ; metric_timezone =
        Aws.Util.option_bind (Aws.Xml.member "MetricTimezone" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.metric_timezone (fun f ->
             Aws.Query.Pair ("MetricTimezone", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ExcludedTimeRanges.member"
              , AnomalyDetectorExcludedTimeRanges.to_query v.excluded_time_ranges ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.metric_timezone (fun f ->
             "MetricTimezone", String.to_json f)
       ; Some
           ( "ExcludedTimeRanges"
           , AnomalyDetectorExcludedTimeRanges.to_json v.excluded_time_ranges )
       ])

let of_json j =
  { excluded_time_ranges =
      AnomalyDetectorExcludedTimeRanges.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ExcludedTimeRanges"))
  ; metric_timezone =
      Aws.Util.option_map (Aws.Json.lookup j "MetricTimezone") String.of_json
  }
