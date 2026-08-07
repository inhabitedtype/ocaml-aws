open Aws.BaseTypes

type t =
  { metric_interval_lower_bound : Double.t option
  ; metric_interval_upper_bound : Double.t option
  ; scaling_adjustment : Integer.t
  }

let make ?metric_interval_lower_bound ?metric_interval_upper_bound ~scaling_adjustment ()
    =
  { metric_interval_lower_bound; metric_interval_upper_bound; scaling_adjustment }

let parse xml =
  Some
    { metric_interval_lower_bound =
        Aws.Util.option_bind (Aws.Xml.member "MetricIntervalLowerBound" xml) Double.parse
    ; metric_interval_upper_bound =
        Aws.Util.option_bind (Aws.Xml.member "MetricIntervalUpperBound" xml) Double.parse
    ; scaling_adjustment =
        Aws.Xml.required
          "ScalingAdjustment"
          (Aws.Util.option_bind (Aws.Xml.member "ScalingAdjustment" xml) Integer.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("ScalingAdjustment", Integer.to_query v.scaling_adjustment))
       ; Aws.Util.option_map v.metric_interval_upper_bound (fun f ->
             Aws.Query.Pair ("MetricIntervalUpperBound", Double.to_query f))
       ; Aws.Util.option_map v.metric_interval_lower_bound (fun f ->
             Aws.Query.Pair ("MetricIntervalLowerBound", Double.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ScalingAdjustment", Integer.to_json v.scaling_adjustment)
       ; Aws.Util.option_map v.metric_interval_upper_bound (fun f ->
             "MetricIntervalUpperBound", Double.to_json f)
       ; Aws.Util.option_map v.metric_interval_lower_bound (fun f ->
             "MetricIntervalLowerBound", Double.to_json f)
       ])

let of_json j =
  { metric_interval_lower_bound =
      Aws.Util.option_map (Aws.Json.lookup j "MetricIntervalLowerBound") Double.of_json
  ; metric_interval_upper_bound =
      Aws.Util.option_map (Aws.Json.lookup j "MetricIntervalUpperBound") Double.of_json
  ; scaling_adjustment =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ScalingAdjustment"))
  }
