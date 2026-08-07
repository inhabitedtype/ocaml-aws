type t =
  { timestamps : PredictiveScalingForecastTimestamps.t
  ; values : PredictiveScalingForecastValues.t
  }

let make ~timestamps ~values () = { timestamps; values }

let parse xml =
  Some
    { timestamps =
        Aws.Xml.required
          "Timestamps"
          (Aws.Util.option_bind
             (Aws.Xml.member "Timestamps" xml)
             PredictiveScalingForecastTimestamps.parse)
    ; values =
        Aws.Xml.required
          "Values"
          (Aws.Util.option_bind
             (Aws.Xml.member "Values" xml)
             PredictiveScalingForecastValues.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("Values.member", PredictiveScalingForecastValues.to_query v.values))
       ; Some
           (Aws.Query.Pair
              ( "Timestamps.member"
              , PredictiveScalingForecastTimestamps.to_query v.timestamps ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Values", PredictiveScalingForecastValues.to_json v.values)
       ; Some ("Timestamps", PredictiveScalingForecastTimestamps.to_json v.timestamps)
       ])

let of_json j =
  { timestamps =
      PredictiveScalingForecastTimestamps.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Timestamps"))
  ; values =
      PredictiveScalingForecastValues.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Values"))
  }
