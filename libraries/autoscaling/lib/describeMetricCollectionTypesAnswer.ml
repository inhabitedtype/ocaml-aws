type t =
  { metrics : MetricCollectionTypes.t
  ; granularities : MetricGranularityTypes.t
  }

let make ?(metrics = []) ?(granularities = []) () = { metrics; granularities }

let parse xml =
  Some
    { metrics =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Metrics" xml)
             MetricCollectionTypes.parse)
    ; granularities =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Granularities" xml)
             MetricGranularityTypes.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("Granularities.member", MetricGranularityTypes.to_query v.granularities))
       ; Some
           (Aws.Query.Pair ("Metrics.member", MetricCollectionTypes.to_query v.metrics))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Granularities", MetricGranularityTypes.to_json v.granularities)
       ; Some ("Metrics", MetricCollectionTypes.to_json v.metrics)
       ])

let of_json j =
  { metrics =
      MetricCollectionTypes.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Metrics"))
  ; granularities =
      MetricGranularityTypes.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Granularities"))
  }
