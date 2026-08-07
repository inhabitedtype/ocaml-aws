type t = { metric_data_queries : MetricDataQueries.t }

let make ~metric_data_queries () = { metric_data_queries }

let parse xml =
  Some
    { metric_data_queries =
        Aws.Xml.required
          "MetricDataQueries"
          (Aws.Util.option_bind
             (Aws.Xml.member "MetricDataQueries" xml)
             MetricDataQueries.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "MetricDataQueries.member"
              , MetricDataQueries.to_query v.metric_data_queries ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("MetricDataQueries", MetricDataQueries.to_json v.metric_data_queries) ])

let of_json j =
  { metric_data_queries =
      MetricDataQueries.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricDataQueries"))
  }
