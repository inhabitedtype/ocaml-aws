type t = { metric_alarms : MetricAlarms.t }

let make ?(metric_alarms = []) () = { metric_alarms }

let parse xml =
  Some
    { metric_alarms =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "MetricAlarms" xml) MetricAlarms.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("MetricAlarms.member", MetricAlarms.to_query v.metric_alarms))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("MetricAlarms", MetricAlarms.to_json v.metric_alarms) ])

let of_json j =
  { metric_alarms =
      MetricAlarms.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricAlarms"))
  }
