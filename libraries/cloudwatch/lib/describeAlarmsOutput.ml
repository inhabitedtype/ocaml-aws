open Aws.BaseTypes

type t =
  { composite_alarms : CompositeAlarms.t
  ; metric_alarms : MetricAlarms.t
  ; next_token : String.t option
  }

let make ?(composite_alarms = []) ?(metric_alarms = []) ?next_token () =
  { composite_alarms; metric_alarms; next_token }

let parse xml =
  Some
    { composite_alarms =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "CompositeAlarms" xml)
             CompositeAlarms.parse)
    ; metric_alarms =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "MetricAlarms" xml) MetricAlarms.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair ("MetricAlarms.member", MetricAlarms.to_query v.metric_alarms))
       ; Some
           (Aws.Query.Pair
              ("CompositeAlarms.member", CompositeAlarms.to_query v.composite_alarms))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("MetricAlarms", MetricAlarms.to_json v.metric_alarms)
       ; Some ("CompositeAlarms", CompositeAlarms.to_json v.composite_alarms)
       ])

let of_json j =
  { composite_alarms =
      CompositeAlarms.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CompositeAlarms"))
  ; metric_alarms =
      MetricAlarms.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricAlarms"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
