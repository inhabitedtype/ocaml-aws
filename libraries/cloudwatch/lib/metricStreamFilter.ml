open Aws.BaseTypes

type t =
  { namespace : String.t option
  ; metric_names : MetricStreamFilterMetricNames.t
  }

let make ?namespace ?(metric_names = []) () = { namespace; metric_names }

let parse xml =
  Some
    { namespace = Aws.Util.option_bind (Aws.Xml.member "Namespace" xml) String.parse
    ; metric_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "MetricNames" xml)
             MetricStreamFilterMetricNames.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("MetricNames.member", MetricStreamFilterMetricNames.to_query v.metric_names))
       ; Aws.Util.option_map v.namespace (fun f ->
             Aws.Query.Pair ("Namespace", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("MetricNames", MetricStreamFilterMetricNames.to_json v.metric_names)
       ; Aws.Util.option_map v.namespace (fun f -> "Namespace", String.to_json f)
       ])

let of_json j =
  { namespace = Aws.Util.option_map (Aws.Json.lookup j "Namespace") String.of_json
  ; metric_names =
      MetricStreamFilterMetricNames.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricNames"))
  }
