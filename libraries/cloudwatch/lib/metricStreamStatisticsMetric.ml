open Aws.BaseTypes

type t =
  { namespace : String.t
  ; metric_name : String.t
  }

let make ~namespace ~metric_name () = { namespace; metric_name }

let parse xml =
  Some
    { namespace =
        Aws.Xml.required
          "Namespace"
          (Aws.Util.option_bind (Aws.Xml.member "Namespace" xml) String.parse)
    ; metric_name =
        Aws.Xml.required
          "MetricName"
          (Aws.Util.option_bind (Aws.Xml.member "MetricName" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("MetricName", String.to_query v.metric_name))
       ; Some (Aws.Query.Pair ("Namespace", String.to_query v.namespace))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("MetricName", String.to_json v.metric_name)
       ; Some ("Namespace", String.to_json v.namespace)
       ])

let of_json j =
  { namespace = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Namespace"))
  ; metric_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricName"))
  }
