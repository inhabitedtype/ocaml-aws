open Aws.BaseTypes

type t = { metric_widget_image : Blob.t option }

let make ?metric_widget_image () = { metric_widget_image }

let parse xml =
  Some
    { metric_widget_image =
        Aws.Util.option_bind (Aws.Xml.member "MetricWidgetImage" xml) Blob.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.metric_widget_image (fun f ->
             Aws.Query.Pair ("MetricWidgetImage", Blob.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.metric_widget_image (fun f ->
             "MetricWidgetImage", Blob.to_json f)
       ])

let of_json j =
  { metric_widget_image =
      Aws.Util.option_map (Aws.Json.lookup j "MetricWidgetImage") Blob.of_json
  }
