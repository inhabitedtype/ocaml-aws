open Aws.BaseTypes

type t =
  { metric_widget : String.t
  ; output_format : String.t option
  }

let make ~metric_widget ?output_format () = { metric_widget; output_format }

let parse xml =
  Some
    { metric_widget =
        Aws.Xml.required
          "MetricWidget"
          (Aws.Util.option_bind (Aws.Xml.member "MetricWidget" xml) String.parse)
    ; output_format =
        Aws.Util.option_bind (Aws.Xml.member "OutputFormat" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.output_format (fun f ->
             Aws.Query.Pair ("OutputFormat", String.to_query f))
       ; Some (Aws.Query.Pair ("MetricWidget", String.to_query v.metric_widget))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.output_format (fun f -> "OutputFormat", String.to_json f)
       ; Some ("MetricWidget", String.to_json v.metric_widget)
       ])

let of_json j =
  { metric_widget =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricWidget"))
  ; output_format = Aws.Util.option_map (Aws.Json.lookup j "OutputFormat") String.of_json
  }
