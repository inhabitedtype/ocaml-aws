open Aws.BaseTypes

type t =
  { metric : Metric.t option
  ; value : Double.t option
  }

let make ?metric ?value () = { metric; value }

let parse xml =
  Some
    { metric = Aws.Util.option_bind (Aws.Xml.member "metric" xml) Metric.parse
    ; value = Aws.Util.option_bind (Aws.Xml.member "value" xml) Double.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.value (fun f ->
             Aws.Query.Pair ("Value", Double.to_query f))
       ; Aws.Util.option_map v.metric (fun f ->
             Aws.Query.Pair ("Metric", Metric.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.value (fun f -> "value", Double.to_json f)
       ; Aws.Util.option_map v.metric (fun f -> "metric", Metric.to_json f)
       ])

let of_json j =
  { metric = Aws.Util.option_map (Aws.Json.lookup j "metric") Metric.of_json
  ; value = Aws.Util.option_map (Aws.Json.lookup j "value") Double.of_json
  }
