open Aws.BaseTypes

type t =
  { predefined_metric_type : PredefinedMetricPairType.t
  ; resource_label : String.t option
  }

let make ~predefined_metric_type ?resource_label () =
  { predefined_metric_type; resource_label }

let parse xml =
  Some
    { predefined_metric_type =
        Aws.Xml.required
          "PredefinedMetricType"
          (Aws.Util.option_bind
             (Aws.Xml.member "PredefinedMetricType" xml)
             PredefinedMetricPairType.parse)
    ; resource_label =
        Aws.Util.option_bind (Aws.Xml.member "ResourceLabel" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_label (fun f ->
             Aws.Query.Pair ("ResourceLabel", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "PredefinedMetricType"
              , PredefinedMetricPairType.to_query v.predefined_metric_type ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_label (fun f -> "ResourceLabel", String.to_json f)
       ; Some
           ( "PredefinedMetricType"
           , PredefinedMetricPairType.to_json v.predefined_metric_type )
       ])

let of_json j =
  { predefined_metric_type =
      PredefinedMetricPairType.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PredefinedMetricType"))
  ; resource_label =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceLabel") String.of_json
  }
