open Aws.BaseTypes

type t =
  { predefined_metric_specification : PredefinedMetricSpecification.t option
  ; customized_metric_specification : CustomizedMetricSpecification.t option
  ; target_value : Double.t
  ; disable_scale_in : Boolean.t option
  }

let make
    ?predefined_metric_specification
    ?customized_metric_specification
    ~target_value
    ?disable_scale_in
    () =
  { predefined_metric_specification
  ; customized_metric_specification
  ; target_value
  ; disable_scale_in
  }

let parse xml =
  Some
    { predefined_metric_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "PredefinedMetricSpecification" xml)
          PredefinedMetricSpecification.parse
    ; customized_metric_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "CustomizedMetricSpecification" xml)
          CustomizedMetricSpecification.parse
    ; target_value =
        Aws.Xml.required
          "TargetValue"
          (Aws.Util.option_bind (Aws.Xml.member "TargetValue" xml) Double.parse)
    ; disable_scale_in =
        Aws.Util.option_bind (Aws.Xml.member "DisableScaleIn" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.disable_scale_in (fun f ->
             Aws.Query.Pair ("DisableScaleIn", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("TargetValue", Double.to_query v.target_value))
       ; Aws.Util.option_map v.customized_metric_specification (fun f ->
             Aws.Query.Pair
               ("CustomizedMetricSpecification", CustomizedMetricSpecification.to_query f))
       ; Aws.Util.option_map v.predefined_metric_specification (fun f ->
             Aws.Query.Pair
               ("PredefinedMetricSpecification", PredefinedMetricSpecification.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.disable_scale_in (fun f ->
             "DisableScaleIn", Boolean.to_json f)
       ; Some ("TargetValue", Double.to_json v.target_value)
       ; Aws.Util.option_map v.customized_metric_specification (fun f ->
             "CustomizedMetricSpecification", CustomizedMetricSpecification.to_json f)
       ; Aws.Util.option_map v.predefined_metric_specification (fun f ->
             "PredefinedMetricSpecification", PredefinedMetricSpecification.to_json f)
       ])

let of_json j =
  { predefined_metric_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "PredefinedMetricSpecification")
        PredefinedMetricSpecification.of_json
  ; customized_metric_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "CustomizedMetricSpecification")
        CustomizedMetricSpecification.of_json
  ; target_value =
      Double.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetValue"))
  ; disable_scale_in =
      Aws.Util.option_map (Aws.Json.lookup j "DisableScaleIn") Boolean.of_json
  }
