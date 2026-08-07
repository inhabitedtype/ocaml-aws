open Aws.BaseTypes

type t =
  { metric_name : String.t
  ; namespace : String.t
  ; statistic : Statistic.t option
  ; extended_statistic : String.t option
  ; dimensions : Dimensions.t
  ; period : Integer.t option
  ; unit : StandardUnit.t option
  }

let make
    ~metric_name
    ~namespace
    ?statistic
    ?extended_statistic
    ?(dimensions = [])
    ?period
    ?unit
    () =
  { metric_name; namespace; statistic; extended_statistic; dimensions; period; unit }

let parse xml =
  Some
    { metric_name =
        Aws.Xml.required
          "MetricName"
          (Aws.Util.option_bind (Aws.Xml.member "MetricName" xml) String.parse)
    ; namespace =
        Aws.Xml.required
          "Namespace"
          (Aws.Util.option_bind (Aws.Xml.member "Namespace" xml) String.parse)
    ; statistic = Aws.Util.option_bind (Aws.Xml.member "Statistic" xml) Statistic.parse
    ; extended_statistic =
        Aws.Util.option_bind (Aws.Xml.member "ExtendedStatistic" xml) String.parse
    ; dimensions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Dimensions" xml) Dimensions.parse)
    ; period = Aws.Util.option_bind (Aws.Xml.member "Period" xml) Integer.parse
    ; unit = Aws.Util.option_bind (Aws.Xml.member "Unit" xml) StandardUnit.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.unit (fun f ->
             Aws.Query.Pair ("Unit", StandardUnit.to_query f))
       ; Aws.Util.option_map v.period (fun f ->
             Aws.Query.Pair ("Period", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Dimensions.member", Dimensions.to_query v.dimensions))
       ; Aws.Util.option_map v.extended_statistic (fun f ->
             Aws.Query.Pair ("ExtendedStatistic", String.to_query f))
       ; Aws.Util.option_map v.statistic (fun f ->
             Aws.Query.Pair ("Statistic", Statistic.to_query f))
       ; Some (Aws.Query.Pair ("Namespace", String.to_query v.namespace))
       ; Some (Aws.Query.Pair ("MetricName", String.to_query v.metric_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.unit (fun f -> "Unit", StandardUnit.to_json f)
       ; Aws.Util.option_map v.period (fun f -> "Period", Integer.to_json f)
       ; Some ("Dimensions", Dimensions.to_json v.dimensions)
       ; Aws.Util.option_map v.extended_statistic (fun f ->
             "ExtendedStatistic", String.to_json f)
       ; Aws.Util.option_map v.statistic (fun f -> "Statistic", Statistic.to_json f)
       ; Some ("Namespace", String.to_json v.namespace)
       ; Some ("MetricName", String.to_json v.metric_name)
       ])

let of_json j =
  { metric_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricName"))
  ; namespace = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Namespace"))
  ; statistic = Aws.Util.option_map (Aws.Json.lookup j "Statistic") Statistic.of_json
  ; extended_statistic =
      Aws.Util.option_map (Aws.Json.lookup j "ExtendedStatistic") String.of_json
  ; dimensions =
      Dimensions.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Dimensions"))
  ; period = Aws.Util.option_map (Aws.Json.lookup j "Period") Integer.of_json
  ; unit = Aws.Util.option_map (Aws.Json.lookup j "Unit") StandardUnit.of_json
  }
