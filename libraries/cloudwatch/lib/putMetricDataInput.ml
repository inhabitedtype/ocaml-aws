open Aws.BaseTypes

type t =
  { namespace : String.t
  ; metric_data : MetricData.t
  ; entity_metric_data : EntityMetricDataList.t
  ; strict_entity_validation : Boolean.t option
  }

let make
    ~namespace
    ?(metric_data = [])
    ?(entity_metric_data = [])
    ?strict_entity_validation
    () =
  { namespace; metric_data; entity_metric_data; strict_entity_validation }

let parse xml =
  Some
    { namespace =
        Aws.Xml.required
          "Namespace"
          (Aws.Util.option_bind (Aws.Xml.member "Namespace" xml) String.parse)
    ; metric_data =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "MetricData" xml) MetricData.parse)
    ; entity_metric_data =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "EntityMetricData" xml)
             EntityMetricDataList.parse)
    ; strict_entity_validation =
        Aws.Util.option_bind (Aws.Xml.member "StrictEntityValidation" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.strict_entity_validation (fun f ->
             Aws.Query.Pair ("StrictEntityValidation", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "EntityMetricData.member"
              , EntityMetricDataList.to_query v.entity_metric_data ))
       ; Some (Aws.Query.Pair ("MetricData.member", MetricData.to_query v.metric_data))
       ; Some (Aws.Query.Pair ("Namespace", String.to_query v.namespace))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.strict_entity_validation (fun f ->
             "StrictEntityValidation", Boolean.to_json f)
       ; Some ("EntityMetricData", EntityMetricDataList.to_json v.entity_metric_data)
       ; Some ("MetricData", MetricData.to_json v.metric_data)
       ; Some ("Namespace", String.to_json v.namespace)
       ])

let of_json j =
  { namespace = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Namespace"))
  ; metric_data =
      MetricData.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricData"))
  ; entity_metric_data =
      EntityMetricDataList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "EntityMetricData"))
  ; strict_entity_validation =
      Aws.Util.option_map (Aws.Json.lookup j "StrictEntityValidation") Boolean.of_json
  }
