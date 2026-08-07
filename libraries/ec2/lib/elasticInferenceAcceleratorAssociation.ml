open Aws.BaseTypes

type t =
  { elastic_inference_accelerator_arn : String.t option
  ; elastic_inference_accelerator_association_id : String.t option
  ; elastic_inference_accelerator_association_state : String.t option
  ; elastic_inference_accelerator_association_time : DateTime.t option
  }

let make
    ?elastic_inference_accelerator_arn
    ?elastic_inference_accelerator_association_id
    ?elastic_inference_accelerator_association_state
    ?elastic_inference_accelerator_association_time
    () =
  { elastic_inference_accelerator_arn
  ; elastic_inference_accelerator_association_id
  ; elastic_inference_accelerator_association_state
  ; elastic_inference_accelerator_association_time
  }

let parse xml =
  Some
    { elastic_inference_accelerator_arn =
        Aws.Util.option_bind
          (Aws.Xml.member "elasticInferenceAcceleratorArn" xml)
          String.parse
    ; elastic_inference_accelerator_association_id =
        Aws.Util.option_bind
          (Aws.Xml.member "elasticInferenceAcceleratorAssociationId" xml)
          String.parse
    ; elastic_inference_accelerator_association_state =
        Aws.Util.option_bind
          (Aws.Xml.member "elasticInferenceAcceleratorAssociationState" xml)
          String.parse
    ; elastic_inference_accelerator_association_time =
        Aws.Util.option_bind
          (Aws.Xml.member "elasticInferenceAcceleratorAssociationTime" xml)
          DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.elastic_inference_accelerator_association_time (fun f ->
             Aws.Query.Pair
               ("ElasticInferenceAcceleratorAssociationTime", DateTime.to_query f))
       ; Aws.Util.option_map v.elastic_inference_accelerator_association_state (fun f ->
             Aws.Query.Pair
               ("ElasticInferenceAcceleratorAssociationState", String.to_query f))
       ; Aws.Util.option_map v.elastic_inference_accelerator_association_id (fun f ->
             Aws.Query.Pair ("ElasticInferenceAcceleratorAssociationId", String.to_query f))
       ; Aws.Util.option_map v.elastic_inference_accelerator_arn (fun f ->
             Aws.Query.Pair ("ElasticInferenceAcceleratorArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.elastic_inference_accelerator_association_time (fun f ->
             "elasticInferenceAcceleratorAssociationTime", DateTime.to_json f)
       ; Aws.Util.option_map v.elastic_inference_accelerator_association_state (fun f ->
             "elasticInferenceAcceleratorAssociationState", String.to_json f)
       ; Aws.Util.option_map v.elastic_inference_accelerator_association_id (fun f ->
             "elasticInferenceAcceleratorAssociationId", String.to_json f)
       ; Aws.Util.option_map v.elastic_inference_accelerator_arn (fun f ->
             "elasticInferenceAcceleratorArn", String.to_json f)
       ])

let of_json j =
  { elastic_inference_accelerator_arn =
      Aws.Util.option_map
        (Aws.Json.lookup j "elasticInferenceAcceleratorArn")
        String.of_json
  ; elastic_inference_accelerator_association_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "elasticInferenceAcceleratorAssociationId")
        String.of_json
  ; elastic_inference_accelerator_association_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "elasticInferenceAcceleratorAssociationState")
        String.of_json
  ; elastic_inference_accelerator_association_time =
      Aws.Util.option_map
        (Aws.Json.lookup j "elasticInferenceAcceleratorAssociationTime")
        DateTime.of_json
  }
