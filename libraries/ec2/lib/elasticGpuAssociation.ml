open Aws.BaseTypes

type t =
  { elastic_gpu_id : String.t option
  ; elastic_gpu_association_id : String.t option
  ; elastic_gpu_association_state : String.t option
  ; elastic_gpu_association_time : String.t option
  }

let make
    ?elastic_gpu_id
    ?elastic_gpu_association_id
    ?elastic_gpu_association_state
    ?elastic_gpu_association_time
    () =
  { elastic_gpu_id
  ; elastic_gpu_association_id
  ; elastic_gpu_association_state
  ; elastic_gpu_association_time
  }

let parse xml =
  Some
    { elastic_gpu_id =
        Aws.Util.option_bind (Aws.Xml.member "elasticGpuId" xml) String.parse
    ; elastic_gpu_association_id =
        Aws.Util.option_bind (Aws.Xml.member "elasticGpuAssociationId" xml) String.parse
    ; elastic_gpu_association_state =
        Aws.Util.option_bind
          (Aws.Xml.member "elasticGpuAssociationState" xml)
          String.parse
    ; elastic_gpu_association_time =
        Aws.Util.option_bind (Aws.Xml.member "elasticGpuAssociationTime" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.elastic_gpu_association_time (fun f ->
             Aws.Query.Pair ("ElasticGpuAssociationTime", String.to_query f))
       ; Aws.Util.option_map v.elastic_gpu_association_state (fun f ->
             Aws.Query.Pair ("ElasticGpuAssociationState", String.to_query f))
       ; Aws.Util.option_map v.elastic_gpu_association_id (fun f ->
             Aws.Query.Pair ("ElasticGpuAssociationId", String.to_query f))
       ; Aws.Util.option_map v.elastic_gpu_id (fun f ->
             Aws.Query.Pair ("ElasticGpuId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.elastic_gpu_association_time (fun f ->
             "elasticGpuAssociationTime", String.to_json f)
       ; Aws.Util.option_map v.elastic_gpu_association_state (fun f ->
             "elasticGpuAssociationState", String.to_json f)
       ; Aws.Util.option_map v.elastic_gpu_association_id (fun f ->
             "elasticGpuAssociationId", String.to_json f)
       ; Aws.Util.option_map v.elastic_gpu_id (fun f -> "elasticGpuId", String.to_json f)
       ])

let of_json j =
  { elastic_gpu_id = Aws.Util.option_map (Aws.Json.lookup j "elasticGpuId") String.of_json
  ; elastic_gpu_association_id =
      Aws.Util.option_map (Aws.Json.lookup j "elasticGpuAssociationId") String.of_json
  ; elastic_gpu_association_state =
      Aws.Util.option_map (Aws.Json.lookup j "elasticGpuAssociationState") String.of_json
  ; elastic_gpu_association_time =
      Aws.Util.option_map (Aws.Json.lookup j "elasticGpuAssociationTime") String.of_json
  }
