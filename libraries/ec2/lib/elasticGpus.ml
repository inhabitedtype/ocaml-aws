open Aws.BaseTypes

type t =
  { elastic_gpu_id : String.t option
  ; availability_zone : String.t option
  ; elastic_gpu_type : String.t option
  ; elastic_gpu_health : ElasticGpuHealth.t option
  ; elastic_gpu_state : ElasticGpuState.t option
  ; instance_id : String.t option
  ; tags : TagList.t
  }

let make
    ?elastic_gpu_id
    ?availability_zone
    ?elastic_gpu_type
    ?elastic_gpu_health
    ?elastic_gpu_state
    ?instance_id
    ?(tags = [])
    () =
  { elastic_gpu_id
  ; availability_zone
  ; elastic_gpu_type
  ; elastic_gpu_health
  ; elastic_gpu_state
  ; instance_id
  ; tags
  }

let parse xml =
  Some
    { elastic_gpu_id =
        Aws.Util.option_bind (Aws.Xml.member "elasticGpuId" xml) String.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; elastic_gpu_type =
        Aws.Util.option_bind (Aws.Xml.member "elasticGpuType" xml) String.parse
    ; elastic_gpu_health =
        Aws.Util.option_bind
          (Aws.Xml.member "elasticGpuHealth" xml)
          ElasticGpuHealth.parse
    ; elastic_gpu_state =
        Aws.Util.option_bind (Aws.Xml.member "elasticGpuState" xml) ElasticGpuState.parse
    ; instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ; Aws.Util.option_map v.elastic_gpu_state (fun f ->
             Aws.Query.Pair ("ElasticGpuState", ElasticGpuState.to_query f))
       ; Aws.Util.option_map v.elastic_gpu_health (fun f ->
             Aws.Query.Pair ("ElasticGpuHealth", ElasticGpuHealth.to_query f))
       ; Aws.Util.option_map v.elastic_gpu_type (fun f ->
             Aws.Query.Pair ("ElasticGpuType", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.elastic_gpu_id (fun f ->
             Aws.Query.Pair ("ElasticGpuId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ; Aws.Util.option_map v.elastic_gpu_state (fun f ->
             "elasticGpuState", ElasticGpuState.to_json f)
       ; Aws.Util.option_map v.elastic_gpu_health (fun f ->
             "elasticGpuHealth", ElasticGpuHealth.to_json f)
       ; Aws.Util.option_map v.elastic_gpu_type (fun f ->
             "elasticGpuType", String.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.elastic_gpu_id (fun f -> "elasticGpuId", String.to_json f)
       ])

let of_json j =
  { elastic_gpu_id = Aws.Util.option_map (Aws.Json.lookup j "elasticGpuId") String.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; elastic_gpu_type =
      Aws.Util.option_map (Aws.Json.lookup j "elasticGpuType") String.of_json
  ; elastic_gpu_health =
      Aws.Util.option_map (Aws.Json.lookup j "elasticGpuHealth") ElasticGpuHealth.of_json
  ; elastic_gpu_state =
      Aws.Util.option_map (Aws.Json.lookup j "elasticGpuState") ElasticGpuState.of_json
  ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
