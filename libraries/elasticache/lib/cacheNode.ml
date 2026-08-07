open Aws.BaseTypes

type t =
  { cache_node_id : String.t option
  ; cache_node_status : String.t option
  ; cache_node_create_time : DateTime.t option
  ; endpoint : Endpoint.t option
  ; parameter_group_status : String.t option
  ; source_cache_node_id : String.t option
  ; customer_availability_zone : String.t option
  ; customer_outpost_arn : String.t option
  }

let make
    ?cache_node_id
    ?cache_node_status
    ?cache_node_create_time
    ?endpoint
    ?parameter_group_status
    ?source_cache_node_id
    ?customer_availability_zone
    ?customer_outpost_arn
    () =
  { cache_node_id
  ; cache_node_status
  ; cache_node_create_time
  ; endpoint
  ; parameter_group_status
  ; source_cache_node_id
  ; customer_availability_zone
  ; customer_outpost_arn
  }

let parse xml =
  Some
    { cache_node_id = Aws.Util.option_bind (Aws.Xml.member "CacheNodeId" xml) String.parse
    ; cache_node_status =
        Aws.Util.option_bind (Aws.Xml.member "CacheNodeStatus" xml) String.parse
    ; cache_node_create_time =
        Aws.Util.option_bind (Aws.Xml.member "CacheNodeCreateTime" xml) DateTime.parse
    ; endpoint = Aws.Util.option_bind (Aws.Xml.member "Endpoint" xml) Endpoint.parse
    ; parameter_group_status =
        Aws.Util.option_bind (Aws.Xml.member "ParameterGroupStatus" xml) String.parse
    ; source_cache_node_id =
        Aws.Util.option_bind (Aws.Xml.member "SourceCacheNodeId" xml) String.parse
    ; customer_availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "CustomerAvailabilityZone" xml) String.parse
    ; customer_outpost_arn =
        Aws.Util.option_bind (Aws.Xml.member "CustomerOutpostArn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.customer_outpost_arn (fun f ->
             Aws.Query.Pair ("CustomerOutpostArn", String.to_query f))
       ; Aws.Util.option_map v.customer_availability_zone (fun f ->
             Aws.Query.Pair ("CustomerAvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.source_cache_node_id (fun f ->
             Aws.Query.Pair ("SourceCacheNodeId", String.to_query f))
       ; Aws.Util.option_map v.parameter_group_status (fun f ->
             Aws.Query.Pair ("ParameterGroupStatus", String.to_query f))
       ; Aws.Util.option_map v.endpoint (fun f ->
             Aws.Query.Pair ("Endpoint", Endpoint.to_query f))
       ; Aws.Util.option_map v.cache_node_create_time (fun f ->
             Aws.Query.Pair ("CacheNodeCreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.cache_node_status (fun f ->
             Aws.Query.Pair ("CacheNodeStatus", String.to_query f))
       ; Aws.Util.option_map v.cache_node_id (fun f ->
             Aws.Query.Pair ("CacheNodeId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.customer_outpost_arn (fun f ->
             "CustomerOutpostArn", String.to_json f)
       ; Aws.Util.option_map v.customer_availability_zone (fun f ->
             "CustomerAvailabilityZone", String.to_json f)
       ; Aws.Util.option_map v.source_cache_node_id (fun f ->
             "SourceCacheNodeId", String.to_json f)
       ; Aws.Util.option_map v.parameter_group_status (fun f ->
             "ParameterGroupStatus", String.to_json f)
       ; Aws.Util.option_map v.endpoint (fun f -> "Endpoint", Endpoint.to_json f)
       ; Aws.Util.option_map v.cache_node_create_time (fun f ->
             "CacheNodeCreateTime", DateTime.to_json f)
       ; Aws.Util.option_map v.cache_node_status (fun f ->
             "CacheNodeStatus", String.to_json f)
       ; Aws.Util.option_map v.cache_node_id (fun f -> "CacheNodeId", String.to_json f)
       ])

let of_json j =
  { cache_node_id = Aws.Util.option_map (Aws.Json.lookup j "CacheNodeId") String.of_json
  ; cache_node_status =
      Aws.Util.option_map (Aws.Json.lookup j "CacheNodeStatus") String.of_json
  ; cache_node_create_time =
      Aws.Util.option_map (Aws.Json.lookup j "CacheNodeCreateTime") DateTime.of_json
  ; endpoint = Aws.Util.option_map (Aws.Json.lookup j "Endpoint") Endpoint.of_json
  ; parameter_group_status =
      Aws.Util.option_map (Aws.Json.lookup j "ParameterGroupStatus") String.of_json
  ; source_cache_node_id =
      Aws.Util.option_map (Aws.Json.lookup j "SourceCacheNodeId") String.of_json
  ; customer_availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "CustomerAvailabilityZone") String.of_json
  ; customer_outpost_arn =
      Aws.Util.option_map (Aws.Json.lookup j "CustomerOutpostArn") String.of_json
  }
