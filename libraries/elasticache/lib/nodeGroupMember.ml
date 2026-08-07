open Aws.BaseTypes

type t =
  { cache_cluster_id : String.t option
  ; cache_node_id : String.t option
  ; read_endpoint : Endpoint.t option
  ; preferred_availability_zone : String.t option
  ; preferred_outpost_arn : String.t option
  ; current_role : String.t option
  }

let make
    ?cache_cluster_id
    ?cache_node_id
    ?read_endpoint
    ?preferred_availability_zone
    ?preferred_outpost_arn
    ?current_role
    () =
  { cache_cluster_id
  ; cache_node_id
  ; read_endpoint
  ; preferred_availability_zone
  ; preferred_outpost_arn
  ; current_role
  }

let parse xml =
  Some
    { cache_cluster_id =
        Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse
    ; cache_node_id = Aws.Util.option_bind (Aws.Xml.member "CacheNodeId" xml) String.parse
    ; read_endpoint =
        Aws.Util.option_bind (Aws.Xml.member "ReadEndpoint" xml) Endpoint.parse
    ; preferred_availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "PreferredAvailabilityZone" xml) String.parse
    ; preferred_outpost_arn =
        Aws.Util.option_bind (Aws.Xml.member "PreferredOutpostArn" xml) String.parse
    ; current_role = Aws.Util.option_bind (Aws.Xml.member "CurrentRole" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.current_role (fun f ->
             Aws.Query.Pair ("CurrentRole", String.to_query f))
       ; Aws.Util.option_map v.preferred_outpost_arn (fun f ->
             Aws.Query.Pair ("PreferredOutpostArn", String.to_query f))
       ; Aws.Util.option_map v.preferred_availability_zone (fun f ->
             Aws.Query.Pair ("PreferredAvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.read_endpoint (fun f ->
             Aws.Query.Pair ("ReadEndpoint", Endpoint.to_query f))
       ; Aws.Util.option_map v.cache_node_id (fun f ->
             Aws.Query.Pair ("CacheNodeId", String.to_query f))
       ; Aws.Util.option_map v.cache_cluster_id (fun f ->
             Aws.Query.Pair ("CacheClusterId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.current_role (fun f -> "CurrentRole", String.to_json f)
       ; Aws.Util.option_map v.preferred_outpost_arn (fun f ->
             "PreferredOutpostArn", String.to_json f)
       ; Aws.Util.option_map v.preferred_availability_zone (fun f ->
             "PreferredAvailabilityZone", String.to_json f)
       ; Aws.Util.option_map v.read_endpoint (fun f -> "ReadEndpoint", Endpoint.to_json f)
       ; Aws.Util.option_map v.cache_node_id (fun f -> "CacheNodeId", String.to_json f)
       ; Aws.Util.option_map v.cache_cluster_id (fun f ->
             "CacheClusterId", String.to_json f)
       ])

let of_json j =
  { cache_cluster_id =
      Aws.Util.option_map (Aws.Json.lookup j "CacheClusterId") String.of_json
  ; cache_node_id = Aws.Util.option_map (Aws.Json.lookup j "CacheNodeId") String.of_json
  ; read_endpoint =
      Aws.Util.option_map (Aws.Json.lookup j "ReadEndpoint") Endpoint.of_json
  ; preferred_availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "PreferredAvailabilityZone") String.of_json
  ; preferred_outpost_arn =
      Aws.Util.option_map (Aws.Json.lookup j "PreferredOutpostArn") String.of_json
  ; current_role = Aws.Util.option_map (Aws.Json.lookup j "CurrentRole") String.of_json
  }
