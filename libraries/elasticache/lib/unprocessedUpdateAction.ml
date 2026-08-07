open Aws.BaseTypes

type t =
  { replication_group_id : String.t option
  ; cache_cluster_id : String.t option
  ; service_update_name : String.t option
  ; error_type : String.t option
  ; error_message : String.t option
  }

let make
    ?replication_group_id
    ?cache_cluster_id
    ?service_update_name
    ?error_type
    ?error_message
    () =
  { replication_group_id
  ; cache_cluster_id
  ; service_update_name
  ; error_type
  ; error_message
  }

let parse xml =
  Some
    { replication_group_id =
        Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse
    ; cache_cluster_id =
        Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse
    ; service_update_name =
        Aws.Util.option_bind (Aws.Xml.member "ServiceUpdateName" xml) String.parse
    ; error_type = Aws.Util.option_bind (Aws.Xml.member "ErrorType" xml) String.parse
    ; error_message =
        Aws.Util.option_bind (Aws.Xml.member "ErrorMessage" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.error_message (fun f ->
             Aws.Query.Pair ("ErrorMessage", String.to_query f))
       ; Aws.Util.option_map v.error_type (fun f ->
             Aws.Query.Pair ("ErrorType", String.to_query f))
       ; Aws.Util.option_map v.service_update_name (fun f ->
             Aws.Query.Pair ("ServiceUpdateName", String.to_query f))
       ; Aws.Util.option_map v.cache_cluster_id (fun f ->
             Aws.Query.Pair ("CacheClusterId", String.to_query f))
       ; Aws.Util.option_map v.replication_group_id (fun f ->
             Aws.Query.Pair ("ReplicationGroupId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.error_message (fun f -> "ErrorMessage", String.to_json f)
       ; Aws.Util.option_map v.error_type (fun f -> "ErrorType", String.to_json f)
       ; Aws.Util.option_map v.service_update_name (fun f ->
             "ServiceUpdateName", String.to_json f)
       ; Aws.Util.option_map v.cache_cluster_id (fun f ->
             "CacheClusterId", String.to_json f)
       ; Aws.Util.option_map v.replication_group_id (fun f ->
             "ReplicationGroupId", String.to_json f)
       ])

let of_json j =
  { replication_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroupId") String.of_json
  ; cache_cluster_id =
      Aws.Util.option_map (Aws.Json.lookup j "CacheClusterId") String.of_json
  ; service_update_name =
      Aws.Util.option_map (Aws.Json.lookup j "ServiceUpdateName") String.of_json
  ; error_type = Aws.Util.option_map (Aws.Json.lookup j "ErrorType") String.of_json
  ; error_message = Aws.Util.option_map (Aws.Json.lookup j "ErrorMessage") String.of_json
  }
