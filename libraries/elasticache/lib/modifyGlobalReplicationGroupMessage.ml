open Aws.BaseTypes

type t =
  { global_replication_group_id : String.t
  ; apply_immediately : Boolean.t
  ; cache_node_type : String.t option
  ; engine : String.t option
  ; engine_version : String.t option
  ; cache_parameter_group_name : String.t option
  ; global_replication_group_description : String.t option
  ; automatic_failover_enabled : Boolean.t option
  }

let make
    ~global_replication_group_id
    ~apply_immediately
    ?cache_node_type
    ?engine
    ?engine_version
    ?cache_parameter_group_name
    ?global_replication_group_description
    ?automatic_failover_enabled
    () =
  { global_replication_group_id
  ; apply_immediately
  ; cache_node_type
  ; engine
  ; engine_version
  ; cache_parameter_group_name
  ; global_replication_group_description
  ; automatic_failover_enabled
  }

let parse xml =
  Some
    { global_replication_group_id =
        Aws.Xml.required
          "GlobalReplicationGroupId"
          (Aws.Util.option_bind
             (Aws.Xml.member "GlobalReplicationGroupId" xml)
             String.parse)
    ; apply_immediately =
        Aws.Xml.required
          "ApplyImmediately"
          (Aws.Util.option_bind (Aws.Xml.member "ApplyImmediately" xml) Boolean.parse)
    ; cache_node_type =
        Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; cache_parameter_group_name =
        Aws.Util.option_bind (Aws.Xml.member "CacheParameterGroupName" xml) String.parse
    ; global_replication_group_description =
        Aws.Util.option_bind
          (Aws.Xml.member "GlobalReplicationGroupDescription" xml)
          String.parse
    ; automatic_failover_enabled =
        Aws.Util.option_bind (Aws.Xml.member "AutomaticFailoverEnabled" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.automatic_failover_enabled (fun f ->
             Aws.Query.Pair ("AutomaticFailoverEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.global_replication_group_description (fun f ->
             Aws.Query.Pair ("GlobalReplicationGroupDescription", String.to_query f))
       ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
             Aws.Query.Pair ("CacheParameterGroupName", String.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Aws.Util.option_map v.cache_node_type (fun f ->
             Aws.Query.Pair ("CacheNodeType", String.to_query f))
       ; Some (Aws.Query.Pair ("ApplyImmediately", Boolean.to_query v.apply_immediately))
       ; Some
           (Aws.Query.Pair
              ("GlobalReplicationGroupId", String.to_query v.global_replication_group_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.automatic_failover_enabled (fun f ->
             "AutomaticFailoverEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.global_replication_group_description (fun f ->
             "GlobalReplicationGroupDescription", String.to_json f)
       ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
             "CacheParameterGroupName", String.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Aws.Util.option_map v.cache_node_type (fun f ->
             "CacheNodeType", String.to_json f)
       ; Some ("ApplyImmediately", Boolean.to_json v.apply_immediately)
       ; Some ("GlobalReplicationGroupId", String.to_json v.global_replication_group_id)
       ])

let of_json j =
  { global_replication_group_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalReplicationGroupId"))
  ; apply_immediately =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ApplyImmediately"))
  ; cache_node_type =
      Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; cache_parameter_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "CacheParameterGroupName") String.of_json
  ; global_replication_group_description =
      Aws.Util.option_map
        (Aws.Json.lookup j "GlobalReplicationGroupDescription")
        String.of_json
  ; automatic_failover_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "AutomaticFailoverEnabled") Boolean.of_json
  }
