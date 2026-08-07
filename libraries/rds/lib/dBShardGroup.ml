open Aws.BaseTypes

type t =
  { d_b_shard_group_resource_id : String.t option
  ; d_b_shard_group_identifier : String.t option
  ; d_b_cluster_identifier : String.t option
  ; max_a_c_u : Double.t option
  ; min_a_c_u : Double.t option
  ; compute_redundancy : Integer.t option
  ; status : String.t option
  ; publicly_accessible : Boolean.t option
  ; endpoint : String.t option
  ; d_b_shard_group_arn : String.t option
  ; tag_list : TagList.t
  }

let make
    ?d_b_shard_group_resource_id
    ?d_b_shard_group_identifier
    ?d_b_cluster_identifier
    ?max_a_c_u
    ?min_a_c_u
    ?compute_redundancy
    ?status
    ?publicly_accessible
    ?endpoint
    ?d_b_shard_group_arn
    ?(tag_list = [])
    () =
  { d_b_shard_group_resource_id
  ; d_b_shard_group_identifier
  ; d_b_cluster_identifier
  ; max_a_c_u
  ; min_a_c_u
  ; compute_redundancy
  ; status
  ; publicly_accessible
  ; endpoint
  ; d_b_shard_group_arn
  ; tag_list
  }

let parse xml =
  Some
    { d_b_shard_group_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "DBShardGroupResourceId" xml) String.parse
    ; d_b_shard_group_identifier =
        Aws.Util.option_bind (Aws.Xml.member "DBShardGroupIdentifier" xml) String.parse
    ; d_b_cluster_identifier =
        Aws.Util.option_bind (Aws.Xml.member "DBClusterIdentifier" xml) String.parse
    ; max_a_c_u = Aws.Util.option_bind (Aws.Xml.member "MaxACU" xml) Double.parse
    ; min_a_c_u = Aws.Util.option_bind (Aws.Xml.member "MinACU" xml) Double.parse
    ; compute_redundancy =
        Aws.Util.option_bind (Aws.Xml.member "ComputeRedundancy" xml) Integer.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; publicly_accessible =
        Aws.Util.option_bind (Aws.Xml.member "PubliclyAccessible" xml) Boolean.parse
    ; endpoint = Aws.Util.option_bind (Aws.Xml.member "Endpoint" xml) String.parse
    ; d_b_shard_group_arn =
        Aws.Util.option_bind (Aws.Xml.member "DBShardGroupArn" xml) String.parse
    ; tag_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "TagList" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagList.member", TagList.to_query v.tag_list))
       ; Aws.Util.option_map v.d_b_shard_group_arn (fun f ->
             Aws.Query.Pair ("DBShardGroupArn", String.to_query f))
       ; Aws.Util.option_map v.endpoint (fun f ->
             Aws.Query.Pair ("Endpoint", String.to_query f))
       ; Aws.Util.option_map v.publicly_accessible (fun f ->
             Aws.Query.Pair ("PubliclyAccessible", Boolean.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.compute_redundancy (fun f ->
             Aws.Query.Pair ("ComputeRedundancy", Integer.to_query f))
       ; Aws.Util.option_map v.min_a_c_u (fun f ->
             Aws.Query.Pair ("MinACU", Double.to_query f))
       ; Aws.Util.option_map v.max_a_c_u (fun f ->
             Aws.Query.Pair ("MaxACU", Double.to_query f))
       ; Aws.Util.option_map v.d_b_cluster_identifier (fun f ->
             Aws.Query.Pair ("DBClusterIdentifier", String.to_query f))
       ; Aws.Util.option_map v.d_b_shard_group_identifier (fun f ->
             Aws.Query.Pair ("DBShardGroupIdentifier", String.to_query f))
       ; Aws.Util.option_map v.d_b_shard_group_resource_id (fun f ->
             Aws.Query.Pair ("DBShardGroupResourceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagList", TagList.to_json v.tag_list)
       ; Aws.Util.option_map v.d_b_shard_group_arn (fun f ->
             "DBShardGroupArn", String.to_json f)
       ; Aws.Util.option_map v.endpoint (fun f -> "Endpoint", String.to_json f)
       ; Aws.Util.option_map v.publicly_accessible (fun f ->
             "PubliclyAccessible", Boolean.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.compute_redundancy (fun f ->
             "ComputeRedundancy", Integer.to_json f)
       ; Aws.Util.option_map v.min_a_c_u (fun f -> "MinACU", Double.to_json f)
       ; Aws.Util.option_map v.max_a_c_u (fun f -> "MaxACU", Double.to_json f)
       ; Aws.Util.option_map v.d_b_cluster_identifier (fun f ->
             "DBClusterIdentifier", String.to_json f)
       ; Aws.Util.option_map v.d_b_shard_group_identifier (fun f ->
             "DBShardGroupIdentifier", String.to_json f)
       ; Aws.Util.option_map v.d_b_shard_group_resource_id (fun f ->
             "DBShardGroupResourceId", String.to_json f)
       ])

let of_json j =
  { d_b_shard_group_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "DBShardGroupResourceId") String.of_json
  ; d_b_shard_group_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBShardGroupIdentifier") String.of_json
  ; d_b_cluster_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterIdentifier") String.of_json
  ; max_a_c_u = Aws.Util.option_map (Aws.Json.lookup j "MaxACU") Double.of_json
  ; min_a_c_u = Aws.Util.option_map (Aws.Json.lookup j "MinACU") Double.of_json
  ; compute_redundancy =
      Aws.Util.option_map (Aws.Json.lookup j "ComputeRedundancy") Integer.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; publicly_accessible =
      Aws.Util.option_map (Aws.Json.lookup j "PubliclyAccessible") Boolean.of_json
  ; endpoint = Aws.Util.option_map (Aws.Json.lookup j "Endpoint") String.of_json
  ; d_b_shard_group_arn =
      Aws.Util.option_map (Aws.Json.lookup j "DBShardGroupArn") String.of_json
  ; tag_list = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagList"))
  }
