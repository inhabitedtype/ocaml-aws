open Aws.BaseTypes

type t =
  { d_b_shard_group_identifier : String.t
  ; d_b_cluster_identifier : String.t
  ; compute_redundancy : Integer.t option
  ; max_a_c_u : Double.t
  ; min_a_c_u : Double.t option
  ; publicly_accessible : Boolean.t option
  ; tags : TagList.t
  }

let make
    ~d_b_shard_group_identifier
    ~d_b_cluster_identifier
    ?compute_redundancy
    ~max_a_c_u
    ?min_a_c_u
    ?publicly_accessible
    ?(tags = [])
    () =
  { d_b_shard_group_identifier
  ; d_b_cluster_identifier
  ; compute_redundancy
  ; max_a_c_u
  ; min_a_c_u
  ; publicly_accessible
  ; tags
  }

let parse xml =
  Some
    { d_b_shard_group_identifier =
        Aws.Xml.required
          "DBShardGroupIdentifier"
          (Aws.Util.option_bind
             (Aws.Xml.member "DBShardGroupIdentifier" xml)
             String.parse)
    ; d_b_cluster_identifier =
        Aws.Xml.required
          "DBClusterIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DBClusterIdentifier" xml) String.parse)
    ; compute_redundancy =
        Aws.Util.option_bind (Aws.Xml.member "ComputeRedundancy" xml) Integer.parse
    ; max_a_c_u =
        Aws.Xml.required
          "MaxACU"
          (Aws.Util.option_bind (Aws.Xml.member "MaxACU" xml) Double.parse)
    ; min_a_c_u = Aws.Util.option_bind (Aws.Xml.member "MinACU" xml) Double.parse
    ; publicly_accessible =
        Aws.Util.option_bind (Aws.Xml.member "PubliclyAccessible" xml) Boolean.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Aws.Util.option_map v.publicly_accessible (fun f ->
             Aws.Query.Pair ("PubliclyAccessible", Boolean.to_query f))
       ; Aws.Util.option_map v.min_a_c_u (fun f ->
             Aws.Query.Pair ("MinACU", Double.to_query f))
       ; Some (Aws.Query.Pair ("MaxACU", Double.to_query v.max_a_c_u))
       ; Aws.Util.option_map v.compute_redundancy (fun f ->
             Aws.Query.Pair ("ComputeRedundancy", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
       ; Some
           (Aws.Query.Pair
              ("DBShardGroupIdentifier", String.to_query v.d_b_shard_group_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.publicly_accessible (fun f ->
             "PubliclyAccessible", Boolean.to_json f)
       ; Aws.Util.option_map v.min_a_c_u (fun f -> "MinACU", Double.to_json f)
       ; Some ("MaxACU", Double.to_json v.max_a_c_u)
       ; Aws.Util.option_map v.compute_redundancy (fun f ->
             "ComputeRedundancy", Integer.to_json f)
       ; Some ("DBClusterIdentifier", String.to_json v.d_b_cluster_identifier)
       ; Some ("DBShardGroupIdentifier", String.to_json v.d_b_shard_group_identifier)
       ])

let of_json j =
  { d_b_shard_group_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBShardGroupIdentifier"))
  ; d_b_cluster_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBClusterIdentifier"))
  ; compute_redundancy =
      Aws.Util.option_map (Aws.Json.lookup j "ComputeRedundancy") Integer.of_json
  ; max_a_c_u = Double.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxACU"))
  ; min_a_c_u = Aws.Util.option_map (Aws.Json.lookup j "MinACU") Double.of_json
  ; publicly_accessible =
      Aws.Util.option_map (Aws.Json.lookup j "PubliclyAccessible") Boolean.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
