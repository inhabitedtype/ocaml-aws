open Aws.BaseTypes

type t =
  { d_b_instance_identifier : String.t option
  ; is_cluster_writer : Boolean.t option
  ; d_b_cluster_parameter_group_status : String.t option
  ; promotion_tier : Integer.t option
  }

let make
    ?d_b_instance_identifier
    ?is_cluster_writer
    ?d_b_cluster_parameter_group_status
    ?promotion_tier
    () =
  { d_b_instance_identifier
  ; is_cluster_writer
  ; d_b_cluster_parameter_group_status
  ; promotion_tier
  }

let parse xml =
  Some
    { d_b_instance_identifier =
        Aws.Util.option_bind (Aws.Xml.member "DBInstanceIdentifier" xml) String.parse
    ; is_cluster_writer =
        Aws.Util.option_bind (Aws.Xml.member "IsClusterWriter" xml) Boolean.parse
    ; d_b_cluster_parameter_group_status =
        Aws.Util.option_bind
          (Aws.Xml.member "DBClusterParameterGroupStatus" xml)
          String.parse
    ; promotion_tier =
        Aws.Util.option_bind (Aws.Xml.member "PromotionTier" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.promotion_tier (fun f ->
             Aws.Query.Pair ("PromotionTier", Integer.to_query f))
       ; Aws.Util.option_map v.d_b_cluster_parameter_group_status (fun f ->
             Aws.Query.Pair ("DBClusterParameterGroupStatus", String.to_query f))
       ; Aws.Util.option_map v.is_cluster_writer (fun f ->
             Aws.Query.Pair ("IsClusterWriter", Boolean.to_query f))
       ; Aws.Util.option_map v.d_b_instance_identifier (fun f ->
             Aws.Query.Pair ("DBInstanceIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.promotion_tier (fun f ->
             "PromotionTier", Integer.to_json f)
       ; Aws.Util.option_map v.d_b_cluster_parameter_group_status (fun f ->
             "DBClusterParameterGroupStatus", String.to_json f)
       ; Aws.Util.option_map v.is_cluster_writer (fun f ->
             "IsClusterWriter", Boolean.to_json f)
       ; Aws.Util.option_map v.d_b_instance_identifier (fun f ->
             "DBInstanceIdentifier", String.to_json f)
       ])

let of_json j =
  { d_b_instance_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBInstanceIdentifier") String.of_json
  ; is_cluster_writer =
      Aws.Util.option_map (Aws.Json.lookup j "IsClusterWriter") Boolean.of_json
  ; d_b_cluster_parameter_group_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "DBClusterParameterGroupStatus")
        String.of_json
  ; promotion_tier =
      Aws.Util.option_map (Aws.Json.lookup j "PromotionTier") Integer.of_json
  }
