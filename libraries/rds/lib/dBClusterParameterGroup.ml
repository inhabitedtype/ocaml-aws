open Aws.BaseTypes

type t =
  { d_b_cluster_parameter_group_name : String.t option
  ; d_b_parameter_group_family : String.t option
  ; description : String.t option
  ; d_b_cluster_parameter_group_arn : String.t option
  }

let make
    ?d_b_cluster_parameter_group_name
    ?d_b_parameter_group_family
    ?description
    ?d_b_cluster_parameter_group_arn
    () =
  { d_b_cluster_parameter_group_name
  ; d_b_parameter_group_family
  ; description
  ; d_b_cluster_parameter_group_arn
  }

let parse xml =
  Some
    { d_b_cluster_parameter_group_name =
        Aws.Util.option_bind
          (Aws.Xml.member "DBClusterParameterGroupName" xml)
          String.parse
    ; d_b_parameter_group_family =
        Aws.Util.option_bind (Aws.Xml.member "DBParameterGroupFamily" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; d_b_cluster_parameter_group_arn =
        Aws.Util.option_bind
          (Aws.Xml.member "DBClusterParameterGroupArn" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_cluster_parameter_group_arn (fun f ->
             Aws.Query.Pair ("DBClusterParameterGroupArn", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.d_b_parameter_group_family (fun f ->
             Aws.Query.Pair ("DBParameterGroupFamily", String.to_query f))
       ; Aws.Util.option_map v.d_b_cluster_parameter_group_name (fun f ->
             Aws.Query.Pair ("DBClusterParameterGroupName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_cluster_parameter_group_arn (fun f ->
             "DBClusterParameterGroupArn", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.d_b_parameter_group_family (fun f ->
             "DBParameterGroupFamily", String.to_json f)
       ; Aws.Util.option_map v.d_b_cluster_parameter_group_name (fun f ->
             "DBClusterParameterGroupName", String.to_json f)
       ])

let of_json j =
  { d_b_cluster_parameter_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterParameterGroupName") String.of_json
  ; d_b_parameter_group_family =
      Aws.Util.option_map (Aws.Json.lookup j "DBParameterGroupFamily") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; d_b_cluster_parameter_group_arn =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterParameterGroupArn") String.of_json
  }
