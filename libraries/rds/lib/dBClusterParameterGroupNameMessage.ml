open Aws.BaseTypes

type t = { d_b_cluster_parameter_group_name : String.t option }

let make ?d_b_cluster_parameter_group_name () = { d_b_cluster_parameter_group_name }

let parse xml =
  Some
    { d_b_cluster_parameter_group_name =
        Aws.Util.option_bind
          (Aws.Xml.member "DBClusterParameterGroupName" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_cluster_parameter_group_name (fun f ->
             Aws.Query.Pair ("DBClusterParameterGroupName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_cluster_parameter_group_name (fun f ->
             "DBClusterParameterGroupName", String.to_json f)
       ])

let of_json j =
  { d_b_cluster_parameter_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterParameterGroupName") String.of_json
  }
