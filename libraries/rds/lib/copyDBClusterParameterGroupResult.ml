type t = { d_b_cluster_parameter_group : DBClusterParameterGroup.t option }

let make ?d_b_cluster_parameter_group () = { d_b_cluster_parameter_group }

let parse xml =
  Some
    { d_b_cluster_parameter_group =
        Aws.Util.option_bind
          (Aws.Xml.member "DBClusterParameterGroup" xml)
          DBClusterParameterGroup.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_cluster_parameter_group (fun f ->
             Aws.Query.Pair ("DBClusterParameterGroup", DBClusterParameterGroup.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_cluster_parameter_group (fun f ->
             "DBClusterParameterGroup", DBClusterParameterGroup.to_json f)
       ])

let of_json j =
  { d_b_cluster_parameter_group =
      Aws.Util.option_map
        (Aws.Json.lookup j "DBClusterParameterGroup")
        DBClusterParameterGroup.of_json
  }
