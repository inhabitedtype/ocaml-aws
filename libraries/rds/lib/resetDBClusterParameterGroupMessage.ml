open Aws.BaseTypes

type t =
  { d_b_cluster_parameter_group_name : String.t
  ; reset_all_parameters : Boolean.t option
  ; parameters : ParametersList.t
  }

let make ~d_b_cluster_parameter_group_name ?reset_all_parameters ?(parameters = []) () =
  { d_b_cluster_parameter_group_name; reset_all_parameters; parameters }

let parse xml =
  Some
    { d_b_cluster_parameter_group_name =
        Aws.Xml.required
          "DBClusterParameterGroupName"
          (Aws.Util.option_bind
             (Aws.Xml.member "DBClusterParameterGroupName" xml)
             String.parse)
    ; reset_all_parameters =
        Aws.Util.option_bind (Aws.Xml.member "ResetAllParameters" xml) Boolean.parse
    ; parameters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Parameters" xml) ParametersList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Parameters.member", ParametersList.to_query v.parameters))
       ; Aws.Util.option_map v.reset_all_parameters (fun f ->
             Aws.Query.Pair ("ResetAllParameters", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "DBClusterParameterGroupName"
              , String.to_query v.d_b_cluster_parameter_group_name ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Parameters", ParametersList.to_json v.parameters)
       ; Aws.Util.option_map v.reset_all_parameters (fun f ->
             "ResetAllParameters", Boolean.to_json f)
       ; Some
           ( "DBClusterParameterGroupName"
           , String.to_json v.d_b_cluster_parameter_group_name )
       ])

let of_json j =
  { d_b_cluster_parameter_group_name =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DBClusterParameterGroupName"))
  ; reset_all_parameters =
      Aws.Util.option_map (Aws.Json.lookup j "ResetAllParameters") Boolean.of_json
  ; parameters =
      ParametersList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
  }
