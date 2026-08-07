open Aws.BaseTypes

type t =
  { d_b_parameter_group_name : String.t option
  ; parameter_apply_status : String.t option
  }

let make ?d_b_parameter_group_name ?parameter_apply_status () =
  { d_b_parameter_group_name; parameter_apply_status }

let parse xml =
  Some
    { d_b_parameter_group_name =
        Aws.Util.option_bind (Aws.Xml.member "DBParameterGroupName" xml) String.parse
    ; parameter_apply_status =
        Aws.Util.option_bind (Aws.Xml.member "ParameterApplyStatus" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.parameter_apply_status (fun f ->
             Aws.Query.Pair ("ParameterApplyStatus", String.to_query f))
       ; Aws.Util.option_map v.d_b_parameter_group_name (fun f ->
             Aws.Query.Pair ("DBParameterGroupName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.parameter_apply_status (fun f ->
             "ParameterApplyStatus", String.to_json f)
       ; Aws.Util.option_map v.d_b_parameter_group_name (fun f ->
             "DBParameterGroupName", String.to_json f)
       ])

let of_json j =
  { d_b_parameter_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "DBParameterGroupName") String.of_json
  ; parameter_apply_status =
      Aws.Util.option_map (Aws.Json.lookup j "ParameterApplyStatus") String.of_json
  }
