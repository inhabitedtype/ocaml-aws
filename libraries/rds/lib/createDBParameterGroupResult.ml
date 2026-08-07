type t = { d_b_parameter_group : DBParameterGroup.t option }

let make ?d_b_parameter_group () = { d_b_parameter_group }

let parse xml =
  Some
    { d_b_parameter_group =
        Aws.Util.option_bind
          (Aws.Xml.member "DBParameterGroup" xml)
          DBParameterGroup.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_parameter_group (fun f ->
             Aws.Query.Pair ("DBParameterGroup", DBParameterGroup.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_parameter_group (fun f ->
             "DBParameterGroup", DBParameterGroup.to_json f)
       ])

let of_json j =
  { d_b_parameter_group =
      Aws.Util.option_map (Aws.Json.lookup j "DBParameterGroup") DBParameterGroup.of_json
  }
