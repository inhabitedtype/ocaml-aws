type t = { d_b_subnet_group : DBSubnetGroup.t option }

let make ?d_b_subnet_group () = { d_b_subnet_group }

let parse xml =
  Some
    { d_b_subnet_group =
        Aws.Util.option_bind (Aws.Xml.member "DBSubnetGroup" xml) DBSubnetGroup.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_subnet_group (fun f ->
             Aws.Query.Pair ("DBSubnetGroup", DBSubnetGroup.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_subnet_group (fun f ->
             "DBSubnetGroup", DBSubnetGroup.to_json f)
       ])

let of_json j =
  { d_b_subnet_group =
      Aws.Util.option_map (Aws.Json.lookup j "DBSubnetGroup") DBSubnetGroup.of_json
  }
