type t = { d_b_security_group : DBSecurityGroup.t option }

let make ?d_b_security_group () = { d_b_security_group }

let parse xml =
  Some
    { d_b_security_group =
        Aws.Util.option_bind (Aws.Xml.member "DBSecurityGroup" xml) DBSecurityGroup.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_security_group (fun f ->
             Aws.Query.Pair ("DBSecurityGroup", DBSecurityGroup.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_security_group (fun f ->
             "DBSecurityGroup", DBSecurityGroup.to_json f)
       ])

let of_json j =
  { d_b_security_group =
      Aws.Util.option_map (Aws.Json.lookup j "DBSecurityGroup") DBSecurityGroup.of_json
  }
