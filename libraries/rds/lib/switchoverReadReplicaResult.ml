type t = { d_b_instance : DBInstance.t option }

let make ?d_b_instance () = { d_b_instance }

let parse xml =
  Some
    { d_b_instance =
        Aws.Util.option_bind (Aws.Xml.member "DBInstance" xml) DBInstance.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_instance (fun f ->
             Aws.Query.Pair ("DBInstance", DBInstance.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_instance (fun f -> "DBInstance", DBInstance.to_json f)
       ])

let of_json j =
  { d_b_instance = Aws.Util.option_map (Aws.Json.lookup j "DBInstance") DBInstance.of_json
  }
