type t = { resource_statement : ResourceStatement.t option }

let make ?resource_statement () = { resource_statement }

let parse xml =
  Some
    { resource_statement =
        Aws.Util.option_bind
          (Aws.Xml.member "resourceStatement" xml)
          ResourceStatement.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_statement (fun f ->
             Aws.Query.Pair ("ResourceStatement", ResourceStatement.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_statement (fun f ->
             "resourceStatement", ResourceStatement.to_json f)
       ])

let of_json j =
  { resource_statement =
      Aws.Util.option_map
        (Aws.Json.lookup j "resourceStatement")
        ResourceStatement.of_json
  }
