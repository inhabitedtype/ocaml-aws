type t = { prefix_list : ManagedPrefixList.t option }

let make ?prefix_list () = { prefix_list }

let parse xml =
  Some
    { prefix_list =
        Aws.Util.option_bind (Aws.Xml.member "prefixList" xml) ManagedPrefixList.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.prefix_list (fun f ->
             Aws.Query.Pair ("PrefixList", ManagedPrefixList.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.prefix_list (fun f ->
             "prefixList", ManagedPrefixList.to_json f)
       ])

let of_json j =
  { prefix_list =
      Aws.Util.option_map (Aws.Json.lookup j "prefixList") ManagedPrefixList.of_json
  }
