type t = { option_group : OptionGroup.t option }

let make ?option_group () = { option_group }

let parse xml =
  Some
    { option_group =
        Aws.Util.option_bind (Aws.Xml.member "OptionGroup" xml) OptionGroup.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.option_group (fun f ->
             Aws.Query.Pair ("OptionGroup", OptionGroup.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.option_group (fun f ->
             "OptionGroup", OptionGroup.to_json f)
       ])

let of_json j =
  { option_group =
      Aws.Util.option_map (Aws.Json.lookup j "OptionGroup") OptionGroup.of_json
  }
