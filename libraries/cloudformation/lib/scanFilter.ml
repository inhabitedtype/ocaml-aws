type t = { types : ResourceTypeFilters.t }

let make ?(types = []) () = { types }

let parse xml =
  Some
    { types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Types" xml) ResourceTypeFilters.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Types.member", ResourceTypeFilters.to_query v.types)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("Types", ResourceTypeFilters.to_json v.types) ])

let of_json j =
  { types =
      ResourceTypeFilters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Types"))
  }
