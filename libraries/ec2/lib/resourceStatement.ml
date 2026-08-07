type t =
  { resources : ValueStringList.t
  ; resource_types : ValueStringList.t
  }

let make ?(resources = []) ?(resource_types = []) () = { resources; resource_types }

let parse xml =
  Some
    { resources =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "resourceSet" xml) ValueStringList.parse)
    ; resource_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "resourceTypeSet" xml)
             ValueStringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("ResourceTypeSet", ValueStringList.to_query v.resource_types))
       ; Some (Aws.Query.Pair ("ResourceSet", ValueStringList.to_query v.resources))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("resourceTypeSet", ValueStringList.to_json v.resource_types)
       ; Some ("resourceSet", ValueStringList.to_json v.resources)
       ])

let of_json j =
  { resources =
      ValueStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "resourceSet"))
  ; resource_types =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "resourceTypeSet"))
  }
