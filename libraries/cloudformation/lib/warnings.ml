type t = { unrecognized_resource_types : ResourceTypes.t }

let make ?(unrecognized_resource_types = []) () = { unrecognized_resource_types }

let parse xml =
  Some
    { unrecognized_resource_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "UnrecognizedResourceTypes" xml)
             ResourceTypes.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "UnrecognizedResourceTypes.member"
              , ResourceTypes.to_query v.unrecognized_resource_types ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "UnrecognizedResourceTypes"
           , ResourceTypes.to_json v.unrecognized_resource_types )
       ])

let of_json j =
  { unrecognized_resource_types =
      ResourceTypes.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "UnrecognizedResourceTypes"))
  }
