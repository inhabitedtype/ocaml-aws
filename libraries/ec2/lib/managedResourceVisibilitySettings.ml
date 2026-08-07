type t = { default_visibility : ManagedResourceDefaultVisibility.t option }

let make ?default_visibility () = { default_visibility }

let parse xml =
  Some
    { default_visibility =
        Aws.Util.option_bind
          (Aws.Xml.member "defaultVisibility" xml)
          ManagedResourceDefaultVisibility.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.default_visibility (fun f ->
             Aws.Query.Pair
               ("DefaultVisibility", ManagedResourceDefaultVisibility.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.default_visibility (fun f ->
             "defaultVisibility", ManagedResourceDefaultVisibility.to_json f)
       ])

let of_json j =
  { default_visibility =
      Aws.Util.option_map
        (Aws.Json.lookup j "defaultVisibility")
        ManagedResourceDefaultVisibility.of_json
  }
