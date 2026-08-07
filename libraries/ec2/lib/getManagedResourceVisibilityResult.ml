type t = { visibility : ManagedResourceVisibilitySettings.t option }

let make ?visibility () = { visibility }

let parse xml =
  Some
    { visibility =
        Aws.Util.option_bind
          (Aws.Xml.member "visibility" xml)
          ManagedResourceVisibilitySettings.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.visibility (fun f ->
             Aws.Query.Pair ("Visibility", ManagedResourceVisibilitySettings.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.visibility (fun f ->
             "visibility", ManagedResourceVisibilitySettings.to_json f)
       ])

let of_json j =
  { visibility =
      Aws.Util.option_map
        (Aws.Json.lookup j "visibility")
        ManagedResourceVisibilitySettings.of_json
  }
