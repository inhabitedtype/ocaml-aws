type t = { key_metadata : KeyMetadata.t option }

let make ?key_metadata () = { key_metadata }

let parse xml =
  Some
    { key_metadata =
        Aws.Util.option_bind (Aws.Xml.member "KeyMetadata" xml) KeyMetadata.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key_metadata (fun f ->
             Aws.Query.Pair ("KeyMetadata", KeyMetadata.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key_metadata (fun f ->
             "KeyMetadata", KeyMetadata.to_json f)
       ])

let of_json j =
  { key_metadata =
      Aws.Util.option_map (Aws.Json.lookup j "KeyMetadata") KeyMetadata.of_json
  }
