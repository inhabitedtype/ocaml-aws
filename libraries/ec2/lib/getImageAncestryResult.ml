type t = { image_ancestry_entries : ImageAncestryEntryList.t }

let make ?(image_ancestry_entries = []) () = { image_ancestry_entries }

let parse xml =
  Some
    { image_ancestry_entries =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "imageAncestryEntrySet" xml)
             ImageAncestryEntryList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ImageAncestryEntrySet"
              , ImageAncestryEntryList.to_query v.image_ancestry_entries ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "imageAncestryEntrySet"
           , ImageAncestryEntryList.to_json v.image_ancestry_entries )
       ])

let of_json j =
  { image_ancestry_entries =
      ImageAncestryEntryList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "imageAncestryEntrySet"))
  }
