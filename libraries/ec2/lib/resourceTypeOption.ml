type t =
  { option_name : ImageReferenceOptionName.t option
  ; option_values : ResourceTypeOptionValuesList.t
  }

let make ?option_name ?(option_values = []) () = { option_name; option_values }

let parse xml =
  Some
    { option_name =
        Aws.Util.option_bind
          (Aws.Xml.member "OptionName" xml)
          ImageReferenceOptionName.parse
    ; option_values =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "OptionValue" xml)
             ResourceTypeOptionValuesList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("OptionValue", ResourceTypeOptionValuesList.to_query v.option_values))
       ; Aws.Util.option_map v.option_name (fun f ->
             Aws.Query.Pair ("OptionName", ImageReferenceOptionName.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("OptionValue", ResourceTypeOptionValuesList.to_json v.option_values)
       ; Aws.Util.option_map v.option_name (fun f ->
             "OptionName", ImageReferenceOptionName.to_json f)
       ])

let of_json j =
  { option_name =
      Aws.Util.option_map
        (Aws.Json.lookup j "OptionName")
        ImageReferenceOptionName.of_json
  ; option_values =
      ResourceTypeOptionValuesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "OptionValue"))
  }
