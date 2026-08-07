type t =
  { type_ : WarningType.t option
  ; properties : WarningProperties.t
  }

let make ?type_ ?(properties = []) () = { type_; properties }

let parse xml =
  Some
    { type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) WarningType.parse
    ; properties =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Properties" xml) WarningProperties.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("Properties.member", WarningProperties.to_query v.properties))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", WarningType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Properties", WarningProperties.to_json v.properties)
       ; Aws.Util.option_map v.type_ (fun f -> "Type", WarningType.to_json f)
       ])

let of_json j =
  { type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") WarningType.of_json
  ; properties =
      WarningProperties.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Properties"))
  }
