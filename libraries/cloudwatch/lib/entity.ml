type t =
  { key_attributes : EntityKeyAttributesMap.t option
  ; attributes : EntityAttributesMap.t option
  }

let make ?key_attributes ?attributes () = { key_attributes; attributes }

let parse xml =
  Some
    { key_attributes =
        Aws.Util.option_bind
          (Aws.Xml.member "KeyAttributes" xml)
          EntityKeyAttributesMap.parse
    ; attributes =
        Aws.Util.option_bind (Aws.Xml.member "Attributes" xml) EntityAttributesMap.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.attributes (fun f ->
             Aws.Query.Pair ("Attributes", EntityAttributesMap.to_query f))
       ; Aws.Util.option_map v.key_attributes (fun f ->
             Aws.Query.Pair ("KeyAttributes", EntityKeyAttributesMap.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.attributes (fun f ->
             "Attributes", EntityAttributesMap.to_json f)
       ; Aws.Util.option_map v.key_attributes (fun f ->
             "KeyAttributes", EntityKeyAttributesMap.to_json f)
       ])

let of_json j =
  { key_attributes =
      Aws.Util.option_map
        (Aws.Json.lookup j "KeyAttributes")
        EntityKeyAttributesMap.of_json
  ; attributes =
      Aws.Util.option_map (Aws.Json.lookup j "Attributes") EntityAttributesMap.of_json
  }
