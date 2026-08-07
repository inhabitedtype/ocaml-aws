type t = { type_configuration_identifiers : TypeConfigurationIdentifiers.t }

let make ~type_configuration_identifiers () = { type_configuration_identifiers }

let parse xml =
  Some
    { type_configuration_identifiers =
        Aws.Xml.required
          "TypeConfigurationIdentifiers"
          (Aws.Util.option_bind
             (Aws.Xml.member "TypeConfigurationIdentifiers" xml)
             TypeConfigurationIdentifiers.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "TypeConfigurationIdentifiers.member"
              , TypeConfigurationIdentifiers.to_query v.type_configuration_identifiers ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "TypeConfigurationIdentifiers"
           , TypeConfigurationIdentifiers.to_json v.type_configuration_identifiers )
       ])

let of_json j =
  { type_configuration_identifiers =
      TypeConfigurationIdentifiers.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TypeConfigurationIdentifiers"))
  }
