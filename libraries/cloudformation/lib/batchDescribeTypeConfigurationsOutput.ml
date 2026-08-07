type t =
  { errors : BatchDescribeTypeConfigurationsErrors.t
  ; unprocessed_type_configurations : UnprocessedTypeConfigurations.t
  ; type_configurations : TypeConfigurationDetailsList.t
  }

let make
    ?(errors = [])
    ?(unprocessed_type_configurations = [])
    ?(type_configurations = [])
    () =
  { errors; unprocessed_type_configurations; type_configurations }

let parse xml =
  Some
    { errors =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Errors" xml)
             BatchDescribeTypeConfigurationsErrors.parse)
    ; unprocessed_type_configurations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "UnprocessedTypeConfigurations" xml)
             UnprocessedTypeConfigurations.parse)
    ; type_configurations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TypeConfigurations" xml)
             TypeConfigurationDetailsList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "TypeConfigurations.member"
              , TypeConfigurationDetailsList.to_query v.type_configurations ))
       ; Some
           (Aws.Query.Pair
              ( "UnprocessedTypeConfigurations.member"
              , UnprocessedTypeConfigurations.to_query v.unprocessed_type_configurations
              ))
       ; Some
           (Aws.Query.Pair
              ("Errors.member", BatchDescribeTypeConfigurationsErrors.to_query v.errors))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "TypeConfigurations"
           , TypeConfigurationDetailsList.to_json v.type_configurations )
       ; Some
           ( "UnprocessedTypeConfigurations"
           , UnprocessedTypeConfigurations.to_json v.unprocessed_type_configurations )
       ; Some ("Errors", BatchDescribeTypeConfigurationsErrors.to_json v.errors)
       ])

let of_json j =
  { errors =
      BatchDescribeTypeConfigurationsErrors.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Errors"))
  ; unprocessed_type_configurations =
      UnprocessedTypeConfigurations.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "UnprocessedTypeConfigurations"))
  ; type_configurations =
      TypeConfigurationDetailsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TypeConfigurations"))
  }
