type t = { athena_integrations : AthenaIntegrationsSet.t }

let make ?(athena_integrations = []) () = { athena_integrations }

let parse xml =
  Some
    { athena_integrations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AthenaIntegration" xml)
             AthenaIntegrationsSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("AthenaIntegration", AthenaIntegrationsSet.to_query v.athena_integrations))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("AthenaIntegration", AthenaIntegrationsSet.to_json v.athena_integrations) ])

let of_json j =
  { athena_integrations =
      AthenaIntegrationsSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AthenaIntegration"))
  }
