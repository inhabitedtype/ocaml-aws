open Aws.BaseTypes

type t = { license_configuration_arn : String.t option }

let make ?license_configuration_arn () = { license_configuration_arn }

let parse xml =
  Some
    { license_configuration_arn =
        Aws.Util.option_bind (Aws.Xml.member "licenseConfigurationArn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.license_configuration_arn (fun f ->
             Aws.Query.Pair ("LicenseConfigurationArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.license_configuration_arn (fun f ->
             "licenseConfigurationArn", String.to_json f)
       ])

let of_json j =
  { license_configuration_arn =
      Aws.Util.option_map (Aws.Json.lookup j "licenseConfigurationArn") String.of_json
  }
