type t = { supported_versions : NitroTpmSupportedVersionsList.t }

let make ?(supported_versions = []) () = { supported_versions }

let parse xml =
  Some
    { supported_versions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "supportedVersions" xml)
             NitroTpmSupportedVersionsList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "SupportedVersions"
              , NitroTpmSupportedVersionsList.to_query v.supported_versions ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "supportedVersions"
           , NitroTpmSupportedVersionsList.to_json v.supported_versions )
       ])

let of_json j =
  { supported_versions =
      NitroTpmSupportedVersionsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "supportedVersions"))
  }
