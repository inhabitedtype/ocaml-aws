type t =
  { ipam_external_resource_verification_token :
      IpamExternalResourceVerificationToken.t option
  }

let make ?ipam_external_resource_verification_token () =
  { ipam_external_resource_verification_token }

let parse xml =
  Some
    { ipam_external_resource_verification_token =
        Aws.Util.option_bind
          (Aws.Xml.member "ipamExternalResourceVerificationToken" xml)
          IpamExternalResourceVerificationToken.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_external_resource_verification_token (fun f ->
             Aws.Query.Pair
               ( "IpamExternalResourceVerificationToken"
               , IpamExternalResourceVerificationToken.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_external_resource_verification_token (fun f ->
             ( "ipamExternalResourceVerificationToken"
             , IpamExternalResourceVerificationToken.to_json f ))
       ])

let of_json j =
  { ipam_external_resource_verification_token =
      Aws.Util.option_map
        (Aws.Json.lookup j "ipamExternalResourceVerificationToken")
        IpamExternalResourceVerificationToken.of_json
  }
