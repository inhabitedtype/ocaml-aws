open Aws.BaseTypes

type t =
  { verified_access_instance_id : String.t option
  ; description : String.t option
  ; verified_access_trust_providers : VerifiedAccessTrustProviderCondensedList.t
  ; creation_time : String.t option
  ; last_updated_time : String.t option
  ; tags : TagList.t
  ; fips_enabled : Boolean.t option
  ; cidr_endpoints_custom_sub_domain : VerifiedAccessInstanceCustomSubDomain.t option
  }

let make
    ?verified_access_instance_id
    ?description
    ?(verified_access_trust_providers = [])
    ?creation_time
    ?last_updated_time
    ?(tags = [])
    ?fips_enabled
    ?cidr_endpoints_custom_sub_domain
    () =
  { verified_access_instance_id
  ; description
  ; verified_access_trust_providers
  ; creation_time
  ; last_updated_time
  ; tags
  ; fips_enabled
  ; cidr_endpoints_custom_sub_domain
  }

let parse xml =
  Some
    { verified_access_instance_id =
        Aws.Util.option_bind (Aws.Xml.member "verifiedAccessInstanceId" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; verified_access_trust_providers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "verifiedAccessTrustProviderSet" xml)
             VerifiedAccessTrustProviderCondensedList.parse)
    ; creation_time =
        Aws.Util.option_bind (Aws.Xml.member "creationTime" xml) String.parse
    ; last_updated_time =
        Aws.Util.option_bind (Aws.Xml.member "lastUpdatedTime" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; fips_enabled = Aws.Util.option_bind (Aws.Xml.member "fipsEnabled" xml) Boolean.parse
    ; cidr_endpoints_custom_sub_domain =
        Aws.Util.option_bind
          (Aws.Xml.member "cidrEndpointsCustomSubDomain" xml)
          VerifiedAccessInstanceCustomSubDomain.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cidr_endpoints_custom_sub_domain (fun f ->
             Aws.Query.Pair
               ( "CidrEndpointsCustomSubDomain"
               , VerifiedAccessInstanceCustomSubDomain.to_query f ))
       ; Aws.Util.option_map v.fips_enabled (fun f ->
             Aws.Query.Pair ("FipsEnabled", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.last_updated_time (fun f ->
             Aws.Query.Pair ("LastUpdatedTime", String.to_query f))
       ; Aws.Util.option_map v.creation_time (fun f ->
             Aws.Query.Pair ("CreationTime", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "VerifiedAccessTrustProviderSet"
              , VerifiedAccessTrustProviderCondensedList.to_query
                  v.verified_access_trust_providers ))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.verified_access_instance_id (fun f ->
             Aws.Query.Pair ("VerifiedAccessInstanceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cidr_endpoints_custom_sub_domain (fun f ->
             ( "cidrEndpointsCustomSubDomain"
             , VerifiedAccessInstanceCustomSubDomain.to_json f ))
       ; Aws.Util.option_map v.fips_enabled (fun f -> "fipsEnabled", Boolean.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.last_updated_time (fun f ->
             "lastUpdatedTime", String.to_json f)
       ; Aws.Util.option_map v.creation_time (fun f -> "creationTime", String.to_json f)
       ; Some
           ( "verifiedAccessTrustProviderSet"
           , VerifiedAccessTrustProviderCondensedList.to_json
               v.verified_access_trust_providers )
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.verified_access_instance_id (fun f ->
             "verifiedAccessInstanceId", String.to_json f)
       ])

let of_json j =
  { verified_access_instance_id =
      Aws.Util.option_map (Aws.Json.lookup j "verifiedAccessInstanceId") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; verified_access_trust_providers =
      VerifiedAccessTrustProviderCondensedList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "verifiedAccessTrustProviderSet"))
  ; creation_time = Aws.Util.option_map (Aws.Json.lookup j "creationTime") String.of_json
  ; last_updated_time =
      Aws.Util.option_map (Aws.Json.lookup j "lastUpdatedTime") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; fips_enabled = Aws.Util.option_map (Aws.Json.lookup j "fipsEnabled") Boolean.of_json
  ; cidr_endpoints_custom_sub_domain =
      Aws.Util.option_map
        (Aws.Json.lookup j "cidrEndpointsCustomSubDomain")
        VerifiedAccessInstanceCustomSubDomain.of_json
  }
