open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; ipam_external_resource_verification_tokens :
      IpamExternalResourceVerificationTokenSet.t
  }

let make ?next_token ?(ipam_external_resource_verification_tokens = []) () =
  { next_token; ipam_external_resource_verification_tokens }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; ipam_external_resource_verification_tokens =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipamExternalResourceVerificationTokenSet" xml)
             IpamExternalResourceVerificationTokenSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "IpamExternalResourceVerificationTokenSet"
              , IpamExternalResourceVerificationTokenSet.to_query
                  v.ipam_external_resource_verification_tokens ))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "ipamExternalResourceVerificationTokenSet"
           , IpamExternalResourceVerificationTokenSet.to_json
               v.ipam_external_resource_verification_tokens )
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; ipam_external_resource_verification_tokens =
      IpamExternalResourceVerificationTokenSet.of_json
        (Aws.Util.of_option_exn
           (Aws.Json.lookup j "ipamExternalResourceVerificationTokenSet"))
  }
