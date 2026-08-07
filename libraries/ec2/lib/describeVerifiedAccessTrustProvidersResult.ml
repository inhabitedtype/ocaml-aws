open Aws.BaseTypes

type t =
  { verified_access_trust_providers : VerifiedAccessTrustProviderList.t
  ; next_token : String.t option
  }

let make ?(verified_access_trust_providers = []) ?next_token () =
  { verified_access_trust_providers; next_token }

let parse xml =
  Some
    { verified_access_trust_providers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "verifiedAccessTrustProviderSet" xml)
             VerifiedAccessTrustProviderList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "VerifiedAccessTrustProviderSet"
              , VerifiedAccessTrustProviderList.to_query v.verified_access_trust_providers
              ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "verifiedAccessTrustProviderSet"
           , VerifiedAccessTrustProviderList.to_json v.verified_access_trust_providers )
       ])

let of_json j =
  { verified_access_trust_providers =
      VerifiedAccessTrustProviderList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "verifiedAccessTrustProviderSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
