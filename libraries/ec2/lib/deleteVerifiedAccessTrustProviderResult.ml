type t = { verified_access_trust_provider : VerifiedAccessTrustProvider.t option }

let make ?verified_access_trust_provider () = { verified_access_trust_provider }

let parse xml =
  Some
    { verified_access_trust_provider =
        Aws.Util.option_bind
          (Aws.Xml.member "verifiedAccessTrustProvider" xml)
          VerifiedAccessTrustProvider.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.verified_access_trust_provider (fun f ->
             Aws.Query.Pair
               ("VerifiedAccessTrustProvider", VerifiedAccessTrustProvider.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.verified_access_trust_provider (fun f ->
             "verifiedAccessTrustProvider", VerifiedAccessTrustProvider.to_json f)
       ])

let of_json j =
  { verified_access_trust_provider =
      Aws.Util.option_map
        (Aws.Json.lookup j "verifiedAccessTrustProvider")
        VerifiedAccessTrustProvider.of_json
  }
