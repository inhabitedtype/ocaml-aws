type t = { verified_access_endpoint : VerifiedAccessEndpoint.t option }

let make ?verified_access_endpoint () = { verified_access_endpoint }

let parse xml =
  Some
    { verified_access_endpoint =
        Aws.Util.option_bind
          (Aws.Xml.member "verifiedAccessEndpoint" xml)
          VerifiedAccessEndpoint.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.verified_access_endpoint (fun f ->
             Aws.Query.Pair ("VerifiedAccessEndpoint", VerifiedAccessEndpoint.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.verified_access_endpoint (fun f ->
             "verifiedAccessEndpoint", VerifiedAccessEndpoint.to_json f)
       ])

let of_json j =
  { verified_access_endpoint =
      Aws.Util.option_map
        (Aws.Json.lookup j "verifiedAccessEndpoint")
        VerifiedAccessEndpoint.of_json
  }
