type t = { verified_access_group : VerifiedAccessGroup.t option }

let make ?verified_access_group () = { verified_access_group }

let parse xml =
  Some
    { verified_access_group =
        Aws.Util.option_bind
          (Aws.Xml.member "verifiedAccessGroup" xml)
          VerifiedAccessGroup.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.verified_access_group (fun f ->
             Aws.Query.Pair ("VerifiedAccessGroup", VerifiedAccessGroup.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.verified_access_group (fun f ->
             "verifiedAccessGroup", VerifiedAccessGroup.to_json f)
       ])

let of_json j =
  { verified_access_group =
      Aws.Util.option_map
        (Aws.Json.lookup j "verifiedAccessGroup")
        VerifiedAccessGroup.of_json
  }
