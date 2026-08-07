type t = { verified_access_instance : VerifiedAccessInstance.t option }

let make ?verified_access_instance () = { verified_access_instance }

let parse xml =
  Some
    { verified_access_instance =
        Aws.Util.option_bind
          (Aws.Xml.member "verifiedAccessInstance" xml)
          VerifiedAccessInstance.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.verified_access_instance (fun f ->
             Aws.Query.Pair ("VerifiedAccessInstance", VerifiedAccessInstance.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.verified_access_instance (fun f ->
             "verifiedAccessInstance", VerifiedAccessInstance.to_json f)
       ])

let of_json j =
  { verified_access_instance =
      Aws.Util.option_map
        (Aws.Json.lookup j "verifiedAccessInstance")
        VerifiedAccessInstance.of_json
  }
