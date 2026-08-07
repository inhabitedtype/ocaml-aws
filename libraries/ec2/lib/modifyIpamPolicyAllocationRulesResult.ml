type t = { ipam_policy_document : IpamPolicyDocument.t option }

let make ?ipam_policy_document () = { ipam_policy_document }

let parse xml =
  Some
    { ipam_policy_document =
        Aws.Util.option_bind
          (Aws.Xml.member "ipamPolicyDocument" xml)
          IpamPolicyDocument.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_policy_document (fun f ->
             Aws.Query.Pair ("IpamPolicyDocument", IpamPolicyDocument.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_policy_document (fun f ->
             "ipamPolicyDocument", IpamPolicyDocument.to_json f)
       ])

let of_json j =
  { ipam_policy_document =
      Aws.Util.option_map
        (Aws.Json.lookup j "ipamPolicyDocument")
        IpamPolicyDocument.of_json
  }
