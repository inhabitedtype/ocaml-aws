type t = { vpc_block_public_access_exclusion : VpcBlockPublicAccessExclusion.t option }

let make ?vpc_block_public_access_exclusion () = { vpc_block_public_access_exclusion }

let parse xml =
  Some
    { vpc_block_public_access_exclusion =
        Aws.Util.option_bind
          (Aws.Xml.member "vpcBlockPublicAccessExclusion" xml)
          VpcBlockPublicAccessExclusion.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_block_public_access_exclusion (fun f ->
             Aws.Query.Pair
               ("VpcBlockPublicAccessExclusion", VpcBlockPublicAccessExclusion.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_block_public_access_exclusion (fun f ->
             "vpcBlockPublicAccessExclusion", VpcBlockPublicAccessExclusion.to_json f)
       ])

let of_json j =
  { vpc_block_public_access_exclusion =
      Aws.Util.option_map
        (Aws.Json.lookup j "vpcBlockPublicAccessExclusion")
        VpcBlockPublicAccessExclusion.of_json
  }
