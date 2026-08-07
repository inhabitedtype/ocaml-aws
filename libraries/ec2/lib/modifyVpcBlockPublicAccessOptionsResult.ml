type t = { vpc_block_public_access_options : VpcBlockPublicAccessOptions.t option }

let make ?vpc_block_public_access_options () = { vpc_block_public_access_options }

let parse xml =
  Some
    { vpc_block_public_access_options =
        Aws.Util.option_bind
          (Aws.Xml.member "vpcBlockPublicAccessOptions" xml)
          VpcBlockPublicAccessOptions.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_block_public_access_options (fun f ->
             Aws.Query.Pair
               ("VpcBlockPublicAccessOptions", VpcBlockPublicAccessOptions.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_block_public_access_options (fun f ->
             "vpcBlockPublicAccessOptions", VpcBlockPublicAccessOptions.to_json f)
       ])

let of_json j =
  { vpc_block_public_access_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "vpcBlockPublicAccessOptions")
        VpcBlockPublicAccessOptions.of_json
  }
