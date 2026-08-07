type t = { account_level : InstanceMetadataDefaultsResponse.t option }

let make ?account_level () = { account_level }

let parse xml =
  Some
    { account_level =
        Aws.Util.option_bind
          (Aws.Xml.member "accountLevel" xml)
          InstanceMetadataDefaultsResponse.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.account_level (fun f ->
             Aws.Query.Pair ("AccountLevel", InstanceMetadataDefaultsResponse.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.account_level (fun f ->
             "accountLevel", InstanceMetadataDefaultsResponse.to_json f)
       ])

let of_json j =
  { account_level =
      Aws.Util.option_map
        (Aws.Json.lookup j "accountLevel")
        InstanceMetadataDefaultsResponse.of_json
  }
