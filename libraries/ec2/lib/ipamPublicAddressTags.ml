type t = { eip_tags : IpamPublicAddressTagList.t }

let make ?(eip_tags = []) () = { eip_tags }

let parse xml =
  Some
    { eip_tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "eipTagSet" xml)
             IpamPublicAddressTagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("EipTagSet", IpamPublicAddressTagList.to_query v.eip_tags))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("eipTagSet", IpamPublicAddressTagList.to_json v.eip_tags) ])

let of_json j =
  { eip_tags =
      IpamPublicAddressTagList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "eipTagSet"))
  }
