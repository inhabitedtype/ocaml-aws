type t = { vpcs : VpcClassicLinkList.t }

let make ?(vpcs = []) () = { vpcs }

let parse xml =
  Some
    { vpcs =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "vpcSet" xml) VpcClassicLinkList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("VpcSet", VpcClassicLinkList.to_query v.vpcs)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("vpcSet", VpcClassicLinkList.to_json v.vpcs) ])

let of_json j =
  { vpcs =
      VpcClassicLinkList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "vpcSet"))
  }
