open Aws.BaseTypes

type t =
  { ipam_prefix_list_resolver_versions : IpamPrefixListResolverVersionSet.t
  ; next_token : String.t option
  }

let make ?(ipam_prefix_list_resolver_versions = []) ?next_token () =
  { ipam_prefix_list_resolver_versions; next_token }

let parse xml =
  Some
    { ipam_prefix_list_resolver_versions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipamPrefixListResolverVersionSet" xml)
             IpamPrefixListResolverVersionSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "IpamPrefixListResolverVersionSet"
              , IpamPrefixListResolverVersionSet.to_query
                  v.ipam_prefix_list_resolver_versions ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "ipamPrefixListResolverVersionSet"
           , IpamPrefixListResolverVersionSet.to_json v.ipam_prefix_list_resolver_versions
           )
       ])

let of_json j =
  { ipam_prefix_list_resolver_versions =
      IpamPrefixListResolverVersionSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipamPrefixListResolverVersionSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
