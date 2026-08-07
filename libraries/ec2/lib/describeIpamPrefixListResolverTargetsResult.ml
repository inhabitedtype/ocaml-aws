open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; ipam_prefix_list_resolver_targets : IpamPrefixListResolverTargetSet.t
  }

let make ?next_token ?(ipam_prefix_list_resolver_targets = []) () =
  { next_token; ipam_prefix_list_resolver_targets }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; ipam_prefix_list_resolver_targets =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipamPrefixListResolverTargetSet" xml)
             IpamPrefixListResolverTargetSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "IpamPrefixListResolverTargetSet"
              , IpamPrefixListResolverTargetSet.to_query
                  v.ipam_prefix_list_resolver_targets ))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "ipamPrefixListResolverTargetSet"
           , IpamPrefixListResolverTargetSet.to_json v.ipam_prefix_list_resolver_targets
           )
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; ipam_prefix_list_resolver_targets =
      IpamPrefixListResolverTargetSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipamPrefixListResolverTargetSet"))
  }
