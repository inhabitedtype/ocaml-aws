open Aws.BaseTypes

type t =
  { ipam_resource_discovery_associations : IpamResourceDiscoveryAssociationSet.t
  ; next_token : String.t option
  }

let make ?(ipam_resource_discovery_associations = []) ?next_token () =
  { ipam_resource_discovery_associations; next_token }

let parse xml =
  Some
    { ipam_resource_discovery_associations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipamResourceDiscoveryAssociationSet" xml)
             IpamResourceDiscoveryAssociationSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "IpamResourceDiscoveryAssociationSet"
              , IpamResourceDiscoveryAssociationSet.to_query
                  v.ipam_resource_discovery_associations ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "ipamResourceDiscoveryAssociationSet"
           , IpamResourceDiscoveryAssociationSet.to_json
               v.ipam_resource_discovery_associations )
       ])

let of_json j =
  { ipam_resource_discovery_associations =
      IpamResourceDiscoveryAssociationSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipamResourceDiscoveryAssociationSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
