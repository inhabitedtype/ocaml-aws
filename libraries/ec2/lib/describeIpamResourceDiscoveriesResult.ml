open Aws.BaseTypes

type t =
  { ipam_resource_discoveries : IpamResourceDiscoverySet.t
  ; next_token : String.t option
  }

let make ?(ipam_resource_discoveries = []) ?next_token () =
  { ipam_resource_discoveries; next_token }

let parse xml =
  Some
    { ipam_resource_discoveries =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipamResourceDiscoverySet" xml)
             IpamResourceDiscoverySet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "IpamResourceDiscoverySet"
              , IpamResourceDiscoverySet.to_query v.ipam_resource_discoveries ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "ipamResourceDiscoverySet"
           , IpamResourceDiscoverySet.to_json v.ipam_resource_discoveries )
       ])

let of_json j =
  { ipam_resource_discoveries =
      IpamResourceDiscoverySet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipamResourceDiscoverySet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
