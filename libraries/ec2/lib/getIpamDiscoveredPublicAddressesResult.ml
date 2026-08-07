open Aws.BaseTypes

type t =
  { ipam_discovered_public_addresses : IpamDiscoveredPublicAddressSet.t
  ; oldest_sample_time : DateTime.t option
  ; next_token : String.t option
  }

let make ?(ipam_discovered_public_addresses = []) ?oldest_sample_time ?next_token () =
  { ipam_discovered_public_addresses; oldest_sample_time; next_token }

let parse xml =
  Some
    { ipam_discovered_public_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipamDiscoveredPublicAddressSet" xml)
             IpamDiscoveredPublicAddressSet.parse)
    ; oldest_sample_time =
        Aws.Util.option_bind (Aws.Xml.member "oldestSampleTime" xml) DateTime.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.oldest_sample_time (fun f ->
             Aws.Query.Pair ("OldestSampleTime", DateTime.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "IpamDiscoveredPublicAddressSet"
              , IpamDiscoveredPublicAddressSet.to_query v.ipam_discovered_public_addresses
              ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Aws.Util.option_map v.oldest_sample_time (fun f ->
             "oldestSampleTime", DateTime.to_json f)
       ; Some
           ( "ipamDiscoveredPublicAddressSet"
           , IpamDiscoveredPublicAddressSet.to_json v.ipam_discovered_public_addresses )
       ])

let of_json j =
  { ipam_discovered_public_addresses =
      IpamDiscoveredPublicAddressSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipamDiscoveredPublicAddressSet"))
  ; oldest_sample_time =
      Aws.Util.option_map (Aws.Json.lookup j "oldestSampleTime") DateTime.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
