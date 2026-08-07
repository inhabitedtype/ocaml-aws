open Aws.BaseTypes

type t =
  { ipam_discovered_accounts : IpamDiscoveredAccountSet.t
  ; next_token : String.t option
  }

let make ?(ipam_discovered_accounts = []) ?next_token () =
  { ipam_discovered_accounts; next_token }

let parse xml =
  Some
    { ipam_discovered_accounts =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipamDiscoveredAccountSet" xml)
             IpamDiscoveredAccountSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "IpamDiscoveredAccountSet"
              , IpamDiscoveredAccountSet.to_query v.ipam_discovered_accounts ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "ipamDiscoveredAccountSet"
           , IpamDiscoveredAccountSet.to_json v.ipam_discovered_accounts )
       ])

let of_json j =
  { ipam_discovered_accounts =
      IpamDiscoveredAccountSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipamDiscoveredAccountSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
