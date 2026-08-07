open Aws.BaseTypes

type t =
  { public_ipv4_pools : PublicIpv4PoolSet.t
  ; next_token : String.t option
  }

let make ?(public_ipv4_pools = []) ?next_token () = { public_ipv4_pools; next_token }

let parse xml =
  Some
    { public_ipv4_pools =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "publicIpv4PoolSet" xml)
             PublicIpv4PoolSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("PublicIpv4PoolSet", PublicIpv4PoolSet.to_query v.public_ipv4_pools))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("publicIpv4PoolSet", PublicIpv4PoolSet.to_json v.public_ipv4_pools)
       ])

let of_json j =
  { public_ipv4_pools =
      PublicIpv4PoolSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "publicIpv4PoolSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
