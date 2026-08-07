open Aws.BaseTypes

type t =
  { ipv6_pools : Ipv6PoolSet.t
  ; next_token : String.t option
  }

let make ?(ipv6_pools = []) ?next_token () = { ipv6_pools; next_token }

let parse xml =
  Some
    { ipv6_pools =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ipv6PoolSet" xml) Ipv6PoolSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("Ipv6PoolSet", Ipv6PoolSet.to_query v.ipv6_pools))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("ipv6PoolSet", Ipv6PoolSet.to_json v.ipv6_pools)
       ])

let of_json j =
  { ipv6_pools =
      Ipv6PoolSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ipv6PoolSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
