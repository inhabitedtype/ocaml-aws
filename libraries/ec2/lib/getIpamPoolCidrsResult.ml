open Aws.BaseTypes

type t =
  { ipam_pool_cidrs : IpamPoolCidrSet.t
  ; next_token : String.t option
  }

let make ?(ipam_pool_cidrs = []) ?next_token () = { ipam_pool_cidrs; next_token }

let parse xml =
  Some
    { ipam_pool_cidrs =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipamPoolCidrSet" xml)
             IpamPoolCidrSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair ("IpamPoolCidrSet", IpamPoolCidrSet.to_query v.ipam_pool_cidrs))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("ipamPoolCidrSet", IpamPoolCidrSet.to_json v.ipam_pool_cidrs)
       ])

let of_json j =
  { ipam_pool_cidrs =
      IpamPoolCidrSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipamPoolCidrSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
