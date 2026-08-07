open Aws.BaseTypes

type t =
  { ipam_pool_allocations : IpamPoolAllocationSet.t
  ; next_token : String.t option
  }

let make ?(ipam_pool_allocations = []) ?next_token () =
  { ipam_pool_allocations; next_token }

let parse xml =
  Some
    { ipam_pool_allocations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipamPoolAllocationSet" xml)
             IpamPoolAllocationSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "IpamPoolAllocationSet"
              , IpamPoolAllocationSet.to_query v.ipam_pool_allocations ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ("ipamPoolAllocationSet", IpamPoolAllocationSet.to_json v.ipam_pool_allocations)
       ])

let of_json j =
  { ipam_pool_allocations =
      IpamPoolAllocationSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipamPoolAllocationSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
