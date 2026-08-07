open Aws.BaseTypes

type t =
  { pool_id : String.t option
  ; pool_address_range : PublicIpv4PoolRange.t option
  }

let make ?pool_id ?pool_address_range () = { pool_id; pool_address_range }

let parse xml =
  Some
    { pool_id = Aws.Util.option_bind (Aws.Xml.member "poolId" xml) String.parse
    ; pool_address_range =
        Aws.Util.option_bind
          (Aws.Xml.member "poolAddressRange" xml)
          PublicIpv4PoolRange.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.pool_address_range (fun f ->
             Aws.Query.Pair ("PoolAddressRange", PublicIpv4PoolRange.to_query f))
       ; Aws.Util.option_map v.pool_id (fun f ->
             Aws.Query.Pair ("PoolId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.pool_address_range (fun f ->
             "poolAddressRange", PublicIpv4PoolRange.to_json f)
       ; Aws.Util.option_map v.pool_id (fun f -> "poolId", String.to_json f)
       ])

let of_json j =
  { pool_id = Aws.Util.option_map (Aws.Json.lookup j "poolId") String.of_json
  ; pool_address_range =
      Aws.Util.option_map
        (Aws.Json.lookup j "poolAddressRange")
        PublicIpv4PoolRange.of_json
  }
