open Aws.BaseTypes

type t =
  { pool_id : String.t option
  ; deprovisioned_addresses : DeprovisionedAddressSet.t
  }

let make ?pool_id ?(deprovisioned_addresses = []) () =
  { pool_id; deprovisioned_addresses }

let parse xml =
  Some
    { pool_id = Aws.Util.option_bind (Aws.Xml.member "poolId" xml) String.parse
    ; deprovisioned_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "deprovisionedAddressSet" xml)
             DeprovisionedAddressSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "DeprovisionedAddressSet"
              , DeprovisionedAddressSet.to_query v.deprovisioned_addresses ))
       ; Aws.Util.option_map v.pool_id (fun f ->
             Aws.Query.Pair ("PoolId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "deprovisionedAddressSet"
           , DeprovisionedAddressSet.to_json v.deprovisioned_addresses )
       ; Aws.Util.option_map v.pool_id (fun f -> "poolId", String.to_json f)
       ])

let of_json j =
  { pool_id = Aws.Util.option_map (Aws.Json.lookup j "poolId") String.of_json
  ; deprovisioned_addresses =
      DeprovisionedAddressSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "deprovisionedAddressSet"))
  }
