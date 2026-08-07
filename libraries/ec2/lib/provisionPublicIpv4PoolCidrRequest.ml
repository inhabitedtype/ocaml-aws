open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; ipam_pool_id : String.t
  ; pool_id : String.t
  ; netmask_length : Integer.t
  ; network_border_group : String.t option
  }

let make ?dry_run ~ipam_pool_id ~pool_id ~netmask_length ?network_border_group () =
  { dry_run; ipam_pool_id; pool_id; netmask_length; network_border_group }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipam_pool_id =
        Aws.Xml.required
          "IpamPoolId"
          (Aws.Util.option_bind (Aws.Xml.member "IpamPoolId" xml) String.parse)
    ; pool_id =
        Aws.Xml.required
          "PoolId"
          (Aws.Util.option_bind (Aws.Xml.member "PoolId" xml) String.parse)
    ; netmask_length =
        Aws.Xml.required
          "NetmaskLength"
          (Aws.Util.option_bind (Aws.Xml.member "NetmaskLength" xml) Integer.parse)
    ; network_border_group =
        Aws.Util.option_bind (Aws.Xml.member "NetworkBorderGroup" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_border_group (fun f ->
             Aws.Query.Pair ("NetworkBorderGroup", String.to_query f))
       ; Some (Aws.Query.Pair ("NetmaskLength", Integer.to_query v.netmask_length))
       ; Some (Aws.Query.Pair ("PoolId", String.to_query v.pool_id))
       ; Some (Aws.Query.Pair ("IpamPoolId", String.to_query v.ipam_pool_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_border_group (fun f ->
             "NetworkBorderGroup", String.to_json f)
       ; Some ("NetmaskLength", Integer.to_json v.netmask_length)
       ; Some ("PoolId", String.to_json v.pool_id)
       ; Some ("IpamPoolId", String.to_json v.ipam_pool_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipam_pool_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamPoolId"))
  ; pool_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PoolId"))
  ; netmask_length =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NetmaskLength"))
  ; network_border_group =
      Aws.Util.option_map (Aws.Json.lookup j "NetworkBorderGroup") String.of_json
  }
