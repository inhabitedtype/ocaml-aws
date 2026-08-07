open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; pool_id : String.t
  ; network_border_group : String.t option
  }

let make ?dry_run ~pool_id ?network_border_group () =
  { dry_run; pool_id; network_border_group }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; pool_id =
        Aws.Xml.required
          "PoolId"
          (Aws.Util.option_bind (Aws.Xml.member "PoolId" xml) String.parse)
    ; network_border_group =
        Aws.Util.option_bind (Aws.Xml.member "NetworkBorderGroup" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_border_group (fun f ->
             Aws.Query.Pair ("NetworkBorderGroup", String.to_query f))
       ; Some (Aws.Query.Pair ("PoolId", String.to_query v.pool_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_border_group (fun f ->
             "NetworkBorderGroup", String.to_json f)
       ; Some ("PoolId", String.to_json v.pool_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; pool_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PoolId"))
  ; network_border_group =
      Aws.Util.option_map (Aws.Json.lookup j "NetworkBorderGroup") String.of_json
  }
