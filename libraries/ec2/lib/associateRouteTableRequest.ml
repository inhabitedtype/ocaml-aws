open Aws.BaseTypes

type t =
  { gateway_id : String.t option
  ; public_ipv4_pool : String.t option
  ; dry_run : Boolean.t option
  ; subnet_id : String.t option
  ; route_table_id : String.t
  }

let make ?gateway_id ?public_ipv4_pool ?dry_run ?subnet_id ~route_table_id () =
  { gateway_id; public_ipv4_pool; dry_run; subnet_id; route_table_id }

let parse xml =
  Some
    { gateway_id = Aws.Util.option_bind (Aws.Xml.member "GatewayId" xml) String.parse
    ; public_ipv4_pool =
        Aws.Util.option_bind (Aws.Xml.member "PublicIpv4Pool" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse
    ; route_table_id =
        Aws.Xml.required
          "routeTableId"
          (Aws.Util.option_bind (Aws.Xml.member "routeTableId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("RouteTableId", String.to_query v.route_table_id))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.public_ipv4_pool (fun f ->
             Aws.Query.Pair ("PublicIpv4Pool", String.to_query f))
       ; Aws.Util.option_map v.gateway_id (fun f ->
             Aws.Query.Pair ("GatewayId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("routeTableId", String.to_json v.route_table_id)
       ; Aws.Util.option_map v.subnet_id (fun f -> "subnetId", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.public_ipv4_pool (fun f ->
             "PublicIpv4Pool", String.to_json f)
       ; Aws.Util.option_map v.gateway_id (fun f -> "GatewayId", String.to_json f)
       ])

let of_json j =
  { gateway_id = Aws.Util.option_map (Aws.Json.lookup j "GatewayId") String.of_json
  ; public_ipv4_pool =
      Aws.Util.option_map (Aws.Json.lookup j "PublicIpv4Pool") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "subnetId") String.of_json
  ; route_table_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "routeTableId"))
  }
