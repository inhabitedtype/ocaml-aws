open Aws.BaseTypes

type t =
  { destination_prefix_list_id : String.t option
  ; dry_run : Boolean.t option
  ; route_table_id : String.t
  ; destination_cidr_block : String.t option
  ; destination_ipv6_cidr_block : String.t option
  }

let make
    ?destination_prefix_list_id
    ?dry_run
    ~route_table_id
    ?destination_cidr_block
    ?destination_ipv6_cidr_block
    () =
  { destination_prefix_list_id
  ; dry_run
  ; route_table_id
  ; destination_cidr_block
  ; destination_ipv6_cidr_block
  }

let parse xml =
  Some
    { destination_prefix_list_id =
        Aws.Util.option_bind (Aws.Xml.member "DestinationPrefixListId" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; route_table_id =
        Aws.Xml.required
          "routeTableId"
          (Aws.Util.option_bind (Aws.Xml.member "routeTableId" xml) String.parse)
    ; destination_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "destinationCidrBlock" xml) String.parse
    ; destination_ipv6_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "destinationIpv6CidrBlock" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.destination_ipv6_cidr_block (fun f ->
             Aws.Query.Pair ("DestinationIpv6CidrBlock", String.to_query f))
       ; Aws.Util.option_map v.destination_cidr_block (fun f ->
             Aws.Query.Pair ("DestinationCidrBlock", String.to_query f))
       ; Some (Aws.Query.Pair ("RouteTableId", String.to_query v.route_table_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.destination_prefix_list_id (fun f ->
             Aws.Query.Pair ("DestinationPrefixListId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.destination_ipv6_cidr_block (fun f ->
             "destinationIpv6CidrBlock", String.to_json f)
       ; Aws.Util.option_map v.destination_cidr_block (fun f ->
             "destinationCidrBlock", String.to_json f)
       ; Some ("routeTableId", String.to_json v.route_table_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.destination_prefix_list_id (fun f ->
             "DestinationPrefixListId", String.to_json f)
       ])

let of_json j =
  { destination_prefix_list_id =
      Aws.Util.option_map (Aws.Json.lookup j "DestinationPrefixListId") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; route_table_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "routeTableId"))
  ; destination_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "destinationCidrBlock") String.of_json
  ; destination_ipv6_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "destinationIpv6CidrBlock") String.of_json
  }
