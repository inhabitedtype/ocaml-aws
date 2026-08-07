open Aws.BaseTypes

type t =
  { route_server_id : String.t
  ; subnet_id : String.t
  ; client_token : String.t option
  ; dry_run : Boolean.t option
  ; tag_specifications : TagSpecificationList.t
  }

let make ~route_server_id ~subnet_id ?client_token ?dry_run ?(tag_specifications = []) ()
    =
  { route_server_id; subnet_id; client_token; dry_run; tag_specifications }

let parse xml =
  Some
    { route_server_id =
        Aws.Xml.required
          "RouteServerId"
          (Aws.Util.option_bind (Aws.Xml.member "RouteServerId" xml) String.parse)
    ; subnet_id =
        Aws.Xml.required
          "SubnetId"
          (Aws.Util.option_bind (Aws.Xml.member "SubnetId" xml) String.parse)
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Some (Aws.Query.Pair ("SubnetId", String.to_query v.subnet_id))
       ; Some (Aws.Query.Pair ("RouteServerId", String.to_query v.route_server_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Some ("SubnetId", String.to_json v.subnet_id)
       ; Some ("RouteServerId", String.to_json v.route_server_id)
       ])

let of_json j =
  { route_server_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RouteServerId"))
  ; subnet_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SubnetId"))
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  }
