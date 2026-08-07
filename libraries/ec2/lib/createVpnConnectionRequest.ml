open Aws.BaseTypes

type t =
  { customer_gateway_id : String.t
  ; type_ : String.t
  ; vpn_gateway_id : String.t option
  ; transit_gateway_id : String.t option
  ; vpn_concentrator_id : String.t option
  ; tag_specifications : TagSpecificationList.t
  ; pre_shared_key_storage : String.t option
  ; dry_run : Boolean.t option
  ; options : VpnConnectionOptionsSpecification.t option
  }

let make
    ~customer_gateway_id
    ~type_
    ?vpn_gateway_id
    ?transit_gateway_id
    ?vpn_concentrator_id
    ?(tag_specifications = [])
    ?pre_shared_key_storage
    ?dry_run
    ?options
    () =
  { customer_gateway_id
  ; type_
  ; vpn_gateway_id
  ; transit_gateway_id
  ; vpn_concentrator_id
  ; tag_specifications
  ; pre_shared_key_storage
  ; dry_run
  ; options
  }

let parse xml =
  Some
    { customer_gateway_id =
        Aws.Xml.required
          "CustomerGatewayId"
          (Aws.Util.option_bind (Aws.Xml.member "CustomerGatewayId" xml) String.parse)
    ; type_ =
        Aws.Xml.required
          "Type"
          (Aws.Util.option_bind (Aws.Xml.member "Type" xml) String.parse)
    ; vpn_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "VpnGatewayId" xml) String.parse
    ; transit_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "TransitGatewayId" xml) String.parse
    ; vpn_concentrator_id =
        Aws.Util.option_bind (Aws.Xml.member "VpnConcentratorId" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; pre_shared_key_storage =
        Aws.Util.option_bind (Aws.Xml.member "PreSharedKeyStorage" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; options =
        Aws.Util.option_bind
          (Aws.Xml.member "options" xml)
          VpnConnectionOptionsSpecification.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.options (fun f ->
             Aws.Query.Pair ("Options", VpnConnectionOptionsSpecification.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.pre_shared_key_storage (fun f ->
             Aws.Query.Pair ("PreSharedKeyStorage", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.vpn_concentrator_id (fun f ->
             Aws.Query.Pair ("VpnConcentratorId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             Aws.Query.Pair ("TransitGatewayId", String.to_query f))
       ; Aws.Util.option_map v.vpn_gateway_id (fun f ->
             Aws.Query.Pair ("VpnGatewayId", String.to_query f))
       ; Some (Aws.Query.Pair ("Type", String.to_query v.type_))
       ; Some
           (Aws.Query.Pair ("CustomerGatewayId", String.to_query v.customer_gateway_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.options (fun f ->
             "options", VpnConnectionOptionsSpecification.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.pre_shared_key_storage (fun f ->
             "PreSharedKeyStorage", String.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.vpn_concentrator_id (fun f ->
             "VpnConcentratorId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             "TransitGatewayId", String.to_json f)
       ; Aws.Util.option_map v.vpn_gateway_id (fun f -> "VpnGatewayId", String.to_json f)
       ; Some ("Type", String.to_json v.type_)
       ; Some ("CustomerGatewayId", String.to_json v.customer_gateway_id)
       ])

let of_json j =
  { customer_gateway_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CustomerGatewayId"))
  ; type_ = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Type"))
  ; vpn_gateway_id = Aws.Util.option_map (Aws.Json.lookup j "VpnGatewayId") String.of_json
  ; transit_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "TransitGatewayId") String.of_json
  ; vpn_concentrator_id =
      Aws.Util.option_map (Aws.Json.lookup j "VpnConcentratorId") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; pre_shared_key_storage =
      Aws.Util.option_map (Aws.Json.lookup j "PreSharedKeyStorage") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; options =
      Aws.Util.option_map
        (Aws.Json.lookup j "options")
        VpnConnectionOptionsSpecification.of_json
  }
