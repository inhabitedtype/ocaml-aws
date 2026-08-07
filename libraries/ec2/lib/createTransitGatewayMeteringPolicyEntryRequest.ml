open Aws.BaseTypes

type t =
  { transit_gateway_metering_policy_id : String.t
  ; policy_rule_number : Integer.t
  ; source_transit_gateway_attachment_id : String.t option
  ; source_transit_gateway_attachment_type : TransitGatewayAttachmentResourceType.t option
  ; source_cidr_block : String.t option
  ; source_port_range : String.t option
  ; destination_transit_gateway_attachment_id : String.t option
  ; destination_transit_gateway_attachment_type :
      TransitGatewayAttachmentResourceType.t option
  ; destination_cidr_block : String.t option
  ; destination_port_range : String.t option
  ; protocol : String.t option
  ; metered_account : TransitGatewayMeteringPayerType.t
  ; dry_run : Boolean.t option
  }

let make
    ~transit_gateway_metering_policy_id
    ~policy_rule_number
    ?source_transit_gateway_attachment_id
    ?source_transit_gateway_attachment_type
    ?source_cidr_block
    ?source_port_range
    ?destination_transit_gateway_attachment_id
    ?destination_transit_gateway_attachment_type
    ?destination_cidr_block
    ?destination_port_range
    ?protocol
    ~metered_account
    ?dry_run
    () =
  { transit_gateway_metering_policy_id
  ; policy_rule_number
  ; source_transit_gateway_attachment_id
  ; source_transit_gateway_attachment_type
  ; source_cidr_block
  ; source_port_range
  ; destination_transit_gateway_attachment_id
  ; destination_transit_gateway_attachment_type
  ; destination_cidr_block
  ; destination_port_range
  ; protocol
  ; metered_account
  ; dry_run
  }

let parse xml =
  Some
    { transit_gateway_metering_policy_id =
        Aws.Xml.required
          "TransitGatewayMeteringPolicyId"
          (Aws.Util.option_bind
             (Aws.Xml.member "TransitGatewayMeteringPolicyId" xml)
             String.parse)
    ; policy_rule_number =
        Aws.Xml.required
          "PolicyRuleNumber"
          (Aws.Util.option_bind (Aws.Xml.member "PolicyRuleNumber" xml) Integer.parse)
    ; source_transit_gateway_attachment_id =
        Aws.Util.option_bind
          (Aws.Xml.member "SourceTransitGatewayAttachmentId" xml)
          String.parse
    ; source_transit_gateway_attachment_type =
        Aws.Util.option_bind
          (Aws.Xml.member "SourceTransitGatewayAttachmentType" xml)
          TransitGatewayAttachmentResourceType.parse
    ; source_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "SourceCidrBlock" xml) String.parse
    ; source_port_range =
        Aws.Util.option_bind (Aws.Xml.member "SourcePortRange" xml) String.parse
    ; destination_transit_gateway_attachment_id =
        Aws.Util.option_bind
          (Aws.Xml.member "DestinationTransitGatewayAttachmentId" xml)
          String.parse
    ; destination_transit_gateway_attachment_type =
        Aws.Util.option_bind
          (Aws.Xml.member "DestinationTransitGatewayAttachmentType" xml)
          TransitGatewayAttachmentResourceType.parse
    ; destination_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "DestinationCidrBlock" xml) String.parse
    ; destination_port_range =
        Aws.Util.option_bind (Aws.Xml.member "DestinationPortRange" xml) String.parse
    ; protocol = Aws.Util.option_bind (Aws.Xml.member "Protocol" xml) String.parse
    ; metered_account =
        Aws.Xml.required
          "MeteredAccount"
          (Aws.Util.option_bind
             (Aws.Xml.member "MeteredAccount" xml)
             TransitGatewayMeteringPayerType.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "MeteredAccount"
              , TransitGatewayMeteringPayerType.to_query v.metered_account ))
       ; Aws.Util.option_map v.protocol (fun f ->
             Aws.Query.Pair ("Protocol", String.to_query f))
       ; Aws.Util.option_map v.destination_port_range (fun f ->
             Aws.Query.Pair ("DestinationPortRange", String.to_query f))
       ; Aws.Util.option_map v.destination_cidr_block (fun f ->
             Aws.Query.Pair ("DestinationCidrBlock", String.to_query f))
       ; Aws.Util.option_map v.destination_transit_gateway_attachment_type (fun f ->
             Aws.Query.Pair
               ( "DestinationTransitGatewayAttachmentType"
               , TransitGatewayAttachmentResourceType.to_query f ))
       ; Aws.Util.option_map v.destination_transit_gateway_attachment_id (fun f ->
             Aws.Query.Pair ("DestinationTransitGatewayAttachmentId", String.to_query f))
       ; Aws.Util.option_map v.source_port_range (fun f ->
             Aws.Query.Pair ("SourcePortRange", String.to_query f))
       ; Aws.Util.option_map v.source_cidr_block (fun f ->
             Aws.Query.Pair ("SourceCidrBlock", String.to_query f))
       ; Aws.Util.option_map v.source_transit_gateway_attachment_type (fun f ->
             Aws.Query.Pair
               ( "SourceTransitGatewayAttachmentType"
               , TransitGatewayAttachmentResourceType.to_query f ))
       ; Aws.Util.option_map v.source_transit_gateway_attachment_id (fun f ->
             Aws.Query.Pair ("SourceTransitGatewayAttachmentId", String.to_query f))
       ; Some (Aws.Query.Pair ("PolicyRuleNumber", Integer.to_query v.policy_rule_number))
       ; Some
           (Aws.Query.Pair
              ( "TransitGatewayMeteringPolicyId"
              , String.to_query v.transit_gateway_metering_policy_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("MeteredAccount", TransitGatewayMeteringPayerType.to_json v.metered_account)
       ; Aws.Util.option_map v.protocol (fun f -> "Protocol", String.to_json f)
       ; Aws.Util.option_map v.destination_port_range (fun f ->
             "DestinationPortRange", String.to_json f)
       ; Aws.Util.option_map v.destination_cidr_block (fun f ->
             "DestinationCidrBlock", String.to_json f)
       ; Aws.Util.option_map v.destination_transit_gateway_attachment_type (fun f ->
             ( "DestinationTransitGatewayAttachmentType"
             , TransitGatewayAttachmentResourceType.to_json f ))
       ; Aws.Util.option_map v.destination_transit_gateway_attachment_id (fun f ->
             "DestinationTransitGatewayAttachmentId", String.to_json f)
       ; Aws.Util.option_map v.source_port_range (fun f ->
             "SourcePortRange", String.to_json f)
       ; Aws.Util.option_map v.source_cidr_block (fun f ->
             "SourceCidrBlock", String.to_json f)
       ; Aws.Util.option_map v.source_transit_gateway_attachment_type (fun f ->
             ( "SourceTransitGatewayAttachmentType"
             , TransitGatewayAttachmentResourceType.to_json f ))
       ; Aws.Util.option_map v.source_transit_gateway_attachment_id (fun f ->
             "SourceTransitGatewayAttachmentId", String.to_json f)
       ; Some ("PolicyRuleNumber", Integer.to_json v.policy_rule_number)
       ; Some
           ( "TransitGatewayMeteringPolicyId"
           , String.to_json v.transit_gateway_metering_policy_id )
       ])

let of_json j =
  { transit_gateway_metering_policy_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TransitGatewayMeteringPolicyId"))
  ; policy_rule_number =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyRuleNumber"))
  ; source_transit_gateway_attachment_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "SourceTransitGatewayAttachmentId")
        String.of_json
  ; source_transit_gateway_attachment_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "SourceTransitGatewayAttachmentType")
        TransitGatewayAttachmentResourceType.of_json
  ; source_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "SourceCidrBlock") String.of_json
  ; source_port_range =
      Aws.Util.option_map (Aws.Json.lookup j "SourcePortRange") String.of_json
  ; destination_transit_gateway_attachment_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "DestinationTransitGatewayAttachmentId")
        String.of_json
  ; destination_transit_gateway_attachment_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "DestinationTransitGatewayAttachmentType")
        TransitGatewayAttachmentResourceType.of_json
  ; destination_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "DestinationCidrBlock") String.of_json
  ; destination_port_range =
      Aws.Util.option_map (Aws.Json.lookup j "DestinationPortRange") String.of_json
  ; protocol = Aws.Util.option_map (Aws.Json.lookup j "Protocol") String.of_json
  ; metered_account =
      TransitGatewayMeteringPayerType.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "MeteredAccount"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
