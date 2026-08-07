open Aws.BaseTypes

type t =
  { add_transit_gateway_cidr_blocks : TransitGatewayCidrBlockStringList.t
  ; remove_transit_gateway_cidr_blocks : TransitGatewayCidrBlockStringList.t
  ; vpn_ecmp_support : VpnEcmpSupportValue.t option
  ; dns_support : DnsSupportValue.t option
  ; security_group_referencing_support : SecurityGroupReferencingSupportValue.t option
  ; auto_accept_shared_attachments : AutoAcceptSharedAttachmentsValue.t option
  ; default_route_table_association : DefaultRouteTableAssociationValue.t option
  ; association_default_route_table_id : String.t option
  ; default_route_table_propagation : DefaultRouteTablePropagationValue.t option
  ; propagation_default_route_table_id : String.t option
  ; amazon_side_asn : Long.t option
  ; encryption_support : EncryptionSupportOptionValue.t option
  }

let make
    ?(add_transit_gateway_cidr_blocks = [])
    ?(remove_transit_gateway_cidr_blocks = [])
    ?vpn_ecmp_support
    ?dns_support
    ?security_group_referencing_support
    ?auto_accept_shared_attachments
    ?default_route_table_association
    ?association_default_route_table_id
    ?default_route_table_propagation
    ?propagation_default_route_table_id
    ?amazon_side_asn
    ?encryption_support
    () =
  { add_transit_gateway_cidr_blocks
  ; remove_transit_gateway_cidr_blocks
  ; vpn_ecmp_support
  ; dns_support
  ; security_group_referencing_support
  ; auto_accept_shared_attachments
  ; default_route_table_association
  ; association_default_route_table_id
  ; default_route_table_propagation
  ; propagation_default_route_table_id
  ; amazon_side_asn
  ; encryption_support
  }

let parse xml =
  Some
    { add_transit_gateway_cidr_blocks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AddTransitGatewayCidrBlocks" xml)
             TransitGatewayCidrBlockStringList.parse)
    ; remove_transit_gateway_cidr_blocks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RemoveTransitGatewayCidrBlocks" xml)
             TransitGatewayCidrBlockStringList.parse)
    ; vpn_ecmp_support =
        Aws.Util.option_bind
          (Aws.Xml.member "VpnEcmpSupport" xml)
          VpnEcmpSupportValue.parse
    ; dns_support =
        Aws.Util.option_bind (Aws.Xml.member "DnsSupport" xml) DnsSupportValue.parse
    ; security_group_referencing_support =
        Aws.Util.option_bind
          (Aws.Xml.member "SecurityGroupReferencingSupport" xml)
          SecurityGroupReferencingSupportValue.parse
    ; auto_accept_shared_attachments =
        Aws.Util.option_bind
          (Aws.Xml.member "AutoAcceptSharedAttachments" xml)
          AutoAcceptSharedAttachmentsValue.parse
    ; default_route_table_association =
        Aws.Util.option_bind
          (Aws.Xml.member "DefaultRouteTableAssociation" xml)
          DefaultRouteTableAssociationValue.parse
    ; association_default_route_table_id =
        Aws.Util.option_bind
          (Aws.Xml.member "AssociationDefaultRouteTableId" xml)
          String.parse
    ; default_route_table_propagation =
        Aws.Util.option_bind
          (Aws.Xml.member "DefaultRouteTablePropagation" xml)
          DefaultRouteTablePropagationValue.parse
    ; propagation_default_route_table_id =
        Aws.Util.option_bind
          (Aws.Xml.member "PropagationDefaultRouteTableId" xml)
          String.parse
    ; amazon_side_asn =
        Aws.Util.option_bind (Aws.Xml.member "AmazonSideAsn" xml) Long.parse
    ; encryption_support =
        Aws.Util.option_bind
          (Aws.Xml.member "EncryptionSupport" xml)
          EncryptionSupportOptionValue.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.encryption_support (fun f ->
             Aws.Query.Pair ("EncryptionSupport", EncryptionSupportOptionValue.to_query f))
       ; Aws.Util.option_map v.amazon_side_asn (fun f ->
             Aws.Query.Pair ("AmazonSideAsn", Long.to_query f))
       ; Aws.Util.option_map v.propagation_default_route_table_id (fun f ->
             Aws.Query.Pair ("PropagationDefaultRouteTableId", String.to_query f))
       ; Aws.Util.option_map v.default_route_table_propagation (fun f ->
             Aws.Query.Pair
               ( "DefaultRouteTablePropagation"
               , DefaultRouteTablePropagationValue.to_query f ))
       ; Aws.Util.option_map v.association_default_route_table_id (fun f ->
             Aws.Query.Pair ("AssociationDefaultRouteTableId", String.to_query f))
       ; Aws.Util.option_map v.default_route_table_association (fun f ->
             Aws.Query.Pair
               ( "DefaultRouteTableAssociation"
               , DefaultRouteTableAssociationValue.to_query f ))
       ; Aws.Util.option_map v.auto_accept_shared_attachments (fun f ->
             Aws.Query.Pair
               ("AutoAcceptSharedAttachments", AutoAcceptSharedAttachmentsValue.to_query f))
       ; Aws.Util.option_map v.security_group_referencing_support (fun f ->
             Aws.Query.Pair
               ( "SecurityGroupReferencingSupport"
               , SecurityGroupReferencingSupportValue.to_query f ))
       ; Aws.Util.option_map v.dns_support (fun f ->
             Aws.Query.Pair ("DnsSupport", DnsSupportValue.to_query f))
       ; Aws.Util.option_map v.vpn_ecmp_support (fun f ->
             Aws.Query.Pair ("VpnEcmpSupport", VpnEcmpSupportValue.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "RemoveTransitGatewayCidrBlocks"
              , TransitGatewayCidrBlockStringList.to_query
                  v.remove_transit_gateway_cidr_blocks ))
       ; Some
           (Aws.Query.Pair
              ( "AddTransitGatewayCidrBlocks"
              , TransitGatewayCidrBlockStringList.to_query
                  v.add_transit_gateway_cidr_blocks ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.encryption_support (fun f ->
             "EncryptionSupport", EncryptionSupportOptionValue.to_json f)
       ; Aws.Util.option_map v.amazon_side_asn (fun f -> "AmazonSideAsn", Long.to_json f)
       ; Aws.Util.option_map v.propagation_default_route_table_id (fun f ->
             "PropagationDefaultRouteTableId", String.to_json f)
       ; Aws.Util.option_map v.default_route_table_propagation (fun f ->
             "DefaultRouteTablePropagation", DefaultRouteTablePropagationValue.to_json f)
       ; Aws.Util.option_map v.association_default_route_table_id (fun f ->
             "AssociationDefaultRouteTableId", String.to_json f)
       ; Aws.Util.option_map v.default_route_table_association (fun f ->
             "DefaultRouteTableAssociation", DefaultRouteTableAssociationValue.to_json f)
       ; Aws.Util.option_map v.auto_accept_shared_attachments (fun f ->
             "AutoAcceptSharedAttachments", AutoAcceptSharedAttachmentsValue.to_json f)
       ; Aws.Util.option_map v.security_group_referencing_support (fun f ->
             ( "SecurityGroupReferencingSupport"
             , SecurityGroupReferencingSupportValue.to_json f ))
       ; Aws.Util.option_map v.dns_support (fun f ->
             "DnsSupport", DnsSupportValue.to_json f)
       ; Aws.Util.option_map v.vpn_ecmp_support (fun f ->
             "VpnEcmpSupport", VpnEcmpSupportValue.to_json f)
       ; Some
           ( "RemoveTransitGatewayCidrBlocks"
           , TransitGatewayCidrBlockStringList.to_json
               v.remove_transit_gateway_cidr_blocks )
       ; Some
           ( "AddTransitGatewayCidrBlocks"
           , TransitGatewayCidrBlockStringList.to_json v.add_transit_gateway_cidr_blocks
           )
       ])

let of_json j =
  { add_transit_gateway_cidr_blocks =
      TransitGatewayCidrBlockStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AddTransitGatewayCidrBlocks"))
  ; remove_transit_gateway_cidr_blocks =
      TransitGatewayCidrBlockStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RemoveTransitGatewayCidrBlocks"))
  ; vpn_ecmp_support =
      Aws.Util.option_map (Aws.Json.lookup j "VpnEcmpSupport") VpnEcmpSupportValue.of_json
  ; dns_support =
      Aws.Util.option_map (Aws.Json.lookup j "DnsSupport") DnsSupportValue.of_json
  ; security_group_referencing_support =
      Aws.Util.option_map
        (Aws.Json.lookup j "SecurityGroupReferencingSupport")
        SecurityGroupReferencingSupportValue.of_json
  ; auto_accept_shared_attachments =
      Aws.Util.option_map
        (Aws.Json.lookup j "AutoAcceptSharedAttachments")
        AutoAcceptSharedAttachmentsValue.of_json
  ; default_route_table_association =
      Aws.Util.option_map
        (Aws.Json.lookup j "DefaultRouteTableAssociation")
        DefaultRouteTableAssociationValue.of_json
  ; association_default_route_table_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "AssociationDefaultRouteTableId")
        String.of_json
  ; default_route_table_propagation =
      Aws.Util.option_map
        (Aws.Json.lookup j "DefaultRouteTablePropagation")
        DefaultRouteTablePropagationValue.of_json
  ; propagation_default_route_table_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "PropagationDefaultRouteTableId")
        String.of_json
  ; amazon_side_asn = Aws.Util.option_map (Aws.Json.lookup j "AmazonSideAsn") Long.of_json
  ; encryption_support =
      Aws.Util.option_map
        (Aws.Json.lookup j "EncryptionSupport")
        EncryptionSupportOptionValue.of_json
  }
