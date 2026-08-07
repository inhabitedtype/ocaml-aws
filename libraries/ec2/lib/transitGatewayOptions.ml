open Aws.BaseTypes

type t =
  { amazon_side_asn : Long.t option
  ; transit_gateway_cidr_blocks : ValueStringList.t
  ; auto_accept_shared_attachments : AutoAcceptSharedAttachmentsValue.t option
  ; default_route_table_association : DefaultRouteTableAssociationValue.t option
  ; association_default_route_table_id : String.t option
  ; default_route_table_propagation : DefaultRouteTablePropagationValue.t option
  ; propagation_default_route_table_id : String.t option
  ; vpn_ecmp_support : VpnEcmpSupportValue.t option
  ; dns_support : DnsSupportValue.t option
  ; security_group_referencing_support : SecurityGroupReferencingSupportValue.t option
  ; multicast_support : MulticastSupportValue.t option
  ; encryption_support : EncryptionSupport.t option
  }

let make
    ?amazon_side_asn
    ?(transit_gateway_cidr_blocks = [])
    ?auto_accept_shared_attachments
    ?default_route_table_association
    ?association_default_route_table_id
    ?default_route_table_propagation
    ?propagation_default_route_table_id
    ?vpn_ecmp_support
    ?dns_support
    ?security_group_referencing_support
    ?multicast_support
    ?encryption_support
    () =
  { amazon_side_asn
  ; transit_gateway_cidr_blocks
  ; auto_accept_shared_attachments
  ; default_route_table_association
  ; association_default_route_table_id
  ; default_route_table_propagation
  ; propagation_default_route_table_id
  ; vpn_ecmp_support
  ; dns_support
  ; security_group_referencing_support
  ; multicast_support
  ; encryption_support
  }

let parse xml =
  Some
    { amazon_side_asn =
        Aws.Util.option_bind (Aws.Xml.member "amazonSideAsn" xml) Long.parse
    ; transit_gateway_cidr_blocks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "transitGatewayCidrBlocks" xml)
             ValueStringList.parse)
    ; auto_accept_shared_attachments =
        Aws.Util.option_bind
          (Aws.Xml.member "autoAcceptSharedAttachments" xml)
          AutoAcceptSharedAttachmentsValue.parse
    ; default_route_table_association =
        Aws.Util.option_bind
          (Aws.Xml.member "defaultRouteTableAssociation" xml)
          DefaultRouteTableAssociationValue.parse
    ; association_default_route_table_id =
        Aws.Util.option_bind
          (Aws.Xml.member "associationDefaultRouteTableId" xml)
          String.parse
    ; default_route_table_propagation =
        Aws.Util.option_bind
          (Aws.Xml.member "defaultRouteTablePropagation" xml)
          DefaultRouteTablePropagationValue.parse
    ; propagation_default_route_table_id =
        Aws.Util.option_bind
          (Aws.Xml.member "propagationDefaultRouteTableId" xml)
          String.parse
    ; vpn_ecmp_support =
        Aws.Util.option_bind
          (Aws.Xml.member "vpnEcmpSupport" xml)
          VpnEcmpSupportValue.parse
    ; dns_support =
        Aws.Util.option_bind (Aws.Xml.member "dnsSupport" xml) DnsSupportValue.parse
    ; security_group_referencing_support =
        Aws.Util.option_bind
          (Aws.Xml.member "securityGroupReferencingSupport" xml)
          SecurityGroupReferencingSupportValue.parse
    ; multicast_support =
        Aws.Util.option_bind
          (Aws.Xml.member "multicastSupport" xml)
          MulticastSupportValue.parse
    ; encryption_support =
        Aws.Util.option_bind
          (Aws.Xml.member "encryptionSupport" xml)
          EncryptionSupport.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.encryption_support (fun f ->
             Aws.Query.Pair ("EncryptionSupport", EncryptionSupport.to_query f))
       ; Aws.Util.option_map v.multicast_support (fun f ->
             Aws.Query.Pair ("MulticastSupport", MulticastSupportValue.to_query f))
       ; Aws.Util.option_map v.security_group_referencing_support (fun f ->
             Aws.Query.Pair
               ( "SecurityGroupReferencingSupport"
               , SecurityGroupReferencingSupportValue.to_query f ))
       ; Aws.Util.option_map v.dns_support (fun f ->
             Aws.Query.Pair ("DnsSupport", DnsSupportValue.to_query f))
       ; Aws.Util.option_map v.vpn_ecmp_support (fun f ->
             Aws.Query.Pair ("VpnEcmpSupport", VpnEcmpSupportValue.to_query f))
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
       ; Some
           (Aws.Query.Pair
              ( "TransitGatewayCidrBlocks"
              , ValueStringList.to_query v.transit_gateway_cidr_blocks ))
       ; Aws.Util.option_map v.amazon_side_asn (fun f ->
             Aws.Query.Pair ("AmazonSideAsn", Long.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.encryption_support (fun f ->
             "encryptionSupport", EncryptionSupport.to_json f)
       ; Aws.Util.option_map v.multicast_support (fun f ->
             "multicastSupport", MulticastSupportValue.to_json f)
       ; Aws.Util.option_map v.security_group_referencing_support (fun f ->
             ( "securityGroupReferencingSupport"
             , SecurityGroupReferencingSupportValue.to_json f ))
       ; Aws.Util.option_map v.dns_support (fun f ->
             "dnsSupport", DnsSupportValue.to_json f)
       ; Aws.Util.option_map v.vpn_ecmp_support (fun f ->
             "vpnEcmpSupport", VpnEcmpSupportValue.to_json f)
       ; Aws.Util.option_map v.propagation_default_route_table_id (fun f ->
             "propagationDefaultRouteTableId", String.to_json f)
       ; Aws.Util.option_map v.default_route_table_propagation (fun f ->
             "defaultRouteTablePropagation", DefaultRouteTablePropagationValue.to_json f)
       ; Aws.Util.option_map v.association_default_route_table_id (fun f ->
             "associationDefaultRouteTableId", String.to_json f)
       ; Aws.Util.option_map v.default_route_table_association (fun f ->
             "defaultRouteTableAssociation", DefaultRouteTableAssociationValue.to_json f)
       ; Aws.Util.option_map v.auto_accept_shared_attachments (fun f ->
             "autoAcceptSharedAttachments", AutoAcceptSharedAttachmentsValue.to_json f)
       ; Some
           ( "transitGatewayCidrBlocks"
           , ValueStringList.to_json v.transit_gateway_cidr_blocks )
       ; Aws.Util.option_map v.amazon_side_asn (fun f -> "amazonSideAsn", Long.to_json f)
       ])

let of_json j =
  { amazon_side_asn = Aws.Util.option_map (Aws.Json.lookup j "amazonSideAsn") Long.of_json
  ; transit_gateway_cidr_blocks =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "transitGatewayCidrBlocks"))
  ; auto_accept_shared_attachments =
      Aws.Util.option_map
        (Aws.Json.lookup j "autoAcceptSharedAttachments")
        AutoAcceptSharedAttachmentsValue.of_json
  ; default_route_table_association =
      Aws.Util.option_map
        (Aws.Json.lookup j "defaultRouteTableAssociation")
        DefaultRouteTableAssociationValue.of_json
  ; association_default_route_table_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "associationDefaultRouteTableId")
        String.of_json
  ; default_route_table_propagation =
      Aws.Util.option_map
        (Aws.Json.lookup j "defaultRouteTablePropagation")
        DefaultRouteTablePropagationValue.of_json
  ; propagation_default_route_table_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "propagationDefaultRouteTableId")
        String.of_json
  ; vpn_ecmp_support =
      Aws.Util.option_map (Aws.Json.lookup j "vpnEcmpSupport") VpnEcmpSupportValue.of_json
  ; dns_support =
      Aws.Util.option_map (Aws.Json.lookup j "dnsSupport") DnsSupportValue.of_json
  ; security_group_referencing_support =
      Aws.Util.option_map
        (Aws.Json.lookup j "securityGroupReferencingSupport")
        SecurityGroupReferencingSupportValue.of_json
  ; multicast_support =
      Aws.Util.option_map
        (Aws.Json.lookup j "multicastSupport")
        MulticastSupportValue.of_json
  ; encryption_support =
      Aws.Util.option_map
        (Aws.Json.lookup j "encryptionSupport")
        EncryptionSupport.of_json
  }
