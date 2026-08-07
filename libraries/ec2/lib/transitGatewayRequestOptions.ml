open Aws.BaseTypes

type t =
  { amazon_side_asn : Long.t option
  ; auto_accept_shared_attachments : AutoAcceptSharedAttachmentsValue.t option
  ; default_route_table_association : DefaultRouteTableAssociationValue.t option
  ; default_route_table_propagation : DefaultRouteTablePropagationValue.t option
  ; vpn_ecmp_support : VpnEcmpSupportValue.t option
  ; dns_support : DnsSupportValue.t option
  ; security_group_referencing_support : SecurityGroupReferencingSupportValue.t option
  ; multicast_support : MulticastSupportValue.t option
  ; transit_gateway_cidr_blocks : TransitGatewayCidrBlockStringList.t
  }

let make
    ?amazon_side_asn
    ?auto_accept_shared_attachments
    ?default_route_table_association
    ?default_route_table_propagation
    ?vpn_ecmp_support
    ?dns_support
    ?security_group_referencing_support
    ?multicast_support
    ?(transit_gateway_cidr_blocks = [])
    () =
  { amazon_side_asn
  ; auto_accept_shared_attachments
  ; default_route_table_association
  ; default_route_table_propagation
  ; vpn_ecmp_support
  ; dns_support
  ; security_group_referencing_support
  ; multicast_support
  ; transit_gateway_cidr_blocks
  }

let parse xml =
  Some
    { amazon_side_asn =
        Aws.Util.option_bind (Aws.Xml.member "AmazonSideAsn" xml) Long.parse
    ; auto_accept_shared_attachments =
        Aws.Util.option_bind
          (Aws.Xml.member "AutoAcceptSharedAttachments" xml)
          AutoAcceptSharedAttachmentsValue.parse
    ; default_route_table_association =
        Aws.Util.option_bind
          (Aws.Xml.member "DefaultRouteTableAssociation" xml)
          DefaultRouteTableAssociationValue.parse
    ; default_route_table_propagation =
        Aws.Util.option_bind
          (Aws.Xml.member "DefaultRouteTablePropagation" xml)
          DefaultRouteTablePropagationValue.parse
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
    ; multicast_support =
        Aws.Util.option_bind
          (Aws.Xml.member "MulticastSupport" xml)
          MulticastSupportValue.parse
    ; transit_gateway_cidr_blocks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TransitGatewayCidrBlocks" xml)
             TransitGatewayCidrBlockStringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "TransitGatewayCidrBlocks"
              , TransitGatewayCidrBlockStringList.to_query v.transit_gateway_cidr_blocks
              ))
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
       ; Aws.Util.option_map v.default_route_table_propagation (fun f ->
             Aws.Query.Pair
               ( "DefaultRouteTablePropagation"
               , DefaultRouteTablePropagationValue.to_query f ))
       ; Aws.Util.option_map v.default_route_table_association (fun f ->
             Aws.Query.Pair
               ( "DefaultRouteTableAssociation"
               , DefaultRouteTableAssociationValue.to_query f ))
       ; Aws.Util.option_map v.auto_accept_shared_attachments (fun f ->
             Aws.Query.Pair
               ("AutoAcceptSharedAttachments", AutoAcceptSharedAttachmentsValue.to_query f))
       ; Aws.Util.option_map v.amazon_side_asn (fun f ->
             Aws.Query.Pair ("AmazonSideAsn", Long.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "TransitGatewayCidrBlocks"
           , TransitGatewayCidrBlockStringList.to_json v.transit_gateway_cidr_blocks )
       ; Aws.Util.option_map v.multicast_support (fun f ->
             "MulticastSupport", MulticastSupportValue.to_json f)
       ; Aws.Util.option_map v.security_group_referencing_support (fun f ->
             ( "SecurityGroupReferencingSupport"
             , SecurityGroupReferencingSupportValue.to_json f ))
       ; Aws.Util.option_map v.dns_support (fun f ->
             "DnsSupport", DnsSupportValue.to_json f)
       ; Aws.Util.option_map v.vpn_ecmp_support (fun f ->
             "VpnEcmpSupport", VpnEcmpSupportValue.to_json f)
       ; Aws.Util.option_map v.default_route_table_propagation (fun f ->
             "DefaultRouteTablePropagation", DefaultRouteTablePropagationValue.to_json f)
       ; Aws.Util.option_map v.default_route_table_association (fun f ->
             "DefaultRouteTableAssociation", DefaultRouteTableAssociationValue.to_json f)
       ; Aws.Util.option_map v.auto_accept_shared_attachments (fun f ->
             "AutoAcceptSharedAttachments", AutoAcceptSharedAttachmentsValue.to_json f)
       ; Aws.Util.option_map v.amazon_side_asn (fun f -> "AmazonSideAsn", Long.to_json f)
       ])

let of_json j =
  { amazon_side_asn = Aws.Util.option_map (Aws.Json.lookup j "AmazonSideAsn") Long.of_json
  ; auto_accept_shared_attachments =
      Aws.Util.option_map
        (Aws.Json.lookup j "AutoAcceptSharedAttachments")
        AutoAcceptSharedAttachmentsValue.of_json
  ; default_route_table_association =
      Aws.Util.option_map
        (Aws.Json.lookup j "DefaultRouteTableAssociation")
        DefaultRouteTableAssociationValue.of_json
  ; default_route_table_propagation =
      Aws.Util.option_map
        (Aws.Json.lookup j "DefaultRouteTablePropagation")
        DefaultRouteTablePropagationValue.of_json
  ; vpn_ecmp_support =
      Aws.Util.option_map (Aws.Json.lookup j "VpnEcmpSupport") VpnEcmpSupportValue.of_json
  ; dns_support =
      Aws.Util.option_map (Aws.Json.lookup j "DnsSupport") DnsSupportValue.of_json
  ; security_group_referencing_support =
      Aws.Util.option_map
        (Aws.Json.lookup j "SecurityGroupReferencingSupport")
        SecurityGroupReferencingSupportValue.of_json
  ; multicast_support =
      Aws.Util.option_map
        (Aws.Json.lookup j "MulticastSupport")
        MulticastSupportValue.of_json
  ; transit_gateway_cidr_blocks =
      TransitGatewayCidrBlockStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TransitGatewayCidrBlocks"))
  }
