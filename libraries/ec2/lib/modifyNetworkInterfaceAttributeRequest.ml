open Aws.BaseTypes

type t =
  { ena_srd_specification : EnaSrdSpecification.t option
  ; enable_primary_ipv6 : Boolean.t option
  ; connection_tracking_specification : ConnectionTrackingSpecificationRequest.t option
  ; associate_public_ip_address : Boolean.t option
  ; associated_subnet_ids : SubnetIdList.t
  ; dry_run : Boolean.t option
  ; network_interface_id : String.t
  ; description : AttributeValue.t option
  ; source_dest_check : AttributeBooleanValue.t option
  ; groups : SecurityGroupIdStringList.t
  ; attachment : NetworkInterfaceAttachmentChanges.t option
  }

let make
    ?ena_srd_specification
    ?enable_primary_ipv6
    ?connection_tracking_specification
    ?associate_public_ip_address
    ?(associated_subnet_ids = [])
    ?dry_run
    ~network_interface_id
    ?description
    ?source_dest_check
    ?(groups = [])
    ?attachment
    () =
  { ena_srd_specification
  ; enable_primary_ipv6
  ; connection_tracking_specification
  ; associate_public_ip_address
  ; associated_subnet_ids
  ; dry_run
  ; network_interface_id
  ; description
  ; source_dest_check
  ; groups
  ; attachment
  }

let parse xml =
  Some
    { ena_srd_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "EnaSrdSpecification" xml)
          EnaSrdSpecification.parse
    ; enable_primary_ipv6 =
        Aws.Util.option_bind (Aws.Xml.member "EnablePrimaryIpv6" xml) Boolean.parse
    ; connection_tracking_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "ConnectionTrackingSpecification" xml)
          ConnectionTrackingSpecificationRequest.parse
    ; associate_public_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "AssociatePublicIpAddress" xml) Boolean.parse
    ; associated_subnet_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AssociatedSubnetId" xml)
             SubnetIdList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; network_interface_id =
        Aws.Xml.required
          "networkInterfaceId"
          (Aws.Util.option_bind (Aws.Xml.member "networkInterfaceId" xml) String.parse)
    ; description =
        Aws.Util.option_bind (Aws.Xml.member "description" xml) AttributeValue.parse
    ; source_dest_check =
        Aws.Util.option_bind
          (Aws.Xml.member "sourceDestCheck" xml)
          AttributeBooleanValue.parse
    ; groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroupId" xml)
             SecurityGroupIdStringList.parse)
    ; attachment =
        Aws.Util.option_bind
          (Aws.Xml.member "attachment" xml)
          NetworkInterfaceAttachmentChanges.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.attachment (fun f ->
             Aws.Query.Pair ("Attachment", NetworkInterfaceAttachmentChanges.to_query f))
       ; Some
           (Aws.Query.Pair ("SecurityGroupId", SecurityGroupIdStringList.to_query v.groups))
       ; Aws.Util.option_map v.source_dest_check (fun f ->
             Aws.Query.Pair ("SourceDestCheck", AttributeBooleanValue.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", AttributeValue.to_query f))
       ; Some
           (Aws.Query.Pair ("NetworkInterfaceId", String.to_query v.network_interface_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AssociatedSubnetId", SubnetIdList.to_query v.associated_subnet_ids))
       ; Aws.Util.option_map v.associate_public_ip_address (fun f ->
             Aws.Query.Pair ("AssociatePublicIpAddress", Boolean.to_query f))
       ; Aws.Util.option_map v.connection_tracking_specification (fun f ->
             Aws.Query.Pair
               ( "ConnectionTrackingSpecification"
               , ConnectionTrackingSpecificationRequest.to_query f ))
       ; Aws.Util.option_map v.enable_primary_ipv6 (fun f ->
             Aws.Query.Pair ("EnablePrimaryIpv6", Boolean.to_query f))
       ; Aws.Util.option_map v.ena_srd_specification (fun f ->
             Aws.Query.Pair ("EnaSrdSpecification", EnaSrdSpecification.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.attachment (fun f ->
             "attachment", NetworkInterfaceAttachmentChanges.to_json f)
       ; Some ("SecurityGroupId", SecurityGroupIdStringList.to_json v.groups)
       ; Aws.Util.option_map v.source_dest_check (fun f ->
             "sourceDestCheck", AttributeBooleanValue.to_json f)
       ; Aws.Util.option_map v.description (fun f ->
             "description", AttributeValue.to_json f)
       ; Some ("networkInterfaceId", String.to_json v.network_interface_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some ("AssociatedSubnetId", SubnetIdList.to_json v.associated_subnet_ids)
       ; Aws.Util.option_map v.associate_public_ip_address (fun f ->
             "AssociatePublicIpAddress", Boolean.to_json f)
       ; Aws.Util.option_map v.connection_tracking_specification (fun f ->
             ( "ConnectionTrackingSpecification"
             , ConnectionTrackingSpecificationRequest.to_json f ))
       ; Aws.Util.option_map v.enable_primary_ipv6 (fun f ->
             "EnablePrimaryIpv6", Boolean.to_json f)
       ; Aws.Util.option_map v.ena_srd_specification (fun f ->
             "EnaSrdSpecification", EnaSrdSpecification.to_json f)
       ])

let of_json j =
  { ena_srd_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "EnaSrdSpecification")
        EnaSrdSpecification.of_json
  ; enable_primary_ipv6 =
      Aws.Util.option_map (Aws.Json.lookup j "EnablePrimaryIpv6") Boolean.of_json
  ; connection_tracking_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "ConnectionTrackingSpecification")
        ConnectionTrackingSpecificationRequest.of_json
  ; associate_public_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "AssociatePublicIpAddress") Boolean.of_json
  ; associated_subnet_ids =
      SubnetIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AssociatedSubnetId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; network_interface_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "networkInterfaceId"))
  ; description =
      Aws.Util.option_map (Aws.Json.lookup j "description") AttributeValue.of_json
  ; source_dest_check =
      Aws.Util.option_map
        (Aws.Json.lookup j "sourceDestCheck")
        AttributeBooleanValue.of_json
  ; groups =
      SecurityGroupIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupId"))
  ; attachment =
      Aws.Util.option_map
        (Aws.Json.lookup j "attachment")
        NetworkInterfaceAttachmentChanges.of_json
  }
