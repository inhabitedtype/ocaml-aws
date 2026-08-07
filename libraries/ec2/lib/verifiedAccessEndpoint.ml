open Aws.BaseTypes

type t =
  { verified_access_instance_id : String.t option
  ; verified_access_group_id : String.t option
  ; verified_access_endpoint_id : String.t option
  ; application_domain : String.t option
  ; endpoint_type : VerifiedAccessEndpointType.t option
  ; attachment_type : VerifiedAccessEndpointAttachmentType.t option
  ; domain_certificate_arn : String.t option
  ; endpoint_domain : String.t option
  ; device_validation_domain : String.t option
  ; security_group_ids : SecurityGroupIdList.t
  ; load_balancer_options : VerifiedAccessEndpointLoadBalancerOptions.t option
  ; network_interface_options : VerifiedAccessEndpointEniOptions.t option
  ; status : VerifiedAccessEndpointStatus.t option
  ; description : String.t option
  ; creation_time : String.t option
  ; last_updated_time : String.t option
  ; deletion_time : String.t option
  ; tags : TagList.t
  ; sse_specification : VerifiedAccessSseSpecificationResponse.t option
  ; rds_options : VerifiedAccessEndpointRdsOptions.t option
  ; cidr_options : VerifiedAccessEndpointCidrOptions.t option
  }

let make
    ?verified_access_instance_id
    ?verified_access_group_id
    ?verified_access_endpoint_id
    ?application_domain
    ?endpoint_type
    ?attachment_type
    ?domain_certificate_arn
    ?endpoint_domain
    ?device_validation_domain
    ?(security_group_ids = [])
    ?load_balancer_options
    ?network_interface_options
    ?status
    ?description
    ?creation_time
    ?last_updated_time
    ?deletion_time
    ?(tags = [])
    ?sse_specification
    ?rds_options
    ?cidr_options
    () =
  { verified_access_instance_id
  ; verified_access_group_id
  ; verified_access_endpoint_id
  ; application_domain
  ; endpoint_type
  ; attachment_type
  ; domain_certificate_arn
  ; endpoint_domain
  ; device_validation_domain
  ; security_group_ids
  ; load_balancer_options
  ; network_interface_options
  ; status
  ; description
  ; creation_time
  ; last_updated_time
  ; deletion_time
  ; tags
  ; sse_specification
  ; rds_options
  ; cidr_options
  }

let parse xml =
  Some
    { verified_access_instance_id =
        Aws.Util.option_bind (Aws.Xml.member "verifiedAccessInstanceId" xml) String.parse
    ; verified_access_group_id =
        Aws.Util.option_bind (Aws.Xml.member "verifiedAccessGroupId" xml) String.parse
    ; verified_access_endpoint_id =
        Aws.Util.option_bind (Aws.Xml.member "verifiedAccessEndpointId" xml) String.parse
    ; application_domain =
        Aws.Util.option_bind (Aws.Xml.member "applicationDomain" xml) String.parse
    ; endpoint_type =
        Aws.Util.option_bind
          (Aws.Xml.member "endpointType" xml)
          VerifiedAccessEndpointType.parse
    ; attachment_type =
        Aws.Util.option_bind
          (Aws.Xml.member "attachmentType" xml)
          VerifiedAccessEndpointAttachmentType.parse
    ; domain_certificate_arn =
        Aws.Util.option_bind (Aws.Xml.member "domainCertificateArn" xml) String.parse
    ; endpoint_domain =
        Aws.Util.option_bind (Aws.Xml.member "endpointDomain" xml) String.parse
    ; device_validation_domain =
        Aws.Util.option_bind (Aws.Xml.member "deviceValidationDomain" xml) String.parse
    ; security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "securityGroupIdSet" xml)
             SecurityGroupIdList.parse)
    ; load_balancer_options =
        Aws.Util.option_bind
          (Aws.Xml.member "loadBalancerOptions" xml)
          VerifiedAccessEndpointLoadBalancerOptions.parse
    ; network_interface_options =
        Aws.Util.option_bind
          (Aws.Xml.member "networkInterfaceOptions" xml)
          VerifiedAccessEndpointEniOptions.parse
    ; status =
        Aws.Util.option_bind
          (Aws.Xml.member "status" xml)
          VerifiedAccessEndpointStatus.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; creation_time =
        Aws.Util.option_bind (Aws.Xml.member "creationTime" xml) String.parse
    ; last_updated_time =
        Aws.Util.option_bind (Aws.Xml.member "lastUpdatedTime" xml) String.parse
    ; deletion_time =
        Aws.Util.option_bind (Aws.Xml.member "deletionTime" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; sse_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "sseSpecification" xml)
          VerifiedAccessSseSpecificationResponse.parse
    ; rds_options =
        Aws.Util.option_bind
          (Aws.Xml.member "rdsOptions" xml)
          VerifiedAccessEndpointRdsOptions.parse
    ; cidr_options =
        Aws.Util.option_bind
          (Aws.Xml.member "cidrOptions" xml)
          VerifiedAccessEndpointCidrOptions.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cidr_options (fun f ->
             Aws.Query.Pair ("CidrOptions", VerifiedAccessEndpointCidrOptions.to_query f))
       ; Aws.Util.option_map v.rds_options (fun f ->
             Aws.Query.Pair ("RdsOptions", VerifiedAccessEndpointRdsOptions.to_query f))
       ; Aws.Util.option_map v.sse_specification (fun f ->
             Aws.Query.Pair
               ("SseSpecification", VerifiedAccessSseSpecificationResponse.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.deletion_time (fun f ->
             Aws.Query.Pair ("DeletionTime", String.to_query f))
       ; Aws.Util.option_map v.last_updated_time (fun f ->
             Aws.Query.Pair ("LastUpdatedTime", String.to_query f))
       ; Aws.Util.option_map v.creation_time (fun f ->
             Aws.Query.Pair ("CreationTime", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", VerifiedAccessEndpointStatus.to_query f))
       ; Aws.Util.option_map v.network_interface_options (fun f ->
             Aws.Query.Pair
               ("NetworkInterfaceOptions", VerifiedAccessEndpointEniOptions.to_query f))
       ; Aws.Util.option_map v.load_balancer_options (fun f ->
             Aws.Query.Pair
               ( "LoadBalancerOptions"
               , VerifiedAccessEndpointLoadBalancerOptions.to_query f ))
       ; Some
           (Aws.Query.Pair
              ("SecurityGroupIdSet", SecurityGroupIdList.to_query v.security_group_ids))
       ; Aws.Util.option_map v.device_validation_domain (fun f ->
             Aws.Query.Pair ("DeviceValidationDomain", String.to_query f))
       ; Aws.Util.option_map v.endpoint_domain (fun f ->
             Aws.Query.Pair ("EndpointDomain", String.to_query f))
       ; Aws.Util.option_map v.domain_certificate_arn (fun f ->
             Aws.Query.Pair ("DomainCertificateArn", String.to_query f))
       ; Aws.Util.option_map v.attachment_type (fun f ->
             Aws.Query.Pair
               ("AttachmentType", VerifiedAccessEndpointAttachmentType.to_query f))
       ; Aws.Util.option_map v.endpoint_type (fun f ->
             Aws.Query.Pair ("EndpointType", VerifiedAccessEndpointType.to_query f))
       ; Aws.Util.option_map v.application_domain (fun f ->
             Aws.Query.Pair ("ApplicationDomain", String.to_query f))
       ; Aws.Util.option_map v.verified_access_endpoint_id (fun f ->
             Aws.Query.Pair ("VerifiedAccessEndpointId", String.to_query f))
       ; Aws.Util.option_map v.verified_access_group_id (fun f ->
             Aws.Query.Pair ("VerifiedAccessGroupId", String.to_query f))
       ; Aws.Util.option_map v.verified_access_instance_id (fun f ->
             Aws.Query.Pair ("VerifiedAccessInstanceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cidr_options (fun f ->
             "cidrOptions", VerifiedAccessEndpointCidrOptions.to_json f)
       ; Aws.Util.option_map v.rds_options (fun f ->
             "rdsOptions", VerifiedAccessEndpointRdsOptions.to_json f)
       ; Aws.Util.option_map v.sse_specification (fun f ->
             "sseSpecification", VerifiedAccessSseSpecificationResponse.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.deletion_time (fun f -> "deletionTime", String.to_json f)
       ; Aws.Util.option_map v.last_updated_time (fun f ->
             "lastUpdatedTime", String.to_json f)
       ; Aws.Util.option_map v.creation_time (fun f -> "creationTime", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.status (fun f ->
             "status", VerifiedAccessEndpointStatus.to_json f)
       ; Aws.Util.option_map v.network_interface_options (fun f ->
             "networkInterfaceOptions", VerifiedAccessEndpointEniOptions.to_json f)
       ; Aws.Util.option_map v.load_balancer_options (fun f ->
             "loadBalancerOptions", VerifiedAccessEndpointLoadBalancerOptions.to_json f)
       ; Some ("securityGroupIdSet", SecurityGroupIdList.to_json v.security_group_ids)
       ; Aws.Util.option_map v.device_validation_domain (fun f ->
             "deviceValidationDomain", String.to_json f)
       ; Aws.Util.option_map v.endpoint_domain (fun f ->
             "endpointDomain", String.to_json f)
       ; Aws.Util.option_map v.domain_certificate_arn (fun f ->
             "domainCertificateArn", String.to_json f)
       ; Aws.Util.option_map v.attachment_type (fun f ->
             "attachmentType", VerifiedAccessEndpointAttachmentType.to_json f)
       ; Aws.Util.option_map v.endpoint_type (fun f ->
             "endpointType", VerifiedAccessEndpointType.to_json f)
       ; Aws.Util.option_map v.application_domain (fun f ->
             "applicationDomain", String.to_json f)
       ; Aws.Util.option_map v.verified_access_endpoint_id (fun f ->
             "verifiedAccessEndpointId", String.to_json f)
       ; Aws.Util.option_map v.verified_access_group_id (fun f ->
             "verifiedAccessGroupId", String.to_json f)
       ; Aws.Util.option_map v.verified_access_instance_id (fun f ->
             "verifiedAccessInstanceId", String.to_json f)
       ])

let of_json j =
  { verified_access_instance_id =
      Aws.Util.option_map (Aws.Json.lookup j "verifiedAccessInstanceId") String.of_json
  ; verified_access_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "verifiedAccessGroupId") String.of_json
  ; verified_access_endpoint_id =
      Aws.Util.option_map (Aws.Json.lookup j "verifiedAccessEndpointId") String.of_json
  ; application_domain =
      Aws.Util.option_map (Aws.Json.lookup j "applicationDomain") String.of_json
  ; endpoint_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "endpointType")
        VerifiedAccessEndpointType.of_json
  ; attachment_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "attachmentType")
        VerifiedAccessEndpointAttachmentType.of_json
  ; domain_certificate_arn =
      Aws.Util.option_map (Aws.Json.lookup j "domainCertificateArn") String.of_json
  ; endpoint_domain =
      Aws.Util.option_map (Aws.Json.lookup j "endpointDomain") String.of_json
  ; device_validation_domain =
      Aws.Util.option_map (Aws.Json.lookup j "deviceValidationDomain") String.of_json
  ; security_group_ids =
      SecurityGroupIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "securityGroupIdSet"))
  ; load_balancer_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "loadBalancerOptions")
        VerifiedAccessEndpointLoadBalancerOptions.of_json
  ; network_interface_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "networkInterfaceOptions")
        VerifiedAccessEndpointEniOptions.of_json
  ; status =
      Aws.Util.option_map
        (Aws.Json.lookup j "status")
        VerifiedAccessEndpointStatus.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; creation_time = Aws.Util.option_map (Aws.Json.lookup j "creationTime") String.of_json
  ; last_updated_time =
      Aws.Util.option_map (Aws.Json.lookup j "lastUpdatedTime") String.of_json
  ; deletion_time = Aws.Util.option_map (Aws.Json.lookup j "deletionTime") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; sse_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "sseSpecification")
        VerifiedAccessSseSpecificationResponse.of_json
  ; rds_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "rdsOptions")
        VerifiedAccessEndpointRdsOptions.of_json
  ; cidr_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "cidrOptions")
        VerifiedAccessEndpointCidrOptions.of_json
  }
