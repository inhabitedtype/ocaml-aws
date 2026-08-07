open Aws.BaseTypes

type t =
  { verified_access_group_id : String.t
  ; endpoint_type : VerifiedAccessEndpointType.t
  ; attachment_type : VerifiedAccessEndpointAttachmentType.t
  ; domain_certificate_arn : String.t option
  ; application_domain : String.t option
  ; endpoint_domain_prefix : String.t option
  ; security_group_ids : SecurityGroupIdList.t
  ; load_balancer_options : CreateVerifiedAccessEndpointLoadBalancerOptions.t option
  ; network_interface_options : CreateVerifiedAccessEndpointEniOptions.t option
  ; description : String.t option
  ; policy_document : String.t option
  ; tag_specifications : TagSpecificationList.t
  ; client_token : String.t option
  ; dry_run : Boolean.t option
  ; sse_specification : VerifiedAccessSseSpecificationRequest.t option
  ; rds_options : CreateVerifiedAccessEndpointRdsOptions.t option
  ; cidr_options : CreateVerifiedAccessEndpointCidrOptions.t option
  }

let make
    ~verified_access_group_id
    ~endpoint_type
    ~attachment_type
    ?domain_certificate_arn
    ?application_domain
    ?endpoint_domain_prefix
    ?(security_group_ids = [])
    ?load_balancer_options
    ?network_interface_options
    ?description
    ?policy_document
    ?(tag_specifications = [])
    ?client_token
    ?dry_run
    ?sse_specification
    ?rds_options
    ?cidr_options
    () =
  { verified_access_group_id
  ; endpoint_type
  ; attachment_type
  ; domain_certificate_arn
  ; application_domain
  ; endpoint_domain_prefix
  ; security_group_ids
  ; load_balancer_options
  ; network_interface_options
  ; description
  ; policy_document
  ; tag_specifications
  ; client_token
  ; dry_run
  ; sse_specification
  ; rds_options
  ; cidr_options
  }

let parse xml =
  Some
    { verified_access_group_id =
        Aws.Xml.required
          "VerifiedAccessGroupId"
          (Aws.Util.option_bind (Aws.Xml.member "VerifiedAccessGroupId" xml) String.parse)
    ; endpoint_type =
        Aws.Xml.required
          "EndpointType"
          (Aws.Util.option_bind
             (Aws.Xml.member "EndpointType" xml)
             VerifiedAccessEndpointType.parse)
    ; attachment_type =
        Aws.Xml.required
          "AttachmentType"
          (Aws.Util.option_bind
             (Aws.Xml.member "AttachmentType" xml)
             VerifiedAccessEndpointAttachmentType.parse)
    ; domain_certificate_arn =
        Aws.Util.option_bind (Aws.Xml.member "DomainCertificateArn" xml) String.parse
    ; application_domain =
        Aws.Util.option_bind (Aws.Xml.member "ApplicationDomain" xml) String.parse
    ; endpoint_domain_prefix =
        Aws.Util.option_bind (Aws.Xml.member "EndpointDomainPrefix" xml) String.parse
    ; security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroupId" xml)
             SecurityGroupIdList.parse)
    ; load_balancer_options =
        Aws.Util.option_bind
          (Aws.Xml.member "LoadBalancerOptions" xml)
          CreateVerifiedAccessEndpointLoadBalancerOptions.parse
    ; network_interface_options =
        Aws.Util.option_bind
          (Aws.Xml.member "NetworkInterfaceOptions" xml)
          CreateVerifiedAccessEndpointEniOptions.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; policy_document =
        Aws.Util.option_bind (Aws.Xml.member "PolicyDocument" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; sse_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "SseSpecification" xml)
          VerifiedAccessSseSpecificationRequest.parse
    ; rds_options =
        Aws.Util.option_bind
          (Aws.Xml.member "RdsOptions" xml)
          CreateVerifiedAccessEndpointRdsOptions.parse
    ; cidr_options =
        Aws.Util.option_bind
          (Aws.Xml.member "CidrOptions" xml)
          CreateVerifiedAccessEndpointCidrOptions.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cidr_options (fun f ->
             Aws.Query.Pair
               ("CidrOptions", CreateVerifiedAccessEndpointCidrOptions.to_query f))
       ; Aws.Util.option_map v.rds_options (fun f ->
             Aws.Query.Pair
               ("RdsOptions", CreateVerifiedAccessEndpointRdsOptions.to_query f))
       ; Aws.Util.option_map v.sse_specification (fun f ->
             Aws.Query.Pair
               ("SseSpecification", VerifiedAccessSseSpecificationRequest.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.policy_document (fun f ->
             Aws.Query.Pair ("PolicyDocument", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.network_interface_options (fun f ->
             Aws.Query.Pair
               ( "NetworkInterfaceOptions"
               , CreateVerifiedAccessEndpointEniOptions.to_query f ))
       ; Aws.Util.option_map v.load_balancer_options (fun f ->
             Aws.Query.Pair
               ( "LoadBalancerOptions"
               , CreateVerifiedAccessEndpointLoadBalancerOptions.to_query f ))
       ; Some
           (Aws.Query.Pair
              ("SecurityGroupId", SecurityGroupIdList.to_query v.security_group_ids))
       ; Aws.Util.option_map v.endpoint_domain_prefix (fun f ->
             Aws.Query.Pair ("EndpointDomainPrefix", String.to_query f))
       ; Aws.Util.option_map v.application_domain (fun f ->
             Aws.Query.Pair ("ApplicationDomain", String.to_query f))
       ; Aws.Util.option_map v.domain_certificate_arn (fun f ->
             Aws.Query.Pair ("DomainCertificateArn", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "AttachmentType"
              , VerifiedAccessEndpointAttachmentType.to_query v.attachment_type ))
       ; Some
           (Aws.Query.Pair
              ("EndpointType", VerifiedAccessEndpointType.to_query v.endpoint_type))
       ; Some
           (Aws.Query.Pair
              ("VerifiedAccessGroupId", String.to_query v.verified_access_group_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cidr_options (fun f ->
             "CidrOptions", CreateVerifiedAccessEndpointCidrOptions.to_json f)
       ; Aws.Util.option_map v.rds_options (fun f ->
             "RdsOptions", CreateVerifiedAccessEndpointRdsOptions.to_json f)
       ; Aws.Util.option_map v.sse_specification (fun f ->
             "SseSpecification", VerifiedAccessSseSpecificationRequest.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.policy_document (fun f ->
             "PolicyDocument", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.network_interface_options (fun f ->
             "NetworkInterfaceOptions", CreateVerifiedAccessEndpointEniOptions.to_json f)
       ; Aws.Util.option_map v.load_balancer_options (fun f ->
             ( "LoadBalancerOptions"
             , CreateVerifiedAccessEndpointLoadBalancerOptions.to_json f ))
       ; Some ("SecurityGroupId", SecurityGroupIdList.to_json v.security_group_ids)
       ; Aws.Util.option_map v.endpoint_domain_prefix (fun f ->
             "EndpointDomainPrefix", String.to_json f)
       ; Aws.Util.option_map v.application_domain (fun f ->
             "ApplicationDomain", String.to_json f)
       ; Aws.Util.option_map v.domain_certificate_arn (fun f ->
             "DomainCertificateArn", String.to_json f)
       ; Some
           ( "AttachmentType"
           , VerifiedAccessEndpointAttachmentType.to_json v.attachment_type )
       ; Some ("EndpointType", VerifiedAccessEndpointType.to_json v.endpoint_type)
       ; Some ("VerifiedAccessGroupId", String.to_json v.verified_access_group_id)
       ])

let of_json j =
  { verified_access_group_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VerifiedAccessGroupId"))
  ; endpoint_type =
      VerifiedAccessEndpointType.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "EndpointType"))
  ; attachment_type =
      VerifiedAccessEndpointAttachmentType.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AttachmentType"))
  ; domain_certificate_arn =
      Aws.Util.option_map (Aws.Json.lookup j "DomainCertificateArn") String.of_json
  ; application_domain =
      Aws.Util.option_map (Aws.Json.lookup j "ApplicationDomain") String.of_json
  ; endpoint_domain_prefix =
      Aws.Util.option_map (Aws.Json.lookup j "EndpointDomainPrefix") String.of_json
  ; security_group_ids =
      SecurityGroupIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupId"))
  ; load_balancer_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "LoadBalancerOptions")
        CreateVerifiedAccessEndpointLoadBalancerOptions.of_json
  ; network_interface_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "NetworkInterfaceOptions")
        CreateVerifiedAccessEndpointEniOptions.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; policy_document =
      Aws.Util.option_map (Aws.Json.lookup j "PolicyDocument") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; sse_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "SseSpecification")
        VerifiedAccessSseSpecificationRequest.of_json
  ; rds_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "RdsOptions")
        CreateVerifiedAccessEndpointRdsOptions.of_json
  ; cidr_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "CidrOptions")
        CreateVerifiedAccessEndpointCidrOptions.of_json
  }
