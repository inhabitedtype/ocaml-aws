open Aws.BaseTypes

type t =
  { ipam_id : String.t option
  ; ipam_scope_id : String.t option
  ; ipam_pool_id : String.t option
  ; resource_region : String.t option
  ; resource_owner_id : String.t option
  ; resource_id : String.t option
  ; resource_name : String.t option
  ; resource_cidr : String.t option
  ; resource_type : IpamResourceType.t option
  ; resource_tags : IpamResourceTagList.t
  ; ip_usage : Double.t option
  ; compliance_status : IpamComplianceStatus.t option
  ; management_state : IpamManagementState.t option
  ; overlap_status : IpamOverlapStatus.t option
  ; vpc_id : String.t option
  ; availability_zone_id : String.t option
  }

let make
    ?ipam_id
    ?ipam_scope_id
    ?ipam_pool_id
    ?resource_region
    ?resource_owner_id
    ?resource_id
    ?resource_name
    ?resource_cidr
    ?resource_type
    ?(resource_tags = [])
    ?ip_usage
    ?compliance_status
    ?management_state
    ?overlap_status
    ?vpc_id
    ?availability_zone_id
    () =
  { ipam_id
  ; ipam_scope_id
  ; ipam_pool_id
  ; resource_region
  ; resource_owner_id
  ; resource_id
  ; resource_name
  ; resource_cidr
  ; resource_type
  ; resource_tags
  ; ip_usage
  ; compliance_status
  ; management_state
  ; overlap_status
  ; vpc_id
  ; availability_zone_id
  }

let parse xml =
  Some
    { ipam_id = Aws.Util.option_bind (Aws.Xml.member "ipamId" xml) String.parse
    ; ipam_scope_id = Aws.Util.option_bind (Aws.Xml.member "ipamScopeId" xml) String.parse
    ; ipam_pool_id = Aws.Util.option_bind (Aws.Xml.member "ipamPoolId" xml) String.parse
    ; resource_region =
        Aws.Util.option_bind (Aws.Xml.member "resourceRegion" xml) String.parse
    ; resource_owner_id =
        Aws.Util.option_bind (Aws.Xml.member "resourceOwnerId" xml) String.parse
    ; resource_id = Aws.Util.option_bind (Aws.Xml.member "resourceId" xml) String.parse
    ; resource_name =
        Aws.Util.option_bind (Aws.Xml.member "resourceName" xml) String.parse
    ; resource_cidr =
        Aws.Util.option_bind (Aws.Xml.member "resourceCidr" xml) String.parse
    ; resource_type =
        Aws.Util.option_bind (Aws.Xml.member "resourceType" xml) IpamResourceType.parse
    ; resource_tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "resourceTagSet" xml)
             IpamResourceTagList.parse)
    ; ip_usage = Aws.Util.option_bind (Aws.Xml.member "ipUsage" xml) Double.parse
    ; compliance_status =
        Aws.Util.option_bind
          (Aws.Xml.member "complianceStatus" xml)
          IpamComplianceStatus.parse
    ; management_state =
        Aws.Util.option_bind
          (Aws.Xml.member "managementState" xml)
          IpamManagementState.parse
    ; overlap_status =
        Aws.Util.option_bind (Aws.Xml.member "overlapStatus" xml) IpamOverlapStatus.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.overlap_status (fun f ->
             Aws.Query.Pair ("OverlapStatus", IpamOverlapStatus.to_query f))
       ; Aws.Util.option_map v.management_state (fun f ->
             Aws.Query.Pair ("ManagementState", IpamManagementState.to_query f))
       ; Aws.Util.option_map v.compliance_status (fun f ->
             Aws.Query.Pair ("ComplianceStatus", IpamComplianceStatus.to_query f))
       ; Aws.Util.option_map v.ip_usage (fun f ->
             Aws.Query.Pair ("IpUsage", Double.to_query f))
       ; Some
           (Aws.Query.Pair ("ResourceTagSet", IpamResourceTagList.to_query v.resource_tags))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", IpamResourceType.to_query f))
       ; Aws.Util.option_map v.resource_cidr (fun f ->
             Aws.Query.Pair ("ResourceCidr", String.to_query f))
       ; Aws.Util.option_map v.resource_name (fun f ->
             Aws.Query.Pair ("ResourceName", String.to_query f))
       ; Aws.Util.option_map v.resource_id (fun f ->
             Aws.Query.Pair ("ResourceId", String.to_query f))
       ; Aws.Util.option_map v.resource_owner_id (fun f ->
             Aws.Query.Pair ("ResourceOwnerId", String.to_query f))
       ; Aws.Util.option_map v.resource_region (fun f ->
             Aws.Query.Pair ("ResourceRegion", String.to_query f))
       ; Aws.Util.option_map v.ipam_pool_id (fun f ->
             Aws.Query.Pair ("IpamPoolId", String.to_query f))
       ; Aws.Util.option_map v.ipam_scope_id (fun f ->
             Aws.Query.Pair ("IpamScopeId", String.to_query f))
       ; Aws.Util.option_map v.ipam_id (fun f ->
             Aws.Query.Pair ("IpamId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Aws.Util.option_map v.overlap_status (fun f ->
             "overlapStatus", IpamOverlapStatus.to_json f)
       ; Aws.Util.option_map v.management_state (fun f ->
             "managementState", IpamManagementState.to_json f)
       ; Aws.Util.option_map v.compliance_status (fun f ->
             "complianceStatus", IpamComplianceStatus.to_json f)
       ; Aws.Util.option_map v.ip_usage (fun f -> "ipUsage", Double.to_json f)
       ; Some ("resourceTagSet", IpamResourceTagList.to_json v.resource_tags)
       ; Aws.Util.option_map v.resource_type (fun f ->
             "resourceType", IpamResourceType.to_json f)
       ; Aws.Util.option_map v.resource_cidr (fun f -> "resourceCidr", String.to_json f)
       ; Aws.Util.option_map v.resource_name (fun f -> "resourceName", String.to_json f)
       ; Aws.Util.option_map v.resource_id (fun f -> "resourceId", String.to_json f)
       ; Aws.Util.option_map v.resource_owner_id (fun f ->
             "resourceOwnerId", String.to_json f)
       ; Aws.Util.option_map v.resource_region (fun f ->
             "resourceRegion", String.to_json f)
       ; Aws.Util.option_map v.ipam_pool_id (fun f -> "ipamPoolId", String.to_json f)
       ; Aws.Util.option_map v.ipam_scope_id (fun f -> "ipamScopeId", String.to_json f)
       ; Aws.Util.option_map v.ipam_id (fun f -> "ipamId", String.to_json f)
       ])

let of_json j =
  { ipam_id = Aws.Util.option_map (Aws.Json.lookup j "ipamId") String.of_json
  ; ipam_scope_id = Aws.Util.option_map (Aws.Json.lookup j "ipamScopeId") String.of_json
  ; ipam_pool_id = Aws.Util.option_map (Aws.Json.lookup j "ipamPoolId") String.of_json
  ; resource_region =
      Aws.Util.option_map (Aws.Json.lookup j "resourceRegion") String.of_json
  ; resource_owner_id =
      Aws.Util.option_map (Aws.Json.lookup j "resourceOwnerId") String.of_json
  ; resource_id = Aws.Util.option_map (Aws.Json.lookup j "resourceId") String.of_json
  ; resource_name = Aws.Util.option_map (Aws.Json.lookup j "resourceName") String.of_json
  ; resource_cidr = Aws.Util.option_map (Aws.Json.lookup j "resourceCidr") String.of_json
  ; resource_type =
      Aws.Util.option_map (Aws.Json.lookup j "resourceType") IpamResourceType.of_json
  ; resource_tags =
      IpamResourceTagList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "resourceTagSet"))
  ; ip_usage = Aws.Util.option_map (Aws.Json.lookup j "ipUsage") Double.of_json
  ; compliance_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "complianceStatus")
        IpamComplianceStatus.of_json
  ; management_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "managementState")
        IpamManagementState.of_json
  ; overlap_status =
      Aws.Util.option_map (Aws.Json.lookup j "overlapStatus") IpamOverlapStatus.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  }
