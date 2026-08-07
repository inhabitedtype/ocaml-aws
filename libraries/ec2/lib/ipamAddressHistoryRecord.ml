open Aws.BaseTypes

type t =
  { resource_owner_id : String.t option
  ; resource_region : String.t option
  ; resource_type : IpamAddressHistoryResourceType.t option
  ; resource_id : String.t option
  ; resource_cidr : String.t option
  ; resource_name : String.t option
  ; resource_compliance_status : IpamComplianceStatus.t option
  ; resource_overlap_status : IpamOverlapStatus.t option
  ; vpc_id : String.t option
  ; sampled_start_time : DateTime.t option
  ; sampled_end_time : DateTime.t option
  }

let make
    ?resource_owner_id
    ?resource_region
    ?resource_type
    ?resource_id
    ?resource_cidr
    ?resource_name
    ?resource_compliance_status
    ?resource_overlap_status
    ?vpc_id
    ?sampled_start_time
    ?sampled_end_time
    () =
  { resource_owner_id
  ; resource_region
  ; resource_type
  ; resource_id
  ; resource_cidr
  ; resource_name
  ; resource_compliance_status
  ; resource_overlap_status
  ; vpc_id
  ; sampled_start_time
  ; sampled_end_time
  }

let parse xml =
  Some
    { resource_owner_id =
        Aws.Util.option_bind (Aws.Xml.member "resourceOwnerId" xml) String.parse
    ; resource_region =
        Aws.Util.option_bind (Aws.Xml.member "resourceRegion" xml) String.parse
    ; resource_type =
        Aws.Util.option_bind
          (Aws.Xml.member "resourceType" xml)
          IpamAddressHistoryResourceType.parse
    ; resource_id = Aws.Util.option_bind (Aws.Xml.member "resourceId" xml) String.parse
    ; resource_cidr =
        Aws.Util.option_bind (Aws.Xml.member "resourceCidr" xml) String.parse
    ; resource_name =
        Aws.Util.option_bind (Aws.Xml.member "resourceName" xml) String.parse
    ; resource_compliance_status =
        Aws.Util.option_bind
          (Aws.Xml.member "resourceComplianceStatus" xml)
          IpamComplianceStatus.parse
    ; resource_overlap_status =
        Aws.Util.option_bind
          (Aws.Xml.member "resourceOverlapStatus" xml)
          IpamOverlapStatus.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; sampled_start_time =
        Aws.Util.option_bind (Aws.Xml.member "sampledStartTime" xml) DateTime.parse
    ; sampled_end_time =
        Aws.Util.option_bind (Aws.Xml.member "sampledEndTime" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.sampled_end_time (fun f ->
             Aws.Query.Pair ("SampledEndTime", DateTime.to_query f))
       ; Aws.Util.option_map v.sampled_start_time (fun f ->
             Aws.Query.Pair ("SampledStartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.resource_overlap_status (fun f ->
             Aws.Query.Pair ("ResourceOverlapStatus", IpamOverlapStatus.to_query f))
       ; Aws.Util.option_map v.resource_compliance_status (fun f ->
             Aws.Query.Pair ("ResourceComplianceStatus", IpamComplianceStatus.to_query f))
       ; Aws.Util.option_map v.resource_name (fun f ->
             Aws.Query.Pair ("ResourceName", String.to_query f))
       ; Aws.Util.option_map v.resource_cidr (fun f ->
             Aws.Query.Pair ("ResourceCidr", String.to_query f))
       ; Aws.Util.option_map v.resource_id (fun f ->
             Aws.Query.Pair ("ResourceId", String.to_query f))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", IpamAddressHistoryResourceType.to_query f))
       ; Aws.Util.option_map v.resource_region (fun f ->
             Aws.Query.Pair ("ResourceRegion", String.to_query f))
       ; Aws.Util.option_map v.resource_owner_id (fun f ->
             Aws.Query.Pair ("ResourceOwnerId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.sampled_end_time (fun f ->
             "sampledEndTime", DateTime.to_json f)
       ; Aws.Util.option_map v.sampled_start_time (fun f ->
             "sampledStartTime", DateTime.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Aws.Util.option_map v.resource_overlap_status (fun f ->
             "resourceOverlapStatus", IpamOverlapStatus.to_json f)
       ; Aws.Util.option_map v.resource_compliance_status (fun f ->
             "resourceComplianceStatus", IpamComplianceStatus.to_json f)
       ; Aws.Util.option_map v.resource_name (fun f -> "resourceName", String.to_json f)
       ; Aws.Util.option_map v.resource_cidr (fun f -> "resourceCidr", String.to_json f)
       ; Aws.Util.option_map v.resource_id (fun f -> "resourceId", String.to_json f)
       ; Aws.Util.option_map v.resource_type (fun f ->
             "resourceType", IpamAddressHistoryResourceType.to_json f)
       ; Aws.Util.option_map v.resource_region (fun f ->
             "resourceRegion", String.to_json f)
       ; Aws.Util.option_map v.resource_owner_id (fun f ->
             "resourceOwnerId", String.to_json f)
       ])

let of_json j =
  { resource_owner_id =
      Aws.Util.option_map (Aws.Json.lookup j "resourceOwnerId") String.of_json
  ; resource_region =
      Aws.Util.option_map (Aws.Json.lookup j "resourceRegion") String.of_json
  ; resource_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "resourceType")
        IpamAddressHistoryResourceType.of_json
  ; resource_id = Aws.Util.option_map (Aws.Json.lookup j "resourceId") String.of_json
  ; resource_cidr = Aws.Util.option_map (Aws.Json.lookup j "resourceCidr") String.of_json
  ; resource_name = Aws.Util.option_map (Aws.Json.lookup j "resourceName") String.of_json
  ; resource_compliance_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "resourceComplianceStatus")
        IpamComplianceStatus.of_json
  ; resource_overlap_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "resourceOverlapStatus")
        IpamOverlapStatus.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; sampled_start_time =
      Aws.Util.option_map (Aws.Json.lookup j "sampledStartTime") DateTime.of_json
  ; sampled_end_time =
      Aws.Util.option_map (Aws.Json.lookup j "sampledEndTime") DateTime.of_json
  }
