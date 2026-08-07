open Aws.BaseTypes

type t =
  { ipam_resource_discovery_id : String.t option
  ; resource_region : String.t option
  ; resource_id : String.t option
  ; resource_owner_id : String.t option
  ; resource_cidr : String.t option
  ; ip_source : IpamResourceCidrIpSource.t option
  ; resource_type : IpamResourceType.t option
  ; resource_tags : IpamResourceTagList.t
  ; ip_usage : Double.t option
  ; vpc_id : String.t option
  ; subnet_id : String.t option
  ; network_interface_attachment_status : IpamNetworkInterfaceAttachmentStatus.t option
  ; sample_time : DateTime.t option
  ; availability_zone_id : String.t option
  }

let make
    ?ipam_resource_discovery_id
    ?resource_region
    ?resource_id
    ?resource_owner_id
    ?resource_cidr
    ?ip_source
    ?resource_type
    ?(resource_tags = [])
    ?ip_usage
    ?vpc_id
    ?subnet_id
    ?network_interface_attachment_status
    ?sample_time
    ?availability_zone_id
    () =
  { ipam_resource_discovery_id
  ; resource_region
  ; resource_id
  ; resource_owner_id
  ; resource_cidr
  ; ip_source
  ; resource_type
  ; resource_tags
  ; ip_usage
  ; vpc_id
  ; subnet_id
  ; network_interface_attachment_status
  ; sample_time
  ; availability_zone_id
  }

let parse xml =
  Some
    { ipam_resource_discovery_id =
        Aws.Util.option_bind (Aws.Xml.member "ipamResourceDiscoveryId" xml) String.parse
    ; resource_region =
        Aws.Util.option_bind (Aws.Xml.member "resourceRegion" xml) String.parse
    ; resource_id = Aws.Util.option_bind (Aws.Xml.member "resourceId" xml) String.parse
    ; resource_owner_id =
        Aws.Util.option_bind (Aws.Xml.member "resourceOwnerId" xml) String.parse
    ; resource_cidr =
        Aws.Util.option_bind (Aws.Xml.member "resourceCidr" xml) String.parse
    ; ip_source =
        Aws.Util.option_bind
          (Aws.Xml.member "ipSource" xml)
          IpamResourceCidrIpSource.parse
    ; resource_type =
        Aws.Util.option_bind (Aws.Xml.member "resourceType" xml) IpamResourceType.parse
    ; resource_tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "resourceTagSet" xml)
             IpamResourceTagList.parse)
    ; ip_usage = Aws.Util.option_bind (Aws.Xml.member "ipUsage" xml) Double.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse
    ; network_interface_attachment_status =
        Aws.Util.option_bind
          (Aws.Xml.member "networkInterfaceAttachmentStatus" xml)
          IpamNetworkInterfaceAttachmentStatus.parse
    ; sample_time = Aws.Util.option_bind (Aws.Xml.member "sampleTime" xml) DateTime.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.sample_time (fun f ->
             Aws.Query.Pair ("SampleTime", DateTime.to_query f))
       ; Aws.Util.option_map v.network_interface_attachment_status (fun f ->
             Aws.Query.Pair
               ( "NetworkInterfaceAttachmentStatus"
               , IpamNetworkInterfaceAttachmentStatus.to_query f ))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.ip_usage (fun f ->
             Aws.Query.Pair ("IpUsage", Double.to_query f))
       ; Some
           (Aws.Query.Pair ("ResourceTagSet", IpamResourceTagList.to_query v.resource_tags))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", IpamResourceType.to_query f))
       ; Aws.Util.option_map v.ip_source (fun f ->
             Aws.Query.Pair ("IpSource", IpamResourceCidrIpSource.to_query f))
       ; Aws.Util.option_map v.resource_cidr (fun f ->
             Aws.Query.Pair ("ResourceCidr", String.to_query f))
       ; Aws.Util.option_map v.resource_owner_id (fun f ->
             Aws.Query.Pair ("ResourceOwnerId", String.to_query f))
       ; Aws.Util.option_map v.resource_id (fun f ->
             Aws.Query.Pair ("ResourceId", String.to_query f))
       ; Aws.Util.option_map v.resource_region (fun f ->
             Aws.Query.Pair ("ResourceRegion", String.to_query f))
       ; Aws.Util.option_map v.ipam_resource_discovery_id (fun f ->
             Aws.Query.Pair ("IpamResourceDiscoveryId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.sample_time (fun f -> "sampleTime", DateTime.to_json f)
       ; Aws.Util.option_map v.network_interface_attachment_status (fun f ->
             ( "networkInterfaceAttachmentStatus"
             , IpamNetworkInterfaceAttachmentStatus.to_json f ))
       ; Aws.Util.option_map v.subnet_id (fun f -> "subnetId", String.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Aws.Util.option_map v.ip_usage (fun f -> "ipUsage", Double.to_json f)
       ; Some ("resourceTagSet", IpamResourceTagList.to_json v.resource_tags)
       ; Aws.Util.option_map v.resource_type (fun f ->
             "resourceType", IpamResourceType.to_json f)
       ; Aws.Util.option_map v.ip_source (fun f ->
             "ipSource", IpamResourceCidrIpSource.to_json f)
       ; Aws.Util.option_map v.resource_cidr (fun f -> "resourceCidr", String.to_json f)
       ; Aws.Util.option_map v.resource_owner_id (fun f ->
             "resourceOwnerId", String.to_json f)
       ; Aws.Util.option_map v.resource_id (fun f -> "resourceId", String.to_json f)
       ; Aws.Util.option_map v.resource_region (fun f ->
             "resourceRegion", String.to_json f)
       ; Aws.Util.option_map v.ipam_resource_discovery_id (fun f ->
             "ipamResourceDiscoveryId", String.to_json f)
       ])

let of_json j =
  { ipam_resource_discovery_id =
      Aws.Util.option_map (Aws.Json.lookup j "ipamResourceDiscoveryId") String.of_json
  ; resource_region =
      Aws.Util.option_map (Aws.Json.lookup j "resourceRegion") String.of_json
  ; resource_id = Aws.Util.option_map (Aws.Json.lookup j "resourceId") String.of_json
  ; resource_owner_id =
      Aws.Util.option_map (Aws.Json.lookup j "resourceOwnerId") String.of_json
  ; resource_cidr = Aws.Util.option_map (Aws.Json.lookup j "resourceCidr") String.of_json
  ; ip_source =
      Aws.Util.option_map (Aws.Json.lookup j "ipSource") IpamResourceCidrIpSource.of_json
  ; resource_type =
      Aws.Util.option_map (Aws.Json.lookup j "resourceType") IpamResourceType.of_json
  ; resource_tags =
      IpamResourceTagList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "resourceTagSet"))
  ; ip_usage = Aws.Util.option_map (Aws.Json.lookup j "ipUsage") Double.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "subnetId") String.of_json
  ; network_interface_attachment_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "networkInterfaceAttachmentStatus")
        IpamNetworkInterfaceAttachmentStatus.of_json
  ; sample_time = Aws.Util.option_map (Aws.Json.lookup j "sampleTime") DateTime.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  }
