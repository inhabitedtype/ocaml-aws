open Aws.BaseTypes

type t =
  { security_group_ids : RequestSpotLaunchSpecificationSecurityGroupIdList.t
  ; security_groups : RequestSpotLaunchSpecificationSecurityGroupList.t
  ; addressing_type : String.t option
  ; block_device_mappings : BlockDeviceMappingList.t
  ; ebs_optimized : Boolean.t option
  ; iam_instance_profile : IamInstanceProfileSpecification.t option
  ; image_id : String.t option
  ; instance_type : InstanceType.t option
  ; kernel_id : String.t option
  ; key_name : String.t option
  ; monitoring : RunInstancesMonitoringEnabled.t option
  ; network_interfaces : InstanceNetworkInterfaceSpecificationList.t
  ; placement : SpotPlacement.t option
  ; ramdisk_id : String.t option
  ; subnet_id : String.t option
  ; user_data : String.t option
  }

let make
    ?(security_group_ids = [])
    ?(security_groups = [])
    ?addressing_type
    ?(block_device_mappings = [])
    ?ebs_optimized
    ?iam_instance_profile
    ?image_id
    ?instance_type
    ?kernel_id
    ?key_name
    ?monitoring
    ?(network_interfaces = [])
    ?placement
    ?ramdisk_id
    ?subnet_id
    ?user_data
    () =
  { security_group_ids
  ; security_groups
  ; addressing_type
  ; block_device_mappings
  ; ebs_optimized
  ; iam_instance_profile
  ; image_id
  ; instance_type
  ; kernel_id
  ; key_name
  ; monitoring
  ; network_interfaces
  ; placement
  ; ramdisk_id
  ; subnet_id
  ; user_data
  }

let parse xml =
  Some
    { security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroupId" xml)
             RequestSpotLaunchSpecificationSecurityGroupIdList.parse)
    ; security_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroup" xml)
             RequestSpotLaunchSpecificationSecurityGroupList.parse)
    ; addressing_type =
        Aws.Util.option_bind (Aws.Xml.member "addressingType" xml) String.parse
    ; block_device_mappings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "blockDeviceMapping" xml)
             BlockDeviceMappingList.parse)
    ; ebs_optimized =
        Aws.Util.option_bind (Aws.Xml.member "ebsOptimized" xml) Boolean.parse
    ; iam_instance_profile =
        Aws.Util.option_bind
          (Aws.Xml.member "iamInstanceProfile" xml)
          IamInstanceProfileSpecification.parse
    ; image_id = Aws.Util.option_bind (Aws.Xml.member "imageId" xml) String.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) InstanceType.parse
    ; kernel_id = Aws.Util.option_bind (Aws.Xml.member "kernelId" xml) String.parse
    ; key_name = Aws.Util.option_bind (Aws.Xml.member "keyName" xml) String.parse
    ; monitoring =
        Aws.Util.option_bind
          (Aws.Xml.member "monitoring" xml)
          RunInstancesMonitoringEnabled.parse
    ; network_interfaces =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "NetworkInterface" xml)
             InstanceNetworkInterfaceSpecificationList.parse)
    ; placement =
        Aws.Util.option_bind (Aws.Xml.member "placement" xml) SpotPlacement.parse
    ; ramdisk_id = Aws.Util.option_bind (Aws.Xml.member "ramdiskId" xml) String.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse
    ; user_data = Aws.Util.option_bind (Aws.Xml.member "userData" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.user_data (fun f ->
             Aws.Query.Pair ("UserData", String.to_query f))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.ramdisk_id (fun f ->
             Aws.Query.Pair ("RamdiskId", String.to_query f))
       ; Aws.Util.option_map v.placement (fun f ->
             Aws.Query.Pair ("Placement", SpotPlacement.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "NetworkInterface"
              , InstanceNetworkInterfaceSpecificationList.to_query v.network_interfaces ))
       ; Aws.Util.option_map v.monitoring (fun f ->
             Aws.Query.Pair ("Monitoring", RunInstancesMonitoringEnabled.to_query f))
       ; Aws.Util.option_map v.key_name (fun f ->
             Aws.Query.Pair ("KeyName", String.to_query f))
       ; Aws.Util.option_map v.kernel_id (fun f ->
             Aws.Query.Pair ("KernelId", String.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", InstanceType.to_query f))
       ; Aws.Util.option_map v.image_id (fun f ->
             Aws.Query.Pair ("ImageId", String.to_query f))
       ; Aws.Util.option_map v.iam_instance_profile (fun f ->
             Aws.Query.Pair
               ("IamInstanceProfile", IamInstanceProfileSpecification.to_query f))
       ; Aws.Util.option_map v.ebs_optimized (fun f ->
             Aws.Query.Pair ("EbsOptimized", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "BlockDeviceMapping"
              , BlockDeviceMappingList.to_query v.block_device_mappings ))
       ; Aws.Util.option_map v.addressing_type (fun f ->
             Aws.Query.Pair ("AddressingType", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SecurityGroup"
              , RequestSpotLaunchSpecificationSecurityGroupList.to_query v.security_groups
              ))
       ; Some
           (Aws.Query.Pair
              ( "SecurityGroupId"
              , RequestSpotLaunchSpecificationSecurityGroupIdList.to_query
                  v.security_group_ids ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.user_data (fun f -> "userData", String.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "subnetId", String.to_json f)
       ; Aws.Util.option_map v.ramdisk_id (fun f -> "ramdiskId", String.to_json f)
       ; Aws.Util.option_map v.placement (fun f -> "placement", SpotPlacement.to_json f)
       ; Some
           ( "NetworkInterface"
           , InstanceNetworkInterfaceSpecificationList.to_json v.network_interfaces )
       ; Aws.Util.option_map v.monitoring (fun f ->
             "monitoring", RunInstancesMonitoringEnabled.to_json f)
       ; Aws.Util.option_map v.key_name (fun f -> "keyName", String.to_json f)
       ; Aws.Util.option_map v.kernel_id (fun f -> "kernelId", String.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f ->
             "instanceType", InstanceType.to_json f)
       ; Aws.Util.option_map v.image_id (fun f -> "imageId", String.to_json f)
       ; Aws.Util.option_map v.iam_instance_profile (fun f ->
             "iamInstanceProfile", IamInstanceProfileSpecification.to_json f)
       ; Aws.Util.option_map v.ebs_optimized (fun f -> "ebsOptimized", Boolean.to_json f)
       ; Some
           ("blockDeviceMapping", BlockDeviceMappingList.to_json v.block_device_mappings)
       ; Aws.Util.option_map v.addressing_type (fun f ->
             "addressingType", String.to_json f)
       ; Some
           ( "SecurityGroup"
           , RequestSpotLaunchSpecificationSecurityGroupList.to_json v.security_groups )
       ; Some
           ( "SecurityGroupId"
           , RequestSpotLaunchSpecificationSecurityGroupIdList.to_json
               v.security_group_ids )
       ])

let of_json j =
  { security_group_ids =
      RequestSpotLaunchSpecificationSecurityGroupIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupId"))
  ; security_groups =
      RequestSpotLaunchSpecificationSecurityGroupList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroup"))
  ; addressing_type =
      Aws.Util.option_map (Aws.Json.lookup j "addressingType") String.of_json
  ; block_device_mappings =
      BlockDeviceMappingList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "blockDeviceMapping"))
  ; ebs_optimized = Aws.Util.option_map (Aws.Json.lookup j "ebsOptimized") Boolean.of_json
  ; iam_instance_profile =
      Aws.Util.option_map
        (Aws.Json.lookup j "iamInstanceProfile")
        IamInstanceProfileSpecification.of_json
  ; image_id = Aws.Util.option_map (Aws.Json.lookup j "imageId") String.of_json
  ; instance_type =
      Aws.Util.option_map (Aws.Json.lookup j "instanceType") InstanceType.of_json
  ; kernel_id = Aws.Util.option_map (Aws.Json.lookup j "kernelId") String.of_json
  ; key_name = Aws.Util.option_map (Aws.Json.lookup j "keyName") String.of_json
  ; monitoring =
      Aws.Util.option_map
        (Aws.Json.lookup j "monitoring")
        RunInstancesMonitoringEnabled.of_json
  ; network_interfaces =
      InstanceNetworkInterfaceSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "NetworkInterface"))
  ; placement = Aws.Util.option_map (Aws.Json.lookup j "placement") SpotPlacement.of_json
  ; ramdisk_id = Aws.Util.option_map (Aws.Json.lookup j "ramdiskId") String.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "subnetId") String.of_json
  ; user_data = Aws.Util.option_map (Aws.Json.lookup j "userData") String.of_json
  }
