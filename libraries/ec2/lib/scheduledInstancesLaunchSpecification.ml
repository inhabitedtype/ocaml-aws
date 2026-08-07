open Aws.BaseTypes

type t =
  { block_device_mappings : ScheduledInstancesBlockDeviceMappingSet.t
  ; ebs_optimized : Boolean.t option
  ; iam_instance_profile : ScheduledInstancesIamInstanceProfile.t option
  ; image_id : String.t
  ; instance_type : String.t option
  ; kernel_id : String.t option
  ; key_name : String.t option
  ; monitoring : ScheduledInstancesMonitoring.t option
  ; network_interfaces : ScheduledInstancesNetworkInterfaceSet.t
  ; placement : ScheduledInstancesPlacement.t option
  ; ramdisk_id : String.t option
  ; security_group_ids : ScheduledInstancesSecurityGroupIdSet.t
  ; subnet_id : String.t option
  ; user_data : String.t option
  }

let make
    ?(block_device_mappings = [])
    ?ebs_optimized
    ?iam_instance_profile
    ~image_id
    ?instance_type
    ?kernel_id
    ?key_name
    ?monitoring
    ?(network_interfaces = [])
    ?placement
    ?ramdisk_id
    ?(security_group_ids = [])
    ?subnet_id
    ?user_data
    () =
  { block_device_mappings
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
  ; security_group_ids
  ; subnet_id
  ; user_data
  }

let parse xml =
  Some
    { block_device_mappings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "BlockDeviceMapping" xml)
             ScheduledInstancesBlockDeviceMappingSet.parse)
    ; ebs_optimized =
        Aws.Util.option_bind (Aws.Xml.member "EbsOptimized" xml) Boolean.parse
    ; iam_instance_profile =
        Aws.Util.option_bind
          (Aws.Xml.member "IamInstanceProfile" xml)
          ScheduledInstancesIamInstanceProfile.parse
    ; image_id =
        Aws.Xml.required
          "ImageId"
          (Aws.Util.option_bind (Aws.Xml.member "ImageId" xml) String.parse)
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "InstanceType" xml) String.parse
    ; kernel_id = Aws.Util.option_bind (Aws.Xml.member "KernelId" xml) String.parse
    ; key_name = Aws.Util.option_bind (Aws.Xml.member "KeyName" xml) String.parse
    ; monitoring =
        Aws.Util.option_bind
          (Aws.Xml.member "Monitoring" xml)
          ScheduledInstancesMonitoring.parse
    ; network_interfaces =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "NetworkInterface" xml)
             ScheduledInstancesNetworkInterfaceSet.parse)
    ; placement =
        Aws.Util.option_bind
          (Aws.Xml.member "Placement" xml)
          ScheduledInstancesPlacement.parse
    ; ramdisk_id = Aws.Util.option_bind (Aws.Xml.member "RamdiskId" xml) String.parse
    ; security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroupId" xml)
             ScheduledInstancesSecurityGroupIdSet.parse)
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "SubnetId" xml) String.parse
    ; user_data = Aws.Util.option_bind (Aws.Xml.member "UserData" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.user_data (fun f ->
             Aws.Query.Pair ("UserData", String.to_query f))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SecurityGroupId"
              , ScheduledInstancesSecurityGroupIdSet.to_query v.security_group_ids ))
       ; Aws.Util.option_map v.ramdisk_id (fun f ->
             Aws.Query.Pair ("RamdiskId", String.to_query f))
       ; Aws.Util.option_map v.placement (fun f ->
             Aws.Query.Pair ("Placement", ScheduledInstancesPlacement.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "NetworkInterface"
              , ScheduledInstancesNetworkInterfaceSet.to_query v.network_interfaces ))
       ; Aws.Util.option_map v.monitoring (fun f ->
             Aws.Query.Pair ("Monitoring", ScheduledInstancesMonitoring.to_query f))
       ; Aws.Util.option_map v.key_name (fun f ->
             Aws.Query.Pair ("KeyName", String.to_query f))
       ; Aws.Util.option_map v.kernel_id (fun f ->
             Aws.Query.Pair ("KernelId", String.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", String.to_query f))
       ; Some (Aws.Query.Pair ("ImageId", String.to_query v.image_id))
       ; Aws.Util.option_map v.iam_instance_profile (fun f ->
             Aws.Query.Pair
               ("IamInstanceProfile", ScheduledInstancesIamInstanceProfile.to_query f))
       ; Aws.Util.option_map v.ebs_optimized (fun f ->
             Aws.Query.Pair ("EbsOptimized", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "BlockDeviceMapping"
              , ScheduledInstancesBlockDeviceMappingSet.to_query v.block_device_mappings
              ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.user_data (fun f -> "UserData", String.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "SubnetId", String.to_json f)
       ; Some
           ( "SecurityGroupId"
           , ScheduledInstancesSecurityGroupIdSet.to_json v.security_group_ids )
       ; Aws.Util.option_map v.ramdisk_id (fun f -> "RamdiskId", String.to_json f)
       ; Aws.Util.option_map v.placement (fun f ->
             "Placement", ScheduledInstancesPlacement.to_json f)
       ; Some
           ( "NetworkInterface"
           , ScheduledInstancesNetworkInterfaceSet.to_json v.network_interfaces )
       ; Aws.Util.option_map v.monitoring (fun f ->
             "Monitoring", ScheduledInstancesMonitoring.to_json f)
       ; Aws.Util.option_map v.key_name (fun f -> "KeyName", String.to_json f)
       ; Aws.Util.option_map v.kernel_id (fun f -> "KernelId", String.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f -> "InstanceType", String.to_json f)
       ; Some ("ImageId", String.to_json v.image_id)
       ; Aws.Util.option_map v.iam_instance_profile (fun f ->
             "IamInstanceProfile", ScheduledInstancesIamInstanceProfile.to_json f)
       ; Aws.Util.option_map v.ebs_optimized (fun f -> "EbsOptimized", Boolean.to_json f)
       ; Some
           ( "BlockDeviceMapping"
           , ScheduledInstancesBlockDeviceMappingSet.to_json v.block_device_mappings )
       ])

let of_json j =
  { block_device_mappings =
      ScheduledInstancesBlockDeviceMappingSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "BlockDeviceMapping"))
  ; ebs_optimized = Aws.Util.option_map (Aws.Json.lookup j "EbsOptimized") Boolean.of_json
  ; iam_instance_profile =
      Aws.Util.option_map
        (Aws.Json.lookup j "IamInstanceProfile")
        ScheduledInstancesIamInstanceProfile.of_json
  ; image_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ImageId"))
  ; instance_type = Aws.Util.option_map (Aws.Json.lookup j "InstanceType") String.of_json
  ; kernel_id = Aws.Util.option_map (Aws.Json.lookup j "KernelId") String.of_json
  ; key_name = Aws.Util.option_map (Aws.Json.lookup j "KeyName") String.of_json
  ; monitoring =
      Aws.Util.option_map
        (Aws.Json.lookup j "Monitoring")
        ScheduledInstancesMonitoring.of_json
  ; network_interfaces =
      ScheduledInstancesNetworkInterfaceSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "NetworkInterface"))
  ; placement =
      Aws.Util.option_map
        (Aws.Json.lookup j "Placement")
        ScheduledInstancesPlacement.of_json
  ; ramdisk_id = Aws.Util.option_map (Aws.Json.lookup j "RamdiskId") String.of_json
  ; security_group_ids =
      ScheduledInstancesSecurityGroupIdSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupId"))
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "SubnetId") String.of_json
  ; user_data = Aws.Util.option_map (Aws.Json.lookup j "UserData") String.of_json
  }
