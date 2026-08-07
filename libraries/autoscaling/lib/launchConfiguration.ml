open Aws.BaseTypes

type t =
  { launch_configuration_name : String.t
  ; launch_configuration_a_r_n : String.t option
  ; image_id : String.t
  ; key_name : String.t option
  ; security_groups : SecurityGroups.t
  ; classic_link_v_p_c_id : String.t option
  ; classic_link_v_p_c_security_groups : ClassicLinkVPCSecurityGroups.t
  ; user_data : String.t option
  ; instance_type : String.t
  ; kernel_id : String.t option
  ; ramdisk_id : String.t option
  ; block_device_mappings : BlockDeviceMappings.t
  ; instance_monitoring : InstanceMonitoring.t option
  ; spot_price : String.t option
  ; iam_instance_profile : String.t option
  ; created_time : DateTime.t
  ; ebs_optimized : Boolean.t option
  ; associate_public_ip_address : Boolean.t option
  ; placement_tenancy : String.t option
  ; metadata_options : InstanceMetadataOptions.t option
  }

let make
    ~launch_configuration_name
    ?launch_configuration_a_r_n
    ~image_id
    ?key_name
    ?(security_groups = [])
    ?classic_link_v_p_c_id
    ?(classic_link_v_p_c_security_groups = [])
    ?user_data
    ~instance_type
    ?kernel_id
    ?ramdisk_id
    ?(block_device_mappings = [])
    ?instance_monitoring
    ?spot_price
    ?iam_instance_profile
    ~created_time
    ?ebs_optimized
    ?associate_public_ip_address
    ?placement_tenancy
    ?metadata_options
    () =
  { launch_configuration_name
  ; launch_configuration_a_r_n
  ; image_id
  ; key_name
  ; security_groups
  ; classic_link_v_p_c_id
  ; classic_link_v_p_c_security_groups
  ; user_data
  ; instance_type
  ; kernel_id
  ; ramdisk_id
  ; block_device_mappings
  ; instance_monitoring
  ; spot_price
  ; iam_instance_profile
  ; created_time
  ; ebs_optimized
  ; associate_public_ip_address
  ; placement_tenancy
  ; metadata_options
  }

let parse xml =
  Some
    { launch_configuration_name =
        Aws.Xml.required
          "LaunchConfigurationName"
          (Aws.Util.option_bind
             (Aws.Xml.member "LaunchConfigurationName" xml)
             String.parse)
    ; launch_configuration_a_r_n =
        Aws.Util.option_bind (Aws.Xml.member "LaunchConfigurationARN" xml) String.parse
    ; image_id =
        Aws.Xml.required
          "ImageId"
          (Aws.Util.option_bind (Aws.Xml.member "ImageId" xml) String.parse)
    ; key_name = Aws.Util.option_bind (Aws.Xml.member "KeyName" xml) String.parse
    ; security_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroups" xml)
             SecurityGroups.parse)
    ; classic_link_v_p_c_id =
        Aws.Util.option_bind (Aws.Xml.member "ClassicLinkVPCId" xml) String.parse
    ; classic_link_v_p_c_security_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ClassicLinkVPCSecurityGroups" xml)
             ClassicLinkVPCSecurityGroups.parse)
    ; user_data = Aws.Util.option_bind (Aws.Xml.member "UserData" xml) String.parse
    ; instance_type =
        Aws.Xml.required
          "InstanceType"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceType" xml) String.parse)
    ; kernel_id = Aws.Util.option_bind (Aws.Xml.member "KernelId" xml) String.parse
    ; ramdisk_id = Aws.Util.option_bind (Aws.Xml.member "RamdiskId" xml) String.parse
    ; block_device_mappings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "BlockDeviceMappings" xml)
             BlockDeviceMappings.parse)
    ; instance_monitoring =
        Aws.Util.option_bind
          (Aws.Xml.member "InstanceMonitoring" xml)
          InstanceMonitoring.parse
    ; spot_price = Aws.Util.option_bind (Aws.Xml.member "SpotPrice" xml) String.parse
    ; iam_instance_profile =
        Aws.Util.option_bind (Aws.Xml.member "IamInstanceProfile" xml) String.parse
    ; created_time =
        Aws.Xml.required
          "CreatedTime"
          (Aws.Util.option_bind (Aws.Xml.member "CreatedTime" xml) DateTime.parse)
    ; ebs_optimized =
        Aws.Util.option_bind (Aws.Xml.member "EbsOptimized" xml) Boolean.parse
    ; associate_public_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "AssociatePublicIpAddress" xml) Boolean.parse
    ; placement_tenancy =
        Aws.Util.option_bind (Aws.Xml.member "PlacementTenancy" xml) String.parse
    ; metadata_options =
        Aws.Util.option_bind
          (Aws.Xml.member "MetadataOptions" xml)
          InstanceMetadataOptions.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.metadata_options (fun f ->
             Aws.Query.Pair ("MetadataOptions", InstanceMetadataOptions.to_query f))
       ; Aws.Util.option_map v.placement_tenancy (fun f ->
             Aws.Query.Pair ("PlacementTenancy", String.to_query f))
       ; Aws.Util.option_map v.associate_public_ip_address (fun f ->
             Aws.Query.Pair ("AssociatePublicIpAddress", Boolean.to_query f))
       ; Aws.Util.option_map v.ebs_optimized (fun f ->
             Aws.Query.Pair ("EbsOptimized", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("CreatedTime", DateTime.to_query v.created_time))
       ; Aws.Util.option_map v.iam_instance_profile (fun f ->
             Aws.Query.Pair ("IamInstanceProfile", String.to_query f))
       ; Aws.Util.option_map v.spot_price (fun f ->
             Aws.Query.Pair ("SpotPrice", String.to_query f))
       ; Aws.Util.option_map v.instance_monitoring (fun f ->
             Aws.Query.Pair ("InstanceMonitoring", InstanceMonitoring.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "BlockDeviceMappings.member"
              , BlockDeviceMappings.to_query v.block_device_mappings ))
       ; Aws.Util.option_map v.ramdisk_id (fun f ->
             Aws.Query.Pair ("RamdiskId", String.to_query f))
       ; Aws.Util.option_map v.kernel_id (fun f ->
             Aws.Query.Pair ("KernelId", String.to_query f))
       ; Some (Aws.Query.Pair ("InstanceType", String.to_query v.instance_type))
       ; Aws.Util.option_map v.user_data (fun f ->
             Aws.Query.Pair ("UserData", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ClassicLinkVPCSecurityGroups.member"
              , ClassicLinkVPCSecurityGroups.to_query v.classic_link_v_p_c_security_groups
              ))
       ; Aws.Util.option_map v.classic_link_v_p_c_id (fun f ->
             Aws.Query.Pair ("ClassicLinkVPCId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("SecurityGroups.member", SecurityGroups.to_query v.security_groups))
       ; Aws.Util.option_map v.key_name (fun f ->
             Aws.Query.Pair ("KeyName", String.to_query f))
       ; Some (Aws.Query.Pair ("ImageId", String.to_query v.image_id))
       ; Aws.Util.option_map v.launch_configuration_a_r_n (fun f ->
             Aws.Query.Pair ("LaunchConfigurationARN", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("LaunchConfigurationName", String.to_query v.launch_configuration_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.metadata_options (fun f ->
             "MetadataOptions", InstanceMetadataOptions.to_json f)
       ; Aws.Util.option_map v.placement_tenancy (fun f ->
             "PlacementTenancy", String.to_json f)
       ; Aws.Util.option_map v.associate_public_ip_address (fun f ->
             "AssociatePublicIpAddress", Boolean.to_json f)
       ; Aws.Util.option_map v.ebs_optimized (fun f -> "EbsOptimized", Boolean.to_json f)
       ; Some ("CreatedTime", DateTime.to_json v.created_time)
       ; Aws.Util.option_map v.iam_instance_profile (fun f ->
             "IamInstanceProfile", String.to_json f)
       ; Aws.Util.option_map v.spot_price (fun f -> "SpotPrice", String.to_json f)
       ; Aws.Util.option_map v.instance_monitoring (fun f ->
             "InstanceMonitoring", InstanceMonitoring.to_json f)
       ; Some ("BlockDeviceMappings", BlockDeviceMappings.to_json v.block_device_mappings)
       ; Aws.Util.option_map v.ramdisk_id (fun f -> "RamdiskId", String.to_json f)
       ; Aws.Util.option_map v.kernel_id (fun f -> "KernelId", String.to_json f)
       ; Some ("InstanceType", String.to_json v.instance_type)
       ; Aws.Util.option_map v.user_data (fun f -> "UserData", String.to_json f)
       ; Some
           ( "ClassicLinkVPCSecurityGroups"
           , ClassicLinkVPCSecurityGroups.to_json v.classic_link_v_p_c_security_groups )
       ; Aws.Util.option_map v.classic_link_v_p_c_id (fun f ->
             "ClassicLinkVPCId", String.to_json f)
       ; Some ("SecurityGroups", SecurityGroups.to_json v.security_groups)
       ; Aws.Util.option_map v.key_name (fun f -> "KeyName", String.to_json f)
       ; Some ("ImageId", String.to_json v.image_id)
       ; Aws.Util.option_map v.launch_configuration_a_r_n (fun f ->
             "LaunchConfigurationARN", String.to_json f)
       ; Some ("LaunchConfigurationName", String.to_json v.launch_configuration_name)
       ])

let of_json j =
  { launch_configuration_name =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LaunchConfigurationName"))
  ; launch_configuration_a_r_n =
      Aws.Util.option_map (Aws.Json.lookup j "LaunchConfigurationARN") String.of_json
  ; image_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ImageId"))
  ; key_name = Aws.Util.option_map (Aws.Json.lookup j "KeyName") String.of_json
  ; security_groups =
      SecurityGroups.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroups"))
  ; classic_link_v_p_c_id =
      Aws.Util.option_map (Aws.Json.lookup j "ClassicLinkVPCId") String.of_json
  ; classic_link_v_p_c_security_groups =
      ClassicLinkVPCSecurityGroups.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ClassicLinkVPCSecurityGroups"))
  ; user_data = Aws.Util.option_map (Aws.Json.lookup j "UserData") String.of_json
  ; instance_type =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceType"))
  ; kernel_id = Aws.Util.option_map (Aws.Json.lookup j "KernelId") String.of_json
  ; ramdisk_id = Aws.Util.option_map (Aws.Json.lookup j "RamdiskId") String.of_json
  ; block_device_mappings =
      BlockDeviceMappings.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "BlockDeviceMappings"))
  ; instance_monitoring =
      Aws.Util.option_map
        (Aws.Json.lookup j "InstanceMonitoring")
        InstanceMonitoring.of_json
  ; spot_price = Aws.Util.option_map (Aws.Json.lookup j "SpotPrice") String.of_json
  ; iam_instance_profile =
      Aws.Util.option_map (Aws.Json.lookup j "IamInstanceProfile") String.of_json
  ; created_time =
      DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CreatedTime"))
  ; ebs_optimized = Aws.Util.option_map (Aws.Json.lookup j "EbsOptimized") Boolean.of_json
  ; associate_public_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "AssociatePublicIpAddress") Boolean.of_json
  ; placement_tenancy =
      Aws.Util.option_map (Aws.Json.lookup j "PlacementTenancy") String.of_json
  ; metadata_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "MetadataOptions")
        InstanceMetadataOptions.of_json
  }
