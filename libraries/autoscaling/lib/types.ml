open Aws.BaseTypes

type calendar = CalendarLib.Calendar.t

module Values = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module Filter = struct
  type t =
    { name : String.t
    ; values : Values.t
    }

  let make ~name ?(values = []) () = { name; values }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      ; values =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Values" xml) Values.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Values.member", Values.to_query v.values))
         ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Values", Values.to_json v.values)
         ; Some ("Name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    ; values = Values.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Values"))
    }
end

module ProcessNames = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module ResourceContentionFault = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module SecurityGroups = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module InstanceMonitoring = struct
  type t = { enabled : Boolean.t option }

  let make ?enabled () = { enabled }

  let parse xml =
    Some { enabled = Aws.Util.option_bind (Aws.Xml.member "Enabled" xml) Boolean.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.enabled (fun f ->
               Aws.Query.Pair ("Enabled", Boolean.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.enabled (fun f -> "Enabled", Boolean.to_json f) ])

  let of_json j =
    { enabled = Aws.Util.option_map (Aws.Json.lookup j "Enabled") Boolean.of_json }
end

module ClassicLinkVPCSecurityGroups = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module Ebs = struct
  type t =
    { snapshot_id : String.t option
    ; volume_size : Integer.t option
    ; volume_type : String.t option
    ; delete_on_termination : Boolean.t option
    ; iops : Integer.t option
    }

  let make ?snapshot_id ?volume_size ?volume_type ?delete_on_termination ?iops () =
    { snapshot_id; volume_size; volume_type; delete_on_termination; iops }

  let parse xml =
    Some
      { snapshot_id = Aws.Util.option_bind (Aws.Xml.member "SnapshotId" xml) String.parse
      ; volume_size = Aws.Util.option_bind (Aws.Xml.member "VolumeSize" xml) Integer.parse
      ; volume_type = Aws.Util.option_bind (Aws.Xml.member "VolumeType" xml) String.parse
      ; delete_on_termination =
          Aws.Util.option_bind (Aws.Xml.member "DeleteOnTermination" xml) Boolean.parse
      ; iops = Aws.Util.option_bind (Aws.Xml.member "Iops" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.iops (fun f ->
               Aws.Query.Pair ("Iops", Integer.to_query f))
         ; Aws.Util.option_map v.delete_on_termination (fun f ->
               Aws.Query.Pair ("DeleteOnTermination", Boolean.to_query f))
         ; Aws.Util.option_map v.volume_type (fun f ->
               Aws.Query.Pair ("VolumeType", String.to_query f))
         ; Aws.Util.option_map v.volume_size (fun f ->
               Aws.Query.Pair ("VolumeSize", Integer.to_query f))
         ; Aws.Util.option_map v.snapshot_id (fun f ->
               Aws.Query.Pair ("SnapshotId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.iops (fun f -> "Iops", Integer.to_json f)
         ; Aws.Util.option_map v.delete_on_termination (fun f ->
               "DeleteOnTermination", Boolean.to_json f)
         ; Aws.Util.option_map v.volume_type (fun f -> "VolumeType", String.to_json f)
         ; Aws.Util.option_map v.volume_size (fun f -> "VolumeSize", Integer.to_json f)
         ; Aws.Util.option_map v.snapshot_id (fun f -> "SnapshotId", String.to_json f)
         ])

  let of_json j =
    { snapshot_id = Aws.Util.option_map (Aws.Json.lookup j "SnapshotId") String.of_json
    ; volume_size = Aws.Util.option_map (Aws.Json.lookup j "VolumeSize") Integer.of_json
    ; volume_type = Aws.Util.option_map (Aws.Json.lookup j "VolumeType") String.of_json
    ; delete_on_termination =
        Aws.Util.option_map (Aws.Json.lookup j "DeleteOnTermination") Boolean.of_json
    ; iops = Aws.Util.option_map (Aws.Json.lookup j "Iops") Integer.of_json
    }
end

module BlockDeviceMapping = struct
  type t =
    { virtual_name : String.t option
    ; device_name : String.t
    ; ebs : Ebs.t option
    ; no_device : Boolean.t option
    }

  let make ?virtual_name ~device_name ?ebs ?no_device () =
    { virtual_name; device_name; ebs; no_device }

  let parse xml =
    Some
      { virtual_name =
          Aws.Util.option_bind (Aws.Xml.member "VirtualName" xml) String.parse
      ; device_name =
          Aws.Xml.required
            "DeviceName"
            (Aws.Util.option_bind (Aws.Xml.member "DeviceName" xml) String.parse)
      ; ebs = Aws.Util.option_bind (Aws.Xml.member "Ebs" xml) Ebs.parse
      ; no_device = Aws.Util.option_bind (Aws.Xml.member "NoDevice" xml) Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.no_device (fun f ->
               Aws.Query.Pair ("NoDevice", Boolean.to_query f))
         ; Aws.Util.option_map v.ebs (fun f -> Aws.Query.Pair ("Ebs", Ebs.to_query f))
         ; Some (Aws.Query.Pair ("DeviceName", String.to_query v.device_name))
         ; Aws.Util.option_map v.virtual_name (fun f ->
               Aws.Query.Pair ("VirtualName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.no_device (fun f -> "NoDevice", Boolean.to_json f)
         ; Aws.Util.option_map v.ebs (fun f -> "Ebs", Ebs.to_json f)
         ; Some ("DeviceName", String.to_json v.device_name)
         ; Aws.Util.option_map v.virtual_name (fun f -> "VirtualName", String.to_json f)
         ])

  let of_json j =
    { virtual_name = Aws.Util.option_map (Aws.Json.lookup j "VirtualName") String.of_json
    ; device_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DeviceName"))
    ; ebs = Aws.Util.option_map (Aws.Json.lookup j "Ebs") Ebs.of_json
    ; no_device = Aws.Util.option_map (Aws.Json.lookup j "NoDevice") Boolean.of_json
    }
end

module BlockDeviceMappings = struct
  type t = BlockDeviceMapping.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map BlockDeviceMapping.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list BlockDeviceMapping.to_query v

  let to_json v = `List (List.map BlockDeviceMapping.to_json v)

  let of_json j = Aws.Json.to_list BlockDeviceMapping.of_json j
end

module LaunchConfiguration = struct
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
          Aws.Util.option_bind
            (Aws.Xml.member "AssociatePublicIpAddress" xml)
            Boolean.parse
      ; placement_tenancy =
          Aws.Util.option_bind (Aws.Xml.member "PlacementTenancy" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.placement_tenancy (fun f ->
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
                , ClassicLinkVPCSecurityGroups.to_query
                    v.classic_link_v_p_c_security_groups ))
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
         [ Aws.Util.option_map v.placement_tenancy (fun f ->
               "PlacementTenancy", String.to_json f)
         ; Aws.Util.option_map v.associate_public_ip_address (fun f ->
               "AssociatePublicIpAddress", Boolean.to_json f)
         ; Aws.Util.option_map v.ebs_optimized (fun f ->
               "EbsOptimized", Boolean.to_json f)
         ; Some ("CreatedTime", DateTime.to_json v.created_time)
         ; Aws.Util.option_map v.iam_instance_profile (fun f ->
               "IamInstanceProfile", String.to_json f)
         ; Aws.Util.option_map v.spot_price (fun f -> "SpotPrice", String.to_json f)
         ; Aws.Util.option_map v.instance_monitoring (fun f ->
               "InstanceMonitoring", InstanceMonitoring.to_json f)
         ; Some
             ("BlockDeviceMappings", BlockDeviceMappings.to_json v.block_device_mappings)
         ; Aws.Util.option_map v.ramdisk_id (fun f -> "RamdiskId", String.to_json f)
         ; Aws.Util.option_map v.kernel_id (fun f -> "KernelId", String.to_json f)
         ; Some ("InstanceType", String.to_json v.instance_type)
         ; Aws.Util.option_map v.user_data (fun f -> "UserData", String.to_json f)
         ; Some
             ( "ClassicLinkVPCSecurityGroups"
             , ClassicLinkVPCSecurityGroups.to_json v.classic_link_v_p_c_security_groups
             )
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
        SecurityGroups.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroups"))
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
    ; ebs_optimized =
        Aws.Util.option_map (Aws.Json.lookup j "EbsOptimized") Boolean.of_json
    ; associate_public_ip_address =
        Aws.Util.option_map (Aws.Json.lookup j "AssociatePublicIpAddress") Boolean.of_json
    ; placement_tenancy =
        Aws.Util.option_map (Aws.Json.lookup j "PlacementTenancy") String.of_json
    }
end

module LaunchConfigurations = struct
  type t = LaunchConfiguration.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map LaunchConfiguration.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list LaunchConfiguration.to_query v

  let to_json v = `List (List.map LaunchConfiguration.to_json v)

  let of_json j = Aws.Json.to_list LaunchConfiguration.of_json j
end

module NotificationConfiguration = struct
  type t =
    { auto_scaling_group_name : String.t option
    ; topic_a_r_n : String.t option
    ; notification_type : String.t option
    }

  let make ?auto_scaling_group_name ?topic_a_r_n ?notification_type () =
    { auto_scaling_group_name; topic_a_r_n; notification_type }

  let parse xml =
    Some
      { auto_scaling_group_name =
          Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse
      ; topic_a_r_n = Aws.Util.option_bind (Aws.Xml.member "TopicARN" xml) String.parse
      ; notification_type =
          Aws.Util.option_bind (Aws.Xml.member "NotificationType" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.notification_type (fun f ->
               Aws.Query.Pair ("NotificationType", String.to_query f))
         ; Aws.Util.option_map v.topic_a_r_n (fun f ->
               Aws.Query.Pair ("TopicARN", String.to_query f))
         ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
               Aws.Query.Pair ("AutoScalingGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.notification_type (fun f ->
               "NotificationType", String.to_json f)
         ; Aws.Util.option_map v.topic_a_r_n (fun f -> "TopicARN", String.to_json f)
         ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
               "AutoScalingGroupName", String.to_json f)
         ])

  let of_json j =
    { auto_scaling_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "AutoScalingGroupName") String.of_json
    ; topic_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "TopicARN") String.of_json
    ; notification_type =
        Aws.Util.option_map (Aws.Json.lookup j "NotificationType") String.of_json
    }
end

module NotificationConfigurations = struct
  type t = NotificationConfiguration.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map NotificationConfiguration.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list NotificationConfiguration.to_query v

  let to_json v = `List (List.map NotificationConfiguration.to_json v)

  let of_json j = Aws.Json.to_list NotificationConfiguration.of_json j
end

module SuspendedProcess = struct
  type t =
    { process_name : String.t option
    ; suspension_reason : String.t option
    }

  let make ?process_name ?suspension_reason () = { process_name; suspension_reason }

  let parse xml =
    Some
      { process_name =
          Aws.Util.option_bind (Aws.Xml.member "ProcessName" xml) String.parse
      ; suspension_reason =
          Aws.Util.option_bind (Aws.Xml.member "SuspensionReason" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.suspension_reason (fun f ->
               Aws.Query.Pair ("SuspensionReason", String.to_query f))
         ; Aws.Util.option_map v.process_name (fun f ->
               Aws.Query.Pair ("ProcessName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.suspension_reason (fun f ->
               "SuspensionReason", String.to_json f)
         ; Aws.Util.option_map v.process_name (fun f -> "ProcessName", String.to_json f)
         ])

  let of_json j =
    { process_name = Aws.Util.option_map (Aws.Json.lookup j "ProcessName") String.of_json
    ; suspension_reason =
        Aws.Util.option_map (Aws.Json.lookup j "SuspensionReason") String.of_json
    }
end

module ScheduledActionNames = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module DescribeScheduledActionsType = struct
  type t =
    { auto_scaling_group_name : String.t option
    ; scheduled_action_names : ScheduledActionNames.t
    ; start_time : DateTime.t option
    ; end_time : DateTime.t option
    ; next_token : String.t option
    ; max_records : Integer.t option
    }

  let make
      ?auto_scaling_group_name
      ?(scheduled_action_names = [])
      ?start_time
      ?end_time
      ?next_token
      ?max_records
      () =
    { auto_scaling_group_name
    ; scheduled_action_names
    ; start_time
    ; end_time
    ; next_token
    ; max_records
    }

  let parse xml =
    Some
      { auto_scaling_group_name =
          Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse
      ; scheduled_action_names =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ScheduledActionNames" xml)
               ScheduledActionNames.parse)
      ; start_time = Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse
      ; end_time = Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Aws.Util.option_map v.end_time (fun f ->
               Aws.Query.Pair ("EndTime", DateTime.to_query f))
         ; Aws.Util.option_map v.start_time (fun f ->
               Aws.Query.Pair ("StartTime", DateTime.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "ScheduledActionNames.member"
                , ScheduledActionNames.to_query v.scheduled_action_names ))
         ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
               Aws.Query.Pair ("AutoScalingGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Aws.Util.option_map v.end_time (fun f -> "EndTime", DateTime.to_json f)
         ; Aws.Util.option_map v.start_time (fun f -> "StartTime", DateTime.to_json f)
         ; Some
             ( "ScheduledActionNames"
             , ScheduledActionNames.to_json v.scheduled_action_names )
         ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
               "AutoScalingGroupName", String.to_json f)
         ])

  let of_json j =
    { auto_scaling_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "AutoScalingGroupName") String.of_json
    ; scheduled_action_names =
        ScheduledActionNames.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ScheduledActionNames"))
    ; start_time = Aws.Util.option_map (Aws.Json.lookup j "StartTime") DateTime.of_json
    ; end_time = Aws.Util.option_map (Aws.Json.lookup j "EndTime") DateTime.of_json
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    }
end

module InstanceIds = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module EnterStandbyQuery = struct
  type t =
    { instance_ids : InstanceIds.t
    ; auto_scaling_group_name : String.t
    ; should_decrement_desired_capacity : Boolean.t
    }

  let make
      ?(instance_ids = [])
      ~auto_scaling_group_name
      ~should_decrement_desired_capacity
      () =
    { instance_ids; auto_scaling_group_name; should_decrement_desired_capacity }

  let parse xml =
    Some
      { instance_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "InstanceIds" xml) InstanceIds.parse)
      ; auto_scaling_group_name =
          Aws.Xml.required
            "AutoScalingGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupName" xml)
               String.parse)
      ; should_decrement_desired_capacity =
          Aws.Xml.required
            "ShouldDecrementDesiredCapacity"
            (Aws.Util.option_bind
               (Aws.Xml.member "ShouldDecrementDesiredCapacity" xml)
               Boolean.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "ShouldDecrementDesiredCapacity"
                , Boolean.to_query v.should_decrement_desired_capacity ))
         ; Some
             (Aws.Query.Pair
                ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
         ; Some
             (Aws.Query.Pair ("InstanceIds.member", InstanceIds.to_query v.instance_ids))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "ShouldDecrementDesiredCapacity"
             , Boolean.to_json v.should_decrement_desired_capacity )
         ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
         ; Some ("InstanceIds", InstanceIds.to_json v.instance_ids)
         ])

  let of_json j =
    { instance_ids =
        InstanceIds.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceIds"))
    ; auto_scaling_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
    ; should_decrement_desired_capacity =
        Boolean.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ShouldDecrementDesiredCapacity"))
    }
end

module CompleteLifecycleActionType = struct
  type t =
    { lifecycle_hook_name : String.t
    ; auto_scaling_group_name : String.t
    ; lifecycle_action_token : String.t
    ; lifecycle_action_result : String.t
    }

  let make
      ~lifecycle_hook_name
      ~auto_scaling_group_name
      ~lifecycle_action_token
      ~lifecycle_action_result
      () =
    { lifecycle_hook_name
    ; auto_scaling_group_name
    ; lifecycle_action_token
    ; lifecycle_action_result
    }

  let parse xml =
    Some
      { lifecycle_hook_name =
          Aws.Xml.required
            "LifecycleHookName"
            (Aws.Util.option_bind (Aws.Xml.member "LifecycleHookName" xml) String.parse)
      ; auto_scaling_group_name =
          Aws.Xml.required
            "AutoScalingGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupName" xml)
               String.parse)
      ; lifecycle_action_token =
          Aws.Xml.required
            "LifecycleActionToken"
            (Aws.Util.option_bind
               (Aws.Xml.member "LifecycleActionToken" xml)
               String.parse)
      ; lifecycle_action_result =
          Aws.Xml.required
            "LifecycleActionResult"
            (Aws.Util.option_bind
               (Aws.Xml.member "LifecycleActionResult" xml)
               String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("LifecycleActionResult", String.to_query v.lifecycle_action_result))
         ; Some
             (Aws.Query.Pair
                ("LifecycleActionToken", String.to_query v.lifecycle_action_token))
         ; Some
             (Aws.Query.Pair
                ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
         ; Some
             (Aws.Query.Pair ("LifecycleHookName", String.to_query v.lifecycle_hook_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("LifecycleActionResult", String.to_json v.lifecycle_action_result)
         ; Some ("LifecycleActionToken", String.to_json v.lifecycle_action_token)
         ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
         ; Some ("LifecycleHookName", String.to_json v.lifecycle_hook_name)
         ])

  let of_json j =
    { lifecycle_hook_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LifecycleHookName"))
    ; auto_scaling_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
    ; lifecycle_action_token =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LifecycleActionToken"))
    ; lifecycle_action_result =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LifecycleActionResult"))
    }
end

module ProcessType = struct
  type t = { process_name : String.t }

  let make ~process_name () = { process_name }

  let parse xml =
    Some
      { process_name =
          Aws.Xml.required
            "ProcessName"
            (Aws.Util.option_bind (Aws.Xml.member "ProcessName" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("ProcessName", String.to_query v.process_name)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("ProcessName", String.to_json v.process_name) ])

  let of_json j =
    { process_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ProcessName"))
    }
end

module Processes = struct
  type t = ProcessType.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map ProcessType.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list ProcessType.to_query v

  let to_json v = `List (List.map ProcessType.to_json v)

  let of_json j = Aws.Json.to_list ProcessType.of_json j
end

module ProcessesType = struct
  type t = { processes : Processes.t }

  let make ?(processes = []) () = { processes }

  let parse xml =
    Some
      { processes =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Processes" xml) Processes.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Processes.member", Processes.to_query v.processes)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("Processes", Processes.to_json v.processes) ])

  let of_json j =
    { processes =
        Processes.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Processes"))
    }
end

module DescribeAutoScalingInstancesType = struct
  type t =
    { instance_ids : InstanceIds.t
    ; max_records : Integer.t option
    ; next_token : String.t option
    }

  let make ?(instance_ids = []) ?max_records ?next_token () =
    { instance_ids; max_records; next_token }

  let parse xml =
    Some
      { instance_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "InstanceIds" xml) InstanceIds.parse)
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some
             (Aws.Query.Pair ("InstanceIds.member", InstanceIds.to_query v.instance_ids))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Some ("InstanceIds", InstanceIds.to_json v.instance_ids)
         ])

  let of_json j =
    { instance_ids =
        InstanceIds.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceIds"))
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module Metrics = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module DisableMetricsCollectionQuery = struct
  type t =
    { auto_scaling_group_name : String.t
    ; metrics : Metrics.t
    }

  let make ~auto_scaling_group_name ?(metrics = []) () =
    { auto_scaling_group_name; metrics }

  let parse xml =
    Some
      { auto_scaling_group_name =
          Aws.Xml.required
            "AutoScalingGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupName" xml)
               String.parse)
      ; metrics =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Metrics" xml) Metrics.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Metrics.member", Metrics.to_query v.metrics))
         ; Some
             (Aws.Query.Pair
                ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Metrics", Metrics.to_json v.metrics)
         ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
         ])

  let of_json j =
    { auto_scaling_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
    ; metrics = Metrics.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Metrics"))
    }
end

module LifecycleHookNames = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module DescribeLifecycleHooksType = struct
  type t =
    { auto_scaling_group_name : String.t
    ; lifecycle_hook_names : LifecycleHookNames.t
    }

  let make ~auto_scaling_group_name ?(lifecycle_hook_names = []) () =
    { auto_scaling_group_name; lifecycle_hook_names }

  let parse xml =
    Some
      { auto_scaling_group_name =
          Aws.Xml.required
            "AutoScalingGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupName" xml)
               String.parse)
      ; lifecycle_hook_names =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "LifecycleHookNames" xml)
               LifecycleHookNames.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "LifecycleHookNames.member"
                , LifecycleHookNames.to_query v.lifecycle_hook_names ))
         ; Some
             (Aws.Query.Pair
                ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("LifecycleHookNames", LifecycleHookNames.to_json v.lifecycle_hook_names)
         ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
         ])

  let of_json j =
    { auto_scaling_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
    ; lifecycle_hook_names =
        LifecycleHookNames.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LifecycleHookNames"))
    }
end

module ScalingActivityStatusCode = struct
  type t =
    | WaitingForSpotInstanceRequestId
    | WaitingForSpotInstanceId
    | WaitingForInstanceId
    | PreInService
    | InProgress
    | WaitingForELBConnectionDraining
    | MidLifecycleAction
    | WaitingForInstanceWarmup
    | Successful
    | Failed
    | Cancelled

  let str_to_t =
    [ "Cancelled", Cancelled
    ; "Failed", Failed
    ; "Successful", Successful
    ; "WaitingForInstanceWarmup", WaitingForInstanceWarmup
    ; "MidLifecycleAction", MidLifecycleAction
    ; "WaitingForELBConnectionDraining", WaitingForELBConnectionDraining
    ; "InProgress", InProgress
    ; "PreInService", PreInService
    ; "WaitingForInstanceId", WaitingForInstanceId
    ; "WaitingForSpotInstanceId", WaitingForSpotInstanceId
    ; "WaitingForSpotInstanceRequestId", WaitingForSpotInstanceRequestId
    ]

  let t_to_str =
    [ Cancelled, "Cancelled"
    ; Failed, "Failed"
    ; Successful, "Successful"
    ; WaitingForInstanceWarmup, "WaitingForInstanceWarmup"
    ; MidLifecycleAction, "MidLifecycleAction"
    ; WaitingForELBConnectionDraining, "WaitingForELBConnectionDraining"
    ; InProgress, "InProgress"
    ; PreInService, "PreInService"
    ; WaitingForInstanceId, "WaitingForInstanceId"
    ; WaitingForSpotInstanceId, "WaitingForSpotInstanceId"
    ; WaitingForSpotInstanceRequestId, "WaitingForSpotInstanceRequestId"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module Activity = struct
  type t =
    { activity_id : String.t
    ; auto_scaling_group_name : String.t
    ; description : String.t option
    ; cause : String.t
    ; start_time : DateTime.t
    ; end_time : DateTime.t option
    ; status_code : ScalingActivityStatusCode.t
    ; status_message : String.t option
    ; progress : Integer.t option
    ; details : String.t option
    }

  let make
      ~activity_id
      ~auto_scaling_group_name
      ?description
      ~cause
      ~start_time
      ?end_time
      ~status_code
      ?status_message
      ?progress
      ?details
      () =
    { activity_id
    ; auto_scaling_group_name
    ; description
    ; cause
    ; start_time
    ; end_time
    ; status_code
    ; status_message
    ; progress
    ; details
    }

  let parse xml =
    Some
      { activity_id =
          Aws.Xml.required
            "ActivityId"
            (Aws.Util.option_bind (Aws.Xml.member "ActivityId" xml) String.parse)
      ; auto_scaling_group_name =
          Aws.Xml.required
            "AutoScalingGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupName" xml)
               String.parse)
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; cause =
          Aws.Xml.required
            "Cause"
            (Aws.Util.option_bind (Aws.Xml.member "Cause" xml) String.parse)
      ; start_time =
          Aws.Xml.required
            "StartTime"
            (Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse)
      ; end_time = Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse
      ; status_code =
          Aws.Xml.required
            "StatusCode"
            (Aws.Util.option_bind
               (Aws.Xml.member "StatusCode" xml)
               ScalingActivityStatusCode.parse)
      ; status_message =
          Aws.Util.option_bind (Aws.Xml.member "StatusMessage" xml) String.parse
      ; progress = Aws.Util.option_bind (Aws.Xml.member "Progress" xml) Integer.parse
      ; details = Aws.Util.option_bind (Aws.Xml.member "Details" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.details (fun f ->
               Aws.Query.Pair ("Details", String.to_query f))
         ; Aws.Util.option_map v.progress (fun f ->
               Aws.Query.Pair ("Progress", Integer.to_query f))
         ; Aws.Util.option_map v.status_message (fun f ->
               Aws.Query.Pair ("StatusMessage", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("StatusCode", ScalingActivityStatusCode.to_query v.status_code))
         ; Aws.Util.option_map v.end_time (fun f ->
               Aws.Query.Pair ("EndTime", DateTime.to_query f))
         ; Some (Aws.Query.Pair ("StartTime", DateTime.to_query v.start_time))
         ; Some (Aws.Query.Pair ("Cause", String.to_query v.cause))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
         ; Some (Aws.Query.Pair ("ActivityId", String.to_query v.activity_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.details (fun f -> "Details", String.to_json f)
         ; Aws.Util.option_map v.progress (fun f -> "Progress", Integer.to_json f)
         ; Aws.Util.option_map v.status_message (fun f ->
               "StatusMessage", String.to_json f)
         ; Some ("StatusCode", ScalingActivityStatusCode.to_json v.status_code)
         ; Aws.Util.option_map v.end_time (fun f -> "EndTime", DateTime.to_json f)
         ; Some ("StartTime", DateTime.to_json v.start_time)
         ; Some ("Cause", String.to_json v.cause)
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
         ; Some ("ActivityId", String.to_json v.activity_id)
         ])

  let of_json j =
    { activity_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ActivityId"))
    ; auto_scaling_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; cause = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Cause"))
    ; start_time =
        DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StartTime"))
    ; end_time = Aws.Util.option_map (Aws.Json.lookup j "EndTime") DateTime.of_json
    ; status_code =
        ScalingActivityStatusCode.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "StatusCode"))
    ; status_message =
        Aws.Util.option_map (Aws.Json.lookup j "StatusMessage") String.of_json
    ; progress = Aws.Util.option_map (Aws.Json.lookup j "Progress") Integer.of_json
    ; details = Aws.Util.option_map (Aws.Json.lookup j "Details") String.of_json
    }
end

module Activities = struct
  type t = Activity.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Activity.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Activity.to_query v

  let to_json v = `List (List.map Activity.to_json v)

  let of_json j = Aws.Json.to_list Activity.of_json j
end

module EnterStandbyAnswer = struct
  type t = { activities : Activities.t }

  let make ?(activities = []) () = { activities }

  let parse xml =
    Some
      { activities =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Activities" xml) Activities.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Activities.member", Activities.to_query v.activities)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("Activities", Activities.to_json v.activities) ])

  let of_json j =
    { activities =
        Activities.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Activities"))
    }
end

module MetricGranularityType = struct
  type t = { granularity : String.t option }

  let make ?granularity () = { granularity }

  let parse xml =
    Some
      { granularity = Aws.Util.option_bind (Aws.Xml.member "Granularity" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.granularity (fun f ->
               Aws.Query.Pair ("Granularity", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.granularity (fun f -> "Granularity", String.to_json f) ])

  let of_json j =
    { granularity = Aws.Util.option_map (Aws.Json.lookup j "Granularity") String.of_json }
end

module MetricGranularityTypes = struct
  type t = MetricGranularityType.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map MetricGranularityType.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list MetricGranularityType.to_query v

  let to_json v = `List (List.map MetricGranularityType.to_json v)

  let of_json j = Aws.Json.to_list MetricGranularityType.of_json j
end

module MetricCollectionType = struct
  type t = { metric : String.t option }

  let make ?metric () = { metric }

  let parse xml =
    Some { metric = Aws.Util.option_bind (Aws.Xml.member "Metric" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.metric (fun f ->
               Aws.Query.Pair ("Metric", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.metric (fun f -> "Metric", String.to_json f) ])

  let of_json j =
    { metric = Aws.Util.option_map (Aws.Json.lookup j "Metric") String.of_json }
end

module MetricCollectionTypes = struct
  type t = MetricCollectionType.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map MetricCollectionType.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list MetricCollectionType.to_query v

  let to_json v = `List (List.map MetricCollectionType.to_json v)

  let of_json j = Aws.Json.to_list MetricCollectionType.of_json j
end

module DescribeMetricCollectionTypesAnswer = struct
  type t =
    { metrics : MetricCollectionTypes.t
    ; granularities : MetricGranularityTypes.t
    }

  let make ?(metrics = []) ?(granularities = []) () = { metrics; granularities }

  let parse xml =
    Some
      { metrics =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "Metrics" xml)
               MetricCollectionTypes.parse)
      ; granularities =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "Granularities" xml)
               MetricGranularityTypes.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("Granularities.member", MetricGranularityTypes.to_query v.granularities))
         ; Some
             (Aws.Query.Pair ("Metrics.member", MetricCollectionTypes.to_query v.metrics))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Granularities", MetricGranularityTypes.to_json v.granularities)
         ; Some ("Metrics", MetricCollectionTypes.to_json v.metrics)
         ])

  let of_json j =
    { metrics =
        MetricCollectionTypes.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Metrics"))
    ; granularities =
        MetricGranularityTypes.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Granularities"))
    }
end

module PolicyARNType = struct
  type t = { policy_a_r_n : String.t option }

  let make ?policy_a_r_n () = { policy_a_r_n }

  let parse xml =
    Some
      { policy_a_r_n = Aws.Util.option_bind (Aws.Xml.member "PolicyARN" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.policy_a_r_n (fun f ->
               Aws.Query.Pair ("PolicyARN", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.policy_a_r_n (fun f -> "PolicyARN", String.to_json f) ])

  let of_json j =
    { policy_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "PolicyARN") String.of_json }
end

module DeletePolicyType = struct
  type t =
    { auto_scaling_group_name : String.t option
    ; policy_name : String.t
    }

  let make ?auto_scaling_group_name ~policy_name () =
    { auto_scaling_group_name; policy_name }

  let parse xml =
    Some
      { auto_scaling_group_name =
          Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse
      ; policy_name =
          Aws.Xml.required
            "PolicyName"
            (Aws.Util.option_bind (Aws.Xml.member "PolicyName" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("PolicyName", String.to_query v.policy_name))
         ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
               Aws.Query.Pair ("AutoScalingGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("PolicyName", String.to_json v.policy_name)
         ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
               "AutoScalingGroupName", String.to_json f)
         ])

  let of_json j =
    { auto_scaling_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "AutoScalingGroupName") String.of_json
    ; policy_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyName"))
    }
end

module AutoScalingGroupNames = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module ScheduledUpdateGroupAction = struct
  type t =
    { auto_scaling_group_name : String.t option
    ; scheduled_action_name : String.t option
    ; scheduled_action_a_r_n : String.t option
    ; time : DateTime.t option
    ; start_time : DateTime.t option
    ; end_time : DateTime.t option
    ; recurrence : String.t option
    ; min_size : Integer.t option
    ; max_size : Integer.t option
    ; desired_capacity : Integer.t option
    }

  let make
      ?auto_scaling_group_name
      ?scheduled_action_name
      ?scheduled_action_a_r_n
      ?time
      ?start_time
      ?end_time
      ?recurrence
      ?min_size
      ?max_size
      ?desired_capacity
      () =
    { auto_scaling_group_name
    ; scheduled_action_name
    ; scheduled_action_a_r_n
    ; time
    ; start_time
    ; end_time
    ; recurrence
    ; min_size
    ; max_size
    ; desired_capacity
    }

  let parse xml =
    Some
      { auto_scaling_group_name =
          Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse
      ; scheduled_action_name =
          Aws.Util.option_bind (Aws.Xml.member "ScheduledActionName" xml) String.parse
      ; scheduled_action_a_r_n =
          Aws.Util.option_bind (Aws.Xml.member "ScheduledActionARN" xml) String.parse
      ; time = Aws.Util.option_bind (Aws.Xml.member "Time" xml) DateTime.parse
      ; start_time = Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse
      ; end_time = Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse
      ; recurrence = Aws.Util.option_bind (Aws.Xml.member "Recurrence" xml) String.parse
      ; min_size = Aws.Util.option_bind (Aws.Xml.member "MinSize" xml) Integer.parse
      ; max_size = Aws.Util.option_bind (Aws.Xml.member "MaxSize" xml) Integer.parse
      ; desired_capacity =
          Aws.Util.option_bind (Aws.Xml.member "DesiredCapacity" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.desired_capacity (fun f ->
               Aws.Query.Pair ("DesiredCapacity", Integer.to_query f))
         ; Aws.Util.option_map v.max_size (fun f ->
               Aws.Query.Pair ("MaxSize", Integer.to_query f))
         ; Aws.Util.option_map v.min_size (fun f ->
               Aws.Query.Pair ("MinSize", Integer.to_query f))
         ; Aws.Util.option_map v.recurrence (fun f ->
               Aws.Query.Pair ("Recurrence", String.to_query f))
         ; Aws.Util.option_map v.end_time (fun f ->
               Aws.Query.Pair ("EndTime", DateTime.to_query f))
         ; Aws.Util.option_map v.start_time (fun f ->
               Aws.Query.Pair ("StartTime", DateTime.to_query f))
         ; Aws.Util.option_map v.time (fun f ->
               Aws.Query.Pair ("Time", DateTime.to_query f))
         ; Aws.Util.option_map v.scheduled_action_a_r_n (fun f ->
               Aws.Query.Pair ("ScheduledActionARN", String.to_query f))
         ; Aws.Util.option_map v.scheduled_action_name (fun f ->
               Aws.Query.Pair ("ScheduledActionName", String.to_query f))
         ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
               Aws.Query.Pair ("AutoScalingGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.desired_capacity (fun f ->
               "DesiredCapacity", Integer.to_json f)
         ; Aws.Util.option_map v.max_size (fun f -> "MaxSize", Integer.to_json f)
         ; Aws.Util.option_map v.min_size (fun f -> "MinSize", Integer.to_json f)
         ; Aws.Util.option_map v.recurrence (fun f -> "Recurrence", String.to_json f)
         ; Aws.Util.option_map v.end_time (fun f -> "EndTime", DateTime.to_json f)
         ; Aws.Util.option_map v.start_time (fun f -> "StartTime", DateTime.to_json f)
         ; Aws.Util.option_map v.time (fun f -> "Time", DateTime.to_json f)
         ; Aws.Util.option_map v.scheduled_action_a_r_n (fun f ->
               "ScheduledActionARN", String.to_json f)
         ; Aws.Util.option_map v.scheduled_action_name (fun f ->
               "ScheduledActionName", String.to_json f)
         ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
               "AutoScalingGroupName", String.to_json f)
         ])

  let of_json j =
    { auto_scaling_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "AutoScalingGroupName") String.of_json
    ; scheduled_action_name =
        Aws.Util.option_map (Aws.Json.lookup j "ScheduledActionName") String.of_json
    ; scheduled_action_a_r_n =
        Aws.Util.option_map (Aws.Json.lookup j "ScheduledActionARN") String.of_json
    ; time = Aws.Util.option_map (Aws.Json.lookup j "Time") DateTime.of_json
    ; start_time = Aws.Util.option_map (Aws.Json.lookup j "StartTime") DateTime.of_json
    ; end_time = Aws.Util.option_map (Aws.Json.lookup j "EndTime") DateTime.of_json
    ; recurrence = Aws.Util.option_map (Aws.Json.lookup j "Recurrence") String.of_json
    ; min_size = Aws.Util.option_map (Aws.Json.lookup j "MinSize") Integer.of_json
    ; max_size = Aws.Util.option_map (Aws.Json.lookup j "MaxSize") Integer.of_json
    ; desired_capacity =
        Aws.Util.option_map (Aws.Json.lookup j "DesiredCapacity") Integer.of_json
    }
end

module ScheduledUpdateGroupActions = struct
  type t = ScheduledUpdateGroupAction.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map ScheduledUpdateGroupAction.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list ScheduledUpdateGroupAction.to_query v

  let to_json v = `List (List.map ScheduledUpdateGroupAction.to_json v)

  let of_json j = Aws.Json.to_list ScheduledUpdateGroupAction.of_json j
end

module ScalingActivityInProgressFault = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module CompleteLifecycleActionAnswer = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ActivitiesType = struct
  type t =
    { activities : Activities.t
    ; next_token : String.t option
    }

  let make ~activities ?next_token () = { activities; next_token }

  let parse xml =
    Some
      { activities =
          Aws.Xml.required
            "Activities"
            (Aws.Util.option_bind (Aws.Xml.member "Activities" xml) Activities.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some (Aws.Query.Pair ("Activities.member", Activities.to_query v.activities))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("Activities", Activities.to_json v.activities)
         ])

  let of_json j =
    { activities =
        Activities.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Activities"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module LoadBalancerState = struct
  type t =
    { load_balancer_name : String.t option
    ; state : String.t option
    }

  let make ?load_balancer_name ?state () = { load_balancer_name; state }

  let parse xml =
    Some
      { load_balancer_name =
          Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse
      ; state = Aws.Util.option_bind (Aws.Xml.member "State" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.state (fun f ->
               Aws.Query.Pair ("State", String.to_query f))
         ; Aws.Util.option_map v.load_balancer_name (fun f ->
               Aws.Query.Pair ("LoadBalancerName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.state (fun f -> "State", String.to_json f)
         ; Aws.Util.option_map v.load_balancer_name (fun f ->
               "LoadBalancerName", String.to_json f)
         ])

  let of_json j =
    { load_balancer_name =
        Aws.Util.option_map (Aws.Json.lookup j "LoadBalancerName") String.of_json
    ; state = Aws.Util.option_map (Aws.Json.lookup j "State") String.of_json
    }
end

module LoadBalancerStates = struct
  type t = LoadBalancerState.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map LoadBalancerState.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list LoadBalancerState.to_query v

  let to_json v = `List (List.map LoadBalancerState.to_json v)

  let of_json j = Aws.Json.to_list LoadBalancerState.of_json j
end

module DescribeLoadBalancersResponse = struct
  type t =
    { load_balancers : LoadBalancerStates.t
    ; next_token : String.t option
    }

  let make ?(load_balancers = []) ?next_token () = { load_balancers; next_token }

  let parse xml =
    Some
      { load_balancers =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "LoadBalancers" xml)
               LoadBalancerStates.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("LoadBalancers.member", LoadBalancerStates.to_query v.load_balancers))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("LoadBalancers", LoadBalancerStates.to_json v.load_balancers)
         ])

  let of_json j =
    { load_balancers =
        LoadBalancerStates.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancers"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module AutoScalingNotificationTypes = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module DescribeAutoScalingNotificationTypesAnswer = struct
  type t = { auto_scaling_notification_types : AutoScalingNotificationTypes.t }

  let make ?(auto_scaling_notification_types = []) () =
    { auto_scaling_notification_types }

  let parse xml =
    Some
      { auto_scaling_notification_types =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingNotificationTypes" xml)
               AutoScalingNotificationTypes.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "AutoScalingNotificationTypes.member"
                , AutoScalingNotificationTypes.to_query v.auto_scaling_notification_types
                ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "AutoScalingNotificationTypes"
             , AutoScalingNotificationTypes.to_json v.auto_scaling_notification_types )
         ])

  let of_json j =
    { auto_scaling_notification_types =
        AutoScalingNotificationTypes.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingNotificationTypes"))
    }
end

module ScheduledActionsType = struct
  type t =
    { scheduled_update_group_actions : ScheduledUpdateGroupActions.t
    ; next_token : String.t option
    }

  let make ?(scheduled_update_group_actions = []) ?next_token () =
    { scheduled_update_group_actions; next_token }

  let parse xml =
    Some
      { scheduled_update_group_actions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ScheduledUpdateGroupActions" xml)
               ScheduledUpdateGroupActions.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "ScheduledUpdateGroupActions.member"
                , ScheduledUpdateGroupActions.to_query v.scheduled_update_group_actions ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some
             ( "ScheduledUpdateGroupActions"
             , ScheduledUpdateGroupActions.to_json v.scheduled_update_group_actions )
         ])

  let of_json j =
    { scheduled_update_group_actions =
        ScheduledUpdateGroupActions.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ScheduledUpdateGroupActions"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module ExitStandbyAnswer = struct
  type t = { activities : Activities.t }

  let make ?(activities = []) () = { activities }

  let parse xml =
    Some
      { activities =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Activities" xml) Activities.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Activities.member", Activities.to_query v.activities)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("Activities", Activities.to_json v.activities) ])

  let of_json j =
    { activities =
        Activities.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Activities"))
    }
end

module TerminationPolicies = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module TagDescription = struct
  type t =
    { resource_id : String.t
    ; resource_type : String.t
    ; key : String.t
    ; value : String.t
    ; propagate_at_launch : Boolean.t
    }

  let make ~resource_id ~resource_type ~key ~value ~propagate_at_launch () =
    { resource_id; resource_type; key; value; propagate_at_launch }

  let parse xml =
    Some
      { resource_id =
          Aws.Xml.required
            "ResourceId"
            (Aws.Util.option_bind (Aws.Xml.member "ResourceId" xml) String.parse)
      ; resource_type =
          Aws.Xml.required
            "ResourceType"
            (Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse)
      ; key =
          Aws.Xml.required
            "Key"
            (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse)
      ; value =
          Aws.Xml.required
            "Value"
            (Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse)
      ; propagate_at_launch =
          Aws.Xml.required
            "PropagateAtLaunch"
            (Aws.Util.option_bind (Aws.Xml.member "PropagateAtLaunch" xml) Boolean.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("PropagateAtLaunch", Boolean.to_query v.propagate_at_launch))
         ; Some (Aws.Query.Pair ("Value", String.to_query v.value))
         ; Some (Aws.Query.Pair ("Key", String.to_query v.key))
         ; Some (Aws.Query.Pair ("ResourceType", String.to_query v.resource_type))
         ; Some (Aws.Query.Pair ("ResourceId", String.to_query v.resource_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("PropagateAtLaunch", Boolean.to_json v.propagate_at_launch)
         ; Some ("Value", String.to_json v.value)
         ; Some ("Key", String.to_json v.key)
         ; Some ("ResourceType", String.to_json v.resource_type)
         ; Some ("ResourceId", String.to_json v.resource_id)
         ])

  let of_json j =
    { resource_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceId"))
    ; resource_type =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceType"))
    ; key = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key"))
    ; value = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Value"))
    ; propagate_at_launch =
        Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PropagateAtLaunch"))
    }
end

module TagDescriptionList = struct
  type t = TagDescription.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map TagDescription.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list TagDescription.to_query v

  let to_json v = `List (List.map TagDescription.to_json v)

  let of_json j = Aws.Json.to_list TagDescription.of_json j
end

module SuspendedProcesses = struct
  type t = SuspendedProcess.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map SuspendedProcess.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list SuspendedProcess.to_query v

  let to_json v = `List (List.map SuspendedProcess.to_json v)

  let of_json j = Aws.Json.to_list SuspendedProcess.of_json j
end

module LoadBalancerNames = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module LifecycleState = struct
  type t =
    | Pending
    | Pending_Wait
    | Pending_Proceed
    | Quarantined
    | InService
    | Terminating
    | Terminating_Wait
    | Terminating_Proceed
    | Terminated
    | Detaching
    | Detached
    | EnteringStandby
    | Standby

  let str_to_t =
    [ "Standby", Standby
    ; "EnteringStandby", EnteringStandby
    ; "Detached", Detached
    ; "Detaching", Detaching
    ; "Terminated", Terminated
    ; "Terminating:Proceed", Terminating_Proceed
    ; "Terminating:Wait", Terminating_Wait
    ; "Terminating", Terminating
    ; "InService", InService
    ; "Quarantined", Quarantined
    ; "Pending:Proceed", Pending_Proceed
    ; "Pending:Wait", Pending_Wait
    ; "Pending", Pending
    ]

  let t_to_str =
    [ Standby, "Standby"
    ; EnteringStandby, "EnteringStandby"
    ; Detached, "Detached"
    ; Detaching, "Detaching"
    ; Terminated, "Terminated"
    ; Terminating_Proceed, "Terminating:Proceed"
    ; Terminating_Wait, "Terminating:Wait"
    ; Terminating, "Terminating"
    ; InService, "InService"
    ; Quarantined, "Quarantined"
    ; Pending_Proceed, "Pending:Proceed"
    ; Pending_Wait, "Pending:Wait"
    ; Pending, "Pending"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module Instance = struct
  type t =
    { instance_id : String.t
    ; availability_zone : String.t
    ; lifecycle_state : LifecycleState.t
    ; health_status : String.t
    ; launch_configuration_name : String.t option
    }

  let make
      ~instance_id
      ~availability_zone
      ~lifecycle_state
      ~health_status
      ?launch_configuration_name
      () =
    { instance_id
    ; availability_zone
    ; lifecycle_state
    ; health_status
    ; launch_configuration_name
    }

  let parse xml =
    Some
      { instance_id =
          Aws.Xml.required
            "InstanceId"
            (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
      ; availability_zone =
          Aws.Xml.required
            "AvailabilityZone"
            (Aws.Util.option_bind (Aws.Xml.member "AvailabilityZone" xml) String.parse)
      ; lifecycle_state =
          Aws.Xml.required
            "LifecycleState"
            (Aws.Util.option_bind
               (Aws.Xml.member "LifecycleState" xml)
               LifecycleState.parse)
      ; health_status =
          Aws.Xml.required
            "HealthStatus"
            (Aws.Util.option_bind (Aws.Xml.member "HealthStatus" xml) String.parse)
      ; launch_configuration_name =
          Aws.Util.option_bind (Aws.Xml.member "LaunchConfigurationName" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.launch_configuration_name (fun f ->
               Aws.Query.Pair ("LaunchConfigurationName", String.to_query f))
         ; Some (Aws.Query.Pair ("HealthStatus", String.to_query v.health_status))
         ; Some
             (Aws.Query.Pair ("LifecycleState", LifecycleState.to_query v.lifecycle_state))
         ; Some (Aws.Query.Pair ("AvailabilityZone", String.to_query v.availability_zone))
         ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.launch_configuration_name (fun f ->
               "LaunchConfigurationName", String.to_json f)
         ; Some ("HealthStatus", String.to_json v.health_status)
         ; Some ("LifecycleState", LifecycleState.to_json v.lifecycle_state)
         ; Some ("AvailabilityZone", String.to_json v.availability_zone)
         ; Some ("InstanceId", String.to_json v.instance_id)
         ])

  let of_json j =
    { instance_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
    ; availability_zone =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZone"))
    ; lifecycle_state =
        LifecycleState.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LifecycleState"))
    ; health_status =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthStatus"))
    ; launch_configuration_name =
        Aws.Util.option_map (Aws.Json.lookup j "LaunchConfigurationName") String.of_json
    }
end

module Instances = struct
  type t = Instance.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Instance.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Instance.to_query v

  let to_json v = `List (List.map Instance.to_json v)

  let of_json j = Aws.Json.to_list Instance.of_json j
end

module EnabledMetric = struct
  type t =
    { metric : String.t option
    ; granularity : String.t option
    }

  let make ?metric ?granularity () = { metric; granularity }

  let parse xml =
    Some
      { metric = Aws.Util.option_bind (Aws.Xml.member "Metric" xml) String.parse
      ; granularity = Aws.Util.option_bind (Aws.Xml.member "Granularity" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.granularity (fun f ->
               Aws.Query.Pair ("Granularity", String.to_query f))
         ; Aws.Util.option_map v.metric (fun f ->
               Aws.Query.Pair ("Metric", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.granularity (fun f -> "Granularity", String.to_json f)
         ; Aws.Util.option_map v.metric (fun f -> "Metric", String.to_json f)
         ])

  let of_json j =
    { metric = Aws.Util.option_map (Aws.Json.lookup j "Metric") String.of_json
    ; granularity = Aws.Util.option_map (Aws.Json.lookup j "Granularity") String.of_json
    }
end

module EnabledMetrics = struct
  type t = EnabledMetric.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map EnabledMetric.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list EnabledMetric.to_query v

  let to_json v = `List (List.map EnabledMetric.to_json v)

  let of_json j = Aws.Json.to_list EnabledMetric.of_json j
end

module AvailabilityZones = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module AutoScalingGroup = struct
  type t =
    { auto_scaling_group_name : String.t
    ; auto_scaling_group_a_r_n : String.t option
    ; launch_configuration_name : String.t option
    ; min_size : Integer.t
    ; max_size : Integer.t
    ; desired_capacity : Integer.t
    ; default_cooldown : Integer.t
    ; availability_zones : AvailabilityZones.t
    ; load_balancer_names : LoadBalancerNames.t
    ; health_check_type : String.t
    ; health_check_grace_period : Integer.t option
    ; instances : Instances.t
    ; created_time : DateTime.t
    ; suspended_processes : SuspendedProcesses.t
    ; placement_group : String.t option
    ; v_p_c_zone_identifier : String.t option
    ; enabled_metrics : EnabledMetrics.t
    ; status : String.t option
    ; tags : TagDescriptionList.t
    ; termination_policies : TerminationPolicies.t
    }

  let make
      ~auto_scaling_group_name
      ?auto_scaling_group_a_r_n
      ?launch_configuration_name
      ~min_size
      ~max_size
      ~desired_capacity
      ~default_cooldown
      ~availability_zones
      ?(load_balancer_names = [])
      ~health_check_type
      ?health_check_grace_period
      ?(instances = [])
      ~created_time
      ?(suspended_processes = [])
      ?placement_group
      ?v_p_c_zone_identifier
      ?(enabled_metrics = [])
      ?status
      ?(tags = [])
      ?(termination_policies = [])
      () =
    { auto_scaling_group_name
    ; auto_scaling_group_a_r_n
    ; launch_configuration_name
    ; min_size
    ; max_size
    ; desired_capacity
    ; default_cooldown
    ; availability_zones
    ; load_balancer_names
    ; health_check_type
    ; health_check_grace_period
    ; instances
    ; created_time
    ; suspended_processes
    ; placement_group
    ; v_p_c_zone_identifier
    ; enabled_metrics
    ; status
    ; tags
    ; termination_policies
    }

  let parse xml =
    Some
      { auto_scaling_group_name =
          Aws.Xml.required
            "AutoScalingGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupName" xml)
               String.parse)
      ; auto_scaling_group_a_r_n =
          Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupARN" xml) String.parse
      ; launch_configuration_name =
          Aws.Util.option_bind (Aws.Xml.member "LaunchConfigurationName" xml) String.parse
      ; min_size =
          Aws.Xml.required
            "MinSize"
            (Aws.Util.option_bind (Aws.Xml.member "MinSize" xml) Integer.parse)
      ; max_size =
          Aws.Xml.required
            "MaxSize"
            (Aws.Util.option_bind (Aws.Xml.member "MaxSize" xml) Integer.parse)
      ; desired_capacity =
          Aws.Xml.required
            "DesiredCapacity"
            (Aws.Util.option_bind (Aws.Xml.member "DesiredCapacity" xml) Integer.parse)
      ; default_cooldown =
          Aws.Xml.required
            "DefaultCooldown"
            (Aws.Util.option_bind (Aws.Xml.member "DefaultCooldown" xml) Integer.parse)
      ; availability_zones =
          Aws.Xml.required
            "AvailabilityZones"
            (Aws.Util.option_bind
               (Aws.Xml.member "AvailabilityZones" xml)
               AvailabilityZones.parse)
      ; load_balancer_names =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "LoadBalancerNames" xml)
               LoadBalancerNames.parse)
      ; health_check_type =
          Aws.Xml.required
            "HealthCheckType"
            (Aws.Util.option_bind (Aws.Xml.member "HealthCheckType" xml) String.parse)
      ; health_check_grace_period =
          Aws.Util.option_bind (Aws.Xml.member "HealthCheckGracePeriod" xml) Integer.parse
      ; instances =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Instances" xml) Instances.parse)
      ; created_time =
          Aws.Xml.required
            "CreatedTime"
            (Aws.Util.option_bind (Aws.Xml.member "CreatedTime" xml) DateTime.parse)
      ; suspended_processes =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "SuspendedProcesses" xml)
               SuspendedProcesses.parse)
      ; placement_group =
          Aws.Util.option_bind (Aws.Xml.member "PlacementGroup" xml) String.parse
      ; v_p_c_zone_identifier =
          Aws.Util.option_bind (Aws.Xml.member "VPCZoneIdentifier" xml) String.parse
      ; enabled_metrics =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "EnabledMetrics" xml)
               EnabledMetrics.parse)
      ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
      ; tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagDescriptionList.parse)
      ; termination_policies =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "TerminationPolicies" xml)
               TerminationPolicies.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "TerminationPolicies.member"
                , TerminationPolicies.to_query v.termination_policies ))
         ; Some (Aws.Query.Pair ("Tags.member", TagDescriptionList.to_query v.tags))
         ; Aws.Util.option_map v.status (fun f ->
               Aws.Query.Pair ("Status", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("EnabledMetrics.member", EnabledMetrics.to_query v.enabled_metrics))
         ; Aws.Util.option_map v.v_p_c_zone_identifier (fun f ->
               Aws.Query.Pair ("VPCZoneIdentifier", String.to_query f))
         ; Aws.Util.option_map v.placement_group (fun f ->
               Aws.Query.Pair ("PlacementGroup", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "SuspendedProcesses.member"
                , SuspendedProcesses.to_query v.suspended_processes ))
         ; Some (Aws.Query.Pair ("CreatedTime", DateTime.to_query v.created_time))
         ; Some (Aws.Query.Pair ("Instances.member", Instances.to_query v.instances))
         ; Aws.Util.option_map v.health_check_grace_period (fun f ->
               Aws.Query.Pair ("HealthCheckGracePeriod", Integer.to_query f))
         ; Some (Aws.Query.Pair ("HealthCheckType", String.to_query v.health_check_type))
         ; Some
             (Aws.Query.Pair
                ( "LoadBalancerNames.member"
                , LoadBalancerNames.to_query v.load_balancer_names ))
         ; Some
             (Aws.Query.Pair
                ( "AvailabilityZones.member"
                , AvailabilityZones.to_query v.availability_zones ))
         ; Some (Aws.Query.Pair ("DefaultCooldown", Integer.to_query v.default_cooldown))
         ; Some (Aws.Query.Pair ("DesiredCapacity", Integer.to_query v.desired_capacity))
         ; Some (Aws.Query.Pair ("MaxSize", Integer.to_query v.max_size))
         ; Some (Aws.Query.Pair ("MinSize", Integer.to_query v.min_size))
         ; Aws.Util.option_map v.launch_configuration_name (fun f ->
               Aws.Query.Pair ("LaunchConfigurationName", String.to_query f))
         ; Aws.Util.option_map v.auto_scaling_group_a_r_n (fun f ->
               Aws.Query.Pair ("AutoScalingGroupARN", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("TerminationPolicies", TerminationPolicies.to_json v.termination_policies)
         ; Some ("Tags", TagDescriptionList.to_json v.tags)
         ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
         ; Some ("EnabledMetrics", EnabledMetrics.to_json v.enabled_metrics)
         ; Aws.Util.option_map v.v_p_c_zone_identifier (fun f ->
               "VPCZoneIdentifier", String.to_json f)
         ; Aws.Util.option_map v.placement_group (fun f ->
               "PlacementGroup", String.to_json f)
         ; Some ("SuspendedProcesses", SuspendedProcesses.to_json v.suspended_processes)
         ; Some ("CreatedTime", DateTime.to_json v.created_time)
         ; Some ("Instances", Instances.to_json v.instances)
         ; Aws.Util.option_map v.health_check_grace_period (fun f ->
               "HealthCheckGracePeriod", Integer.to_json f)
         ; Some ("HealthCheckType", String.to_json v.health_check_type)
         ; Some ("LoadBalancerNames", LoadBalancerNames.to_json v.load_balancer_names)
         ; Some ("AvailabilityZones", AvailabilityZones.to_json v.availability_zones)
         ; Some ("DefaultCooldown", Integer.to_json v.default_cooldown)
         ; Some ("DesiredCapacity", Integer.to_json v.desired_capacity)
         ; Some ("MaxSize", Integer.to_json v.max_size)
         ; Some ("MinSize", Integer.to_json v.min_size)
         ; Aws.Util.option_map v.launch_configuration_name (fun f ->
               "LaunchConfigurationName", String.to_json f)
         ; Aws.Util.option_map v.auto_scaling_group_a_r_n (fun f ->
               "AutoScalingGroupARN", String.to_json f)
         ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
         ])

  let of_json j =
    { auto_scaling_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
    ; auto_scaling_group_a_r_n =
        Aws.Util.option_map (Aws.Json.lookup j "AutoScalingGroupARN") String.of_json
    ; launch_configuration_name =
        Aws.Util.option_map (Aws.Json.lookup j "LaunchConfigurationName") String.of_json
    ; min_size = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MinSize"))
    ; max_size = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxSize"))
    ; desired_capacity =
        Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DesiredCapacity"))
    ; default_cooldown =
        Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DefaultCooldown"))
    ; availability_zones =
        AvailabilityZones.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZones"))
    ; load_balancer_names =
        LoadBalancerNames.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerNames"))
    ; health_check_type =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheckType"))
    ; health_check_grace_period =
        Aws.Util.option_map (Aws.Json.lookup j "HealthCheckGracePeriod") Integer.of_json
    ; instances =
        Instances.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Instances"))
    ; created_time =
        DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CreatedTime"))
    ; suspended_processes =
        SuspendedProcesses.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "SuspendedProcesses"))
    ; placement_group =
        Aws.Util.option_map (Aws.Json.lookup j "PlacementGroup") String.of_json
    ; v_p_c_zone_identifier =
        Aws.Util.option_map (Aws.Json.lookup j "VPCZoneIdentifier") String.of_json
    ; enabled_metrics =
        EnabledMetrics.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "EnabledMetrics"))
    ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
    ; tags =
        TagDescriptionList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    ; termination_policies =
        TerminationPolicies.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "TerminationPolicies"))
    }
end

module AutoScalingGroups = struct
  type t = AutoScalingGroup.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map AutoScalingGroup.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list AutoScalingGroup.to_query v

  let to_json v = `List (List.map AutoScalingGroup.to_json v)

  let of_json j = Aws.Json.to_list AutoScalingGroup.of_json j
end

module LaunchConfigurationsType = struct
  type t =
    { launch_configurations : LaunchConfigurations.t
    ; next_token : String.t option
    }

  let make ~launch_configurations ?next_token () = { launch_configurations; next_token }

  let parse xml =
    Some
      { launch_configurations =
          Aws.Xml.required
            "LaunchConfigurations"
            (Aws.Util.option_bind
               (Aws.Xml.member "LaunchConfigurations" xml)
               LaunchConfigurations.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "LaunchConfigurations.member"
                , LaunchConfigurations.to_query v.launch_configurations ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some
             ("LaunchConfigurations", LaunchConfigurations.to_json v.launch_configurations)
         ])

  let of_json j =
    { launch_configurations =
        LaunchConfigurations.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LaunchConfigurations"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module DescribeNotificationConfigurationsType = struct
  type t =
    { auto_scaling_group_names : AutoScalingGroupNames.t
    ; next_token : String.t option
    ; max_records : Integer.t option
    }

  let make ?(auto_scaling_group_names = []) ?next_token ?max_records () =
    { auto_scaling_group_names; next_token; max_records }

  let parse xml =
    Some
      { auto_scaling_group_names =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupNames" xml)
               AutoScalingGroupNames.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "AutoScalingGroupNames.member"
                , AutoScalingGroupNames.to_query v.auto_scaling_group_names ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some
             ( "AutoScalingGroupNames"
             , AutoScalingGroupNames.to_json v.auto_scaling_group_names )
         ])

  let of_json j =
    { auto_scaling_group_names =
        AutoScalingGroupNames.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupNames"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    }
end

module LaunchConfigurationNameType = struct
  type t = { launch_configuration_name : String.t }

  let make ~launch_configuration_name () = { launch_configuration_name }

  let parse xml =
    Some
      { launch_configuration_name =
          Aws.Xml.required
            "LaunchConfigurationName"
            (Aws.Util.option_bind
               (Aws.Xml.member "LaunchConfigurationName" xml)
               String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("LaunchConfigurationName", String.to_query v.launch_configuration_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("LaunchConfigurationName", String.to_json v.launch_configuration_name) ])

  let of_json j =
    { launch_configuration_name =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LaunchConfigurationName"))
    }
end

module AutoScalingInstanceDetails = struct
  type t =
    { instance_id : String.t
    ; auto_scaling_group_name : String.t
    ; availability_zone : String.t
    ; lifecycle_state : String.t
    ; health_status : String.t
    ; launch_configuration_name : String.t
    }

  let make
      ~instance_id
      ~auto_scaling_group_name
      ~availability_zone
      ~lifecycle_state
      ~health_status
      ~launch_configuration_name
      () =
    { instance_id
    ; auto_scaling_group_name
    ; availability_zone
    ; lifecycle_state
    ; health_status
    ; launch_configuration_name
    }

  let parse xml =
    Some
      { instance_id =
          Aws.Xml.required
            "InstanceId"
            (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
      ; auto_scaling_group_name =
          Aws.Xml.required
            "AutoScalingGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupName" xml)
               String.parse)
      ; availability_zone =
          Aws.Xml.required
            "AvailabilityZone"
            (Aws.Util.option_bind (Aws.Xml.member "AvailabilityZone" xml) String.parse)
      ; lifecycle_state =
          Aws.Xml.required
            "LifecycleState"
            (Aws.Util.option_bind (Aws.Xml.member "LifecycleState" xml) String.parse)
      ; health_status =
          Aws.Xml.required
            "HealthStatus"
            (Aws.Util.option_bind (Aws.Xml.member "HealthStatus" xml) String.parse)
      ; launch_configuration_name =
          Aws.Xml.required
            "LaunchConfigurationName"
            (Aws.Util.option_bind
               (Aws.Xml.member "LaunchConfigurationName" xml)
               String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("LaunchConfigurationName", String.to_query v.launch_configuration_name))
         ; Some (Aws.Query.Pair ("HealthStatus", String.to_query v.health_status))
         ; Some (Aws.Query.Pair ("LifecycleState", String.to_query v.lifecycle_state))
         ; Some (Aws.Query.Pair ("AvailabilityZone", String.to_query v.availability_zone))
         ; Some
             (Aws.Query.Pair
                ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
         ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("LaunchConfigurationName", String.to_json v.launch_configuration_name)
         ; Some ("HealthStatus", String.to_json v.health_status)
         ; Some ("LifecycleState", String.to_json v.lifecycle_state)
         ; Some ("AvailabilityZone", String.to_json v.availability_zone)
         ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
         ; Some ("InstanceId", String.to_json v.instance_id)
         ])

  let of_json j =
    { instance_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
    ; auto_scaling_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
    ; availability_zone =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZone"))
    ; lifecycle_state =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LifecycleState"))
    ; health_status =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthStatus"))
    ; launch_configuration_name =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LaunchConfigurationName"))
    }
end

module AutoScalingGroupNamesType = struct
  type t =
    { auto_scaling_group_names : AutoScalingGroupNames.t
    ; next_token : String.t option
    ; max_records : Integer.t option
    }

  let make ?(auto_scaling_group_names = []) ?next_token ?max_records () =
    { auto_scaling_group_names; next_token; max_records }

  let parse xml =
    Some
      { auto_scaling_group_names =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupNames" xml)
               AutoScalingGroupNames.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "AutoScalingGroupNames.member"
                , AutoScalingGroupNames.to_query v.auto_scaling_group_names ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some
             ( "AutoScalingGroupNames"
             , AutoScalingGroupNames.to_json v.auto_scaling_group_names )
         ])

  let of_json j =
    { auto_scaling_group_names =
        AutoScalingGroupNames.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupNames"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    }
end

module DeleteLifecycleHookAnswer = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidNextToken = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module Tag = struct
  type t =
    { resource_id : String.t
    ; resource_type : String.t
    ; key : String.t
    ; value : String.t
    ; propagate_at_launch : Boolean.t
    }

  let make ~resource_id ~resource_type ~key ~value ~propagate_at_launch () =
    { resource_id; resource_type; key; value; propagate_at_launch }

  let parse xml =
    Some
      { resource_id =
          Aws.Xml.required
            "ResourceId"
            (Aws.Util.option_bind (Aws.Xml.member "ResourceId" xml) String.parse)
      ; resource_type =
          Aws.Xml.required
            "ResourceType"
            (Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse)
      ; key =
          Aws.Xml.required
            "Key"
            (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse)
      ; value =
          Aws.Xml.required
            "Value"
            (Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse)
      ; propagate_at_launch =
          Aws.Xml.required
            "PropagateAtLaunch"
            (Aws.Util.option_bind (Aws.Xml.member "PropagateAtLaunch" xml) Boolean.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("PropagateAtLaunch", Boolean.to_query v.propagate_at_launch))
         ; Some (Aws.Query.Pair ("Value", String.to_query v.value))
         ; Some (Aws.Query.Pair ("Key", String.to_query v.key))
         ; Some (Aws.Query.Pair ("ResourceType", String.to_query v.resource_type))
         ; Some (Aws.Query.Pair ("ResourceId", String.to_query v.resource_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("PropagateAtLaunch", Boolean.to_json v.propagate_at_launch)
         ; Some ("Value", String.to_json v.value)
         ; Some ("Key", String.to_json v.key)
         ; Some ("ResourceType", String.to_json v.resource_type)
         ; Some ("ResourceId", String.to_json v.resource_id)
         ])

  let of_json j =
    { resource_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceId"))
    ; resource_type =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceType"))
    ; key = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key"))
    ; value = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Value"))
    ; propagate_at_launch =
        Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PropagateAtLaunch"))
    }
end

module Tags = struct
  type t = Tag.t list

  let make elems () = elems

  let parse xml = Aws.Util.option_all (List.map Tag.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Tag.to_query v

  let to_json v = `List (List.map Tag.to_json v)

  let of_json j = Aws.Json.to_list Tag.of_json j
end

module CreateOrUpdateTagsType = struct
  type t = { tags : Tags.t }

  let make ~tags () = { tags }

  let parse xml =
    Some
      { tags =
          Aws.Xml.required
            "Tags"
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) Tags.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Tags.member", Tags.to_query v.tags)) ])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [ Some ("Tags", Tags.to_json v.tags) ])

  let of_json j =
    { tags = Tags.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags")) }
end

module TerminateInstanceInAutoScalingGroupType = struct
  type t =
    { instance_id : String.t
    ; should_decrement_desired_capacity : Boolean.t
    }

  let make ~instance_id ~should_decrement_desired_capacity () =
    { instance_id; should_decrement_desired_capacity }

  let parse xml =
    Some
      { instance_id =
          Aws.Xml.required
            "InstanceId"
            (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
      ; should_decrement_desired_capacity =
          Aws.Xml.required
            "ShouldDecrementDesiredCapacity"
            (Aws.Util.option_bind
               (Aws.Xml.member "ShouldDecrementDesiredCapacity" xml)
               Boolean.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "ShouldDecrementDesiredCapacity"
                , Boolean.to_query v.should_decrement_desired_capacity ))
         ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "ShouldDecrementDesiredCapacity"
             , Boolean.to_json v.should_decrement_desired_capacity )
         ; Some ("InstanceId", String.to_json v.instance_id)
         ])

  let of_json j =
    { instance_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
    ; should_decrement_desired_capacity =
        Boolean.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ShouldDecrementDesiredCapacity"))
    }
end

module AdjustmentType = struct
  type t = { adjustment_type : String.t option }

  let make ?adjustment_type () = { adjustment_type }

  let parse xml =
    Some
      { adjustment_type =
          Aws.Util.option_bind (Aws.Xml.member "AdjustmentType" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.adjustment_type (fun f ->
               Aws.Query.Pair ("AdjustmentType", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.adjustment_type (fun f ->
               "AdjustmentType", String.to_json f)
         ])

  let of_json j =
    { adjustment_type =
        Aws.Util.option_map (Aws.Json.lookup j "AdjustmentType") String.of_json
    }
end

module AdjustmentTypes = struct
  type t = AdjustmentType.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map AdjustmentType.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list AdjustmentType.to_query v

  let to_json v = `List (List.map AdjustmentType.to_json v)

  let of_json j = Aws.Json.to_list AdjustmentType.of_json j
end

module DescribeNotificationConfigurationsAnswer = struct
  type t =
    { notification_configurations : NotificationConfigurations.t
    ; next_token : String.t option
    }

  let make ~notification_configurations ?next_token () =
    { notification_configurations; next_token }

  let parse xml =
    Some
      { notification_configurations =
          Aws.Xml.required
            "NotificationConfigurations"
            (Aws.Util.option_bind
               (Aws.Xml.member "NotificationConfigurations" xml)
               NotificationConfigurations.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "NotificationConfigurations.member"
                , NotificationConfigurations.to_query v.notification_configurations ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some
             ( "NotificationConfigurations"
             , NotificationConfigurations.to_json v.notification_configurations )
         ])

  let of_json j =
    { notification_configurations =
        NotificationConfigurations.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "NotificationConfigurations"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module AttachInstancesQuery = struct
  type t =
    { instance_ids : InstanceIds.t
    ; auto_scaling_group_name : String.t
    }

  let make ?(instance_ids = []) ~auto_scaling_group_name () =
    { instance_ids; auto_scaling_group_name }

  let parse xml =
    Some
      { instance_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "InstanceIds" xml) InstanceIds.parse)
      ; auto_scaling_group_name =
          Aws.Xml.required
            "AutoScalingGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupName" xml)
               String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
         ; Some
             (Aws.Query.Pair ("InstanceIds.member", InstanceIds.to_query v.instance_ids))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
         ; Some ("InstanceIds", InstanceIds.to_json v.instance_ids)
         ])

  let of_json j =
    { instance_ids =
        InstanceIds.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceIds"))
    ; auto_scaling_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
    }
end

module PolicyTypes = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module PolicyNames = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module DescribePoliciesType = struct
  type t =
    { auto_scaling_group_name : String.t option
    ; policy_names : PolicyNames.t
    ; policy_types : PolicyTypes.t
    ; next_token : String.t option
    ; max_records : Integer.t option
    }

  let make
      ?auto_scaling_group_name
      ?(policy_names = [])
      ?(policy_types = [])
      ?next_token
      ?max_records
      () =
    { auto_scaling_group_name; policy_names; policy_types; next_token; max_records }

  let parse xml =
    Some
      { auto_scaling_group_name =
          Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse
      ; policy_names =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "PolicyNames" xml) PolicyNames.parse)
      ; policy_types =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "PolicyTypes" xml) PolicyTypes.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair ("PolicyTypes.member", PolicyTypes.to_query v.policy_types))
         ; Some
             (Aws.Query.Pair ("PolicyNames.member", PolicyNames.to_query v.policy_names))
         ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
               Aws.Query.Pair ("AutoScalingGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("PolicyTypes", PolicyTypes.to_json v.policy_types)
         ; Some ("PolicyNames", PolicyNames.to_json v.policy_names)
         ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
               "AutoScalingGroupName", String.to_json f)
         ])

  let of_json j =
    { auto_scaling_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "AutoScalingGroupName") String.of_json
    ; policy_names =
        PolicyNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyNames"))
    ; policy_types =
        PolicyTypes.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyTypes"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    }
end

module EnableMetricsCollectionQuery = struct
  type t =
    { auto_scaling_group_name : String.t
    ; metrics : Metrics.t
    ; granularity : String.t
    }

  let make ~auto_scaling_group_name ?(metrics = []) ~granularity () =
    { auto_scaling_group_name; metrics; granularity }

  let parse xml =
    Some
      { auto_scaling_group_name =
          Aws.Xml.required
            "AutoScalingGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupName" xml)
               String.parse)
      ; metrics =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Metrics" xml) Metrics.parse)
      ; granularity =
          Aws.Xml.required
            "Granularity"
            (Aws.Util.option_bind (Aws.Xml.member "Granularity" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Granularity", String.to_query v.granularity))
         ; Some (Aws.Query.Pair ("Metrics.member", Metrics.to_query v.metrics))
         ; Some
             (Aws.Query.Pair
                ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Granularity", String.to_json v.granularity)
         ; Some ("Metrics", Metrics.to_json v.metrics)
         ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
         ])

  let of_json j =
    { auto_scaling_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
    ; metrics = Metrics.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Metrics"))
    ; granularity =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Granularity"))
    }
end

module StepAdjustment = struct
  type t =
    { metric_interval_lower_bound : Double.t option
    ; metric_interval_upper_bound : Double.t option
    ; scaling_adjustment : Integer.t
    }

  let make
      ?metric_interval_lower_bound
      ?metric_interval_upper_bound
      ~scaling_adjustment
      () =
    { metric_interval_lower_bound; metric_interval_upper_bound; scaling_adjustment }

  let parse xml =
    Some
      { metric_interval_lower_bound =
          Aws.Util.option_bind
            (Aws.Xml.member "MetricIntervalLowerBound" xml)
            Double.parse
      ; metric_interval_upper_bound =
          Aws.Util.option_bind
            (Aws.Xml.member "MetricIntervalUpperBound" xml)
            Double.parse
      ; scaling_adjustment =
          Aws.Xml.required
            "ScalingAdjustment"
            (Aws.Util.option_bind (Aws.Xml.member "ScalingAdjustment" xml) Integer.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("ScalingAdjustment", Integer.to_query v.scaling_adjustment))
         ; Aws.Util.option_map v.metric_interval_upper_bound (fun f ->
               Aws.Query.Pair ("MetricIntervalUpperBound", Double.to_query f))
         ; Aws.Util.option_map v.metric_interval_lower_bound (fun f ->
               Aws.Query.Pair ("MetricIntervalLowerBound", Double.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ScalingAdjustment", Integer.to_json v.scaling_adjustment)
         ; Aws.Util.option_map v.metric_interval_upper_bound (fun f ->
               "MetricIntervalUpperBound", Double.to_json f)
         ; Aws.Util.option_map v.metric_interval_lower_bound (fun f ->
               "MetricIntervalLowerBound", Double.to_json f)
         ])

  let of_json j =
    { metric_interval_lower_bound =
        Aws.Util.option_map (Aws.Json.lookup j "MetricIntervalLowerBound") Double.of_json
    ; metric_interval_upper_bound =
        Aws.Util.option_map (Aws.Json.lookup j "MetricIntervalUpperBound") Double.of_json
    ; scaling_adjustment =
        Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ScalingAdjustment"))
    }
end

module StepAdjustments = struct
  type t = StepAdjustment.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map StepAdjustment.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list StepAdjustment.to_query v

  let to_json v = `List (List.map StepAdjustment.to_json v)

  let of_json j = Aws.Json.to_list StepAdjustment.of_json j
end

module PutScalingPolicyType = struct
  type t =
    { auto_scaling_group_name : String.t
    ; policy_name : String.t
    ; policy_type : String.t option
    ; adjustment_type : String.t
    ; min_adjustment_step : Integer.t option
    ; min_adjustment_magnitude : Integer.t option
    ; scaling_adjustment : Integer.t option
    ; cooldown : Integer.t option
    ; metric_aggregation_type : String.t option
    ; step_adjustments : StepAdjustments.t
    ; estimated_instance_warmup : Integer.t option
    }

  let make
      ~auto_scaling_group_name
      ~policy_name
      ?policy_type
      ~adjustment_type
      ?min_adjustment_step
      ?min_adjustment_magnitude
      ?scaling_adjustment
      ?cooldown
      ?metric_aggregation_type
      ?(step_adjustments = [])
      ?estimated_instance_warmup
      () =
    { auto_scaling_group_name
    ; policy_name
    ; policy_type
    ; adjustment_type
    ; min_adjustment_step
    ; min_adjustment_magnitude
    ; scaling_adjustment
    ; cooldown
    ; metric_aggregation_type
    ; step_adjustments
    ; estimated_instance_warmup
    }

  let parse xml =
    Some
      { auto_scaling_group_name =
          Aws.Xml.required
            "AutoScalingGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupName" xml)
               String.parse)
      ; policy_name =
          Aws.Xml.required
            "PolicyName"
            (Aws.Util.option_bind (Aws.Xml.member "PolicyName" xml) String.parse)
      ; policy_type = Aws.Util.option_bind (Aws.Xml.member "PolicyType" xml) String.parse
      ; adjustment_type =
          Aws.Xml.required
            "AdjustmentType"
            (Aws.Util.option_bind (Aws.Xml.member "AdjustmentType" xml) String.parse)
      ; min_adjustment_step =
          Aws.Util.option_bind (Aws.Xml.member "MinAdjustmentStep" xml) Integer.parse
      ; min_adjustment_magnitude =
          Aws.Util.option_bind (Aws.Xml.member "MinAdjustmentMagnitude" xml) Integer.parse
      ; scaling_adjustment =
          Aws.Util.option_bind (Aws.Xml.member "ScalingAdjustment" xml) Integer.parse
      ; cooldown = Aws.Util.option_bind (Aws.Xml.member "Cooldown" xml) Integer.parse
      ; metric_aggregation_type =
          Aws.Util.option_bind (Aws.Xml.member "MetricAggregationType" xml) String.parse
      ; step_adjustments =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "StepAdjustments" xml)
               StepAdjustments.parse)
      ; estimated_instance_warmup =
          Aws.Util.option_bind
            (Aws.Xml.member "EstimatedInstanceWarmup" xml)
            Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.estimated_instance_warmup (fun f ->
               Aws.Query.Pair ("EstimatedInstanceWarmup", Integer.to_query f))
         ; Some
             (Aws.Query.Pair
                ("StepAdjustments.member", StepAdjustments.to_query v.step_adjustments))
         ; Aws.Util.option_map v.metric_aggregation_type (fun f ->
               Aws.Query.Pair ("MetricAggregationType", String.to_query f))
         ; Aws.Util.option_map v.cooldown (fun f ->
               Aws.Query.Pair ("Cooldown", Integer.to_query f))
         ; Aws.Util.option_map v.scaling_adjustment (fun f ->
               Aws.Query.Pair ("ScalingAdjustment", Integer.to_query f))
         ; Aws.Util.option_map v.min_adjustment_magnitude (fun f ->
               Aws.Query.Pair ("MinAdjustmentMagnitude", Integer.to_query f))
         ; Aws.Util.option_map v.min_adjustment_step (fun f ->
               Aws.Query.Pair ("MinAdjustmentStep", Integer.to_query f))
         ; Some (Aws.Query.Pair ("AdjustmentType", String.to_query v.adjustment_type))
         ; Aws.Util.option_map v.policy_type (fun f ->
               Aws.Query.Pair ("PolicyType", String.to_query f))
         ; Some (Aws.Query.Pair ("PolicyName", String.to_query v.policy_name))
         ; Some
             (Aws.Query.Pair
                ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.estimated_instance_warmup (fun f ->
               "EstimatedInstanceWarmup", Integer.to_json f)
         ; Some ("StepAdjustments", StepAdjustments.to_json v.step_adjustments)
         ; Aws.Util.option_map v.metric_aggregation_type (fun f ->
               "MetricAggregationType", String.to_json f)
         ; Aws.Util.option_map v.cooldown (fun f -> "Cooldown", Integer.to_json f)
         ; Aws.Util.option_map v.scaling_adjustment (fun f ->
               "ScalingAdjustment", Integer.to_json f)
         ; Aws.Util.option_map v.min_adjustment_magnitude (fun f ->
               "MinAdjustmentMagnitude", Integer.to_json f)
         ; Aws.Util.option_map v.min_adjustment_step (fun f ->
               "MinAdjustmentStep", Integer.to_json f)
         ; Some ("AdjustmentType", String.to_json v.adjustment_type)
         ; Aws.Util.option_map v.policy_type (fun f -> "PolicyType", String.to_json f)
         ; Some ("PolicyName", String.to_json v.policy_name)
         ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
         ])

  let of_json j =
    { auto_scaling_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
    ; policy_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyName"))
    ; policy_type = Aws.Util.option_map (Aws.Json.lookup j "PolicyType") String.of_json
    ; adjustment_type =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AdjustmentType"))
    ; min_adjustment_step =
        Aws.Util.option_map (Aws.Json.lookup j "MinAdjustmentStep") Integer.of_json
    ; min_adjustment_magnitude =
        Aws.Util.option_map (Aws.Json.lookup j "MinAdjustmentMagnitude") Integer.of_json
    ; scaling_adjustment =
        Aws.Util.option_map (Aws.Json.lookup j "ScalingAdjustment") Integer.of_json
    ; cooldown = Aws.Util.option_map (Aws.Json.lookup j "Cooldown") Integer.of_json
    ; metric_aggregation_type =
        Aws.Util.option_map (Aws.Json.lookup j "MetricAggregationType") String.of_json
    ; step_adjustments =
        StepAdjustments.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "StepAdjustments"))
    ; estimated_instance_warmup =
        Aws.Util.option_map (Aws.Json.lookup j "EstimatedInstanceWarmup") Integer.of_json
    }
end

module SetInstanceHealthQuery = struct
  type t =
    { instance_id : String.t
    ; health_status : String.t
    ; should_respect_grace_period : Boolean.t option
    }

  let make ~instance_id ~health_status ?should_respect_grace_period () =
    { instance_id; health_status; should_respect_grace_period }

  let parse xml =
    Some
      { instance_id =
          Aws.Xml.required
            "InstanceId"
            (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
      ; health_status =
          Aws.Xml.required
            "HealthStatus"
            (Aws.Util.option_bind (Aws.Xml.member "HealthStatus" xml) String.parse)
      ; should_respect_grace_period =
          Aws.Util.option_bind
            (Aws.Xml.member "ShouldRespectGracePeriod" xml)
            Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.should_respect_grace_period (fun f ->
               Aws.Query.Pair ("ShouldRespectGracePeriod", Boolean.to_query f))
         ; Some (Aws.Query.Pair ("HealthStatus", String.to_query v.health_status))
         ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.should_respect_grace_period (fun f ->
               "ShouldRespectGracePeriod", Boolean.to_json f)
         ; Some ("HealthStatus", String.to_json v.health_status)
         ; Some ("InstanceId", String.to_json v.instance_id)
         ])

  let of_json j =
    { instance_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
    ; health_status =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthStatus"))
    ; should_respect_grace_period =
        Aws.Util.option_map (Aws.Json.lookup j "ShouldRespectGracePeriod") Boolean.of_json
    }
end

module Alarm = struct
  type t =
    { alarm_name : String.t option
    ; alarm_a_r_n : String.t option
    }

  let make ?alarm_name ?alarm_a_r_n () = { alarm_name; alarm_a_r_n }

  let parse xml =
    Some
      { alarm_name = Aws.Util.option_bind (Aws.Xml.member "AlarmName" xml) String.parse
      ; alarm_a_r_n = Aws.Util.option_bind (Aws.Xml.member "AlarmARN" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.alarm_a_r_n (fun f ->
               Aws.Query.Pair ("AlarmARN", String.to_query f))
         ; Aws.Util.option_map v.alarm_name (fun f ->
               Aws.Query.Pair ("AlarmName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.alarm_a_r_n (fun f -> "AlarmARN", String.to_json f)
         ; Aws.Util.option_map v.alarm_name (fun f -> "AlarmName", String.to_json f)
         ])

  let of_json j =
    { alarm_name = Aws.Util.option_map (Aws.Json.lookup j "AlarmName") String.of_json
    ; alarm_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "AlarmARN") String.of_json
    }
end

module Alarms = struct
  type t = Alarm.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Alarm.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Alarm.to_query v

  let to_json v = `List (List.map Alarm.to_json v)

  let of_json j = Aws.Json.to_list Alarm.of_json j
end

module ScalingPolicy = struct
  type t =
    { auto_scaling_group_name : String.t option
    ; policy_name : String.t option
    ; policy_a_r_n : String.t option
    ; policy_type : String.t option
    ; adjustment_type : String.t option
    ; min_adjustment_step : Integer.t option
    ; min_adjustment_magnitude : Integer.t option
    ; scaling_adjustment : Integer.t option
    ; cooldown : Integer.t option
    ; step_adjustments : StepAdjustments.t
    ; metric_aggregation_type : String.t option
    ; estimated_instance_warmup : Integer.t option
    ; alarms : Alarms.t
    }

  let make
      ?auto_scaling_group_name
      ?policy_name
      ?policy_a_r_n
      ?policy_type
      ?adjustment_type
      ?min_adjustment_step
      ?min_adjustment_magnitude
      ?scaling_adjustment
      ?cooldown
      ?(step_adjustments = [])
      ?metric_aggregation_type
      ?estimated_instance_warmup
      ?(alarms = [])
      () =
    { auto_scaling_group_name
    ; policy_name
    ; policy_a_r_n
    ; policy_type
    ; adjustment_type
    ; min_adjustment_step
    ; min_adjustment_magnitude
    ; scaling_adjustment
    ; cooldown
    ; step_adjustments
    ; metric_aggregation_type
    ; estimated_instance_warmup
    ; alarms
    }

  let parse xml =
    Some
      { auto_scaling_group_name =
          Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse
      ; policy_name = Aws.Util.option_bind (Aws.Xml.member "PolicyName" xml) String.parse
      ; policy_a_r_n = Aws.Util.option_bind (Aws.Xml.member "PolicyARN" xml) String.parse
      ; policy_type = Aws.Util.option_bind (Aws.Xml.member "PolicyType" xml) String.parse
      ; adjustment_type =
          Aws.Util.option_bind (Aws.Xml.member "AdjustmentType" xml) String.parse
      ; min_adjustment_step =
          Aws.Util.option_bind (Aws.Xml.member "MinAdjustmentStep" xml) Integer.parse
      ; min_adjustment_magnitude =
          Aws.Util.option_bind (Aws.Xml.member "MinAdjustmentMagnitude" xml) Integer.parse
      ; scaling_adjustment =
          Aws.Util.option_bind (Aws.Xml.member "ScalingAdjustment" xml) Integer.parse
      ; cooldown = Aws.Util.option_bind (Aws.Xml.member "Cooldown" xml) Integer.parse
      ; step_adjustments =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "StepAdjustments" xml)
               StepAdjustments.parse)
      ; metric_aggregation_type =
          Aws.Util.option_bind (Aws.Xml.member "MetricAggregationType" xml) String.parse
      ; estimated_instance_warmup =
          Aws.Util.option_bind
            (Aws.Xml.member "EstimatedInstanceWarmup" xml)
            Integer.parse
      ; alarms =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Alarms" xml) Alarms.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Alarms.member", Alarms.to_query v.alarms))
         ; Aws.Util.option_map v.estimated_instance_warmup (fun f ->
               Aws.Query.Pair ("EstimatedInstanceWarmup", Integer.to_query f))
         ; Aws.Util.option_map v.metric_aggregation_type (fun f ->
               Aws.Query.Pair ("MetricAggregationType", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("StepAdjustments.member", StepAdjustments.to_query v.step_adjustments))
         ; Aws.Util.option_map v.cooldown (fun f ->
               Aws.Query.Pair ("Cooldown", Integer.to_query f))
         ; Aws.Util.option_map v.scaling_adjustment (fun f ->
               Aws.Query.Pair ("ScalingAdjustment", Integer.to_query f))
         ; Aws.Util.option_map v.min_adjustment_magnitude (fun f ->
               Aws.Query.Pair ("MinAdjustmentMagnitude", Integer.to_query f))
         ; Aws.Util.option_map v.min_adjustment_step (fun f ->
               Aws.Query.Pair ("MinAdjustmentStep", Integer.to_query f))
         ; Aws.Util.option_map v.adjustment_type (fun f ->
               Aws.Query.Pair ("AdjustmentType", String.to_query f))
         ; Aws.Util.option_map v.policy_type (fun f ->
               Aws.Query.Pair ("PolicyType", String.to_query f))
         ; Aws.Util.option_map v.policy_a_r_n (fun f ->
               Aws.Query.Pair ("PolicyARN", String.to_query f))
         ; Aws.Util.option_map v.policy_name (fun f ->
               Aws.Query.Pair ("PolicyName", String.to_query f))
         ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
               Aws.Query.Pair ("AutoScalingGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Alarms", Alarms.to_json v.alarms)
         ; Aws.Util.option_map v.estimated_instance_warmup (fun f ->
               "EstimatedInstanceWarmup", Integer.to_json f)
         ; Aws.Util.option_map v.metric_aggregation_type (fun f ->
               "MetricAggregationType", String.to_json f)
         ; Some ("StepAdjustments", StepAdjustments.to_json v.step_adjustments)
         ; Aws.Util.option_map v.cooldown (fun f -> "Cooldown", Integer.to_json f)
         ; Aws.Util.option_map v.scaling_adjustment (fun f ->
               "ScalingAdjustment", Integer.to_json f)
         ; Aws.Util.option_map v.min_adjustment_magnitude (fun f ->
               "MinAdjustmentMagnitude", Integer.to_json f)
         ; Aws.Util.option_map v.min_adjustment_step (fun f ->
               "MinAdjustmentStep", Integer.to_json f)
         ; Aws.Util.option_map v.adjustment_type (fun f ->
               "AdjustmentType", String.to_json f)
         ; Aws.Util.option_map v.policy_type (fun f -> "PolicyType", String.to_json f)
         ; Aws.Util.option_map v.policy_a_r_n (fun f -> "PolicyARN", String.to_json f)
         ; Aws.Util.option_map v.policy_name (fun f -> "PolicyName", String.to_json f)
         ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
               "AutoScalingGroupName", String.to_json f)
         ])

  let of_json j =
    { auto_scaling_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "AutoScalingGroupName") String.of_json
    ; policy_name = Aws.Util.option_map (Aws.Json.lookup j "PolicyName") String.of_json
    ; policy_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "PolicyARN") String.of_json
    ; policy_type = Aws.Util.option_map (Aws.Json.lookup j "PolicyType") String.of_json
    ; adjustment_type =
        Aws.Util.option_map (Aws.Json.lookup j "AdjustmentType") String.of_json
    ; min_adjustment_step =
        Aws.Util.option_map (Aws.Json.lookup j "MinAdjustmentStep") Integer.of_json
    ; min_adjustment_magnitude =
        Aws.Util.option_map (Aws.Json.lookup j "MinAdjustmentMagnitude") Integer.of_json
    ; scaling_adjustment =
        Aws.Util.option_map (Aws.Json.lookup j "ScalingAdjustment") Integer.of_json
    ; cooldown = Aws.Util.option_map (Aws.Json.lookup j "Cooldown") Integer.of_json
    ; step_adjustments =
        StepAdjustments.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "StepAdjustments"))
    ; metric_aggregation_type =
        Aws.Util.option_map (Aws.Json.lookup j "MetricAggregationType") String.of_json
    ; estimated_instance_warmup =
        Aws.Util.option_map (Aws.Json.lookup j "EstimatedInstanceWarmup") Integer.of_json
    ; alarms = Alarms.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Alarms"))
    }
end

module ScalingPolicies = struct
  type t = ScalingPolicy.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map ScalingPolicy.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list ScalingPolicy.to_query v

  let to_json v = `List (List.map ScalingPolicy.to_json v)

  let of_json j = Aws.Json.to_list ScalingPolicy.of_json j
end

module ExecutePolicyType = struct
  type t =
    { auto_scaling_group_name : String.t option
    ; policy_name : String.t
    ; honor_cooldown : Boolean.t option
    ; metric_value : Double.t option
    ; breach_threshold : Double.t option
    }

  let make
      ?auto_scaling_group_name
      ~policy_name
      ?honor_cooldown
      ?metric_value
      ?breach_threshold
      () =
    { auto_scaling_group_name
    ; policy_name
    ; honor_cooldown
    ; metric_value
    ; breach_threshold
    }

  let parse xml =
    Some
      { auto_scaling_group_name =
          Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse
      ; policy_name =
          Aws.Xml.required
            "PolicyName"
            (Aws.Util.option_bind (Aws.Xml.member "PolicyName" xml) String.parse)
      ; honor_cooldown =
          Aws.Util.option_bind (Aws.Xml.member "HonorCooldown" xml) Boolean.parse
      ; metric_value =
          Aws.Util.option_bind (Aws.Xml.member "MetricValue" xml) Double.parse
      ; breach_threshold =
          Aws.Util.option_bind (Aws.Xml.member "BreachThreshold" xml) Double.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.breach_threshold (fun f ->
               Aws.Query.Pair ("BreachThreshold", Double.to_query f))
         ; Aws.Util.option_map v.metric_value (fun f ->
               Aws.Query.Pair ("MetricValue", Double.to_query f))
         ; Aws.Util.option_map v.honor_cooldown (fun f ->
               Aws.Query.Pair ("HonorCooldown", Boolean.to_query f))
         ; Some (Aws.Query.Pair ("PolicyName", String.to_query v.policy_name))
         ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
               Aws.Query.Pair ("AutoScalingGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.breach_threshold (fun f ->
               "BreachThreshold", Double.to_json f)
         ; Aws.Util.option_map v.metric_value (fun f -> "MetricValue", Double.to_json f)
         ; Aws.Util.option_map v.honor_cooldown (fun f ->
               "HonorCooldown", Boolean.to_json f)
         ; Some ("PolicyName", String.to_json v.policy_name)
         ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
               "AutoScalingGroupName", String.to_json f)
         ])

  let of_json j =
    { auto_scaling_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "AutoScalingGroupName") String.of_json
    ; policy_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyName"))
    ; honor_cooldown =
        Aws.Util.option_map (Aws.Json.lookup j "HonorCooldown") Boolean.of_json
    ; metric_value = Aws.Util.option_map (Aws.Json.lookup j "MetricValue") Double.of_json
    ; breach_threshold =
        Aws.Util.option_map (Aws.Json.lookup j "BreachThreshold") Double.of_json
    }
end

module AutoScalingGroupsType = struct
  type t =
    { auto_scaling_groups : AutoScalingGroups.t
    ; next_token : String.t option
    }

  let make ~auto_scaling_groups ?next_token () = { auto_scaling_groups; next_token }

  let parse xml =
    Some
      { auto_scaling_groups =
          Aws.Xml.required
            "AutoScalingGroups"
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroups" xml)
               AutoScalingGroups.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "AutoScalingGroups.member"
                , AutoScalingGroups.to_query v.auto_scaling_groups ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("AutoScalingGroups", AutoScalingGroups.to_json v.auto_scaling_groups)
         ])

  let of_json j =
    { auto_scaling_groups =
        AutoScalingGroups.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroups"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module DescribeAdjustmentTypesAnswer = struct
  type t = { adjustment_types : AdjustmentTypes.t }

  let make ?(adjustment_types = []) () = { adjustment_types }

  let parse xml =
    Some
      { adjustment_types =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "AdjustmentTypes" xml)
               AdjustmentTypes.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("AdjustmentTypes.member", AdjustmentTypes.to_query v.adjustment_types))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("AdjustmentTypes", AdjustmentTypes.to_json v.adjustment_types) ])

  let of_json j =
    { adjustment_types =
        AdjustmentTypes.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AdjustmentTypes"))
    }
end

module DescribeTerminationPolicyTypesAnswer = struct
  type t = { termination_policy_types : TerminationPolicies.t }

  let make ?(termination_policy_types = []) () = { termination_policy_types }

  let parse xml =
    Some
      { termination_policy_types =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "TerminationPolicyTypes" xml)
               TerminationPolicies.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "TerminationPolicyTypes.member"
                , TerminationPolicies.to_query v.termination_policy_types ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "TerminationPolicyTypes"
             , TerminationPolicies.to_json v.termination_policy_types )
         ])

  let of_json j =
    { termination_policy_types =
        TerminationPolicies.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "TerminationPolicyTypes"))
    }
end

module DeleteTagsType = struct
  type t = { tags : Tags.t }

  let make ~tags () = { tags }

  let parse xml =
    Some
      { tags =
          Aws.Xml.required
            "Tags"
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) Tags.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Tags.member", Tags.to_query v.tags)) ])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [ Some ("Tags", Tags.to_json v.tags) ])

  let of_json j =
    { tags = Tags.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags")) }
end

module LaunchConfigurationNames = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module DescribeAccountLimitsAnswer = struct
  type t =
    { max_number_of_auto_scaling_groups : Integer.t option
    ; max_number_of_launch_configurations : Integer.t option
    }

  let make ?max_number_of_auto_scaling_groups ?max_number_of_launch_configurations () =
    { max_number_of_auto_scaling_groups; max_number_of_launch_configurations }

  let parse xml =
    Some
      { max_number_of_auto_scaling_groups =
          Aws.Util.option_bind
            (Aws.Xml.member "MaxNumberOfAutoScalingGroups" xml)
            Integer.parse
      ; max_number_of_launch_configurations =
          Aws.Util.option_bind
            (Aws.Xml.member "MaxNumberOfLaunchConfigurations" xml)
            Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_number_of_launch_configurations (fun f ->
               Aws.Query.Pair ("MaxNumberOfLaunchConfigurations", Integer.to_query f))
         ; Aws.Util.option_map v.max_number_of_auto_scaling_groups (fun f ->
               Aws.Query.Pair ("MaxNumberOfAutoScalingGroups", Integer.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_number_of_launch_configurations (fun f ->
               "MaxNumberOfLaunchConfigurations", Integer.to_json f)
         ; Aws.Util.option_map v.max_number_of_auto_scaling_groups (fun f ->
               "MaxNumberOfAutoScalingGroups", Integer.to_json f)
         ])

  let of_json j =
    { max_number_of_auto_scaling_groups =
        Aws.Util.option_map
          (Aws.Json.lookup j "MaxNumberOfAutoScalingGroups")
          Integer.of_json
    ; max_number_of_launch_configurations =
        Aws.Util.option_map
          (Aws.Json.lookup j "MaxNumberOfLaunchConfigurations")
          Integer.of_json
    }
end

module DeleteNotificationConfigurationType = struct
  type t =
    { auto_scaling_group_name : String.t
    ; topic_a_r_n : String.t
    }

  let make ~auto_scaling_group_name ~topic_a_r_n () =
    { auto_scaling_group_name; topic_a_r_n }

  let parse xml =
    Some
      { auto_scaling_group_name =
          Aws.Xml.required
            "AutoScalingGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupName" xml)
               String.parse)
      ; topic_a_r_n =
          Aws.Xml.required
            "TopicARN"
            (Aws.Util.option_bind (Aws.Xml.member "TopicARN" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("TopicARN", String.to_query v.topic_a_r_n))
         ; Some
             (Aws.Query.Pair
                ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("TopicARN", String.to_json v.topic_a_r_n)
         ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
         ])

  let of_json j =
    { auto_scaling_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
    ; topic_a_r_n = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TopicARN"))
    }
end

module AlreadyExistsFault = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module Filters = struct
  type t = Filter.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Filter.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Filter.to_query v

  let to_json v = `List (List.map Filter.to_json v)

  let of_json j = Aws.Json.to_list Filter.of_json j
end

module PutScheduledUpdateGroupActionType = struct
  type t =
    { auto_scaling_group_name : String.t
    ; scheduled_action_name : String.t
    ; time : DateTime.t option
    ; start_time : DateTime.t option
    ; end_time : DateTime.t option
    ; recurrence : String.t option
    ; min_size : Integer.t option
    ; max_size : Integer.t option
    ; desired_capacity : Integer.t option
    }

  let make
      ~auto_scaling_group_name
      ~scheduled_action_name
      ?time
      ?start_time
      ?end_time
      ?recurrence
      ?min_size
      ?max_size
      ?desired_capacity
      () =
    { auto_scaling_group_name
    ; scheduled_action_name
    ; time
    ; start_time
    ; end_time
    ; recurrence
    ; min_size
    ; max_size
    ; desired_capacity
    }

  let parse xml =
    Some
      { auto_scaling_group_name =
          Aws.Xml.required
            "AutoScalingGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupName" xml)
               String.parse)
      ; scheduled_action_name =
          Aws.Xml.required
            "ScheduledActionName"
            (Aws.Util.option_bind (Aws.Xml.member "ScheduledActionName" xml) String.parse)
      ; time = Aws.Util.option_bind (Aws.Xml.member "Time" xml) DateTime.parse
      ; start_time = Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse
      ; end_time = Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse
      ; recurrence = Aws.Util.option_bind (Aws.Xml.member "Recurrence" xml) String.parse
      ; min_size = Aws.Util.option_bind (Aws.Xml.member "MinSize" xml) Integer.parse
      ; max_size = Aws.Util.option_bind (Aws.Xml.member "MaxSize" xml) Integer.parse
      ; desired_capacity =
          Aws.Util.option_bind (Aws.Xml.member "DesiredCapacity" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.desired_capacity (fun f ->
               Aws.Query.Pair ("DesiredCapacity", Integer.to_query f))
         ; Aws.Util.option_map v.max_size (fun f ->
               Aws.Query.Pair ("MaxSize", Integer.to_query f))
         ; Aws.Util.option_map v.min_size (fun f ->
               Aws.Query.Pair ("MinSize", Integer.to_query f))
         ; Aws.Util.option_map v.recurrence (fun f ->
               Aws.Query.Pair ("Recurrence", String.to_query f))
         ; Aws.Util.option_map v.end_time (fun f ->
               Aws.Query.Pair ("EndTime", DateTime.to_query f))
         ; Aws.Util.option_map v.start_time (fun f ->
               Aws.Query.Pair ("StartTime", DateTime.to_query f))
         ; Aws.Util.option_map v.time (fun f ->
               Aws.Query.Pair ("Time", DateTime.to_query f))
         ; Some
             (Aws.Query.Pair
                ("ScheduledActionName", String.to_query v.scheduled_action_name))
         ; Some
             (Aws.Query.Pair
                ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.desired_capacity (fun f ->
               "DesiredCapacity", Integer.to_json f)
         ; Aws.Util.option_map v.max_size (fun f -> "MaxSize", Integer.to_json f)
         ; Aws.Util.option_map v.min_size (fun f -> "MinSize", Integer.to_json f)
         ; Aws.Util.option_map v.recurrence (fun f -> "Recurrence", String.to_json f)
         ; Aws.Util.option_map v.end_time (fun f -> "EndTime", DateTime.to_json f)
         ; Aws.Util.option_map v.start_time (fun f -> "StartTime", DateTime.to_json f)
         ; Aws.Util.option_map v.time (fun f -> "Time", DateTime.to_json f)
         ; Some ("ScheduledActionName", String.to_json v.scheduled_action_name)
         ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
         ])

  let of_json j =
    { auto_scaling_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
    ; scheduled_action_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ScheduledActionName"))
    ; time = Aws.Util.option_map (Aws.Json.lookup j "Time") DateTime.of_json
    ; start_time = Aws.Util.option_map (Aws.Json.lookup j "StartTime") DateTime.of_json
    ; end_time = Aws.Util.option_map (Aws.Json.lookup j "EndTime") DateTime.of_json
    ; recurrence = Aws.Util.option_map (Aws.Json.lookup j "Recurrence") String.of_json
    ; min_size = Aws.Util.option_map (Aws.Json.lookup j "MinSize") Integer.of_json
    ; max_size = Aws.Util.option_map (Aws.Json.lookup j "MaxSize") Integer.of_json
    ; desired_capacity =
        Aws.Util.option_map (Aws.Json.lookup j "DesiredCapacity") Integer.of_json
    }
end

module CreateLaunchConfigurationType = struct
  type t =
    { launch_configuration_name : String.t
    ; image_id : String.t option
    ; key_name : String.t option
    ; security_groups : SecurityGroups.t
    ; classic_link_v_p_c_id : String.t option
    ; classic_link_v_p_c_security_groups : ClassicLinkVPCSecurityGroups.t
    ; user_data : String.t option
    ; instance_id : String.t option
    ; instance_type : String.t option
    ; kernel_id : String.t option
    ; ramdisk_id : String.t option
    ; block_device_mappings : BlockDeviceMappings.t
    ; instance_monitoring : InstanceMonitoring.t option
    ; spot_price : String.t option
    ; iam_instance_profile : String.t option
    ; ebs_optimized : Boolean.t option
    ; associate_public_ip_address : Boolean.t option
    ; placement_tenancy : String.t option
    }

  let make
      ~launch_configuration_name
      ?image_id
      ?key_name
      ?(security_groups = [])
      ?classic_link_v_p_c_id
      ?(classic_link_v_p_c_security_groups = [])
      ?user_data
      ?instance_id
      ?instance_type
      ?kernel_id
      ?ramdisk_id
      ?(block_device_mappings = [])
      ?instance_monitoring
      ?spot_price
      ?iam_instance_profile
      ?ebs_optimized
      ?associate_public_ip_address
      ?placement_tenancy
      () =
    { launch_configuration_name
    ; image_id
    ; key_name
    ; security_groups
    ; classic_link_v_p_c_id
    ; classic_link_v_p_c_security_groups
    ; user_data
    ; instance_id
    ; instance_type
    ; kernel_id
    ; ramdisk_id
    ; block_device_mappings
    ; instance_monitoring
    ; spot_price
    ; iam_instance_profile
    ; ebs_optimized
    ; associate_public_ip_address
    ; placement_tenancy
    }

  let parse xml =
    Some
      { launch_configuration_name =
          Aws.Xml.required
            "LaunchConfigurationName"
            (Aws.Util.option_bind
               (Aws.Xml.member "LaunchConfigurationName" xml)
               String.parse)
      ; image_id = Aws.Util.option_bind (Aws.Xml.member "ImageId" xml) String.parse
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
      ; instance_id = Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse
      ; instance_type =
          Aws.Util.option_bind (Aws.Xml.member "InstanceType" xml) String.parse
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
      ; ebs_optimized =
          Aws.Util.option_bind (Aws.Xml.member "EbsOptimized" xml) Boolean.parse
      ; associate_public_ip_address =
          Aws.Util.option_bind
            (Aws.Xml.member "AssociatePublicIpAddress" xml)
            Boolean.parse
      ; placement_tenancy =
          Aws.Util.option_bind (Aws.Xml.member "PlacementTenancy" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.placement_tenancy (fun f ->
               Aws.Query.Pair ("PlacementTenancy", String.to_query f))
         ; Aws.Util.option_map v.associate_public_ip_address (fun f ->
               Aws.Query.Pair ("AssociatePublicIpAddress", Boolean.to_query f))
         ; Aws.Util.option_map v.ebs_optimized (fun f ->
               Aws.Query.Pair ("EbsOptimized", Boolean.to_query f))
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
         ; Aws.Util.option_map v.instance_type (fun f ->
               Aws.Query.Pair ("InstanceType", String.to_query f))
         ; Aws.Util.option_map v.instance_id (fun f ->
               Aws.Query.Pair ("InstanceId", String.to_query f))
         ; Aws.Util.option_map v.user_data (fun f ->
               Aws.Query.Pair ("UserData", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "ClassicLinkVPCSecurityGroups.member"
                , ClassicLinkVPCSecurityGroups.to_query
                    v.classic_link_v_p_c_security_groups ))
         ; Aws.Util.option_map v.classic_link_v_p_c_id (fun f ->
               Aws.Query.Pair ("ClassicLinkVPCId", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("SecurityGroups.member", SecurityGroups.to_query v.security_groups))
         ; Aws.Util.option_map v.key_name (fun f ->
               Aws.Query.Pair ("KeyName", String.to_query f))
         ; Aws.Util.option_map v.image_id (fun f ->
               Aws.Query.Pair ("ImageId", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("LaunchConfigurationName", String.to_query v.launch_configuration_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.placement_tenancy (fun f ->
               "PlacementTenancy", String.to_json f)
         ; Aws.Util.option_map v.associate_public_ip_address (fun f ->
               "AssociatePublicIpAddress", Boolean.to_json f)
         ; Aws.Util.option_map v.ebs_optimized (fun f ->
               "EbsOptimized", Boolean.to_json f)
         ; Aws.Util.option_map v.iam_instance_profile (fun f ->
               "IamInstanceProfile", String.to_json f)
         ; Aws.Util.option_map v.spot_price (fun f -> "SpotPrice", String.to_json f)
         ; Aws.Util.option_map v.instance_monitoring (fun f ->
               "InstanceMonitoring", InstanceMonitoring.to_json f)
         ; Some
             ("BlockDeviceMappings", BlockDeviceMappings.to_json v.block_device_mappings)
         ; Aws.Util.option_map v.ramdisk_id (fun f -> "RamdiskId", String.to_json f)
         ; Aws.Util.option_map v.kernel_id (fun f -> "KernelId", String.to_json f)
         ; Aws.Util.option_map v.instance_type (fun f -> "InstanceType", String.to_json f)
         ; Aws.Util.option_map v.instance_id (fun f -> "InstanceId", String.to_json f)
         ; Aws.Util.option_map v.user_data (fun f -> "UserData", String.to_json f)
         ; Some
             ( "ClassicLinkVPCSecurityGroups"
             , ClassicLinkVPCSecurityGroups.to_json v.classic_link_v_p_c_security_groups
             )
         ; Aws.Util.option_map v.classic_link_v_p_c_id (fun f ->
               "ClassicLinkVPCId", String.to_json f)
         ; Some ("SecurityGroups", SecurityGroups.to_json v.security_groups)
         ; Aws.Util.option_map v.key_name (fun f -> "KeyName", String.to_json f)
         ; Aws.Util.option_map v.image_id (fun f -> "ImageId", String.to_json f)
         ; Some ("LaunchConfigurationName", String.to_json v.launch_configuration_name)
         ])

  let of_json j =
    { launch_configuration_name =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LaunchConfigurationName"))
    ; image_id = Aws.Util.option_map (Aws.Json.lookup j "ImageId") String.of_json
    ; key_name = Aws.Util.option_map (Aws.Json.lookup j "KeyName") String.of_json
    ; security_groups =
        SecurityGroups.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroups"))
    ; classic_link_v_p_c_id =
        Aws.Util.option_map (Aws.Json.lookup j "ClassicLinkVPCId") String.of_json
    ; classic_link_v_p_c_security_groups =
        ClassicLinkVPCSecurityGroups.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ClassicLinkVPCSecurityGroups"))
    ; user_data = Aws.Util.option_map (Aws.Json.lookup j "UserData") String.of_json
    ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "InstanceId") String.of_json
    ; instance_type =
        Aws.Util.option_map (Aws.Json.lookup j "InstanceType") String.of_json
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
    ; ebs_optimized =
        Aws.Util.option_map (Aws.Json.lookup j "EbsOptimized") Boolean.of_json
    ; associate_public_ip_address =
        Aws.Util.option_map (Aws.Json.lookup j "AssociatePublicIpAddress") Boolean.of_json
    ; placement_tenancy =
        Aws.Util.option_map (Aws.Json.lookup j "PlacementTenancy") String.of_json
    }
end

module DescribeLifecycleHookTypesAnswer = struct
  type t = { lifecycle_hook_types : AutoScalingNotificationTypes.t }

  let make ?(lifecycle_hook_types = []) () = { lifecycle_hook_types }

  let parse xml =
    Some
      { lifecycle_hook_types =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "LifecycleHookTypes" xml)
               AutoScalingNotificationTypes.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "LifecycleHookTypes.member"
                , AutoScalingNotificationTypes.to_query v.lifecycle_hook_types ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "LifecycleHookTypes"
             , AutoScalingNotificationTypes.to_json v.lifecycle_hook_types )
         ])

  let of_json j =
    { lifecycle_hook_types =
        AutoScalingNotificationTypes.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LifecycleHookTypes"))
    }
end

module LimitExceededFault = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module DetachLoadBalancersResultType = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module LaunchConfigurationNamesType = struct
  type t =
    { launch_configuration_names : LaunchConfigurationNames.t
    ; next_token : String.t option
    ; max_records : Integer.t option
    }

  let make ?(launch_configuration_names = []) ?next_token ?max_records () =
    { launch_configuration_names; next_token; max_records }

  let parse xml =
    Some
      { launch_configuration_names =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "LaunchConfigurationNames" xml)
               LaunchConfigurationNames.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "LaunchConfigurationNames.member"
                , LaunchConfigurationNames.to_query v.launch_configuration_names ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some
             ( "LaunchConfigurationNames"
             , LaunchConfigurationNames.to_json v.launch_configuration_names )
         ])

  let of_json j =
    { launch_configuration_names =
        LaunchConfigurationNames.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LaunchConfigurationNames"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    }
end

module DetachInstancesAnswer = struct
  type t = { activities : Activities.t }

  let make ?(activities = []) () = { activities }

  let parse xml =
    Some
      { activities =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Activities" xml) Activities.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Activities.member", Activities.to_query v.activities)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("Activities", Activities.to_json v.activities) ])

  let of_json j =
    { activities =
        Activities.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Activities"))
    }
end

module ActivityIds = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module SetDesiredCapacityType = struct
  type t =
    { auto_scaling_group_name : String.t
    ; desired_capacity : Integer.t
    ; honor_cooldown : Boolean.t option
    }

  let make ~auto_scaling_group_name ~desired_capacity ?honor_cooldown () =
    { auto_scaling_group_name; desired_capacity; honor_cooldown }

  let parse xml =
    Some
      { auto_scaling_group_name =
          Aws.Xml.required
            "AutoScalingGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupName" xml)
               String.parse)
      ; desired_capacity =
          Aws.Xml.required
            "DesiredCapacity"
            (Aws.Util.option_bind (Aws.Xml.member "DesiredCapacity" xml) Integer.parse)
      ; honor_cooldown =
          Aws.Util.option_bind (Aws.Xml.member "HonorCooldown" xml) Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.honor_cooldown (fun f ->
               Aws.Query.Pair ("HonorCooldown", Boolean.to_query f))
         ; Some (Aws.Query.Pair ("DesiredCapacity", Integer.to_query v.desired_capacity))
         ; Some
             (Aws.Query.Pair
                ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.honor_cooldown (fun f ->
               "HonorCooldown", Boolean.to_json f)
         ; Some ("DesiredCapacity", Integer.to_json v.desired_capacity)
         ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
         ])

  let of_json j =
    { auto_scaling_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
    ; desired_capacity =
        Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DesiredCapacity"))
    ; honor_cooldown =
        Aws.Util.option_map (Aws.Json.lookup j "HonorCooldown") Boolean.of_json
    }
end

module LifecycleHook = struct
  type t =
    { lifecycle_hook_name : String.t option
    ; auto_scaling_group_name : String.t option
    ; lifecycle_transition : String.t option
    ; notification_target_a_r_n : String.t option
    ; role_a_r_n : String.t option
    ; notification_metadata : String.t option
    ; heartbeat_timeout : Integer.t option
    ; global_timeout : Integer.t option
    ; default_result : String.t option
    }

  let make
      ?lifecycle_hook_name
      ?auto_scaling_group_name
      ?lifecycle_transition
      ?notification_target_a_r_n
      ?role_a_r_n
      ?notification_metadata
      ?heartbeat_timeout
      ?global_timeout
      ?default_result
      () =
    { lifecycle_hook_name
    ; auto_scaling_group_name
    ; lifecycle_transition
    ; notification_target_a_r_n
    ; role_a_r_n
    ; notification_metadata
    ; heartbeat_timeout
    ; global_timeout
    ; default_result
    }

  let parse xml =
    Some
      { lifecycle_hook_name =
          Aws.Util.option_bind (Aws.Xml.member "LifecycleHookName" xml) String.parse
      ; auto_scaling_group_name =
          Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse
      ; lifecycle_transition =
          Aws.Util.option_bind (Aws.Xml.member "LifecycleTransition" xml) String.parse
      ; notification_target_a_r_n =
          Aws.Util.option_bind (Aws.Xml.member "NotificationTargetARN" xml) String.parse
      ; role_a_r_n = Aws.Util.option_bind (Aws.Xml.member "RoleARN" xml) String.parse
      ; notification_metadata =
          Aws.Util.option_bind (Aws.Xml.member "NotificationMetadata" xml) String.parse
      ; heartbeat_timeout =
          Aws.Util.option_bind (Aws.Xml.member "HeartbeatTimeout" xml) Integer.parse
      ; global_timeout =
          Aws.Util.option_bind (Aws.Xml.member "GlobalTimeout" xml) Integer.parse
      ; default_result =
          Aws.Util.option_bind (Aws.Xml.member "DefaultResult" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.default_result (fun f ->
               Aws.Query.Pair ("DefaultResult", String.to_query f))
         ; Aws.Util.option_map v.global_timeout (fun f ->
               Aws.Query.Pair ("GlobalTimeout", Integer.to_query f))
         ; Aws.Util.option_map v.heartbeat_timeout (fun f ->
               Aws.Query.Pair ("HeartbeatTimeout", Integer.to_query f))
         ; Aws.Util.option_map v.notification_metadata (fun f ->
               Aws.Query.Pair ("NotificationMetadata", String.to_query f))
         ; Aws.Util.option_map v.role_a_r_n (fun f ->
               Aws.Query.Pair ("RoleARN", String.to_query f))
         ; Aws.Util.option_map v.notification_target_a_r_n (fun f ->
               Aws.Query.Pair ("NotificationTargetARN", String.to_query f))
         ; Aws.Util.option_map v.lifecycle_transition (fun f ->
               Aws.Query.Pair ("LifecycleTransition", String.to_query f))
         ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
               Aws.Query.Pair ("AutoScalingGroupName", String.to_query f))
         ; Aws.Util.option_map v.lifecycle_hook_name (fun f ->
               Aws.Query.Pair ("LifecycleHookName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.default_result (fun f ->
               "DefaultResult", String.to_json f)
         ; Aws.Util.option_map v.global_timeout (fun f ->
               "GlobalTimeout", Integer.to_json f)
         ; Aws.Util.option_map v.heartbeat_timeout (fun f ->
               "HeartbeatTimeout", Integer.to_json f)
         ; Aws.Util.option_map v.notification_metadata (fun f ->
               "NotificationMetadata", String.to_json f)
         ; Aws.Util.option_map v.role_a_r_n (fun f -> "RoleARN", String.to_json f)
         ; Aws.Util.option_map v.notification_target_a_r_n (fun f ->
               "NotificationTargetARN", String.to_json f)
         ; Aws.Util.option_map v.lifecycle_transition (fun f ->
               "LifecycleTransition", String.to_json f)
         ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
               "AutoScalingGroupName", String.to_json f)
         ; Aws.Util.option_map v.lifecycle_hook_name (fun f ->
               "LifecycleHookName", String.to_json f)
         ])

  let of_json j =
    { lifecycle_hook_name =
        Aws.Util.option_map (Aws.Json.lookup j "LifecycleHookName") String.of_json
    ; auto_scaling_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "AutoScalingGroupName") String.of_json
    ; lifecycle_transition =
        Aws.Util.option_map (Aws.Json.lookup j "LifecycleTransition") String.of_json
    ; notification_target_a_r_n =
        Aws.Util.option_map (Aws.Json.lookup j "NotificationTargetARN") String.of_json
    ; role_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "RoleARN") String.of_json
    ; notification_metadata =
        Aws.Util.option_map (Aws.Json.lookup j "NotificationMetadata") String.of_json
    ; heartbeat_timeout =
        Aws.Util.option_map (Aws.Json.lookup j "HeartbeatTimeout") Integer.of_json
    ; global_timeout =
        Aws.Util.option_map (Aws.Json.lookup j "GlobalTimeout") Integer.of_json
    ; default_result =
        Aws.Util.option_map (Aws.Json.lookup j "DefaultResult") String.of_json
    }
end

module LifecycleHooks = struct
  type t = LifecycleHook.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map LifecycleHook.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list LifecycleHook.to_query v

  let to_json v = `List (List.map LifecycleHook.to_json v)

  let of_json j = Aws.Json.to_list LifecycleHook.of_json j
end

module DescribeLifecycleHooksAnswer = struct
  type t = { lifecycle_hooks : LifecycleHooks.t }

  let make ?(lifecycle_hooks = []) () = { lifecycle_hooks }

  let parse xml =
    Some
      { lifecycle_hooks =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "LifecycleHooks" xml)
               LifecycleHooks.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("LifecycleHooks.member", LifecycleHooks.to_query v.lifecycle_hooks))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("LifecycleHooks", LifecycleHooks.to_json v.lifecycle_hooks) ])

  let of_json j =
    { lifecycle_hooks =
        LifecycleHooks.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LifecycleHooks"))
    }
end

module DeleteScheduledActionType = struct
  type t =
    { auto_scaling_group_name : String.t option
    ; scheduled_action_name : String.t
    }

  let make ?auto_scaling_group_name ~scheduled_action_name () =
    { auto_scaling_group_name; scheduled_action_name }

  let parse xml =
    Some
      { auto_scaling_group_name =
          Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse
      ; scheduled_action_name =
          Aws.Xml.required
            "ScheduledActionName"
            (Aws.Util.option_bind (Aws.Xml.member "ScheduledActionName" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("ScheduledActionName", String.to_query v.scheduled_action_name))
         ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
               Aws.Query.Pair ("AutoScalingGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ScheduledActionName", String.to_json v.scheduled_action_name)
         ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
               "AutoScalingGroupName", String.to_json f)
         ])

  let of_json j =
    { auto_scaling_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "AutoScalingGroupName") String.of_json
    ; scheduled_action_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ScheduledActionName"))
    }
end

module DescribeScalingActivitiesType = struct
  type t =
    { activity_ids : ActivityIds.t
    ; auto_scaling_group_name : String.t option
    ; max_records : Integer.t option
    ; next_token : String.t option
    }

  let make ?(activity_ids = []) ?auto_scaling_group_name ?max_records ?next_token () =
    { activity_ids; auto_scaling_group_name; max_records; next_token }

  let parse xml =
    Some
      { activity_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "ActivityIds" xml) ActivityIds.parse)
      ; auto_scaling_group_name =
          Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
               Aws.Query.Pair ("AutoScalingGroupName", String.to_query f))
         ; Some
             (Aws.Query.Pair ("ActivityIds.member", ActivityIds.to_query v.activity_ids))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
               "AutoScalingGroupName", String.to_json f)
         ; Some ("ActivityIds", ActivityIds.to_json v.activity_ids)
         ])

  let of_json j =
    { activity_ids =
        ActivityIds.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ActivityIds"))
    ; auto_scaling_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "AutoScalingGroupName") String.of_json
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module AttachLoadBalancersResultType = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module TagsType = struct
  type t =
    { tags : TagDescriptionList.t
    ; next_token : String.t option
    }

  let make ?(tags = []) ?next_token () = { tags; next_token }

  let parse xml =
    Some
      { tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagDescriptionList.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some (Aws.Query.Pair ("Tags.member", TagDescriptionList.to_query v.tags))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("Tags", TagDescriptionList.to_json v.tags)
         ])

  let of_json j =
    { tags =
        TagDescriptionList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module AutoScalingInstances = struct
  type t = AutoScalingInstanceDetails.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map AutoScalingInstanceDetails.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list AutoScalingInstanceDetails.to_query v

  let to_json v = `List (List.map AutoScalingInstanceDetails.to_json v)

  let of_json j = Aws.Json.to_list AutoScalingInstanceDetails.of_json j
end

module CreateAutoScalingGroupType = struct
  type t =
    { auto_scaling_group_name : String.t
    ; launch_configuration_name : String.t option
    ; instance_id : String.t option
    ; min_size : Integer.t
    ; max_size : Integer.t
    ; desired_capacity : Integer.t option
    ; default_cooldown : Integer.t option
    ; availability_zones : AvailabilityZones.t
    ; load_balancer_names : LoadBalancerNames.t
    ; health_check_type : String.t option
    ; health_check_grace_period : Integer.t option
    ; placement_group : String.t option
    ; v_p_c_zone_identifier : String.t option
    ; termination_policies : TerminationPolicies.t
    ; tags : Tags.t
    }

  let make
      ~auto_scaling_group_name
      ?launch_configuration_name
      ?instance_id
      ~min_size
      ~max_size
      ?desired_capacity
      ?default_cooldown
      ?(availability_zones = [])
      ?(load_balancer_names = [])
      ?health_check_type
      ?health_check_grace_period
      ?placement_group
      ?v_p_c_zone_identifier
      ?(termination_policies = [])
      ?(tags = [])
      () =
    { auto_scaling_group_name
    ; launch_configuration_name
    ; instance_id
    ; min_size
    ; max_size
    ; desired_capacity
    ; default_cooldown
    ; availability_zones
    ; load_balancer_names
    ; health_check_type
    ; health_check_grace_period
    ; placement_group
    ; v_p_c_zone_identifier
    ; termination_policies
    ; tags
    }

  let parse xml =
    Some
      { auto_scaling_group_name =
          Aws.Xml.required
            "AutoScalingGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupName" xml)
               String.parse)
      ; launch_configuration_name =
          Aws.Util.option_bind (Aws.Xml.member "LaunchConfigurationName" xml) String.parse
      ; instance_id = Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse
      ; min_size =
          Aws.Xml.required
            "MinSize"
            (Aws.Util.option_bind (Aws.Xml.member "MinSize" xml) Integer.parse)
      ; max_size =
          Aws.Xml.required
            "MaxSize"
            (Aws.Util.option_bind (Aws.Xml.member "MaxSize" xml) Integer.parse)
      ; desired_capacity =
          Aws.Util.option_bind (Aws.Xml.member "DesiredCapacity" xml) Integer.parse
      ; default_cooldown =
          Aws.Util.option_bind (Aws.Xml.member "DefaultCooldown" xml) Integer.parse
      ; availability_zones =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "AvailabilityZones" xml)
               AvailabilityZones.parse)
      ; load_balancer_names =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "LoadBalancerNames" xml)
               LoadBalancerNames.parse)
      ; health_check_type =
          Aws.Util.option_bind (Aws.Xml.member "HealthCheckType" xml) String.parse
      ; health_check_grace_period =
          Aws.Util.option_bind (Aws.Xml.member "HealthCheckGracePeriod" xml) Integer.parse
      ; placement_group =
          Aws.Util.option_bind (Aws.Xml.member "PlacementGroup" xml) String.parse
      ; v_p_c_zone_identifier =
          Aws.Util.option_bind (Aws.Xml.member "VPCZoneIdentifier" xml) String.parse
      ; termination_policies =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "TerminationPolicies" xml)
               TerminationPolicies.parse)
      ; tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) Tags.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Tags.member", Tags.to_query v.tags))
         ; Some
             (Aws.Query.Pair
                ( "TerminationPolicies.member"
                , TerminationPolicies.to_query v.termination_policies ))
         ; Aws.Util.option_map v.v_p_c_zone_identifier (fun f ->
               Aws.Query.Pair ("VPCZoneIdentifier", String.to_query f))
         ; Aws.Util.option_map v.placement_group (fun f ->
               Aws.Query.Pair ("PlacementGroup", String.to_query f))
         ; Aws.Util.option_map v.health_check_grace_period (fun f ->
               Aws.Query.Pair ("HealthCheckGracePeriod", Integer.to_query f))
         ; Aws.Util.option_map v.health_check_type (fun f ->
               Aws.Query.Pair ("HealthCheckType", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "LoadBalancerNames.member"
                , LoadBalancerNames.to_query v.load_balancer_names ))
         ; Some
             (Aws.Query.Pair
                ( "AvailabilityZones.member"
                , AvailabilityZones.to_query v.availability_zones ))
         ; Aws.Util.option_map v.default_cooldown (fun f ->
               Aws.Query.Pair ("DefaultCooldown", Integer.to_query f))
         ; Aws.Util.option_map v.desired_capacity (fun f ->
               Aws.Query.Pair ("DesiredCapacity", Integer.to_query f))
         ; Some (Aws.Query.Pair ("MaxSize", Integer.to_query v.max_size))
         ; Some (Aws.Query.Pair ("MinSize", Integer.to_query v.min_size))
         ; Aws.Util.option_map v.instance_id (fun f ->
               Aws.Query.Pair ("InstanceId", String.to_query f))
         ; Aws.Util.option_map v.launch_configuration_name (fun f ->
               Aws.Query.Pair ("LaunchConfigurationName", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Tags", Tags.to_json v.tags)
         ; Some ("TerminationPolicies", TerminationPolicies.to_json v.termination_policies)
         ; Aws.Util.option_map v.v_p_c_zone_identifier (fun f ->
               "VPCZoneIdentifier", String.to_json f)
         ; Aws.Util.option_map v.placement_group (fun f ->
               "PlacementGroup", String.to_json f)
         ; Aws.Util.option_map v.health_check_grace_period (fun f ->
               "HealthCheckGracePeriod", Integer.to_json f)
         ; Aws.Util.option_map v.health_check_type (fun f ->
               "HealthCheckType", String.to_json f)
         ; Some ("LoadBalancerNames", LoadBalancerNames.to_json v.load_balancer_names)
         ; Some ("AvailabilityZones", AvailabilityZones.to_json v.availability_zones)
         ; Aws.Util.option_map v.default_cooldown (fun f ->
               "DefaultCooldown", Integer.to_json f)
         ; Aws.Util.option_map v.desired_capacity (fun f ->
               "DesiredCapacity", Integer.to_json f)
         ; Some ("MaxSize", Integer.to_json v.max_size)
         ; Some ("MinSize", Integer.to_json v.min_size)
         ; Aws.Util.option_map v.instance_id (fun f -> "InstanceId", String.to_json f)
         ; Aws.Util.option_map v.launch_configuration_name (fun f ->
               "LaunchConfigurationName", String.to_json f)
         ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
         ])

  let of_json j =
    { auto_scaling_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
    ; launch_configuration_name =
        Aws.Util.option_map (Aws.Json.lookup j "LaunchConfigurationName") String.of_json
    ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "InstanceId") String.of_json
    ; min_size = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MinSize"))
    ; max_size = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxSize"))
    ; desired_capacity =
        Aws.Util.option_map (Aws.Json.lookup j "DesiredCapacity") Integer.of_json
    ; default_cooldown =
        Aws.Util.option_map (Aws.Json.lookup j "DefaultCooldown") Integer.of_json
    ; availability_zones =
        AvailabilityZones.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZones"))
    ; load_balancer_names =
        LoadBalancerNames.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerNames"))
    ; health_check_type =
        Aws.Util.option_map (Aws.Json.lookup j "HealthCheckType") String.of_json
    ; health_check_grace_period =
        Aws.Util.option_map (Aws.Json.lookup j "HealthCheckGracePeriod") Integer.of_json
    ; placement_group =
        Aws.Util.option_map (Aws.Json.lookup j "PlacementGroup") String.of_json
    ; v_p_c_zone_identifier =
        Aws.Util.option_map (Aws.Json.lookup j "VPCZoneIdentifier") String.of_json
    ; termination_policies =
        TerminationPolicies.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "TerminationPolicies"))
    ; tags = Tags.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    }
end

module UpdateAutoScalingGroupType = struct
  type t =
    { auto_scaling_group_name : String.t
    ; launch_configuration_name : String.t option
    ; min_size : Integer.t option
    ; max_size : Integer.t option
    ; desired_capacity : Integer.t option
    ; default_cooldown : Integer.t option
    ; availability_zones : AvailabilityZones.t
    ; health_check_type : String.t option
    ; health_check_grace_period : Integer.t option
    ; placement_group : String.t option
    ; v_p_c_zone_identifier : String.t option
    ; termination_policies : TerminationPolicies.t
    }

  let make
      ~auto_scaling_group_name
      ?launch_configuration_name
      ?min_size
      ?max_size
      ?desired_capacity
      ?default_cooldown
      ?(availability_zones = [])
      ?health_check_type
      ?health_check_grace_period
      ?placement_group
      ?v_p_c_zone_identifier
      ?(termination_policies = [])
      () =
    { auto_scaling_group_name
    ; launch_configuration_name
    ; min_size
    ; max_size
    ; desired_capacity
    ; default_cooldown
    ; availability_zones
    ; health_check_type
    ; health_check_grace_period
    ; placement_group
    ; v_p_c_zone_identifier
    ; termination_policies
    }

  let parse xml =
    Some
      { auto_scaling_group_name =
          Aws.Xml.required
            "AutoScalingGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupName" xml)
               String.parse)
      ; launch_configuration_name =
          Aws.Util.option_bind (Aws.Xml.member "LaunchConfigurationName" xml) String.parse
      ; min_size = Aws.Util.option_bind (Aws.Xml.member "MinSize" xml) Integer.parse
      ; max_size = Aws.Util.option_bind (Aws.Xml.member "MaxSize" xml) Integer.parse
      ; desired_capacity =
          Aws.Util.option_bind (Aws.Xml.member "DesiredCapacity" xml) Integer.parse
      ; default_cooldown =
          Aws.Util.option_bind (Aws.Xml.member "DefaultCooldown" xml) Integer.parse
      ; availability_zones =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "AvailabilityZones" xml)
               AvailabilityZones.parse)
      ; health_check_type =
          Aws.Util.option_bind (Aws.Xml.member "HealthCheckType" xml) String.parse
      ; health_check_grace_period =
          Aws.Util.option_bind (Aws.Xml.member "HealthCheckGracePeriod" xml) Integer.parse
      ; placement_group =
          Aws.Util.option_bind (Aws.Xml.member "PlacementGroup" xml) String.parse
      ; v_p_c_zone_identifier =
          Aws.Util.option_bind (Aws.Xml.member "VPCZoneIdentifier" xml) String.parse
      ; termination_policies =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "TerminationPolicies" xml)
               TerminationPolicies.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "TerminationPolicies.member"
                , TerminationPolicies.to_query v.termination_policies ))
         ; Aws.Util.option_map v.v_p_c_zone_identifier (fun f ->
               Aws.Query.Pair ("VPCZoneIdentifier", String.to_query f))
         ; Aws.Util.option_map v.placement_group (fun f ->
               Aws.Query.Pair ("PlacementGroup", String.to_query f))
         ; Aws.Util.option_map v.health_check_grace_period (fun f ->
               Aws.Query.Pair ("HealthCheckGracePeriod", Integer.to_query f))
         ; Aws.Util.option_map v.health_check_type (fun f ->
               Aws.Query.Pair ("HealthCheckType", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "AvailabilityZones.member"
                , AvailabilityZones.to_query v.availability_zones ))
         ; Aws.Util.option_map v.default_cooldown (fun f ->
               Aws.Query.Pair ("DefaultCooldown", Integer.to_query f))
         ; Aws.Util.option_map v.desired_capacity (fun f ->
               Aws.Query.Pair ("DesiredCapacity", Integer.to_query f))
         ; Aws.Util.option_map v.max_size (fun f ->
               Aws.Query.Pair ("MaxSize", Integer.to_query f))
         ; Aws.Util.option_map v.min_size (fun f ->
               Aws.Query.Pair ("MinSize", Integer.to_query f))
         ; Aws.Util.option_map v.launch_configuration_name (fun f ->
               Aws.Query.Pair ("LaunchConfigurationName", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("TerminationPolicies", TerminationPolicies.to_json v.termination_policies)
         ; Aws.Util.option_map v.v_p_c_zone_identifier (fun f ->
               "VPCZoneIdentifier", String.to_json f)
         ; Aws.Util.option_map v.placement_group (fun f ->
               "PlacementGroup", String.to_json f)
         ; Aws.Util.option_map v.health_check_grace_period (fun f ->
               "HealthCheckGracePeriod", Integer.to_json f)
         ; Aws.Util.option_map v.health_check_type (fun f ->
               "HealthCheckType", String.to_json f)
         ; Some ("AvailabilityZones", AvailabilityZones.to_json v.availability_zones)
         ; Aws.Util.option_map v.default_cooldown (fun f ->
               "DefaultCooldown", Integer.to_json f)
         ; Aws.Util.option_map v.desired_capacity (fun f ->
               "DesiredCapacity", Integer.to_json f)
         ; Aws.Util.option_map v.max_size (fun f -> "MaxSize", Integer.to_json f)
         ; Aws.Util.option_map v.min_size (fun f -> "MinSize", Integer.to_json f)
         ; Aws.Util.option_map v.launch_configuration_name (fun f ->
               "LaunchConfigurationName", String.to_json f)
         ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
         ])

  let of_json j =
    { auto_scaling_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
    ; launch_configuration_name =
        Aws.Util.option_map (Aws.Json.lookup j "LaunchConfigurationName") String.of_json
    ; min_size = Aws.Util.option_map (Aws.Json.lookup j "MinSize") Integer.of_json
    ; max_size = Aws.Util.option_map (Aws.Json.lookup j "MaxSize") Integer.of_json
    ; desired_capacity =
        Aws.Util.option_map (Aws.Json.lookup j "DesiredCapacity") Integer.of_json
    ; default_cooldown =
        Aws.Util.option_map (Aws.Json.lookup j "DefaultCooldown") Integer.of_json
    ; availability_zones =
        AvailabilityZones.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZones"))
    ; health_check_type =
        Aws.Util.option_map (Aws.Json.lookup j "HealthCheckType") String.of_json
    ; health_check_grace_period =
        Aws.Util.option_map (Aws.Json.lookup j "HealthCheckGracePeriod") Integer.of_json
    ; placement_group =
        Aws.Util.option_map (Aws.Json.lookup j "PlacementGroup") String.of_json
    ; v_p_c_zone_identifier =
        Aws.Util.option_map (Aws.Json.lookup j "VPCZoneIdentifier") String.of_json
    ; termination_policies =
        TerminationPolicies.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "TerminationPolicies"))
    }
end

module PoliciesType = struct
  type t =
    { scaling_policies : ScalingPolicies.t
    ; next_token : String.t option
    }

  let make ?(scaling_policies = []) ?next_token () = { scaling_policies; next_token }

  let parse xml =
    Some
      { scaling_policies =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ScalingPolicies" xml)
               ScalingPolicies.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("ScalingPolicies.member", ScalingPolicies.to_query v.scaling_policies))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("ScalingPolicies", ScalingPolicies.to_json v.scaling_policies)
         ])

  let of_json j =
    { scaling_policies =
        ScalingPolicies.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ScalingPolicies"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module PutNotificationConfigurationType = struct
  type t =
    { auto_scaling_group_name : String.t
    ; topic_a_r_n : String.t
    ; notification_types : AutoScalingNotificationTypes.t
    }

  let make ~auto_scaling_group_name ~topic_a_r_n ~notification_types () =
    { auto_scaling_group_name; topic_a_r_n; notification_types }

  let parse xml =
    Some
      { auto_scaling_group_name =
          Aws.Xml.required
            "AutoScalingGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupName" xml)
               String.parse)
      ; topic_a_r_n =
          Aws.Xml.required
            "TopicARN"
            (Aws.Util.option_bind (Aws.Xml.member "TopicARN" xml) String.parse)
      ; notification_types =
          Aws.Xml.required
            "NotificationTypes"
            (Aws.Util.option_bind
               (Aws.Xml.member "NotificationTypes" xml)
               AutoScalingNotificationTypes.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "NotificationTypes.member"
                , AutoScalingNotificationTypes.to_query v.notification_types ))
         ; Some (Aws.Query.Pair ("TopicARN", String.to_query v.topic_a_r_n))
         ; Some
             (Aws.Query.Pair
                ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "NotificationTypes"
             , AutoScalingNotificationTypes.to_json v.notification_types )
         ; Some ("TopicARN", String.to_json v.topic_a_r_n)
         ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
         ])

  let of_json j =
    { auto_scaling_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
    ; topic_a_r_n = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TopicARN"))
    ; notification_types =
        AutoScalingNotificationTypes.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "NotificationTypes"))
    }
end

module AutoScalingInstancesType = struct
  type t =
    { auto_scaling_instances : AutoScalingInstances.t
    ; next_token : String.t option
    }

  let make ?(auto_scaling_instances = []) ?next_token () =
    { auto_scaling_instances; next_token }

  let parse xml =
    Some
      { auto_scaling_instances =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingInstances" xml)
               AutoScalingInstances.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "AutoScalingInstances.member"
                , AutoScalingInstances.to_query v.auto_scaling_instances ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some
             ( "AutoScalingInstances"
             , AutoScalingInstances.to_json v.auto_scaling_instances )
         ])

  let of_json j =
    { auto_scaling_instances =
        AutoScalingInstances.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingInstances"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module RecordLifecycleActionHeartbeatType = struct
  type t =
    { lifecycle_hook_name : String.t
    ; auto_scaling_group_name : String.t
    ; lifecycle_action_token : String.t
    }

  let make ~lifecycle_hook_name ~auto_scaling_group_name ~lifecycle_action_token () =
    { lifecycle_hook_name; auto_scaling_group_name; lifecycle_action_token }

  let parse xml =
    Some
      { lifecycle_hook_name =
          Aws.Xml.required
            "LifecycleHookName"
            (Aws.Util.option_bind (Aws.Xml.member "LifecycleHookName" xml) String.parse)
      ; auto_scaling_group_name =
          Aws.Xml.required
            "AutoScalingGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupName" xml)
               String.parse)
      ; lifecycle_action_token =
          Aws.Xml.required
            "LifecycleActionToken"
            (Aws.Util.option_bind
               (Aws.Xml.member "LifecycleActionToken" xml)
               String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("LifecycleActionToken", String.to_query v.lifecycle_action_token))
         ; Some
             (Aws.Query.Pair
                ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
         ; Some
             (Aws.Query.Pair ("LifecycleHookName", String.to_query v.lifecycle_hook_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("LifecycleActionToken", String.to_json v.lifecycle_action_token)
         ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
         ; Some ("LifecycleHookName", String.to_json v.lifecycle_hook_name)
         ])

  let of_json j =
    { lifecycle_hook_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LifecycleHookName"))
    ; auto_scaling_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
    ; lifecycle_action_token =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LifecycleActionToken"))
    }
end

module AttachLoadBalancersType = struct
  type t =
    { auto_scaling_group_name : String.t option
    ; load_balancer_names : LoadBalancerNames.t
    }

  let make ?auto_scaling_group_name ?(load_balancer_names = []) () =
    { auto_scaling_group_name; load_balancer_names }

  let parse xml =
    Some
      { auto_scaling_group_name =
          Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse
      ; load_balancer_names =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "LoadBalancerNames" xml)
               LoadBalancerNames.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "LoadBalancerNames.member"
                , LoadBalancerNames.to_query v.load_balancer_names ))
         ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
               Aws.Query.Pair ("AutoScalingGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("LoadBalancerNames", LoadBalancerNames.to_json v.load_balancer_names)
         ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
               "AutoScalingGroupName", String.to_json f)
         ])

  let of_json j =
    { auto_scaling_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "AutoScalingGroupName") String.of_json
    ; load_balancer_names =
        LoadBalancerNames.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerNames"))
    }
end

module DescribeLoadBalancersRequest = struct
  type t =
    { auto_scaling_group_name : String.t
    ; next_token : String.t option
    ; max_records : Integer.t option
    }

  let make ~auto_scaling_group_name ?next_token ?max_records () =
    { auto_scaling_group_name; next_token; max_records }

  let parse xml =
    Some
      { auto_scaling_group_name =
          Aws.Xml.required
            "AutoScalingGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupName" xml)
               String.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
         ])

  let of_json j =
    { auto_scaling_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    }
end

module ResourceInUseFault = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module DetachLoadBalancersType = struct
  type t =
    { auto_scaling_group_name : String.t option
    ; load_balancer_names : LoadBalancerNames.t
    }

  let make ?auto_scaling_group_name ?(load_balancer_names = []) () =
    { auto_scaling_group_name; load_balancer_names }

  let parse xml =
    Some
      { auto_scaling_group_name =
          Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse
      ; load_balancer_names =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "LoadBalancerNames" xml)
               LoadBalancerNames.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "LoadBalancerNames.member"
                , LoadBalancerNames.to_query v.load_balancer_names ))
         ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
               Aws.Query.Pair ("AutoScalingGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("LoadBalancerNames", LoadBalancerNames.to_json v.load_balancer_names)
         ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
               "AutoScalingGroupName", String.to_json f)
         ])

  let of_json j =
    { auto_scaling_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "AutoScalingGroupName") String.of_json
    ; load_balancer_names =
        LoadBalancerNames.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerNames"))
    }
end

module ScalingProcessQuery = struct
  type t =
    { auto_scaling_group_name : String.t
    ; scaling_processes : ProcessNames.t
    }

  let make ~auto_scaling_group_name ?(scaling_processes = []) () =
    { auto_scaling_group_name; scaling_processes }

  let parse xml =
    Some
      { auto_scaling_group_name =
          Aws.Xml.required
            "AutoScalingGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupName" xml)
               String.parse)
      ; scaling_processes =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ScalingProcesses" xml)
               ProcessNames.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("ScalingProcesses.member", ProcessNames.to_query v.scaling_processes))
         ; Some
             (Aws.Query.Pair
                ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ScalingProcesses", ProcessNames.to_json v.scaling_processes)
         ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
         ])

  let of_json j =
    { auto_scaling_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
    ; scaling_processes =
        ProcessNames.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ScalingProcesses"))
    }
end

module DetachInstancesQuery = struct
  type t =
    { instance_ids : InstanceIds.t
    ; auto_scaling_group_name : String.t
    ; should_decrement_desired_capacity : Boolean.t
    }

  let make
      ?(instance_ids = [])
      ~auto_scaling_group_name
      ~should_decrement_desired_capacity
      () =
    { instance_ids; auto_scaling_group_name; should_decrement_desired_capacity }

  let parse xml =
    Some
      { instance_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "InstanceIds" xml) InstanceIds.parse)
      ; auto_scaling_group_name =
          Aws.Xml.required
            "AutoScalingGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupName" xml)
               String.parse)
      ; should_decrement_desired_capacity =
          Aws.Xml.required
            "ShouldDecrementDesiredCapacity"
            (Aws.Util.option_bind
               (Aws.Xml.member "ShouldDecrementDesiredCapacity" xml)
               Boolean.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "ShouldDecrementDesiredCapacity"
                , Boolean.to_query v.should_decrement_desired_capacity ))
         ; Some
             (Aws.Query.Pair
                ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
         ; Some
             (Aws.Query.Pair ("InstanceIds.member", InstanceIds.to_query v.instance_ids))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "ShouldDecrementDesiredCapacity"
             , Boolean.to_json v.should_decrement_desired_capacity )
         ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
         ; Some ("InstanceIds", InstanceIds.to_json v.instance_ids)
         ])

  let of_json j =
    { instance_ids =
        InstanceIds.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceIds"))
    ; auto_scaling_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
    ; should_decrement_desired_capacity =
        Boolean.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ShouldDecrementDesiredCapacity"))
    }
end

module DeleteLifecycleHookType = struct
  type t =
    { lifecycle_hook_name : String.t
    ; auto_scaling_group_name : String.t
    }

  let make ~lifecycle_hook_name ~auto_scaling_group_name () =
    { lifecycle_hook_name; auto_scaling_group_name }

  let parse xml =
    Some
      { lifecycle_hook_name =
          Aws.Xml.required
            "LifecycleHookName"
            (Aws.Util.option_bind (Aws.Xml.member "LifecycleHookName" xml) String.parse)
      ; auto_scaling_group_name =
          Aws.Xml.required
            "AutoScalingGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupName" xml)
               String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
         ; Some
             (Aws.Query.Pair ("LifecycleHookName", String.to_query v.lifecycle_hook_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
         ; Some ("LifecycleHookName", String.to_json v.lifecycle_hook_name)
         ])

  let of_json j =
    { lifecycle_hook_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LifecycleHookName"))
    ; auto_scaling_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
    }
end

module PutLifecycleHookAnswer = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module PutLifecycleHookType = struct
  type t =
    { lifecycle_hook_name : String.t
    ; auto_scaling_group_name : String.t
    ; lifecycle_transition : String.t option
    ; role_a_r_n : String.t option
    ; notification_target_a_r_n : String.t option
    ; notification_metadata : String.t option
    ; heartbeat_timeout : Integer.t option
    ; default_result : String.t option
    }

  let make
      ~lifecycle_hook_name
      ~auto_scaling_group_name
      ?lifecycle_transition
      ?role_a_r_n
      ?notification_target_a_r_n
      ?notification_metadata
      ?heartbeat_timeout
      ?default_result
      () =
    { lifecycle_hook_name
    ; auto_scaling_group_name
    ; lifecycle_transition
    ; role_a_r_n
    ; notification_target_a_r_n
    ; notification_metadata
    ; heartbeat_timeout
    ; default_result
    }

  let parse xml =
    Some
      { lifecycle_hook_name =
          Aws.Xml.required
            "LifecycleHookName"
            (Aws.Util.option_bind (Aws.Xml.member "LifecycleHookName" xml) String.parse)
      ; auto_scaling_group_name =
          Aws.Xml.required
            "AutoScalingGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupName" xml)
               String.parse)
      ; lifecycle_transition =
          Aws.Util.option_bind (Aws.Xml.member "LifecycleTransition" xml) String.parse
      ; role_a_r_n = Aws.Util.option_bind (Aws.Xml.member "RoleARN" xml) String.parse
      ; notification_target_a_r_n =
          Aws.Util.option_bind (Aws.Xml.member "NotificationTargetARN" xml) String.parse
      ; notification_metadata =
          Aws.Util.option_bind (Aws.Xml.member "NotificationMetadata" xml) String.parse
      ; heartbeat_timeout =
          Aws.Util.option_bind (Aws.Xml.member "HeartbeatTimeout" xml) Integer.parse
      ; default_result =
          Aws.Util.option_bind (Aws.Xml.member "DefaultResult" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.default_result (fun f ->
               Aws.Query.Pair ("DefaultResult", String.to_query f))
         ; Aws.Util.option_map v.heartbeat_timeout (fun f ->
               Aws.Query.Pair ("HeartbeatTimeout", Integer.to_query f))
         ; Aws.Util.option_map v.notification_metadata (fun f ->
               Aws.Query.Pair ("NotificationMetadata", String.to_query f))
         ; Aws.Util.option_map v.notification_target_a_r_n (fun f ->
               Aws.Query.Pair ("NotificationTargetARN", String.to_query f))
         ; Aws.Util.option_map v.role_a_r_n (fun f ->
               Aws.Query.Pair ("RoleARN", String.to_query f))
         ; Aws.Util.option_map v.lifecycle_transition (fun f ->
               Aws.Query.Pair ("LifecycleTransition", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
         ; Some
             (Aws.Query.Pair ("LifecycleHookName", String.to_query v.lifecycle_hook_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.default_result (fun f ->
               "DefaultResult", String.to_json f)
         ; Aws.Util.option_map v.heartbeat_timeout (fun f ->
               "HeartbeatTimeout", Integer.to_json f)
         ; Aws.Util.option_map v.notification_metadata (fun f ->
               "NotificationMetadata", String.to_json f)
         ; Aws.Util.option_map v.notification_target_a_r_n (fun f ->
               "NotificationTargetARN", String.to_json f)
         ; Aws.Util.option_map v.role_a_r_n (fun f -> "RoleARN", String.to_json f)
         ; Aws.Util.option_map v.lifecycle_transition (fun f ->
               "LifecycleTransition", String.to_json f)
         ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
         ; Some ("LifecycleHookName", String.to_json v.lifecycle_hook_name)
         ])

  let of_json j =
    { lifecycle_hook_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LifecycleHookName"))
    ; auto_scaling_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
    ; lifecycle_transition =
        Aws.Util.option_map (Aws.Json.lookup j "LifecycleTransition") String.of_json
    ; role_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "RoleARN") String.of_json
    ; notification_target_a_r_n =
        Aws.Util.option_map (Aws.Json.lookup j "NotificationTargetARN") String.of_json
    ; notification_metadata =
        Aws.Util.option_map (Aws.Json.lookup j "NotificationMetadata") String.of_json
    ; heartbeat_timeout =
        Aws.Util.option_map (Aws.Json.lookup j "HeartbeatTimeout") Integer.of_json
    ; default_result =
        Aws.Util.option_map (Aws.Json.lookup j "DefaultResult") String.of_json
    }
end

module DescribeTagsType = struct
  type t =
    { filters : Filters.t
    ; next_token : String.t option
    ; max_records : Integer.t option
    }

  let make ?(filters = []) ?next_token ?max_records () =
    { filters; next_token; max_records }

  let parse xml =
    Some
      { filters =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Filters" xml) Filters.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some (Aws.Query.Pair ("Filters.member", Filters.to_query v.filters))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("Filters", Filters.to_json v.filters)
         ])

  let of_json j =
    { filters = Filters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filters"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    }
end

module DeleteAutoScalingGroupType = struct
  type t =
    { auto_scaling_group_name : String.t
    ; force_delete : Boolean.t option
    }

  let make ~auto_scaling_group_name ?force_delete () =
    { auto_scaling_group_name; force_delete }

  let parse xml =
    Some
      { auto_scaling_group_name =
          Aws.Xml.required
            "AutoScalingGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupName" xml)
               String.parse)
      ; force_delete =
          Aws.Util.option_bind (Aws.Xml.member "ForceDelete" xml) Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.force_delete (fun f ->
               Aws.Query.Pair ("ForceDelete", Boolean.to_query f))
         ; Some
             (Aws.Query.Pair
                ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.force_delete (fun f -> "ForceDelete", Boolean.to_json f)
         ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
         ])

  let of_json j =
    { auto_scaling_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
    ; force_delete = Aws.Util.option_map (Aws.Json.lookup j "ForceDelete") Boolean.of_json
    }
end

module ActivityType = struct
  type t = { activity : Activity.t option }

  let make ?activity () = { activity }

  let parse xml =
    Some
      { activity = Aws.Util.option_bind (Aws.Xml.member "Activity" xml) Activity.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.activity (fun f ->
               Aws.Query.Pair ("Activity", Activity.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.activity (fun f -> "Activity", Activity.to_json f) ])

  let of_json j =
    { activity = Aws.Util.option_map (Aws.Json.lookup j "Activity") Activity.of_json }
end

module RecordLifecycleActionHeartbeatAnswer = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ExitStandbyQuery = struct
  type t =
    { instance_ids : InstanceIds.t
    ; auto_scaling_group_name : String.t
    }

  let make ?(instance_ids = []) ~auto_scaling_group_name () =
    { instance_ids; auto_scaling_group_name }

  let parse xml =
    Some
      { instance_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "InstanceIds" xml) InstanceIds.parse)
      ; auto_scaling_group_name =
          Aws.Xml.required
            "AutoScalingGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "AutoScalingGroupName" xml)
               String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
         ; Some
             (Aws.Query.Pair ("InstanceIds.member", InstanceIds.to_query v.instance_ids))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
         ; Some ("InstanceIds", InstanceIds.to_json v.instance_ids)
         ])

  let of_json j =
    { instance_ids =
        InstanceIds.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceIds"))
    ; auto_scaling_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
    }
end
