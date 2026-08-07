open Aws.BaseTypes

type t =
  { addressing_type : String.t option
  ; block_device_mappings : BlockDeviceMappingList.t
  ; ebs_optimized : Boolean.t option
  ; iam_instance_profile : IamInstanceProfileSpecification.t option
  ; image_id : String.t option
  ; instance_type : InstanceType.t option
  ; kernel_id : String.t option
  ; key_name : String.t option
  ; monitoring : SpotFleetMonitoring.t option
  ; network_interfaces : InstanceNetworkInterfaceSpecificationList.t
  ; placement : SpotPlacement.t option
  ; ramdisk_id : String.t option
  ; spot_price : String.t option
  ; subnet_id : String.t option
  ; user_data : String.t option
  ; weighted_capacity : Double.t option
  ; tag_specifications : SpotFleetTagSpecificationList.t
  ; instance_requirements : InstanceRequirements.t option
  ; security_groups : GroupIdentifierList.t
  }

let make
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
    ?spot_price
    ?subnet_id
    ?user_data
    ?weighted_capacity
    ?(tag_specifications = [])
    ?instance_requirements
    ?(security_groups = [])
    () =
  { addressing_type
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
  ; spot_price
  ; subnet_id
  ; user_data
  ; weighted_capacity
  ; tag_specifications
  ; instance_requirements
  ; security_groups
  }

let parse xml =
  Some
    { addressing_type =
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
        Aws.Util.option_bind (Aws.Xml.member "monitoring" xml) SpotFleetMonitoring.parse
    ; network_interfaces =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "networkInterfaceSet" xml)
             InstanceNetworkInterfaceSpecificationList.parse)
    ; placement =
        Aws.Util.option_bind (Aws.Xml.member "placement" xml) SpotPlacement.parse
    ; ramdisk_id = Aws.Util.option_bind (Aws.Xml.member "ramdiskId" xml) String.parse
    ; spot_price = Aws.Util.option_bind (Aws.Xml.member "spotPrice" xml) String.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse
    ; user_data = Aws.Util.option_bind (Aws.Xml.member "userData" xml) String.parse
    ; weighted_capacity =
        Aws.Util.option_bind (Aws.Xml.member "weightedCapacity" xml) Double.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "tagSpecificationSet" xml)
             SpotFleetTagSpecificationList.parse)
    ; instance_requirements =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceRequirements" xml)
          InstanceRequirements.parse
    ; security_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "groupSet" xml) GroupIdentifierList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("GroupSet", GroupIdentifierList.to_query v.security_groups))
       ; Aws.Util.option_map v.instance_requirements (fun f ->
             Aws.Query.Pair ("InstanceRequirements", InstanceRequirements.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "TagSpecificationSet"
              , SpotFleetTagSpecificationList.to_query v.tag_specifications ))
       ; Aws.Util.option_map v.weighted_capacity (fun f ->
             Aws.Query.Pair ("WeightedCapacity", Double.to_query f))
       ; Aws.Util.option_map v.user_data (fun f ->
             Aws.Query.Pair ("UserData", String.to_query f))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.spot_price (fun f ->
             Aws.Query.Pair ("SpotPrice", String.to_query f))
       ; Aws.Util.option_map v.ramdisk_id (fun f ->
             Aws.Query.Pair ("RamdiskId", String.to_query f))
       ; Aws.Util.option_map v.placement (fun f ->
             Aws.Query.Pair ("Placement", SpotPlacement.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "NetworkInterfaceSet"
              , InstanceNetworkInterfaceSpecificationList.to_query v.network_interfaces ))
       ; Aws.Util.option_map v.monitoring (fun f ->
             Aws.Query.Pair ("Monitoring", SpotFleetMonitoring.to_query f))
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
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("groupSet", GroupIdentifierList.to_json v.security_groups)
       ; Aws.Util.option_map v.instance_requirements (fun f ->
             "instanceRequirements", InstanceRequirements.to_json f)
       ; Some
           ( "tagSpecificationSet"
           , SpotFleetTagSpecificationList.to_json v.tag_specifications )
       ; Aws.Util.option_map v.weighted_capacity (fun f ->
             "weightedCapacity", Double.to_json f)
       ; Aws.Util.option_map v.user_data (fun f -> "userData", String.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "subnetId", String.to_json f)
       ; Aws.Util.option_map v.spot_price (fun f -> "spotPrice", String.to_json f)
       ; Aws.Util.option_map v.ramdisk_id (fun f -> "ramdiskId", String.to_json f)
       ; Aws.Util.option_map v.placement (fun f -> "placement", SpotPlacement.to_json f)
       ; Some
           ( "networkInterfaceSet"
           , InstanceNetworkInterfaceSpecificationList.to_json v.network_interfaces )
       ; Aws.Util.option_map v.monitoring (fun f ->
             "monitoring", SpotFleetMonitoring.to_json f)
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
       ])

let of_json j =
  { addressing_type =
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
      Aws.Util.option_map (Aws.Json.lookup j "monitoring") SpotFleetMonitoring.of_json
  ; network_interfaces =
      InstanceNetworkInterfaceSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "networkInterfaceSet"))
  ; placement = Aws.Util.option_map (Aws.Json.lookup j "placement") SpotPlacement.of_json
  ; ramdisk_id = Aws.Util.option_map (Aws.Json.lookup j "ramdiskId") String.of_json
  ; spot_price = Aws.Util.option_map (Aws.Json.lookup j "spotPrice") String.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "subnetId") String.of_json
  ; user_data = Aws.Util.option_map (Aws.Json.lookup j "userData") String.of_json
  ; weighted_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "weightedCapacity") Double.of_json
  ; tag_specifications =
      SpotFleetTagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSpecificationSet"))
  ; instance_requirements =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceRequirements")
        InstanceRequirements.of_json
  ; security_groups =
      GroupIdentifierList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "groupSet"))
  }
