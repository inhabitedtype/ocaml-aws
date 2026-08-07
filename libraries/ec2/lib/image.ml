open Aws.BaseTypes

type t =
  { platform_details : String.t option
  ; usage_operation : String.t option
  ; block_device_mappings : BlockDeviceMappingList.t
  ; description : String.t option
  ; ena_support : Boolean.t option
  ; hypervisor : HypervisorType.t
  ; image_owner_alias : String.t option
  ; name : String.t option
  ; root_device_name : String.t option
  ; root_device_type : DeviceType.t
  ; sriov_net_support : String.t option
  ; state_reason : StateReason.t option
  ; tags : TagList.t
  ; virtualization_type : VirtualizationType.t
  ; boot_mode : BootModeValues.t option
  ; tpm_support : TpmSupportValues.t option
  ; deprecation_time : String.t option
  ; imds_support : ImdsSupportValues.t option
  ; source_instance_id : String.t option
  ; deregistration_protection : String.t option
  ; last_launched_time : String.t option
  ; image_allowed : Boolean.t option
  ; source_image_id : String.t option
  ; source_image_region : String.t option
  ; free_tier_eligible : Boolean.t option
  ; image_watermarks : ImageWatermarkList.t
  ; image_id : String.t
  ; image_location : String.t
  ; state : ImageState.t
  ; owner_id : String.t
  ; creation_date : String.t option
  ; public : Boolean.t
  ; product_codes : ProductCodeList.t
  ; architecture : ArchitectureValues.t
  ; image_type : ImageTypeValues.t
  ; kernel_id : String.t option
  ; ramdisk_id : String.t option
  ; platform : PlatformValues.t option
  }

let make
    ?platform_details
    ?usage_operation
    ?(block_device_mappings = [])
    ?description
    ?ena_support
    ~hypervisor
    ?image_owner_alias
    ?name
    ?root_device_name
    ~root_device_type
    ?sriov_net_support
    ?state_reason
    ?(tags = [])
    ~virtualization_type
    ?boot_mode
    ?tpm_support
    ?deprecation_time
    ?imds_support
    ?source_instance_id
    ?deregistration_protection
    ?last_launched_time
    ?image_allowed
    ?source_image_id
    ?source_image_region
    ?free_tier_eligible
    ?(image_watermarks = [])
    ~image_id
    ~image_location
    ~state
    ~owner_id
    ?creation_date
    ~public
    ?(product_codes = [])
    ~architecture
    ~image_type
    ?kernel_id
    ?ramdisk_id
    ?platform
    () =
  { platform_details
  ; usage_operation
  ; block_device_mappings
  ; description
  ; ena_support
  ; hypervisor
  ; image_owner_alias
  ; name
  ; root_device_name
  ; root_device_type
  ; sriov_net_support
  ; state_reason
  ; tags
  ; virtualization_type
  ; boot_mode
  ; tpm_support
  ; deprecation_time
  ; imds_support
  ; source_instance_id
  ; deregistration_protection
  ; last_launched_time
  ; image_allowed
  ; source_image_id
  ; source_image_region
  ; free_tier_eligible
  ; image_watermarks
  ; image_id
  ; image_location
  ; state
  ; owner_id
  ; creation_date
  ; public
  ; product_codes
  ; architecture
  ; image_type
  ; kernel_id
  ; ramdisk_id
  ; platform
  }

let parse xml =
  Some
    { platform_details =
        Aws.Util.option_bind (Aws.Xml.member "platformDetails" xml) String.parse
    ; usage_operation =
        Aws.Util.option_bind (Aws.Xml.member "usageOperation" xml) String.parse
    ; block_device_mappings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "blockDeviceMapping" xml)
             BlockDeviceMappingList.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; ena_support = Aws.Util.option_bind (Aws.Xml.member "enaSupport" xml) Boolean.parse
    ; hypervisor =
        Aws.Xml.required
          "hypervisor"
          (Aws.Util.option_bind (Aws.Xml.member "hypervisor" xml) HypervisorType.parse)
    ; image_owner_alias =
        Aws.Util.option_bind (Aws.Xml.member "imageOwnerAlias" xml) String.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "name" xml) String.parse
    ; root_device_name =
        Aws.Util.option_bind (Aws.Xml.member "rootDeviceName" xml) String.parse
    ; root_device_type =
        Aws.Xml.required
          "rootDeviceType"
          (Aws.Util.option_bind (Aws.Xml.member "rootDeviceType" xml) DeviceType.parse)
    ; sriov_net_support =
        Aws.Util.option_bind (Aws.Xml.member "sriovNetSupport" xml) String.parse
    ; state_reason =
        Aws.Util.option_bind (Aws.Xml.member "stateReason" xml) StateReason.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; virtualization_type =
        Aws.Xml.required
          "virtualizationType"
          (Aws.Util.option_bind
             (Aws.Xml.member "virtualizationType" xml)
             VirtualizationType.parse)
    ; boot_mode =
        Aws.Util.option_bind (Aws.Xml.member "bootMode" xml) BootModeValues.parse
    ; tpm_support =
        Aws.Util.option_bind (Aws.Xml.member "tpmSupport" xml) TpmSupportValues.parse
    ; deprecation_time =
        Aws.Util.option_bind (Aws.Xml.member "deprecationTime" xml) String.parse
    ; imds_support =
        Aws.Util.option_bind (Aws.Xml.member "imdsSupport" xml) ImdsSupportValues.parse
    ; source_instance_id =
        Aws.Util.option_bind (Aws.Xml.member "sourceInstanceId" xml) String.parse
    ; deregistration_protection =
        Aws.Util.option_bind (Aws.Xml.member "deregistrationProtection" xml) String.parse
    ; last_launched_time =
        Aws.Util.option_bind (Aws.Xml.member "lastLaunchedTime" xml) String.parse
    ; image_allowed =
        Aws.Util.option_bind (Aws.Xml.member "imageAllowed" xml) Boolean.parse
    ; source_image_id =
        Aws.Util.option_bind (Aws.Xml.member "sourceImageId" xml) String.parse
    ; source_image_region =
        Aws.Util.option_bind (Aws.Xml.member "sourceImageRegion" xml) String.parse
    ; free_tier_eligible =
        Aws.Util.option_bind (Aws.Xml.member "freeTierEligible" xml) Boolean.parse
    ; image_watermarks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "imageWatermarkSet" xml)
             ImageWatermarkList.parse)
    ; image_id =
        Aws.Xml.required
          "imageId"
          (Aws.Util.option_bind (Aws.Xml.member "imageId" xml) String.parse)
    ; image_location =
        Aws.Xml.required
          "imageLocation"
          (Aws.Util.option_bind (Aws.Xml.member "imageLocation" xml) String.parse)
    ; state =
        Aws.Xml.required
          "imageState"
          (Aws.Util.option_bind (Aws.Xml.member "imageState" xml) ImageState.parse)
    ; owner_id =
        Aws.Xml.required
          "imageOwnerId"
          (Aws.Util.option_bind (Aws.Xml.member "imageOwnerId" xml) String.parse)
    ; creation_date =
        Aws.Util.option_bind (Aws.Xml.member "creationDate" xml) String.parse
    ; public =
        Aws.Xml.required
          "isPublic"
          (Aws.Util.option_bind (Aws.Xml.member "isPublic" xml) Boolean.parse)
    ; product_codes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "productCodes" xml) ProductCodeList.parse)
    ; architecture =
        Aws.Xml.required
          "architecture"
          (Aws.Util.option_bind
             (Aws.Xml.member "architecture" xml)
             ArchitectureValues.parse)
    ; image_type =
        Aws.Xml.required
          "imageType"
          (Aws.Util.option_bind (Aws.Xml.member "imageType" xml) ImageTypeValues.parse)
    ; kernel_id = Aws.Util.option_bind (Aws.Xml.member "kernelId" xml) String.parse
    ; ramdisk_id = Aws.Util.option_bind (Aws.Xml.member "ramdiskId" xml) String.parse
    ; platform = Aws.Util.option_bind (Aws.Xml.member "platform" xml) PlatformValues.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.platform (fun f ->
             Aws.Query.Pair ("Platform", PlatformValues.to_query f))
       ; Aws.Util.option_map v.ramdisk_id (fun f ->
             Aws.Query.Pair ("RamdiskId", String.to_query f))
       ; Aws.Util.option_map v.kernel_id (fun f ->
             Aws.Query.Pair ("KernelId", String.to_query f))
       ; Some (Aws.Query.Pair ("ImageType", ImageTypeValues.to_query v.image_type))
       ; Some
           (Aws.Query.Pair ("Architecture", ArchitectureValues.to_query v.architecture))
       ; Some (Aws.Query.Pair ("ProductCodes", ProductCodeList.to_query v.product_codes))
       ; Some (Aws.Query.Pair ("IsPublic", Boolean.to_query v.public))
       ; Aws.Util.option_map v.creation_date (fun f ->
             Aws.Query.Pair ("CreationDate", String.to_query f))
       ; Some (Aws.Query.Pair ("ImageOwnerId", String.to_query v.owner_id))
       ; Some (Aws.Query.Pair ("ImageState", ImageState.to_query v.state))
       ; Some (Aws.Query.Pair ("ImageLocation", String.to_query v.image_location))
       ; Some (Aws.Query.Pair ("ImageId", String.to_query v.image_id))
       ; Some
           (Aws.Query.Pair
              ("ImageWatermarkSet", ImageWatermarkList.to_query v.image_watermarks))
       ; Aws.Util.option_map v.free_tier_eligible (fun f ->
             Aws.Query.Pair ("FreeTierEligible", Boolean.to_query f))
       ; Aws.Util.option_map v.source_image_region (fun f ->
             Aws.Query.Pair ("SourceImageRegion", String.to_query f))
       ; Aws.Util.option_map v.source_image_id (fun f ->
             Aws.Query.Pair ("SourceImageId", String.to_query f))
       ; Aws.Util.option_map v.image_allowed (fun f ->
             Aws.Query.Pair ("ImageAllowed", Boolean.to_query f))
       ; Aws.Util.option_map v.last_launched_time (fun f ->
             Aws.Query.Pair ("LastLaunchedTime", String.to_query f))
       ; Aws.Util.option_map v.deregistration_protection (fun f ->
             Aws.Query.Pair ("DeregistrationProtection", String.to_query f))
       ; Aws.Util.option_map v.source_instance_id (fun f ->
             Aws.Query.Pair ("SourceInstanceId", String.to_query f))
       ; Aws.Util.option_map v.imds_support (fun f ->
             Aws.Query.Pair ("ImdsSupport", ImdsSupportValues.to_query f))
       ; Aws.Util.option_map v.deprecation_time (fun f ->
             Aws.Query.Pair ("DeprecationTime", String.to_query f))
       ; Aws.Util.option_map v.tpm_support (fun f ->
             Aws.Query.Pair ("TpmSupport", TpmSupportValues.to_query f))
       ; Aws.Util.option_map v.boot_mode (fun f ->
             Aws.Query.Pair ("BootMode", BootModeValues.to_query f))
       ; Some
           (Aws.Query.Pair
              ("VirtualizationType", VirtualizationType.to_query v.virtualization_type))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.state_reason (fun f ->
             Aws.Query.Pair ("StateReason", StateReason.to_query f))
       ; Aws.Util.option_map v.sriov_net_support (fun f ->
             Aws.Query.Pair ("SriovNetSupport", String.to_query f))
       ; Some (Aws.Query.Pair ("RootDeviceType", DeviceType.to_query v.root_device_type))
       ; Aws.Util.option_map v.root_device_name (fun f ->
             Aws.Query.Pair ("RootDeviceName", String.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Aws.Util.option_map v.image_owner_alias (fun f ->
             Aws.Query.Pair ("ImageOwnerAlias", String.to_query f))
       ; Some (Aws.Query.Pair ("Hypervisor", HypervisorType.to_query v.hypervisor))
       ; Aws.Util.option_map v.ena_support (fun f ->
             Aws.Query.Pair ("EnaSupport", Boolean.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "BlockDeviceMapping"
              , BlockDeviceMappingList.to_query v.block_device_mappings ))
       ; Aws.Util.option_map v.usage_operation (fun f ->
             Aws.Query.Pair ("UsageOperation", String.to_query f))
       ; Aws.Util.option_map v.platform_details (fun f ->
             Aws.Query.Pair ("PlatformDetails", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.platform (fun f -> "platform", PlatformValues.to_json f)
       ; Aws.Util.option_map v.ramdisk_id (fun f -> "ramdiskId", String.to_json f)
       ; Aws.Util.option_map v.kernel_id (fun f -> "kernelId", String.to_json f)
       ; Some ("imageType", ImageTypeValues.to_json v.image_type)
       ; Some ("architecture", ArchitectureValues.to_json v.architecture)
       ; Some ("productCodes", ProductCodeList.to_json v.product_codes)
       ; Some ("isPublic", Boolean.to_json v.public)
       ; Aws.Util.option_map v.creation_date (fun f -> "creationDate", String.to_json f)
       ; Some ("imageOwnerId", String.to_json v.owner_id)
       ; Some ("imageState", ImageState.to_json v.state)
       ; Some ("imageLocation", String.to_json v.image_location)
       ; Some ("imageId", String.to_json v.image_id)
       ; Some ("imageWatermarkSet", ImageWatermarkList.to_json v.image_watermarks)
       ; Aws.Util.option_map v.free_tier_eligible (fun f ->
             "freeTierEligible", Boolean.to_json f)
       ; Aws.Util.option_map v.source_image_region (fun f ->
             "sourceImageRegion", String.to_json f)
       ; Aws.Util.option_map v.source_image_id (fun f ->
             "sourceImageId", String.to_json f)
       ; Aws.Util.option_map v.image_allowed (fun f -> "imageAllowed", Boolean.to_json f)
       ; Aws.Util.option_map v.last_launched_time (fun f ->
             "lastLaunchedTime", String.to_json f)
       ; Aws.Util.option_map v.deregistration_protection (fun f ->
             "deregistrationProtection", String.to_json f)
       ; Aws.Util.option_map v.source_instance_id (fun f ->
             "sourceInstanceId", String.to_json f)
       ; Aws.Util.option_map v.imds_support (fun f ->
             "imdsSupport", ImdsSupportValues.to_json f)
       ; Aws.Util.option_map v.deprecation_time (fun f ->
             "deprecationTime", String.to_json f)
       ; Aws.Util.option_map v.tpm_support (fun f ->
             "tpmSupport", TpmSupportValues.to_json f)
       ; Aws.Util.option_map v.boot_mode (fun f -> "bootMode", BootModeValues.to_json f)
       ; Some ("virtualizationType", VirtualizationType.to_json v.virtualization_type)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.state_reason (fun f ->
             "stateReason", StateReason.to_json f)
       ; Aws.Util.option_map v.sriov_net_support (fun f ->
             "sriovNetSupport", String.to_json f)
       ; Some ("rootDeviceType", DeviceType.to_json v.root_device_type)
       ; Aws.Util.option_map v.root_device_name (fun f ->
             "rootDeviceName", String.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "name", String.to_json f)
       ; Aws.Util.option_map v.image_owner_alias (fun f ->
             "imageOwnerAlias", String.to_json f)
       ; Some ("hypervisor", HypervisorType.to_json v.hypervisor)
       ; Aws.Util.option_map v.ena_support (fun f -> "enaSupport", Boolean.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Some
           ("blockDeviceMapping", BlockDeviceMappingList.to_json v.block_device_mappings)
       ; Aws.Util.option_map v.usage_operation (fun f ->
             "usageOperation", String.to_json f)
       ; Aws.Util.option_map v.platform_details (fun f ->
             "platformDetails", String.to_json f)
       ])

let of_json j =
  { platform_details =
      Aws.Util.option_map (Aws.Json.lookup j "platformDetails") String.of_json
  ; usage_operation =
      Aws.Util.option_map (Aws.Json.lookup j "usageOperation") String.of_json
  ; block_device_mappings =
      BlockDeviceMappingList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "blockDeviceMapping"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; ena_support = Aws.Util.option_map (Aws.Json.lookup j "enaSupport") Boolean.of_json
  ; hypervisor =
      HypervisorType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "hypervisor"))
  ; image_owner_alias =
      Aws.Util.option_map (Aws.Json.lookup j "imageOwnerAlias") String.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "name") String.of_json
  ; root_device_name =
      Aws.Util.option_map (Aws.Json.lookup j "rootDeviceName") String.of_json
  ; root_device_type =
      DeviceType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "rootDeviceType"))
  ; sriov_net_support =
      Aws.Util.option_map (Aws.Json.lookup j "sriovNetSupport") String.of_json
  ; state_reason =
      Aws.Util.option_map (Aws.Json.lookup j "stateReason") StateReason.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; virtualization_type =
      VirtualizationType.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "virtualizationType"))
  ; boot_mode = Aws.Util.option_map (Aws.Json.lookup j "bootMode") BootModeValues.of_json
  ; tpm_support =
      Aws.Util.option_map (Aws.Json.lookup j "tpmSupport") TpmSupportValues.of_json
  ; deprecation_time =
      Aws.Util.option_map (Aws.Json.lookup j "deprecationTime") String.of_json
  ; imds_support =
      Aws.Util.option_map (Aws.Json.lookup j "imdsSupport") ImdsSupportValues.of_json
  ; source_instance_id =
      Aws.Util.option_map (Aws.Json.lookup j "sourceInstanceId") String.of_json
  ; deregistration_protection =
      Aws.Util.option_map (Aws.Json.lookup j "deregistrationProtection") String.of_json
  ; last_launched_time =
      Aws.Util.option_map (Aws.Json.lookup j "lastLaunchedTime") String.of_json
  ; image_allowed = Aws.Util.option_map (Aws.Json.lookup j "imageAllowed") Boolean.of_json
  ; source_image_id =
      Aws.Util.option_map (Aws.Json.lookup j "sourceImageId") String.of_json
  ; source_image_region =
      Aws.Util.option_map (Aws.Json.lookup j "sourceImageRegion") String.of_json
  ; free_tier_eligible =
      Aws.Util.option_map (Aws.Json.lookup j "freeTierEligible") Boolean.of_json
  ; image_watermarks =
      ImageWatermarkList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "imageWatermarkSet"))
  ; image_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "imageId"))
  ; image_location =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "imageLocation"))
  ; state = ImageState.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "imageState"))
  ; owner_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "imageOwnerId"))
  ; creation_date = Aws.Util.option_map (Aws.Json.lookup j "creationDate") String.of_json
  ; public = Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "isPublic"))
  ; product_codes =
      ProductCodeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "productCodes"))
  ; architecture =
      ArchitectureValues.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "architecture"))
  ; image_type =
      ImageTypeValues.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "imageType"))
  ; kernel_id = Aws.Util.option_map (Aws.Json.lookup j "kernelId") String.of_json
  ; ramdisk_id = Aws.Util.option_map (Aws.Json.lookup j "ramdiskId") String.of_json
  ; platform = Aws.Util.option_map (Aws.Json.lookup j "platform") PlatformValues.of_json
  }
