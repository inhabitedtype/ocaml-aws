open Aws.BaseTypes

type t =
  { description : AttributeValue.t option
  ; kernel_id : AttributeValue.t option
  ; ramdisk_id : AttributeValue.t option
  ; sriov_net_support : AttributeValue.t option
  ; boot_mode : AttributeValue.t option
  ; tpm_support : AttributeValue.t option
  ; uefi_data : AttributeValue.t option
  ; last_launched_time : AttributeValue.t option
  ; imds_support : AttributeValue.t option
  ; deregistration_protection : AttributeValue.t option
  ; image_id : String.t option
  ; launch_permissions : LaunchPermissionList.t
  ; product_codes : ProductCodeList.t
  ; block_device_mappings : BlockDeviceMappingList.t
  }

let make
    ?description
    ?kernel_id
    ?ramdisk_id
    ?sriov_net_support
    ?boot_mode
    ?tpm_support
    ?uefi_data
    ?last_launched_time
    ?imds_support
    ?deregistration_protection
    ?image_id
    ?(launch_permissions = [])
    ?(product_codes = [])
    ?(block_device_mappings = [])
    () =
  { description
  ; kernel_id
  ; ramdisk_id
  ; sriov_net_support
  ; boot_mode
  ; tpm_support
  ; uefi_data
  ; last_launched_time
  ; imds_support
  ; deregistration_protection
  ; image_id
  ; launch_permissions
  ; product_codes
  ; block_device_mappings
  }

let parse xml =
  Some
    { description =
        Aws.Util.option_bind (Aws.Xml.member "description" xml) AttributeValue.parse
    ; kernel_id = Aws.Util.option_bind (Aws.Xml.member "kernel" xml) AttributeValue.parse
    ; ramdisk_id =
        Aws.Util.option_bind (Aws.Xml.member "ramdisk" xml) AttributeValue.parse
    ; sriov_net_support =
        Aws.Util.option_bind (Aws.Xml.member "sriovNetSupport" xml) AttributeValue.parse
    ; boot_mode =
        Aws.Util.option_bind (Aws.Xml.member "bootMode" xml) AttributeValue.parse
    ; tpm_support =
        Aws.Util.option_bind (Aws.Xml.member "tpmSupport" xml) AttributeValue.parse
    ; uefi_data =
        Aws.Util.option_bind (Aws.Xml.member "uefiData" xml) AttributeValue.parse
    ; last_launched_time =
        Aws.Util.option_bind (Aws.Xml.member "lastLaunchedTime" xml) AttributeValue.parse
    ; imds_support =
        Aws.Util.option_bind (Aws.Xml.member "imdsSupport" xml) AttributeValue.parse
    ; deregistration_protection =
        Aws.Util.option_bind
          (Aws.Xml.member "deregistrationProtection" xml)
          AttributeValue.parse
    ; image_id = Aws.Util.option_bind (Aws.Xml.member "imageId" xml) String.parse
    ; launch_permissions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "launchPermission" xml)
             LaunchPermissionList.parse)
    ; product_codes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "productCodes" xml) ProductCodeList.parse)
    ; block_device_mappings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "blockDeviceMapping" xml)
             BlockDeviceMappingList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "BlockDeviceMapping"
              , BlockDeviceMappingList.to_query v.block_device_mappings ))
       ; Some (Aws.Query.Pair ("ProductCodes", ProductCodeList.to_query v.product_codes))
       ; Some
           (Aws.Query.Pair
              ("LaunchPermission", LaunchPermissionList.to_query v.launch_permissions))
       ; Aws.Util.option_map v.image_id (fun f ->
             Aws.Query.Pair ("ImageId", String.to_query f))
       ; Aws.Util.option_map v.deregistration_protection (fun f ->
             Aws.Query.Pair ("DeregistrationProtection", AttributeValue.to_query f))
       ; Aws.Util.option_map v.imds_support (fun f ->
             Aws.Query.Pair ("ImdsSupport", AttributeValue.to_query f))
       ; Aws.Util.option_map v.last_launched_time (fun f ->
             Aws.Query.Pair ("LastLaunchedTime", AttributeValue.to_query f))
       ; Aws.Util.option_map v.uefi_data (fun f ->
             Aws.Query.Pair ("UefiData", AttributeValue.to_query f))
       ; Aws.Util.option_map v.tpm_support (fun f ->
             Aws.Query.Pair ("TpmSupport", AttributeValue.to_query f))
       ; Aws.Util.option_map v.boot_mode (fun f ->
             Aws.Query.Pair ("BootMode", AttributeValue.to_query f))
       ; Aws.Util.option_map v.sriov_net_support (fun f ->
             Aws.Query.Pair ("SriovNetSupport", AttributeValue.to_query f))
       ; Aws.Util.option_map v.ramdisk_id (fun f ->
             Aws.Query.Pair ("Ramdisk", AttributeValue.to_query f))
       ; Aws.Util.option_map v.kernel_id (fun f ->
             Aws.Query.Pair ("Kernel", AttributeValue.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", AttributeValue.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ("blockDeviceMapping", BlockDeviceMappingList.to_json v.block_device_mappings)
       ; Some ("productCodes", ProductCodeList.to_json v.product_codes)
       ; Some ("launchPermission", LaunchPermissionList.to_json v.launch_permissions)
       ; Aws.Util.option_map v.image_id (fun f -> "imageId", String.to_json f)
       ; Aws.Util.option_map v.deregistration_protection (fun f ->
             "deregistrationProtection", AttributeValue.to_json f)
       ; Aws.Util.option_map v.imds_support (fun f ->
             "imdsSupport", AttributeValue.to_json f)
       ; Aws.Util.option_map v.last_launched_time (fun f ->
             "lastLaunchedTime", AttributeValue.to_json f)
       ; Aws.Util.option_map v.uefi_data (fun f -> "uefiData", AttributeValue.to_json f)
       ; Aws.Util.option_map v.tpm_support (fun f ->
             "tpmSupport", AttributeValue.to_json f)
       ; Aws.Util.option_map v.boot_mode (fun f -> "bootMode", AttributeValue.to_json f)
       ; Aws.Util.option_map v.sriov_net_support (fun f ->
             "sriovNetSupport", AttributeValue.to_json f)
       ; Aws.Util.option_map v.ramdisk_id (fun f -> "ramdisk", AttributeValue.to_json f)
       ; Aws.Util.option_map v.kernel_id (fun f -> "kernel", AttributeValue.to_json f)
       ; Aws.Util.option_map v.description (fun f ->
             "description", AttributeValue.to_json f)
       ])

let of_json j =
  { description =
      Aws.Util.option_map (Aws.Json.lookup j "description") AttributeValue.of_json
  ; kernel_id = Aws.Util.option_map (Aws.Json.lookup j "kernel") AttributeValue.of_json
  ; ramdisk_id = Aws.Util.option_map (Aws.Json.lookup j "ramdisk") AttributeValue.of_json
  ; sriov_net_support =
      Aws.Util.option_map (Aws.Json.lookup j "sriovNetSupport") AttributeValue.of_json
  ; boot_mode = Aws.Util.option_map (Aws.Json.lookup j "bootMode") AttributeValue.of_json
  ; tpm_support =
      Aws.Util.option_map (Aws.Json.lookup j "tpmSupport") AttributeValue.of_json
  ; uefi_data = Aws.Util.option_map (Aws.Json.lookup j "uefiData") AttributeValue.of_json
  ; last_launched_time =
      Aws.Util.option_map (Aws.Json.lookup j "lastLaunchedTime") AttributeValue.of_json
  ; imds_support =
      Aws.Util.option_map (Aws.Json.lookup j "imdsSupport") AttributeValue.of_json
  ; deregistration_protection =
      Aws.Util.option_map
        (Aws.Json.lookup j "deregistrationProtection")
        AttributeValue.of_json
  ; image_id = Aws.Util.option_map (Aws.Json.lookup j "imageId") String.of_json
  ; launch_permissions =
      LaunchPermissionList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "launchPermission"))
  ; product_codes =
      ProductCodeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "productCodes"))
  ; block_device_mappings =
      BlockDeviceMappingList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "blockDeviceMapping"))
  }
