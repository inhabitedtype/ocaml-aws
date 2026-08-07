open Aws.BaseTypes

type t =
  { image_location : String.t option
  ; billing_products : BillingProductList.t
  ; boot_mode : BootModeValues.t option
  ; tpm_support : TpmSupportValues.t option
  ; uefi_data : String.t option
  ; imds_support : ImdsSupportValues.t option
  ; tag_specifications : TagSpecificationList.t
  ; dry_run : Boolean.t option
  ; name : String.t
  ; description : String.t option
  ; architecture : ArchitectureValues.t option
  ; kernel_id : String.t option
  ; ramdisk_id : String.t option
  ; root_device_name : String.t option
  ; block_device_mappings : BlockDeviceMappingRequestList.t
  ; virtualization_type : String.t option
  ; sriov_net_support : String.t option
  ; ena_support : Boolean.t option
  }

let make
    ?image_location
    ?(billing_products = [])
    ?boot_mode
    ?tpm_support
    ?uefi_data
    ?imds_support
    ?(tag_specifications = [])
    ?dry_run
    ~name
    ?description
    ?architecture
    ?kernel_id
    ?ramdisk_id
    ?root_device_name
    ?(block_device_mappings = [])
    ?virtualization_type
    ?sriov_net_support
    ?ena_support
    () =
  { image_location
  ; billing_products
  ; boot_mode
  ; tpm_support
  ; uefi_data
  ; imds_support
  ; tag_specifications
  ; dry_run
  ; name
  ; description
  ; architecture
  ; kernel_id
  ; ramdisk_id
  ; root_device_name
  ; block_device_mappings
  ; virtualization_type
  ; sriov_net_support
  ; ena_support
  }

let parse xml =
  Some
    { image_location =
        Aws.Util.option_bind (Aws.Xml.member "ImageLocation" xml) String.parse
    ; billing_products =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "BillingProduct" xml)
             BillingProductList.parse)
    ; boot_mode =
        Aws.Util.option_bind (Aws.Xml.member "BootMode" xml) BootModeValues.parse
    ; tpm_support =
        Aws.Util.option_bind (Aws.Xml.member "TpmSupport" xml) TpmSupportValues.parse
    ; uefi_data = Aws.Util.option_bind (Aws.Xml.member "UefiData" xml) String.parse
    ; imds_support =
        Aws.Util.option_bind (Aws.Xml.member "ImdsSupport" xml) ImdsSupportValues.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; name =
        Aws.Xml.required
          "name"
          (Aws.Util.option_bind (Aws.Xml.member "name" xml) String.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; architecture =
        Aws.Util.option_bind (Aws.Xml.member "architecture" xml) ArchitectureValues.parse
    ; kernel_id = Aws.Util.option_bind (Aws.Xml.member "kernelId" xml) String.parse
    ; ramdisk_id = Aws.Util.option_bind (Aws.Xml.member "ramdiskId" xml) String.parse
    ; root_device_name =
        Aws.Util.option_bind (Aws.Xml.member "rootDeviceName" xml) String.parse
    ; block_device_mappings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "BlockDeviceMapping" xml)
             BlockDeviceMappingRequestList.parse)
    ; virtualization_type =
        Aws.Util.option_bind (Aws.Xml.member "virtualizationType" xml) String.parse
    ; sriov_net_support =
        Aws.Util.option_bind (Aws.Xml.member "sriovNetSupport" xml) String.parse
    ; ena_support = Aws.Util.option_bind (Aws.Xml.member "enaSupport" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ena_support (fun f ->
             Aws.Query.Pair ("EnaSupport", Boolean.to_query f))
       ; Aws.Util.option_map v.sriov_net_support (fun f ->
             Aws.Query.Pair ("SriovNetSupport", String.to_query f))
       ; Aws.Util.option_map v.virtualization_type (fun f ->
             Aws.Query.Pair ("VirtualizationType", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "BlockDeviceMapping"
              , BlockDeviceMappingRequestList.to_query v.block_device_mappings ))
       ; Aws.Util.option_map v.root_device_name (fun f ->
             Aws.Query.Pair ("RootDeviceName", String.to_query f))
       ; Aws.Util.option_map v.ramdisk_id (fun f ->
             Aws.Query.Pair ("RamdiskId", String.to_query f))
       ; Aws.Util.option_map v.kernel_id (fun f ->
             Aws.Query.Pair ("KernelId", String.to_query f))
       ; Aws.Util.option_map v.architecture (fun f ->
             Aws.Query.Pair ("Architecture", ArchitectureValues.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.imds_support (fun f ->
             Aws.Query.Pair ("ImdsSupport", ImdsSupportValues.to_query f))
       ; Aws.Util.option_map v.uefi_data (fun f ->
             Aws.Query.Pair ("UefiData", String.to_query f))
       ; Aws.Util.option_map v.tpm_support (fun f ->
             Aws.Query.Pair ("TpmSupport", TpmSupportValues.to_query f))
       ; Aws.Util.option_map v.boot_mode (fun f ->
             Aws.Query.Pair ("BootMode", BootModeValues.to_query f))
       ; Some
           (Aws.Query.Pair
              ("BillingProduct", BillingProductList.to_query v.billing_products))
       ; Aws.Util.option_map v.image_location (fun f ->
             Aws.Query.Pair ("ImageLocation", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ena_support (fun f -> "enaSupport", Boolean.to_json f)
       ; Aws.Util.option_map v.sriov_net_support (fun f ->
             "sriovNetSupport", String.to_json f)
       ; Aws.Util.option_map v.virtualization_type (fun f ->
             "virtualizationType", String.to_json f)
       ; Some
           ( "BlockDeviceMapping"
           , BlockDeviceMappingRequestList.to_json v.block_device_mappings )
       ; Aws.Util.option_map v.root_device_name (fun f ->
             "rootDeviceName", String.to_json f)
       ; Aws.Util.option_map v.ramdisk_id (fun f -> "ramdiskId", String.to_json f)
       ; Aws.Util.option_map v.kernel_id (fun f -> "kernelId", String.to_json f)
       ; Aws.Util.option_map v.architecture (fun f ->
             "architecture", ArchitectureValues.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Some ("name", String.to_json v.name)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.imds_support (fun f ->
             "ImdsSupport", ImdsSupportValues.to_json f)
       ; Aws.Util.option_map v.uefi_data (fun f -> "UefiData", String.to_json f)
       ; Aws.Util.option_map v.tpm_support (fun f ->
             "TpmSupport", TpmSupportValues.to_json f)
       ; Aws.Util.option_map v.boot_mode (fun f -> "BootMode", BootModeValues.to_json f)
       ; Some ("BillingProduct", BillingProductList.to_json v.billing_products)
       ; Aws.Util.option_map v.image_location (fun f -> "ImageLocation", String.to_json f)
       ])

let of_json j =
  { image_location =
      Aws.Util.option_map (Aws.Json.lookup j "ImageLocation") String.of_json
  ; billing_products =
      BillingProductList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "BillingProduct"))
  ; boot_mode = Aws.Util.option_map (Aws.Json.lookup j "BootMode") BootModeValues.of_json
  ; tpm_support =
      Aws.Util.option_map (Aws.Json.lookup j "TpmSupport") TpmSupportValues.of_json
  ; uefi_data = Aws.Util.option_map (Aws.Json.lookup j "UefiData") String.of_json
  ; imds_support =
      Aws.Util.option_map (Aws.Json.lookup j "ImdsSupport") ImdsSupportValues.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "name"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; architecture =
      Aws.Util.option_map (Aws.Json.lookup j "architecture") ArchitectureValues.of_json
  ; kernel_id = Aws.Util.option_map (Aws.Json.lookup j "kernelId") String.of_json
  ; ramdisk_id = Aws.Util.option_map (Aws.Json.lookup j "ramdiskId") String.of_json
  ; root_device_name =
      Aws.Util.option_map (Aws.Json.lookup j "rootDeviceName") String.of_json
  ; block_device_mappings =
      BlockDeviceMappingRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "BlockDeviceMapping"))
  ; virtualization_type =
      Aws.Util.option_map (Aws.Json.lookup j "virtualizationType") String.of_json
  ; sriov_net_support =
      Aws.Util.option_map (Aws.Json.lookup j "sriovNetSupport") String.of_json
  ; ena_support = Aws.Util.option_map (Aws.Json.lookup j "enaSupport") Boolean.of_json
  }
