open Aws.BaseTypes

type t =
  { architecture : String.t option
  ; client_data : ClientData.t option
  ; client_token : String.t option
  ; description : String.t option
  ; disk_containers : ImageDiskContainerList.t
  ; dry_run : Boolean.t option
  ; encrypted : Boolean.t option
  ; hypervisor : String.t option
  ; kms_key_id : String.t option
  ; license_type : String.t option
  ; platform : String.t option
  ; role_name : String.t option
  ; license_specifications : ImportImageLicenseSpecificationListRequest.t
  ; tag_specifications : TagSpecificationList.t
  ; usage_operation : String.t option
  ; boot_mode : BootModeValues.t option
  }

let make
    ?architecture
    ?client_data
    ?client_token
    ?description
    ?(disk_containers = [])
    ?dry_run
    ?encrypted
    ?hypervisor
    ?kms_key_id
    ?license_type
    ?platform
    ?role_name
    ?(license_specifications = [])
    ?(tag_specifications = [])
    ?usage_operation
    ?boot_mode
    () =
  { architecture
  ; client_data
  ; client_token
  ; description
  ; disk_containers
  ; dry_run
  ; encrypted
  ; hypervisor
  ; kms_key_id
  ; license_type
  ; platform
  ; role_name
  ; license_specifications
  ; tag_specifications
  ; usage_operation
  ; boot_mode
  }

let parse xml =
  Some
    { architecture = Aws.Util.option_bind (Aws.Xml.member "Architecture" xml) String.parse
    ; client_data =
        Aws.Util.option_bind (Aws.Xml.member "ClientData" xml) ClientData.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; disk_containers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DiskContainer" xml)
             ImageDiskContainerList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; encrypted = Aws.Util.option_bind (Aws.Xml.member "Encrypted" xml) Boolean.parse
    ; hypervisor = Aws.Util.option_bind (Aws.Xml.member "Hypervisor" xml) String.parse
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; license_type = Aws.Util.option_bind (Aws.Xml.member "LicenseType" xml) String.parse
    ; platform = Aws.Util.option_bind (Aws.Xml.member "Platform" xml) String.parse
    ; role_name = Aws.Util.option_bind (Aws.Xml.member "RoleName" xml) String.parse
    ; license_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LicenseSpecifications" xml)
             ImportImageLicenseSpecificationListRequest.parse)
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; usage_operation =
        Aws.Util.option_bind (Aws.Xml.member "UsageOperation" xml) String.parse
    ; boot_mode =
        Aws.Util.option_bind (Aws.Xml.member "BootMode" xml) BootModeValues.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.boot_mode (fun f ->
             Aws.Query.Pair ("BootMode", BootModeValues.to_query f))
       ; Aws.Util.option_map v.usage_operation (fun f ->
             Aws.Query.Pair ("UsageOperation", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Some
           (Aws.Query.Pair
              ( "LicenseSpecifications"
              , ImportImageLicenseSpecificationListRequest.to_query
                  v.license_specifications ))
       ; Aws.Util.option_map v.role_name (fun f ->
             Aws.Query.Pair ("RoleName", String.to_query f))
       ; Aws.Util.option_map v.platform (fun f ->
             Aws.Query.Pair ("Platform", String.to_query f))
       ; Aws.Util.option_map v.license_type (fun f ->
             Aws.Query.Pair ("LicenseType", String.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.hypervisor (fun f ->
             Aws.Query.Pair ("Hypervisor", String.to_query f))
       ; Aws.Util.option_map v.encrypted (fun f ->
             Aws.Query.Pair ("Encrypted", Boolean.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("DiskContainer", ImageDiskContainerList.to_query v.disk_containers))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.client_data (fun f ->
             Aws.Query.Pair ("ClientData", ClientData.to_query f))
       ; Aws.Util.option_map v.architecture (fun f ->
             Aws.Query.Pair ("Architecture", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.boot_mode (fun f -> "BootMode", BootModeValues.to_json f)
       ; Aws.Util.option_map v.usage_operation (fun f ->
             "UsageOperation", String.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Some
           ( "LicenseSpecifications"
           , ImportImageLicenseSpecificationListRequest.to_json v.license_specifications
           )
       ; Aws.Util.option_map v.role_name (fun f -> "RoleName", String.to_json f)
       ; Aws.Util.option_map v.platform (fun f -> "Platform", String.to_json f)
       ; Aws.Util.option_map v.license_type (fun f -> "LicenseType", String.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.hypervisor (fun f -> "Hypervisor", String.to_json f)
       ; Aws.Util.option_map v.encrypted (fun f -> "Encrypted", Boolean.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("DiskContainer", ImageDiskContainerList.to_json v.disk_containers)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.client_data (fun f -> "ClientData", ClientData.to_json f)
       ; Aws.Util.option_map v.architecture (fun f -> "Architecture", String.to_json f)
       ])

let of_json j =
  { architecture = Aws.Util.option_map (Aws.Json.lookup j "Architecture") String.of_json
  ; client_data = Aws.Util.option_map (Aws.Json.lookup j "ClientData") ClientData.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; disk_containers =
      ImageDiskContainerList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DiskContainer"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; encrypted = Aws.Util.option_map (Aws.Json.lookup j "Encrypted") Boolean.of_json
  ; hypervisor = Aws.Util.option_map (Aws.Json.lookup j "Hypervisor") String.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; license_type = Aws.Util.option_map (Aws.Json.lookup j "LicenseType") String.of_json
  ; platform = Aws.Util.option_map (Aws.Json.lookup j "Platform") String.of_json
  ; role_name = Aws.Util.option_map (Aws.Json.lookup j "RoleName") String.of_json
  ; license_specifications =
      ImportImageLicenseSpecificationListRequest.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LicenseSpecifications"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; usage_operation =
      Aws.Util.option_map (Aws.Json.lookup j "UsageOperation") String.of_json
  ; boot_mode = Aws.Util.option_map (Aws.Json.lookup j "BootMode") BootModeValues.of_json
  }
