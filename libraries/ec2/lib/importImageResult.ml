open Aws.BaseTypes

type t =
  { architecture : String.t option
  ; description : String.t option
  ; encrypted : Boolean.t option
  ; hypervisor : String.t option
  ; image_id : String.t option
  ; import_task_id : String.t option
  ; kms_key_id : String.t option
  ; license_type : String.t option
  ; platform : String.t option
  ; progress : String.t option
  ; snapshot_details : SnapshotDetailList.t
  ; status : String.t option
  ; status_message : String.t option
  ; license_specifications : ImportImageLicenseSpecificationListResponse.t
  ; tags : TagList.t
  ; usage_operation : String.t option
  }

let make
    ?architecture
    ?description
    ?encrypted
    ?hypervisor
    ?image_id
    ?import_task_id
    ?kms_key_id
    ?license_type
    ?platform
    ?progress
    ?(snapshot_details = [])
    ?status
    ?status_message
    ?(license_specifications = [])
    ?(tags = [])
    ?usage_operation
    () =
  { architecture
  ; description
  ; encrypted
  ; hypervisor
  ; image_id
  ; import_task_id
  ; kms_key_id
  ; license_type
  ; platform
  ; progress
  ; snapshot_details
  ; status
  ; status_message
  ; license_specifications
  ; tags
  ; usage_operation
  }

let parse xml =
  Some
    { architecture = Aws.Util.option_bind (Aws.Xml.member "architecture" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; encrypted = Aws.Util.option_bind (Aws.Xml.member "encrypted" xml) Boolean.parse
    ; hypervisor = Aws.Util.option_bind (Aws.Xml.member "hypervisor" xml) String.parse
    ; image_id = Aws.Util.option_bind (Aws.Xml.member "imageId" xml) String.parse
    ; import_task_id =
        Aws.Util.option_bind (Aws.Xml.member "importTaskId" xml) String.parse
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "kmsKeyId" xml) String.parse
    ; license_type = Aws.Util.option_bind (Aws.Xml.member "licenseType" xml) String.parse
    ; platform = Aws.Util.option_bind (Aws.Xml.member "platform" xml) String.parse
    ; progress = Aws.Util.option_bind (Aws.Xml.member "progress" xml) String.parse
    ; snapshot_details =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "snapshotDetailSet" xml)
             SnapshotDetailList.parse)
    ; status = Aws.Util.option_bind (Aws.Xml.member "status" xml) String.parse
    ; status_message =
        Aws.Util.option_bind (Aws.Xml.member "statusMessage" xml) String.parse
    ; license_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "licenseSpecifications" xml)
             ImportImageLicenseSpecificationListResponse.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; usage_operation =
        Aws.Util.option_bind (Aws.Xml.member "usageOperation" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.usage_operation (fun f ->
             Aws.Query.Pair ("UsageOperation", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Some
           (Aws.Query.Pair
              ( "LicenseSpecifications"
              , ImportImageLicenseSpecificationListResponse.to_query
                  v.license_specifications ))
       ; Aws.Util.option_map v.status_message (fun f ->
             Aws.Query.Pair ("StatusMessage", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("SnapshotDetailSet", SnapshotDetailList.to_query v.snapshot_details))
       ; Aws.Util.option_map v.progress (fun f ->
             Aws.Query.Pair ("Progress", String.to_query f))
       ; Aws.Util.option_map v.platform (fun f ->
             Aws.Query.Pair ("Platform", String.to_query f))
       ; Aws.Util.option_map v.license_type (fun f ->
             Aws.Query.Pair ("LicenseType", String.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.import_task_id (fun f ->
             Aws.Query.Pair ("ImportTaskId", String.to_query f))
       ; Aws.Util.option_map v.image_id (fun f ->
             Aws.Query.Pair ("ImageId", String.to_query f))
       ; Aws.Util.option_map v.hypervisor (fun f ->
             Aws.Query.Pair ("Hypervisor", String.to_query f))
       ; Aws.Util.option_map v.encrypted (fun f ->
             Aws.Query.Pair ("Encrypted", Boolean.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.architecture (fun f ->
             Aws.Query.Pair ("Architecture", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.usage_operation (fun f ->
             "usageOperation", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Some
           ( "licenseSpecifications"
           , ImportImageLicenseSpecificationListResponse.to_json v.license_specifications
           )
       ; Aws.Util.option_map v.status_message (fun f -> "statusMessage", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "status", String.to_json f)
       ; Some ("snapshotDetailSet", SnapshotDetailList.to_json v.snapshot_details)
       ; Aws.Util.option_map v.progress (fun f -> "progress", String.to_json f)
       ; Aws.Util.option_map v.platform (fun f -> "platform", String.to_json f)
       ; Aws.Util.option_map v.license_type (fun f -> "licenseType", String.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "kmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.import_task_id (fun f -> "importTaskId", String.to_json f)
       ; Aws.Util.option_map v.image_id (fun f -> "imageId", String.to_json f)
       ; Aws.Util.option_map v.hypervisor (fun f -> "hypervisor", String.to_json f)
       ; Aws.Util.option_map v.encrypted (fun f -> "encrypted", Boolean.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.architecture (fun f -> "architecture", String.to_json f)
       ])

let of_json j =
  { architecture = Aws.Util.option_map (Aws.Json.lookup j "architecture") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; encrypted = Aws.Util.option_map (Aws.Json.lookup j "encrypted") Boolean.of_json
  ; hypervisor = Aws.Util.option_map (Aws.Json.lookup j "hypervisor") String.of_json
  ; image_id = Aws.Util.option_map (Aws.Json.lookup j "imageId") String.of_json
  ; import_task_id = Aws.Util.option_map (Aws.Json.lookup j "importTaskId") String.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "kmsKeyId") String.of_json
  ; license_type = Aws.Util.option_map (Aws.Json.lookup j "licenseType") String.of_json
  ; platform = Aws.Util.option_map (Aws.Json.lookup j "platform") String.of_json
  ; progress = Aws.Util.option_map (Aws.Json.lookup j "progress") String.of_json
  ; snapshot_details =
      SnapshotDetailList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "snapshotDetailSet"))
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") String.of_json
  ; status_message =
      Aws.Util.option_map (Aws.Json.lookup j "statusMessage") String.of_json
  ; license_specifications =
      ImportImageLicenseSpecificationListResponse.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "licenseSpecifications"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; usage_operation =
      Aws.Util.option_map (Aws.Json.lookup j "usageOperation") String.of_json
  }
