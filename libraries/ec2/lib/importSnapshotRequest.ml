open Aws.BaseTypes

type t =
  { client_data : ClientData.t option
  ; client_token : String.t option
  ; description : String.t option
  ; disk_container : SnapshotDiskContainer.t option
  ; dry_run : Boolean.t option
  ; encrypted : Boolean.t option
  ; kms_key_id : String.t option
  ; role_name : String.t option
  ; tag_specifications : TagSpecificationList.t
  }

let make
    ?client_data
    ?client_token
    ?description
    ?disk_container
    ?dry_run
    ?encrypted
    ?kms_key_id
    ?role_name
    ?(tag_specifications = [])
    () =
  { client_data
  ; client_token
  ; description
  ; disk_container
  ; dry_run
  ; encrypted
  ; kms_key_id
  ; role_name
  ; tag_specifications
  }

let parse xml =
  Some
    { client_data =
        Aws.Util.option_bind (Aws.Xml.member "ClientData" xml) ClientData.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; disk_container =
        Aws.Util.option_bind
          (Aws.Xml.member "DiskContainer" xml)
          SnapshotDiskContainer.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; encrypted = Aws.Util.option_bind (Aws.Xml.member "Encrypted" xml) Boolean.parse
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; role_name = Aws.Util.option_bind (Aws.Xml.member "RoleName" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.role_name (fun f ->
             Aws.Query.Pair ("RoleName", String.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.encrypted (fun f ->
             Aws.Query.Pair ("Encrypted", Boolean.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.disk_container (fun f ->
             Aws.Query.Pair ("DiskContainer", SnapshotDiskContainer.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.client_data (fun f ->
             Aws.Query.Pair ("ClientData", ClientData.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.role_name (fun f -> "RoleName", String.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.encrypted (fun f -> "Encrypted", Boolean.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.disk_container (fun f ->
             "DiskContainer", SnapshotDiskContainer.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.client_data (fun f -> "ClientData", ClientData.to_json f)
       ])

let of_json j =
  { client_data = Aws.Util.option_map (Aws.Json.lookup j "ClientData") ClientData.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; disk_container =
      Aws.Util.option_map
        (Aws.Json.lookup j "DiskContainer")
        SnapshotDiskContainer.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; encrypted = Aws.Util.option_map (Aws.Json.lookup j "Encrypted") Boolean.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; role_name = Aws.Util.option_map (Aws.Json.lookup j "RoleName") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  }
