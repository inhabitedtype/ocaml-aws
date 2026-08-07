open Aws.BaseTypes

type t =
  { tag_specifications : TagSpecificationList.t
  ; snapshot_location : SnapshotLocationEnum.t option
  ; dry_run : Boolean.t option
  ; instance_id : String.t
  ; name : String.t
  ; description : String.t option
  ; no_reboot : Boolean.t option
  ; block_device_mappings : BlockDeviceMappingRequestList.t
  }

let make
    ?(tag_specifications = [])
    ?snapshot_location
    ?dry_run
    ~instance_id
    ~name
    ?description
    ?no_reboot
    ?(block_device_mappings = [])
    () =
  { tag_specifications
  ; snapshot_location
  ; dry_run
  ; instance_id
  ; name
  ; description
  ; no_reboot
  ; block_device_mappings
  }

let parse xml =
  Some
    { tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; snapshot_location =
        Aws.Util.option_bind
          (Aws.Xml.member "SnapshotLocation" xml)
          SnapshotLocationEnum.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; instance_id =
        Aws.Xml.required
          "instanceId"
          (Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse)
    ; name =
        Aws.Xml.required
          "name"
          (Aws.Util.option_bind (Aws.Xml.member "name" xml) String.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; no_reboot = Aws.Util.option_bind (Aws.Xml.member "noReboot" xml) Boolean.parse
    ; block_device_mappings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "blockDeviceMapping" xml)
             BlockDeviceMappingRequestList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "BlockDeviceMapping"
              , BlockDeviceMappingRequestList.to_query v.block_device_mappings ))
       ; Aws.Util.option_map v.no_reboot (fun f ->
             Aws.Query.Pair ("NoReboot", Boolean.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.snapshot_location (fun f ->
             Aws.Query.Pair ("SnapshotLocation", SnapshotLocationEnum.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "blockDeviceMapping"
           , BlockDeviceMappingRequestList.to_json v.block_device_mappings )
       ; Aws.Util.option_map v.no_reboot (fun f -> "noReboot", Boolean.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Some ("name", String.to_json v.name)
       ; Some ("instanceId", String.to_json v.instance_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.snapshot_location (fun f ->
             "SnapshotLocation", SnapshotLocationEnum.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ])

let of_json j =
  { tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; snapshot_location =
      Aws.Util.option_map
        (Aws.Json.lookup j "SnapshotLocation")
        SnapshotLocationEnum.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceId"))
  ; name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "name"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; no_reboot = Aws.Util.option_map (Aws.Json.lookup j "noReboot") Boolean.of_json
  ; block_device_mappings =
      BlockDeviceMappingRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "blockDeviceMapping"))
  }
