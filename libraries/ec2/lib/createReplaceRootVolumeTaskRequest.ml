open Aws.BaseTypes

type t =
  { instance_id : String.t
  ; snapshot_id : String.t option
  ; client_token : String.t option
  ; dry_run : Boolean.t option
  ; tag_specifications : TagSpecificationList.t
  ; image_id : String.t option
  ; delete_replaced_root_volume : Boolean.t option
  ; volume_initialization_rate : Long.t option
  }

let make
    ~instance_id
    ?snapshot_id
    ?client_token
    ?dry_run
    ?(tag_specifications = [])
    ?image_id
    ?delete_replaced_root_volume
    ?volume_initialization_rate
    () =
  { instance_id
  ; snapshot_id
  ; client_token
  ; dry_run
  ; tag_specifications
  ; image_id
  ; delete_replaced_root_volume
  ; volume_initialization_rate
  }

let parse xml =
  Some
    { instance_id =
        Aws.Xml.required
          "InstanceId"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
    ; snapshot_id = Aws.Util.option_bind (Aws.Xml.member "SnapshotId" xml) String.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; image_id = Aws.Util.option_bind (Aws.Xml.member "ImageId" xml) String.parse
    ; delete_replaced_root_volume =
        Aws.Util.option_bind (Aws.Xml.member "DeleteReplacedRootVolume" xml) Boolean.parse
    ; volume_initialization_rate =
        Aws.Util.option_bind (Aws.Xml.member "VolumeInitializationRate" xml) Long.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.volume_initialization_rate (fun f ->
             Aws.Query.Pair ("VolumeInitializationRate", Long.to_query f))
       ; Aws.Util.option_map v.delete_replaced_root_volume (fun f ->
             Aws.Query.Pair ("DeleteReplacedRootVolume", Boolean.to_query f))
       ; Aws.Util.option_map v.image_id (fun f ->
             Aws.Query.Pair ("ImageId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.snapshot_id (fun f ->
             Aws.Query.Pair ("SnapshotId", String.to_query f))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.volume_initialization_rate (fun f ->
             "VolumeInitializationRate", Long.to_json f)
       ; Aws.Util.option_map v.delete_replaced_root_volume (fun f ->
             "DeleteReplacedRootVolume", Boolean.to_json f)
       ; Aws.Util.option_map v.image_id (fun f -> "ImageId", String.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.snapshot_id (fun f -> "SnapshotId", String.to_json f)
       ; Some ("InstanceId", String.to_json v.instance_id)
       ])

let of_json j =
  { instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  ; snapshot_id = Aws.Util.option_map (Aws.Json.lookup j "SnapshotId") String.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; image_id = Aws.Util.option_map (Aws.Json.lookup j "ImageId") String.of_json
  ; delete_replaced_root_volume =
      Aws.Util.option_map (Aws.Json.lookup j "DeleteReplacedRootVolume") Boolean.of_json
  ; volume_initialization_rate =
      Aws.Util.option_map (Aws.Json.lookup j "VolumeInitializationRate") Long.of_json
  }
