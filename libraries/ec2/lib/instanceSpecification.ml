open Aws.BaseTypes

type t =
  { instance_id : String.t
  ; exclude_boot_volume : Boolean.t option
  ; exclude_data_volume_ids : VolumeIdStringList.t
  }

let make ~instance_id ?exclude_boot_volume ?(exclude_data_volume_ids = []) () =
  { instance_id; exclude_boot_volume; exclude_data_volume_ids }

let parse xml =
  Some
    { instance_id =
        Aws.Xml.required
          "InstanceId"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
    ; exclude_boot_volume =
        Aws.Util.option_bind (Aws.Xml.member "ExcludeBootVolume" xml) Boolean.parse
    ; exclude_data_volume_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ExcludeDataVolumeId" xml)
             VolumeIdStringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ExcludeDataVolumeId"
              , VolumeIdStringList.to_query v.exclude_data_volume_ids ))
       ; Aws.Util.option_map v.exclude_boot_volume (fun f ->
             Aws.Query.Pair ("ExcludeBootVolume", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ExcludeDataVolumeId", VolumeIdStringList.to_json v.exclude_data_volume_ids)
       ; Aws.Util.option_map v.exclude_boot_volume (fun f ->
             "ExcludeBootVolume", Boolean.to_json f)
       ; Some ("InstanceId", String.to_json v.instance_id)
       ])

let of_json j =
  { instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  ; exclude_boot_volume =
      Aws.Util.option_map (Aws.Json.lookup j "ExcludeBootVolume") Boolean.of_json
  ; exclude_data_volume_ids =
      VolumeIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ExcludeDataVolumeId"))
  }
