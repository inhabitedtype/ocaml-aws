open Aws.BaseTypes

type t =
  { snapshot_id : String.t option
  ; recycle_bin_enter_time : DateTime.t option
  ; recycle_bin_exit_time : DateTime.t option
  ; description : String.t option
  ; volume_id : String.t option
  }

let make
    ?snapshot_id
    ?recycle_bin_enter_time
    ?recycle_bin_exit_time
    ?description
    ?volume_id
    () =
  { snapshot_id; recycle_bin_enter_time; recycle_bin_exit_time; description; volume_id }

let parse xml =
  Some
    { snapshot_id = Aws.Util.option_bind (Aws.Xml.member "snapshotId" xml) String.parse
    ; recycle_bin_enter_time =
        Aws.Util.option_bind (Aws.Xml.member "recycleBinEnterTime" xml) DateTime.parse
    ; recycle_bin_exit_time =
        Aws.Util.option_bind (Aws.Xml.member "recycleBinExitTime" xml) DateTime.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; volume_id = Aws.Util.option_bind (Aws.Xml.member "volumeId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.volume_id (fun f ->
             Aws.Query.Pair ("VolumeId", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.recycle_bin_exit_time (fun f ->
             Aws.Query.Pair ("RecycleBinExitTime", DateTime.to_query f))
       ; Aws.Util.option_map v.recycle_bin_enter_time (fun f ->
             Aws.Query.Pair ("RecycleBinEnterTime", DateTime.to_query f))
       ; Aws.Util.option_map v.snapshot_id (fun f ->
             Aws.Query.Pair ("SnapshotId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.volume_id (fun f -> "volumeId", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.recycle_bin_exit_time (fun f ->
             "recycleBinExitTime", DateTime.to_json f)
       ; Aws.Util.option_map v.recycle_bin_enter_time (fun f ->
             "recycleBinEnterTime", DateTime.to_json f)
       ; Aws.Util.option_map v.snapshot_id (fun f -> "snapshotId", String.to_json f)
       ])

let of_json j =
  { snapshot_id = Aws.Util.option_map (Aws.Json.lookup j "snapshotId") String.of_json
  ; recycle_bin_enter_time =
      Aws.Util.option_map (Aws.Json.lookup j "recycleBinEnterTime") DateTime.of_json
  ; recycle_bin_exit_time =
      Aws.Util.option_map (Aws.Json.lookup j "recycleBinExitTime") DateTime.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; volume_id = Aws.Util.option_map (Aws.Json.lookup j "volumeId") String.of_json
  }
