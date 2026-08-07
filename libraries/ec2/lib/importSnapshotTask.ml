open Aws.BaseTypes

type t =
  { description : String.t option
  ; import_task_id : String.t option
  ; snapshot_task_detail : SnapshotTaskDetail.t option
  ; tags : TagList.t
  }

let make ?description ?import_task_id ?snapshot_task_detail ?(tags = []) () =
  { description; import_task_id; snapshot_task_detail; tags }

let parse xml =
  Some
    { description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; import_task_id =
        Aws.Util.option_bind (Aws.Xml.member "importTaskId" xml) String.parse
    ; snapshot_task_detail =
        Aws.Util.option_bind
          (Aws.Xml.member "snapshotTaskDetail" xml)
          SnapshotTaskDetail.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.snapshot_task_detail (fun f ->
             Aws.Query.Pair ("SnapshotTaskDetail", SnapshotTaskDetail.to_query f))
       ; Aws.Util.option_map v.import_task_id (fun f ->
             Aws.Query.Pair ("ImportTaskId", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.snapshot_task_detail (fun f ->
             "snapshotTaskDetail", SnapshotTaskDetail.to_json f)
       ; Aws.Util.option_map v.import_task_id (fun f -> "importTaskId", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ])

let of_json j =
  { description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; import_task_id = Aws.Util.option_map (Aws.Json.lookup j "importTaskId") String.of_json
  ; snapshot_task_detail =
      Aws.Util.option_map
        (Aws.Json.lookup j "snapshotTaskDetail")
        SnapshotTaskDetail.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
