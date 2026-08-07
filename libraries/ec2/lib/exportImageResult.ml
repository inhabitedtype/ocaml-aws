open Aws.BaseTypes

type t =
  { description : String.t option
  ; disk_image_format : DiskImageFormat.t option
  ; export_image_task_id : String.t option
  ; image_id : String.t option
  ; role_name : String.t option
  ; progress : String.t option
  ; s3_export_location : ExportTaskS3Location.t option
  ; status : String.t option
  ; status_message : String.t option
  ; tags : TagList.t
  }

let make
    ?description
    ?disk_image_format
    ?export_image_task_id
    ?image_id
    ?role_name
    ?progress
    ?s3_export_location
    ?status
    ?status_message
    ?(tags = [])
    () =
  { description
  ; disk_image_format
  ; export_image_task_id
  ; image_id
  ; role_name
  ; progress
  ; s3_export_location
  ; status
  ; status_message
  ; tags
  }

let parse xml =
  Some
    { description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; disk_image_format =
        Aws.Util.option_bind (Aws.Xml.member "diskImageFormat" xml) DiskImageFormat.parse
    ; export_image_task_id =
        Aws.Util.option_bind (Aws.Xml.member "exportImageTaskId" xml) String.parse
    ; image_id = Aws.Util.option_bind (Aws.Xml.member "imageId" xml) String.parse
    ; role_name = Aws.Util.option_bind (Aws.Xml.member "roleName" xml) String.parse
    ; progress = Aws.Util.option_bind (Aws.Xml.member "progress" xml) String.parse
    ; s3_export_location =
        Aws.Util.option_bind
          (Aws.Xml.member "s3ExportLocation" xml)
          ExportTaskS3Location.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "status" xml) String.parse
    ; status_message =
        Aws.Util.option_bind (Aws.Xml.member "statusMessage" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.status_message (fun f ->
             Aws.Query.Pair ("StatusMessage", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.s3_export_location (fun f ->
             Aws.Query.Pair ("S3ExportLocation", ExportTaskS3Location.to_query f))
       ; Aws.Util.option_map v.progress (fun f ->
             Aws.Query.Pair ("Progress", String.to_query f))
       ; Aws.Util.option_map v.role_name (fun f ->
             Aws.Query.Pair ("RoleName", String.to_query f))
       ; Aws.Util.option_map v.image_id (fun f ->
             Aws.Query.Pair ("ImageId", String.to_query f))
       ; Aws.Util.option_map v.export_image_task_id (fun f ->
             Aws.Query.Pair ("ExportImageTaskId", String.to_query f))
       ; Aws.Util.option_map v.disk_image_format (fun f ->
             Aws.Query.Pair ("DiskImageFormat", DiskImageFormat.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.status_message (fun f -> "statusMessage", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "status", String.to_json f)
       ; Aws.Util.option_map v.s3_export_location (fun f ->
             "s3ExportLocation", ExportTaskS3Location.to_json f)
       ; Aws.Util.option_map v.progress (fun f -> "progress", String.to_json f)
       ; Aws.Util.option_map v.role_name (fun f -> "roleName", String.to_json f)
       ; Aws.Util.option_map v.image_id (fun f -> "imageId", String.to_json f)
       ; Aws.Util.option_map v.export_image_task_id (fun f ->
             "exportImageTaskId", String.to_json f)
       ; Aws.Util.option_map v.disk_image_format (fun f ->
             "diskImageFormat", DiskImageFormat.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ])

let of_json j =
  { description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; disk_image_format =
      Aws.Util.option_map (Aws.Json.lookup j "diskImageFormat") DiskImageFormat.of_json
  ; export_image_task_id =
      Aws.Util.option_map (Aws.Json.lookup j "exportImageTaskId") String.of_json
  ; image_id = Aws.Util.option_map (Aws.Json.lookup j "imageId") String.of_json
  ; role_name = Aws.Util.option_map (Aws.Json.lookup j "roleName") String.of_json
  ; progress = Aws.Util.option_map (Aws.Json.lookup j "progress") String.of_json
  ; s3_export_location =
      Aws.Util.option_map
        (Aws.Json.lookup j "s3ExportLocation")
        ExportTaskS3Location.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") String.of_json
  ; status_message =
      Aws.Util.option_map (Aws.Json.lookup j "statusMessage") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
