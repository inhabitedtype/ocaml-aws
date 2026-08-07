open Aws.BaseTypes

type t =
  { description : String.t option
  ; device_name : String.t option
  ; disk_image_size : Double.t option
  ; format : String.t option
  ; progress : String.t option
  ; snapshot_id : String.t option
  ; status : String.t option
  ; status_message : String.t option
  ; url : String.t option
  ; user_bucket : UserBucketDetails.t option
  }

let make
    ?description
    ?device_name
    ?disk_image_size
    ?format
    ?progress
    ?snapshot_id
    ?status
    ?status_message
    ?url
    ?user_bucket
    () =
  { description
  ; device_name
  ; disk_image_size
  ; format
  ; progress
  ; snapshot_id
  ; status
  ; status_message
  ; url
  ; user_bucket
  }

let parse xml =
  Some
    { description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; device_name = Aws.Util.option_bind (Aws.Xml.member "deviceName" xml) String.parse
    ; disk_image_size =
        Aws.Util.option_bind (Aws.Xml.member "diskImageSize" xml) Double.parse
    ; format = Aws.Util.option_bind (Aws.Xml.member "format" xml) String.parse
    ; progress = Aws.Util.option_bind (Aws.Xml.member "progress" xml) String.parse
    ; snapshot_id = Aws.Util.option_bind (Aws.Xml.member "snapshotId" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "status" xml) String.parse
    ; status_message =
        Aws.Util.option_bind (Aws.Xml.member "statusMessage" xml) String.parse
    ; url = Aws.Util.option_bind (Aws.Xml.member "url" xml) String.parse
    ; user_bucket =
        Aws.Util.option_bind (Aws.Xml.member "userBucket" xml) UserBucketDetails.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.user_bucket (fun f ->
             Aws.Query.Pair ("UserBucket", UserBucketDetails.to_query f))
       ; Aws.Util.option_map v.url (fun f -> Aws.Query.Pair ("Url", String.to_query f))
       ; Aws.Util.option_map v.status_message (fun f ->
             Aws.Query.Pair ("StatusMessage", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.snapshot_id (fun f ->
             Aws.Query.Pair ("SnapshotId", String.to_query f))
       ; Aws.Util.option_map v.progress (fun f ->
             Aws.Query.Pair ("Progress", String.to_query f))
       ; Aws.Util.option_map v.format (fun f ->
             Aws.Query.Pair ("Format", String.to_query f))
       ; Aws.Util.option_map v.disk_image_size (fun f ->
             Aws.Query.Pair ("DiskImageSize", Double.to_query f))
       ; Aws.Util.option_map v.device_name (fun f ->
             Aws.Query.Pair ("DeviceName", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.user_bucket (fun f ->
             "userBucket", UserBucketDetails.to_json f)
       ; Aws.Util.option_map v.url (fun f -> "url", String.to_json f)
       ; Aws.Util.option_map v.status_message (fun f -> "statusMessage", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "status", String.to_json f)
       ; Aws.Util.option_map v.snapshot_id (fun f -> "snapshotId", String.to_json f)
       ; Aws.Util.option_map v.progress (fun f -> "progress", String.to_json f)
       ; Aws.Util.option_map v.format (fun f -> "format", String.to_json f)
       ; Aws.Util.option_map v.disk_image_size (fun f ->
             "diskImageSize", Double.to_json f)
       ; Aws.Util.option_map v.device_name (fun f -> "deviceName", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ])

let of_json j =
  { description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; device_name = Aws.Util.option_map (Aws.Json.lookup j "deviceName") String.of_json
  ; disk_image_size =
      Aws.Util.option_map (Aws.Json.lookup j "diskImageSize") Double.of_json
  ; format = Aws.Util.option_map (Aws.Json.lookup j "format") String.of_json
  ; progress = Aws.Util.option_map (Aws.Json.lookup j "progress") String.of_json
  ; snapshot_id = Aws.Util.option_map (Aws.Json.lookup j "snapshotId") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") String.of_json
  ; status_message =
      Aws.Util.option_map (Aws.Json.lookup j "statusMessage") String.of_json
  ; url = Aws.Util.option_map (Aws.Json.lookup j "url") String.of_json
  ; user_bucket =
      Aws.Util.option_map (Aws.Json.lookup j "userBucket") UserBucketDetails.of_json
  }
