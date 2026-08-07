open Aws.BaseTypes

type t =
  { description : String.t option
  ; device_name : String.t option
  ; format : String.t option
  ; snapshot_id : String.t option
  ; url : String.t option
  ; user_bucket : UserBucket.t option
  }

let make ?description ?device_name ?format ?snapshot_id ?url ?user_bucket () =
  { description; device_name; format; snapshot_id; url; user_bucket }

let parse xml =
  Some
    { description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; device_name = Aws.Util.option_bind (Aws.Xml.member "DeviceName" xml) String.parse
    ; format = Aws.Util.option_bind (Aws.Xml.member "Format" xml) String.parse
    ; snapshot_id = Aws.Util.option_bind (Aws.Xml.member "SnapshotId" xml) String.parse
    ; url = Aws.Util.option_bind (Aws.Xml.member "Url" xml) String.parse
    ; user_bucket =
        Aws.Util.option_bind (Aws.Xml.member "UserBucket" xml) UserBucket.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.user_bucket (fun f ->
             Aws.Query.Pair ("UserBucket", UserBucket.to_query f))
       ; Aws.Util.option_map v.url (fun f -> Aws.Query.Pair ("Url", String.to_query f))
       ; Aws.Util.option_map v.snapshot_id (fun f ->
             Aws.Query.Pair ("SnapshotId", String.to_query f))
       ; Aws.Util.option_map v.format (fun f ->
             Aws.Query.Pair ("Format", String.to_query f))
       ; Aws.Util.option_map v.device_name (fun f ->
             Aws.Query.Pair ("DeviceName", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.user_bucket (fun f -> "UserBucket", UserBucket.to_json f)
       ; Aws.Util.option_map v.url (fun f -> "Url", String.to_json f)
       ; Aws.Util.option_map v.snapshot_id (fun f -> "SnapshotId", String.to_json f)
       ; Aws.Util.option_map v.format (fun f -> "Format", String.to_json f)
       ; Aws.Util.option_map v.device_name (fun f -> "DeviceName", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ])

let of_json j =
  { description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; device_name = Aws.Util.option_map (Aws.Json.lookup j "DeviceName") String.of_json
  ; format = Aws.Util.option_map (Aws.Json.lookup j "Format") String.of_json
  ; snapshot_id = Aws.Util.option_map (Aws.Json.lookup j "SnapshotId") String.of_json
  ; url = Aws.Util.option_map (Aws.Json.lookup j "Url") String.of_json
  ; user_bucket = Aws.Util.option_map (Aws.Json.lookup j "UserBucket") UserBucket.of_json
  }
