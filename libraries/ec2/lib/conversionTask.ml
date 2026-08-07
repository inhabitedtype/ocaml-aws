open Aws.BaseTypes

type t =
  { conversion_task_id : String.t option
  ; expiration_time : String.t option
  ; import_instance : ImportInstanceTaskDetails.t option
  ; import_volume : ImportVolumeTaskDetails.t option
  ; state : ConversionTaskState.t option
  ; status_message : String.t option
  ; tags : TagList.t
  }

let make
    ?conversion_task_id
    ?expiration_time
    ?import_instance
    ?import_volume
    ?state
    ?status_message
    ?(tags = [])
    () =
  { conversion_task_id
  ; expiration_time
  ; import_instance
  ; import_volume
  ; state
  ; status_message
  ; tags
  }

let parse xml =
  Some
    { conversion_task_id =
        Aws.Util.option_bind (Aws.Xml.member "conversionTaskId" xml) String.parse
    ; expiration_time =
        Aws.Util.option_bind (Aws.Xml.member "expirationTime" xml) String.parse
    ; import_instance =
        Aws.Util.option_bind
          (Aws.Xml.member "importInstance" xml)
          ImportInstanceTaskDetails.parse
    ; import_volume =
        Aws.Util.option_bind
          (Aws.Xml.member "importVolume" xml)
          ImportVolumeTaskDetails.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) ConversionTaskState.parse
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
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", ConversionTaskState.to_query f))
       ; Aws.Util.option_map v.import_volume (fun f ->
             Aws.Query.Pair ("ImportVolume", ImportVolumeTaskDetails.to_query f))
       ; Aws.Util.option_map v.import_instance (fun f ->
             Aws.Query.Pair ("ImportInstance", ImportInstanceTaskDetails.to_query f))
       ; Aws.Util.option_map v.expiration_time (fun f ->
             Aws.Query.Pair ("ExpirationTime", String.to_query f))
       ; Aws.Util.option_map v.conversion_task_id (fun f ->
             Aws.Query.Pair ("ConversionTaskId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.status_message (fun f -> "statusMessage", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", ConversionTaskState.to_json f)
       ; Aws.Util.option_map v.import_volume (fun f ->
             "importVolume", ImportVolumeTaskDetails.to_json f)
       ; Aws.Util.option_map v.import_instance (fun f ->
             "importInstance", ImportInstanceTaskDetails.to_json f)
       ; Aws.Util.option_map v.expiration_time (fun f ->
             "expirationTime", String.to_json f)
       ; Aws.Util.option_map v.conversion_task_id (fun f ->
             "conversionTaskId", String.to_json f)
       ])

let of_json j =
  { conversion_task_id =
      Aws.Util.option_map (Aws.Json.lookup j "conversionTaskId") String.of_json
  ; expiration_time =
      Aws.Util.option_map (Aws.Json.lookup j "expirationTime") String.of_json
  ; import_instance =
      Aws.Util.option_map
        (Aws.Json.lookup j "importInstance")
        ImportInstanceTaskDetails.of_json
  ; import_volume =
      Aws.Util.option_map
        (Aws.Json.lookup j "importVolume")
        ImportVolumeTaskDetails.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") ConversionTaskState.of_json
  ; status_message =
      Aws.Util.option_map (Aws.Json.lookup j "statusMessage") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
