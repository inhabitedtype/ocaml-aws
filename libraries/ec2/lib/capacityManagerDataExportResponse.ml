open Aws.BaseTypes

type t =
  { capacity_manager_data_export_id : String.t option
  ; s3_bucket_name : String.t option
  ; s3_bucket_prefix : String.t option
  ; schedule : Schedule.t option
  ; output_format : OutputFormat.t option
  ; create_time : DateTime.t option
  ; latest_delivery_status : CapacityManagerDataExportStatus.t option
  ; latest_delivery_status_message : String.t option
  ; latest_delivery_s3_location_uri : String.t option
  ; latest_delivery_time : DateTime.t option
  ; tags : TagList.t
  }

let make
    ?capacity_manager_data_export_id
    ?s3_bucket_name
    ?s3_bucket_prefix
    ?schedule
    ?output_format
    ?create_time
    ?latest_delivery_status
    ?latest_delivery_status_message
    ?latest_delivery_s3_location_uri
    ?latest_delivery_time
    ?(tags = [])
    () =
  { capacity_manager_data_export_id
  ; s3_bucket_name
  ; s3_bucket_prefix
  ; schedule
  ; output_format
  ; create_time
  ; latest_delivery_status
  ; latest_delivery_status_message
  ; latest_delivery_s3_location_uri
  ; latest_delivery_time
  ; tags
  }

let parse xml =
  Some
    { capacity_manager_data_export_id =
        Aws.Util.option_bind
          (Aws.Xml.member "capacityManagerDataExportId" xml)
          String.parse
    ; s3_bucket_name =
        Aws.Util.option_bind (Aws.Xml.member "s3BucketName" xml) String.parse
    ; s3_bucket_prefix =
        Aws.Util.option_bind (Aws.Xml.member "s3BucketPrefix" xml) String.parse
    ; schedule = Aws.Util.option_bind (Aws.Xml.member "schedule" xml) Schedule.parse
    ; output_format =
        Aws.Util.option_bind (Aws.Xml.member "outputFormat" xml) OutputFormat.parse
    ; create_time = Aws.Util.option_bind (Aws.Xml.member "createTime" xml) DateTime.parse
    ; latest_delivery_status =
        Aws.Util.option_bind
          (Aws.Xml.member "latestDeliveryStatus" xml)
          CapacityManagerDataExportStatus.parse
    ; latest_delivery_status_message =
        Aws.Util.option_bind
          (Aws.Xml.member "latestDeliveryStatusMessage" xml)
          String.parse
    ; latest_delivery_s3_location_uri =
        Aws.Util.option_bind
          (Aws.Xml.member "latestDeliveryS3LocationUri" xml)
          String.parse
    ; latest_delivery_time =
        Aws.Util.option_bind (Aws.Xml.member "latestDeliveryTime" xml) DateTime.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.latest_delivery_time (fun f ->
             Aws.Query.Pair ("LatestDeliveryTime", DateTime.to_query f))
       ; Aws.Util.option_map v.latest_delivery_s3_location_uri (fun f ->
             Aws.Query.Pair ("LatestDeliveryS3LocationUri", String.to_query f))
       ; Aws.Util.option_map v.latest_delivery_status_message (fun f ->
             Aws.Query.Pair ("LatestDeliveryStatusMessage", String.to_query f))
       ; Aws.Util.option_map v.latest_delivery_status (fun f ->
             Aws.Query.Pair
               ("LatestDeliveryStatus", CapacityManagerDataExportStatus.to_query f))
       ; Aws.Util.option_map v.create_time (fun f ->
             Aws.Query.Pair ("CreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.output_format (fun f ->
             Aws.Query.Pair ("OutputFormat", OutputFormat.to_query f))
       ; Aws.Util.option_map v.schedule (fun f ->
             Aws.Query.Pair ("Schedule", Schedule.to_query f))
       ; Aws.Util.option_map v.s3_bucket_prefix (fun f ->
             Aws.Query.Pair ("S3BucketPrefix", String.to_query f))
       ; Aws.Util.option_map v.s3_bucket_name (fun f ->
             Aws.Query.Pair ("S3BucketName", String.to_query f))
       ; Aws.Util.option_map v.capacity_manager_data_export_id (fun f ->
             Aws.Query.Pair ("CapacityManagerDataExportId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.latest_delivery_time (fun f ->
             "latestDeliveryTime", DateTime.to_json f)
       ; Aws.Util.option_map v.latest_delivery_s3_location_uri (fun f ->
             "latestDeliveryS3LocationUri", String.to_json f)
       ; Aws.Util.option_map v.latest_delivery_status_message (fun f ->
             "latestDeliveryStatusMessage", String.to_json f)
       ; Aws.Util.option_map v.latest_delivery_status (fun f ->
             "latestDeliveryStatus", CapacityManagerDataExportStatus.to_json f)
       ; Aws.Util.option_map v.create_time (fun f -> "createTime", DateTime.to_json f)
       ; Aws.Util.option_map v.output_format (fun f ->
             "outputFormat", OutputFormat.to_json f)
       ; Aws.Util.option_map v.schedule (fun f -> "schedule", Schedule.to_json f)
       ; Aws.Util.option_map v.s3_bucket_prefix (fun f ->
             "s3BucketPrefix", String.to_json f)
       ; Aws.Util.option_map v.s3_bucket_name (fun f -> "s3BucketName", String.to_json f)
       ; Aws.Util.option_map v.capacity_manager_data_export_id (fun f ->
             "capacityManagerDataExportId", String.to_json f)
       ])

let of_json j =
  { capacity_manager_data_export_id =
      Aws.Util.option_map (Aws.Json.lookup j "capacityManagerDataExportId") String.of_json
  ; s3_bucket_name = Aws.Util.option_map (Aws.Json.lookup j "s3BucketName") String.of_json
  ; s3_bucket_prefix =
      Aws.Util.option_map (Aws.Json.lookup j "s3BucketPrefix") String.of_json
  ; schedule = Aws.Util.option_map (Aws.Json.lookup j "schedule") Schedule.of_json
  ; output_format =
      Aws.Util.option_map (Aws.Json.lookup j "outputFormat") OutputFormat.of_json
  ; create_time = Aws.Util.option_map (Aws.Json.lookup j "createTime") DateTime.of_json
  ; latest_delivery_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "latestDeliveryStatus")
        CapacityManagerDataExportStatus.of_json
  ; latest_delivery_status_message =
      Aws.Util.option_map (Aws.Json.lookup j "latestDeliveryStatusMessage") String.of_json
  ; latest_delivery_s3_location_uri =
      Aws.Util.option_map (Aws.Json.lookup j "latestDeliveryS3LocationUri") String.of_json
  ; latest_delivery_time =
      Aws.Util.option_map (Aws.Json.lookup j "latestDeliveryTime") DateTime.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
