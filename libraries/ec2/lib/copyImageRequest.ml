open Aws.BaseTypes

type t =
  { client_token : String.t option
  ; description : String.t option
  ; encrypted : Boolean.t option
  ; kms_key_id : String.t option
  ; name : String.t
  ; source_image_id : String.t
  ; source_region : String.t
  ; destination_outpost_arn : String.t option
  ; copy_image_tags : Boolean.t option
  ; tag_specifications : TagSpecificationList.t
  ; snapshot_copy_completion_duration_minutes : Long.t option
  ; destination_availability_zone : String.t option
  ; destination_availability_zone_id : String.t option
  ; dry_run : Boolean.t option
  }

let make
    ?client_token
    ?description
    ?encrypted
    ?kms_key_id
    ~name
    ~source_image_id
    ~source_region
    ?destination_outpost_arn
    ?copy_image_tags
    ?(tag_specifications = [])
    ?snapshot_copy_completion_duration_minutes
    ?destination_availability_zone
    ?destination_availability_zone_id
    ?dry_run
    () =
  { client_token
  ; description
  ; encrypted
  ; kms_key_id
  ; name
  ; source_image_id
  ; source_region
  ; destination_outpost_arn
  ; copy_image_tags
  ; tag_specifications
  ; snapshot_copy_completion_duration_minutes
  ; destination_availability_zone
  ; destination_availability_zone_id
  ; dry_run
  }

let parse xml =
  Some
    { client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; encrypted = Aws.Util.option_bind (Aws.Xml.member "encrypted" xml) Boolean.parse
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "kmsKeyId" xml) String.parse
    ; name =
        Aws.Xml.required
          "Name"
          (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
    ; source_image_id =
        Aws.Xml.required
          "SourceImageId"
          (Aws.Util.option_bind (Aws.Xml.member "SourceImageId" xml) String.parse)
    ; source_region =
        Aws.Xml.required
          "SourceRegion"
          (Aws.Util.option_bind (Aws.Xml.member "SourceRegion" xml) String.parse)
    ; destination_outpost_arn =
        Aws.Util.option_bind (Aws.Xml.member "DestinationOutpostArn" xml) String.parse
    ; copy_image_tags =
        Aws.Util.option_bind (Aws.Xml.member "CopyImageTags" xml) Boolean.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; snapshot_copy_completion_duration_minutes =
        Aws.Util.option_bind
          (Aws.Xml.member "SnapshotCopyCompletionDurationMinutes" xml)
          Long.parse
    ; destination_availability_zone =
        Aws.Util.option_bind
          (Aws.Xml.member "DestinationAvailabilityZone" xml)
          String.parse
    ; destination_availability_zone_id =
        Aws.Util.option_bind
          (Aws.Xml.member "DestinationAvailabilityZoneId" xml)
          String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.destination_availability_zone_id (fun f ->
             Aws.Query.Pair ("DestinationAvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.destination_availability_zone (fun f ->
             Aws.Query.Pair ("DestinationAvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.snapshot_copy_completion_duration_minutes (fun f ->
             Aws.Query.Pair ("SnapshotCopyCompletionDurationMinutes", Long.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.copy_image_tags (fun f ->
             Aws.Query.Pair ("CopyImageTags", Boolean.to_query f))
       ; Aws.Util.option_map v.destination_outpost_arn (fun f ->
             Aws.Query.Pair ("DestinationOutpostArn", String.to_query f))
       ; Some (Aws.Query.Pair ("SourceRegion", String.to_query v.source_region))
       ; Some (Aws.Query.Pair ("SourceImageId", String.to_query v.source_image_id))
       ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.encrypted (fun f ->
             Aws.Query.Pair ("Encrypted", Boolean.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.destination_availability_zone_id (fun f ->
             "DestinationAvailabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.destination_availability_zone (fun f ->
             "DestinationAvailabilityZone", String.to_json f)
       ; Aws.Util.option_map v.snapshot_copy_completion_duration_minutes (fun f ->
             "SnapshotCopyCompletionDurationMinutes", Long.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.copy_image_tags (fun f ->
             "CopyImageTags", Boolean.to_json f)
       ; Aws.Util.option_map v.destination_outpost_arn (fun f ->
             "DestinationOutpostArn", String.to_json f)
       ; Some ("SourceRegion", String.to_json v.source_region)
       ; Some ("SourceImageId", String.to_json v.source_image_id)
       ; Some ("Name", String.to_json v.name)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "kmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.encrypted (fun f -> "encrypted", Boolean.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ])

let of_json j =
  { client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; encrypted = Aws.Util.option_map (Aws.Json.lookup j "encrypted") Boolean.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "kmsKeyId") String.of_json
  ; name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
  ; source_image_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceImageId"))
  ; source_region =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceRegion"))
  ; destination_outpost_arn =
      Aws.Util.option_map (Aws.Json.lookup j "DestinationOutpostArn") String.of_json
  ; copy_image_tags =
      Aws.Util.option_map (Aws.Json.lookup j "CopyImageTags") Boolean.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; snapshot_copy_completion_duration_minutes =
      Aws.Util.option_map
        (Aws.Json.lookup j "SnapshotCopyCompletionDurationMinutes")
        Long.of_json
  ; destination_availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "DestinationAvailabilityZone") String.of_json
  ; destination_availability_zone_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "DestinationAvailabilityZoneId")
        String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
