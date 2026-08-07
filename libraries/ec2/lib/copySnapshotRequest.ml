open Aws.BaseTypes

type t =
  { description : String.t option
  ; destination_outpost_arn : String.t option
  ; destination_region : String.t option
  ; encrypted : Boolean.t option
  ; kms_key_id : String.t option
  ; presigned_url : String.t option
  ; source_region : String.t
  ; source_snapshot_id : String.t
  ; tag_specifications : TagSpecificationList.t
  ; completion_duration_minutes : Integer.t option
  ; destination_availability_zone : String.t option
  ; dry_run : Boolean.t option
  }

let make
    ?description
    ?destination_outpost_arn
    ?destination_region
    ?encrypted
    ?kms_key_id
    ?presigned_url
    ~source_region
    ~source_snapshot_id
    ?(tag_specifications = [])
    ?completion_duration_minutes
    ?destination_availability_zone
    ?dry_run
    () =
  { description
  ; destination_outpost_arn
  ; destination_region
  ; encrypted
  ; kms_key_id
  ; presigned_url
  ; source_region
  ; source_snapshot_id
  ; tag_specifications
  ; completion_duration_minutes
  ; destination_availability_zone
  ; dry_run
  }

let parse xml =
  Some
    { description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; destination_outpost_arn =
        Aws.Util.option_bind (Aws.Xml.member "DestinationOutpostArn" xml) String.parse
    ; destination_region =
        Aws.Util.option_bind (Aws.Xml.member "destinationRegion" xml) String.parse
    ; encrypted = Aws.Util.option_bind (Aws.Xml.member "encrypted" xml) Boolean.parse
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "kmsKeyId" xml) String.parse
    ; presigned_url =
        Aws.Util.option_bind (Aws.Xml.member "presignedUrl" xml) String.parse
    ; source_region =
        Aws.Xml.required
          "SourceRegion"
          (Aws.Util.option_bind (Aws.Xml.member "SourceRegion" xml) String.parse)
    ; source_snapshot_id =
        Aws.Xml.required
          "SourceSnapshotId"
          (Aws.Util.option_bind (Aws.Xml.member "SourceSnapshotId" xml) String.parse)
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; completion_duration_minutes =
        Aws.Util.option_bind
          (Aws.Xml.member "CompletionDurationMinutes" xml)
          Integer.parse
    ; destination_availability_zone =
        Aws.Util.option_bind
          (Aws.Xml.member "DestinationAvailabilityZone" xml)
          String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.destination_availability_zone (fun f ->
             Aws.Query.Pair ("DestinationAvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.completion_duration_minutes (fun f ->
             Aws.Query.Pair ("CompletionDurationMinutes", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Some (Aws.Query.Pair ("SourceSnapshotId", String.to_query v.source_snapshot_id))
       ; Some (Aws.Query.Pair ("SourceRegion", String.to_query v.source_region))
       ; Aws.Util.option_map v.presigned_url (fun f ->
             Aws.Query.Pair ("PresignedUrl", String.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.encrypted (fun f ->
             Aws.Query.Pair ("Encrypted", Boolean.to_query f))
       ; Aws.Util.option_map v.destination_region (fun f ->
             Aws.Query.Pair ("DestinationRegion", String.to_query f))
       ; Aws.Util.option_map v.destination_outpost_arn (fun f ->
             Aws.Query.Pair ("DestinationOutpostArn", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.destination_availability_zone (fun f ->
             "DestinationAvailabilityZone", String.to_json f)
       ; Aws.Util.option_map v.completion_duration_minutes (fun f ->
             "CompletionDurationMinutes", Integer.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Some ("SourceSnapshotId", String.to_json v.source_snapshot_id)
       ; Some ("SourceRegion", String.to_json v.source_region)
       ; Aws.Util.option_map v.presigned_url (fun f -> "presignedUrl", String.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "kmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.encrypted (fun f -> "encrypted", Boolean.to_json f)
       ; Aws.Util.option_map v.destination_region (fun f ->
             "destinationRegion", String.to_json f)
       ; Aws.Util.option_map v.destination_outpost_arn (fun f ->
             "DestinationOutpostArn", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ])

let of_json j =
  { description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; destination_outpost_arn =
      Aws.Util.option_map (Aws.Json.lookup j "DestinationOutpostArn") String.of_json
  ; destination_region =
      Aws.Util.option_map (Aws.Json.lookup j "destinationRegion") String.of_json
  ; encrypted = Aws.Util.option_map (Aws.Json.lookup j "encrypted") Boolean.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "kmsKeyId") String.of_json
  ; presigned_url = Aws.Util.option_map (Aws.Json.lookup j "presignedUrl") String.of_json
  ; source_region =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceRegion"))
  ; source_snapshot_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceSnapshotId"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; completion_duration_minutes =
      Aws.Util.option_map (Aws.Json.lookup j "CompletionDurationMinutes") Integer.of_json
  ; destination_availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "DestinationAvailabilityZone") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
