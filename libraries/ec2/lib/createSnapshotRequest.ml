open Aws.BaseTypes

type t =
  { description : String.t option
  ; outpost_arn : String.t option
  ; volume_id : String.t
  ; tag_specifications : TagSpecificationList.t
  ; location : SnapshotLocationEnum.t option
  ; dry_run : Boolean.t option
  }

let make
    ?description
    ?outpost_arn
    ~volume_id
    ?(tag_specifications = [])
    ?location
    ?dry_run
    () =
  { description; outpost_arn; volume_id; tag_specifications; location; dry_run }

let parse xml =
  Some
    { description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; outpost_arn = Aws.Util.option_bind (Aws.Xml.member "OutpostArn" xml) String.parse
    ; volume_id =
        Aws.Xml.required
          "VolumeId"
          (Aws.Util.option_bind (Aws.Xml.member "VolumeId" xml) String.parse)
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; location =
        Aws.Util.option_bind (Aws.Xml.member "Location" xml) SnapshotLocationEnum.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.location (fun f ->
             Aws.Query.Pair ("Location", SnapshotLocationEnum.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Some (Aws.Query.Pair ("VolumeId", String.to_query v.volume_id))
       ; Aws.Util.option_map v.outpost_arn (fun f ->
             Aws.Query.Pair ("OutpostArn", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.location (fun f ->
             "Location", SnapshotLocationEnum.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Some ("VolumeId", String.to_json v.volume_id)
       ; Aws.Util.option_map v.outpost_arn (fun f -> "OutpostArn", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ])

let of_json j =
  { description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; outpost_arn = Aws.Util.option_map (Aws.Json.lookup j "OutpostArn") String.of_json
  ; volume_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VolumeId"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; location =
      Aws.Util.option_map (Aws.Json.lookup j "Location") SnapshotLocationEnum.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
