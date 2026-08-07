open Aws.BaseTypes

type t =
  { description : String.t option
  ; instance_specification : InstanceSpecification.t
  ; outpost_arn : String.t option
  ; tag_specifications : TagSpecificationList.t
  ; dry_run : Boolean.t option
  ; copy_tags_from_source : CopyTagsFromSource.t option
  ; location : SnapshotLocationEnum.t option
  }

let make
    ?description
    ~instance_specification
    ?outpost_arn
    ?(tag_specifications = [])
    ?dry_run
    ?copy_tags_from_source
    ?location
    () =
  { description
  ; instance_specification
  ; outpost_arn
  ; tag_specifications
  ; dry_run
  ; copy_tags_from_source
  ; location
  }

let parse xml =
  Some
    { description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; instance_specification =
        Aws.Xml.required
          "InstanceSpecification"
          (Aws.Util.option_bind
             (Aws.Xml.member "InstanceSpecification" xml)
             InstanceSpecification.parse)
    ; outpost_arn = Aws.Util.option_bind (Aws.Xml.member "OutpostArn" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; copy_tags_from_source =
        Aws.Util.option_bind
          (Aws.Xml.member "CopyTagsFromSource" xml)
          CopyTagsFromSource.parse
    ; location =
        Aws.Util.option_bind (Aws.Xml.member "Location" xml) SnapshotLocationEnum.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.location (fun f ->
             Aws.Query.Pair ("Location", SnapshotLocationEnum.to_query f))
       ; Aws.Util.option_map v.copy_tags_from_source (fun f ->
             Aws.Query.Pair ("CopyTagsFromSource", CopyTagsFromSource.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.outpost_arn (fun f ->
             Aws.Query.Pair ("OutpostArn", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "InstanceSpecification"
              , InstanceSpecification.to_query v.instance_specification ))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.location (fun f ->
             "Location", SnapshotLocationEnum.to_json f)
       ; Aws.Util.option_map v.copy_tags_from_source (fun f ->
             "CopyTagsFromSource", CopyTagsFromSource.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.outpost_arn (fun f -> "OutpostArn", String.to_json f)
       ; Some
           ( "InstanceSpecification"
           , InstanceSpecification.to_json v.instance_specification )
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ])

let of_json j =
  { description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; instance_specification =
      InstanceSpecification.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceSpecification"))
  ; outpost_arn = Aws.Util.option_map (Aws.Json.lookup j "OutpostArn") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; copy_tags_from_source =
      Aws.Util.option_map
        (Aws.Json.lookup j "CopyTagsFromSource")
        CopyTagsFromSource.of_json
  ; location =
      Aws.Util.option_map (Aws.Json.lookup j "Location") SnapshotLocationEnum.of_json
  }
