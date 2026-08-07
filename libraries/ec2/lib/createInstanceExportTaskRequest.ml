open Aws.BaseTypes

type t =
  { tag_specifications : TagSpecificationList.t
  ; description : String.t option
  ; instance_id : String.t
  ; target_environment : ExportEnvironment.t
  ; export_to_s3_task : ExportToS3TaskSpecification.t
  }

let make
    ?(tag_specifications = [])
    ?description
    ~instance_id
    ~target_environment
    ~export_to_s3_task
    () =
  { tag_specifications; description; instance_id; target_environment; export_to_s3_task }

let parse xml =
  Some
    { tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; instance_id =
        Aws.Xml.required
          "instanceId"
          (Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse)
    ; target_environment =
        Aws.Xml.required
          "targetEnvironment"
          (Aws.Util.option_bind
             (Aws.Xml.member "targetEnvironment" xml)
             ExportEnvironment.parse)
    ; export_to_s3_task =
        Aws.Xml.required
          "exportToS3"
          (Aws.Util.option_bind
             (Aws.Xml.member "exportToS3" xml)
             ExportToS3TaskSpecification.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("ExportToS3", ExportToS3TaskSpecification.to_query v.export_to_s3_task))
       ; Some
           (Aws.Query.Pair
              ("TargetEnvironment", ExportEnvironment.to_query v.target_environment))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("exportToS3", ExportToS3TaskSpecification.to_json v.export_to_s3_task)
       ; Some ("targetEnvironment", ExportEnvironment.to_json v.target_environment)
       ; Some ("instanceId", String.to_json v.instance_id)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ])

let of_json j =
  { tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceId"))
  ; target_environment =
      ExportEnvironment.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "targetEnvironment"))
  ; export_to_s3_task =
      ExportToS3TaskSpecification.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "exportToS3"))
  }
