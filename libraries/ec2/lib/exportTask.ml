open Aws.BaseTypes

type t =
  { description : String.t
  ; export_task_id : String.t
  ; export_to_s3_task : ExportToS3Task.t
  ; instance_export_details : InstanceExportDetails.t
  ; state : ExportTaskState.t
  ; status_message : String.t
  ; tags : TagList.t
  }

let make
    ~description
    ~export_task_id
    ~export_to_s3_task
    ~instance_export_details
    ~state
    ~status_message
    ?(tags = [])
    () =
  { description
  ; export_task_id
  ; export_to_s3_task
  ; instance_export_details
  ; state
  ; status_message
  ; tags
  }

let parse xml =
  Some
    { description =
        Aws.Xml.required
          "description"
          (Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse)
    ; export_task_id =
        Aws.Xml.required
          "exportTaskId"
          (Aws.Util.option_bind (Aws.Xml.member "exportTaskId" xml) String.parse)
    ; export_to_s3_task =
        Aws.Xml.required
          "exportToS3"
          (Aws.Util.option_bind (Aws.Xml.member "exportToS3" xml) ExportToS3Task.parse)
    ; instance_export_details =
        Aws.Xml.required
          "instanceExport"
          (Aws.Util.option_bind
             (Aws.Xml.member "instanceExport" xml)
             InstanceExportDetails.parse)
    ; state =
        Aws.Xml.required
          "state"
          (Aws.Util.option_bind (Aws.Xml.member "state" xml) ExportTaskState.parse)
    ; status_message =
        Aws.Xml.required
          "statusMessage"
          (Aws.Util.option_bind (Aws.Xml.member "statusMessage" xml) String.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Some (Aws.Query.Pair ("StatusMessage", String.to_query v.status_message))
       ; Some (Aws.Query.Pair ("State", ExportTaskState.to_query v.state))
       ; Some
           (Aws.Query.Pair
              ("InstanceExport", InstanceExportDetails.to_query v.instance_export_details))
       ; Some (Aws.Query.Pair ("ExportToS3", ExportToS3Task.to_query v.export_to_s3_task))
       ; Some (Aws.Query.Pair ("ExportTaskId", String.to_query v.export_task_id))
       ; Some (Aws.Query.Pair ("Description", String.to_query v.description))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Some ("statusMessage", String.to_json v.status_message)
       ; Some ("state", ExportTaskState.to_json v.state)
       ; Some ("instanceExport", InstanceExportDetails.to_json v.instance_export_details)
       ; Some ("exportToS3", ExportToS3Task.to_json v.export_to_s3_task)
       ; Some ("exportTaskId", String.to_json v.export_task_id)
       ; Some ("description", String.to_json v.description)
       ])

let of_json j =
  { description =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "description"))
  ; export_task_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "exportTaskId"))
  ; export_to_s3_task =
      ExportToS3Task.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "exportToS3"))
  ; instance_export_details =
      InstanceExportDetails.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceExport"))
  ; state = ExportTaskState.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "state"))
  ; status_message =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "statusMessage"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
