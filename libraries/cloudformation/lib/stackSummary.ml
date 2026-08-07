open Aws.BaseTypes

type t =
  { stack_id : String.t option
  ; stack_name : String.t
  ; template_description : String.t option
  ; creation_time : DateTime.t
  ; last_updated_time : DateTime.t option
  ; deletion_time : DateTime.t option
  ; stack_status : StackStatus.t
  ; stack_status_reason : String.t option
  ; parent_id : String.t option
  ; root_id : String.t option
  ; drift_information : StackDriftInformationSummary.t option
  ; last_operations : LastOperations.t
  }

let make
    ?stack_id
    ~stack_name
    ?template_description
    ~creation_time
    ?last_updated_time
    ?deletion_time
    ~stack_status
    ?stack_status_reason
    ?parent_id
    ?root_id
    ?drift_information
    ?(last_operations = [])
    () =
  { stack_id
  ; stack_name
  ; template_description
  ; creation_time
  ; last_updated_time
  ; deletion_time
  ; stack_status
  ; stack_status_reason
  ; parent_id
  ; root_id
  ; drift_information
  ; last_operations
  }

let parse xml =
  Some
    { stack_id = Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse
    ; stack_name =
        Aws.Xml.required
          "StackName"
          (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
    ; template_description =
        Aws.Util.option_bind (Aws.Xml.member "TemplateDescription" xml) String.parse
    ; creation_time =
        Aws.Xml.required
          "CreationTime"
          (Aws.Util.option_bind (Aws.Xml.member "CreationTime" xml) DateTime.parse)
    ; last_updated_time =
        Aws.Util.option_bind (Aws.Xml.member "LastUpdatedTime" xml) DateTime.parse
    ; deletion_time =
        Aws.Util.option_bind (Aws.Xml.member "DeletionTime" xml) DateTime.parse
    ; stack_status =
        Aws.Xml.required
          "StackStatus"
          (Aws.Util.option_bind (Aws.Xml.member "StackStatus" xml) StackStatus.parse)
    ; stack_status_reason =
        Aws.Util.option_bind (Aws.Xml.member "StackStatusReason" xml) String.parse
    ; parent_id = Aws.Util.option_bind (Aws.Xml.member "ParentId" xml) String.parse
    ; root_id = Aws.Util.option_bind (Aws.Xml.member "RootId" xml) String.parse
    ; drift_information =
        Aws.Util.option_bind
          (Aws.Xml.member "DriftInformation" xml)
          StackDriftInformationSummary.parse
    ; last_operations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LastOperations" xml)
             LastOperations.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("LastOperations.member", LastOperations.to_query v.last_operations))
       ; Aws.Util.option_map v.drift_information (fun f ->
             Aws.Query.Pair ("DriftInformation", StackDriftInformationSummary.to_query f))
       ; Aws.Util.option_map v.root_id (fun f ->
             Aws.Query.Pair ("RootId", String.to_query f))
       ; Aws.Util.option_map v.parent_id (fun f ->
             Aws.Query.Pair ("ParentId", String.to_query f))
       ; Aws.Util.option_map v.stack_status_reason (fun f ->
             Aws.Query.Pair ("StackStatusReason", String.to_query f))
       ; Some (Aws.Query.Pair ("StackStatus", StackStatus.to_query v.stack_status))
       ; Aws.Util.option_map v.deletion_time (fun f ->
             Aws.Query.Pair ("DeletionTime", DateTime.to_query f))
       ; Aws.Util.option_map v.last_updated_time (fun f ->
             Aws.Query.Pair ("LastUpdatedTime", DateTime.to_query f))
       ; Some (Aws.Query.Pair ("CreationTime", DateTime.to_query v.creation_time))
       ; Aws.Util.option_map v.template_description (fun f ->
             Aws.Query.Pair ("TemplateDescription", String.to_query f))
       ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
       ; Aws.Util.option_map v.stack_id (fun f ->
             Aws.Query.Pair ("StackId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("LastOperations", LastOperations.to_json v.last_operations)
       ; Aws.Util.option_map v.drift_information (fun f ->
             "DriftInformation", StackDriftInformationSummary.to_json f)
       ; Aws.Util.option_map v.root_id (fun f -> "RootId", String.to_json f)
       ; Aws.Util.option_map v.parent_id (fun f -> "ParentId", String.to_json f)
       ; Aws.Util.option_map v.stack_status_reason (fun f ->
             "StackStatusReason", String.to_json f)
       ; Some ("StackStatus", StackStatus.to_json v.stack_status)
       ; Aws.Util.option_map v.deletion_time (fun f -> "DeletionTime", DateTime.to_json f)
       ; Aws.Util.option_map v.last_updated_time (fun f ->
             "LastUpdatedTime", DateTime.to_json f)
       ; Some ("CreationTime", DateTime.to_json v.creation_time)
       ; Aws.Util.option_map v.template_description (fun f ->
             "TemplateDescription", String.to_json f)
       ; Some ("StackName", String.to_json v.stack_name)
       ; Aws.Util.option_map v.stack_id (fun f -> "StackId", String.to_json f)
       ])

let of_json j =
  { stack_id = Aws.Util.option_map (Aws.Json.lookup j "StackId") String.of_json
  ; stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
  ; template_description =
      Aws.Util.option_map (Aws.Json.lookup j "TemplateDescription") String.of_json
  ; creation_time =
      DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CreationTime"))
  ; last_updated_time =
      Aws.Util.option_map (Aws.Json.lookup j "LastUpdatedTime") DateTime.of_json
  ; deletion_time =
      Aws.Util.option_map (Aws.Json.lookup j "DeletionTime") DateTime.of_json
  ; stack_status =
      StackStatus.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackStatus"))
  ; stack_status_reason =
      Aws.Util.option_map (Aws.Json.lookup j "StackStatusReason") String.of_json
  ; parent_id = Aws.Util.option_map (Aws.Json.lookup j "ParentId") String.of_json
  ; root_id = Aws.Util.option_map (Aws.Json.lookup j "RootId") String.of_json
  ; drift_information =
      Aws.Util.option_map
        (Aws.Json.lookup j "DriftInformation")
        StackDriftInformationSummary.of_json
  ; last_operations =
      LastOperations.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LastOperations"))
  }
