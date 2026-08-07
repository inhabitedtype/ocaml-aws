open Aws.BaseTypes

type t =
  { generated_template_id : String.t option
  ; generated_template_name : String.t option
  ; resources : ResourceDetails.t
  ; status : GeneratedTemplateStatus.t option
  ; status_reason : String.t option
  ; creation_time : DateTime.t option
  ; last_updated_time : DateTime.t option
  ; progress : TemplateProgress.t option
  ; stack_id : String.t option
  ; template_configuration : TemplateConfiguration.t option
  ; total_warnings : Integer.t option
  }

let make
    ?generated_template_id
    ?generated_template_name
    ?(resources = [])
    ?status
    ?status_reason
    ?creation_time
    ?last_updated_time
    ?progress
    ?stack_id
    ?template_configuration
    ?total_warnings
    () =
  { generated_template_id
  ; generated_template_name
  ; resources
  ; status
  ; status_reason
  ; creation_time
  ; last_updated_time
  ; progress
  ; stack_id
  ; template_configuration
  ; total_warnings
  }

let parse xml =
  Some
    { generated_template_id =
        Aws.Util.option_bind (Aws.Xml.member "GeneratedTemplateId" xml) String.parse
    ; generated_template_name =
        Aws.Util.option_bind (Aws.Xml.member "GeneratedTemplateName" xml) String.parse
    ; resources =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Resources" xml) ResourceDetails.parse)
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "Status" xml) GeneratedTemplateStatus.parse
    ; status_reason =
        Aws.Util.option_bind (Aws.Xml.member "StatusReason" xml) String.parse
    ; creation_time =
        Aws.Util.option_bind (Aws.Xml.member "CreationTime" xml) DateTime.parse
    ; last_updated_time =
        Aws.Util.option_bind (Aws.Xml.member "LastUpdatedTime" xml) DateTime.parse
    ; progress =
        Aws.Util.option_bind (Aws.Xml.member "Progress" xml) TemplateProgress.parse
    ; stack_id = Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse
    ; template_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "TemplateConfiguration" xml)
          TemplateConfiguration.parse
    ; total_warnings =
        Aws.Util.option_bind (Aws.Xml.member "TotalWarnings" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.total_warnings (fun f ->
             Aws.Query.Pair ("TotalWarnings", Integer.to_query f))
       ; Aws.Util.option_map v.template_configuration (fun f ->
             Aws.Query.Pair ("TemplateConfiguration", TemplateConfiguration.to_query f))
       ; Aws.Util.option_map v.stack_id (fun f ->
             Aws.Query.Pair ("StackId", String.to_query f))
       ; Aws.Util.option_map v.progress (fun f ->
             Aws.Query.Pair ("Progress", TemplateProgress.to_query f))
       ; Aws.Util.option_map v.last_updated_time (fun f ->
             Aws.Query.Pair ("LastUpdatedTime", DateTime.to_query f))
       ; Aws.Util.option_map v.creation_time (fun f ->
             Aws.Query.Pair ("CreationTime", DateTime.to_query f))
       ; Aws.Util.option_map v.status_reason (fun f ->
             Aws.Query.Pair ("StatusReason", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", GeneratedTemplateStatus.to_query f))
       ; Some (Aws.Query.Pair ("Resources.member", ResourceDetails.to_query v.resources))
       ; Aws.Util.option_map v.generated_template_name (fun f ->
             Aws.Query.Pair ("GeneratedTemplateName", String.to_query f))
       ; Aws.Util.option_map v.generated_template_id (fun f ->
             Aws.Query.Pair ("GeneratedTemplateId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.total_warnings (fun f ->
             "TotalWarnings", Integer.to_json f)
       ; Aws.Util.option_map v.template_configuration (fun f ->
             "TemplateConfiguration", TemplateConfiguration.to_json f)
       ; Aws.Util.option_map v.stack_id (fun f -> "StackId", String.to_json f)
       ; Aws.Util.option_map v.progress (fun f -> "Progress", TemplateProgress.to_json f)
       ; Aws.Util.option_map v.last_updated_time (fun f ->
             "LastUpdatedTime", DateTime.to_json f)
       ; Aws.Util.option_map v.creation_time (fun f -> "CreationTime", DateTime.to_json f)
       ; Aws.Util.option_map v.status_reason (fun f -> "StatusReason", String.to_json f)
       ; Aws.Util.option_map v.status (fun f ->
             "Status", GeneratedTemplateStatus.to_json f)
       ; Some ("Resources", ResourceDetails.to_json v.resources)
       ; Aws.Util.option_map v.generated_template_name (fun f ->
             "GeneratedTemplateName", String.to_json f)
       ; Aws.Util.option_map v.generated_template_id (fun f ->
             "GeneratedTemplateId", String.to_json f)
       ])

let of_json j =
  { generated_template_id =
      Aws.Util.option_map (Aws.Json.lookup j "GeneratedTemplateId") String.of_json
  ; generated_template_name =
      Aws.Util.option_map (Aws.Json.lookup j "GeneratedTemplateName") String.of_json
  ; resources =
      ResourceDetails.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Resources"))
  ; status =
      Aws.Util.option_map (Aws.Json.lookup j "Status") GeneratedTemplateStatus.of_json
  ; status_reason = Aws.Util.option_map (Aws.Json.lookup j "StatusReason") String.of_json
  ; creation_time =
      Aws.Util.option_map (Aws.Json.lookup j "CreationTime") DateTime.of_json
  ; last_updated_time =
      Aws.Util.option_map (Aws.Json.lookup j "LastUpdatedTime") DateTime.of_json
  ; progress = Aws.Util.option_map (Aws.Json.lookup j "Progress") TemplateProgress.of_json
  ; stack_id = Aws.Util.option_map (Aws.Json.lookup j "StackId") String.of_json
  ; template_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "TemplateConfiguration")
        TemplateConfiguration.of_json
  ; total_warnings =
      Aws.Util.option_map (Aws.Json.lookup j "TotalWarnings") Integer.of_json
  }
