open Aws.BaseTypes

type t =
  { generated_template_id : String.t option
  ; generated_template_name : String.t option
  ; status : GeneratedTemplateStatus.t option
  ; status_reason : String.t option
  ; creation_time : DateTime.t option
  ; last_updated_time : DateTime.t option
  ; number_of_resources : Integer.t option
  }

let make
    ?generated_template_id
    ?generated_template_name
    ?status
    ?status_reason
    ?creation_time
    ?last_updated_time
    ?number_of_resources
    () =
  { generated_template_id
  ; generated_template_name
  ; status
  ; status_reason
  ; creation_time
  ; last_updated_time
  ; number_of_resources
  }

let parse xml =
  Some
    { generated_template_id =
        Aws.Util.option_bind (Aws.Xml.member "GeneratedTemplateId" xml) String.parse
    ; generated_template_name =
        Aws.Util.option_bind (Aws.Xml.member "GeneratedTemplateName" xml) String.parse
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "Status" xml) GeneratedTemplateStatus.parse
    ; status_reason =
        Aws.Util.option_bind (Aws.Xml.member "StatusReason" xml) String.parse
    ; creation_time =
        Aws.Util.option_bind (Aws.Xml.member "CreationTime" xml) DateTime.parse
    ; last_updated_time =
        Aws.Util.option_bind (Aws.Xml.member "LastUpdatedTime" xml) DateTime.parse
    ; number_of_resources =
        Aws.Util.option_bind (Aws.Xml.member "NumberOfResources" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.number_of_resources (fun f ->
             Aws.Query.Pair ("NumberOfResources", Integer.to_query f))
       ; Aws.Util.option_map v.last_updated_time (fun f ->
             Aws.Query.Pair ("LastUpdatedTime", DateTime.to_query f))
       ; Aws.Util.option_map v.creation_time (fun f ->
             Aws.Query.Pair ("CreationTime", DateTime.to_query f))
       ; Aws.Util.option_map v.status_reason (fun f ->
             Aws.Query.Pair ("StatusReason", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", GeneratedTemplateStatus.to_query f))
       ; Aws.Util.option_map v.generated_template_name (fun f ->
             Aws.Query.Pair ("GeneratedTemplateName", String.to_query f))
       ; Aws.Util.option_map v.generated_template_id (fun f ->
             Aws.Query.Pair ("GeneratedTemplateId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.number_of_resources (fun f ->
             "NumberOfResources", Integer.to_json f)
       ; Aws.Util.option_map v.last_updated_time (fun f ->
             "LastUpdatedTime", DateTime.to_json f)
       ; Aws.Util.option_map v.creation_time (fun f -> "CreationTime", DateTime.to_json f)
       ; Aws.Util.option_map v.status_reason (fun f -> "StatusReason", String.to_json f)
       ; Aws.Util.option_map v.status (fun f ->
             "Status", GeneratedTemplateStatus.to_json f)
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
  ; status =
      Aws.Util.option_map (Aws.Json.lookup j "Status") GeneratedTemplateStatus.of_json
  ; status_reason = Aws.Util.option_map (Aws.Json.lookup j "StatusReason") String.of_json
  ; creation_time =
      Aws.Util.option_map (Aws.Json.lookup j "CreationTime") DateTime.of_json
  ; last_updated_time =
      Aws.Util.option_map (Aws.Json.lookup j "LastUpdatedTime") DateTime.of_json
  ; number_of_resources =
      Aws.Util.option_map (Aws.Json.lookup j "NumberOfResources") Integer.of_json
  }
