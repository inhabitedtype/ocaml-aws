open Aws.BaseTypes

type t =
  { launch_template_id : String.t option
  ; launch_template_name : String.t option
  ; create_time : DateTime.t option
  ; created_by : String.t option
  ; default_version_number : Long.t option
  ; latest_version_number : Long.t option
  ; tags : TagList.t
  ; operator : OperatorResponse.t option
  }

let make
    ?launch_template_id
    ?launch_template_name
    ?create_time
    ?created_by
    ?default_version_number
    ?latest_version_number
    ?(tags = [])
    ?operator
    () =
  { launch_template_id
  ; launch_template_name
  ; create_time
  ; created_by
  ; default_version_number
  ; latest_version_number
  ; tags
  ; operator
  }

let parse xml =
  Some
    { launch_template_id =
        Aws.Util.option_bind (Aws.Xml.member "launchTemplateId" xml) String.parse
    ; launch_template_name =
        Aws.Util.option_bind (Aws.Xml.member "launchTemplateName" xml) String.parse
    ; create_time = Aws.Util.option_bind (Aws.Xml.member "createTime" xml) DateTime.parse
    ; created_by = Aws.Util.option_bind (Aws.Xml.member "createdBy" xml) String.parse
    ; default_version_number =
        Aws.Util.option_bind (Aws.Xml.member "defaultVersionNumber" xml) Long.parse
    ; latest_version_number =
        Aws.Util.option_bind (Aws.Xml.member "latestVersionNumber" xml) Long.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; operator =
        Aws.Util.option_bind (Aws.Xml.member "operator" xml) OperatorResponse.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.operator (fun f ->
             Aws.Query.Pair ("Operator", OperatorResponse.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.latest_version_number (fun f ->
             Aws.Query.Pair ("LatestVersionNumber", Long.to_query f))
       ; Aws.Util.option_map v.default_version_number (fun f ->
             Aws.Query.Pair ("DefaultVersionNumber", Long.to_query f))
       ; Aws.Util.option_map v.created_by (fun f ->
             Aws.Query.Pair ("CreatedBy", String.to_query f))
       ; Aws.Util.option_map v.create_time (fun f ->
             Aws.Query.Pair ("CreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.launch_template_name (fun f ->
             Aws.Query.Pair ("LaunchTemplateName", String.to_query f))
       ; Aws.Util.option_map v.launch_template_id (fun f ->
             Aws.Query.Pair ("LaunchTemplateId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.operator (fun f -> "operator", OperatorResponse.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.latest_version_number (fun f ->
             "latestVersionNumber", Long.to_json f)
       ; Aws.Util.option_map v.default_version_number (fun f ->
             "defaultVersionNumber", Long.to_json f)
       ; Aws.Util.option_map v.created_by (fun f -> "createdBy", String.to_json f)
       ; Aws.Util.option_map v.create_time (fun f -> "createTime", DateTime.to_json f)
       ; Aws.Util.option_map v.launch_template_name (fun f ->
             "launchTemplateName", String.to_json f)
       ; Aws.Util.option_map v.launch_template_id (fun f ->
             "launchTemplateId", String.to_json f)
       ])

let of_json j =
  { launch_template_id =
      Aws.Util.option_map (Aws.Json.lookup j "launchTemplateId") String.of_json
  ; launch_template_name =
      Aws.Util.option_map (Aws.Json.lookup j "launchTemplateName") String.of_json
  ; create_time = Aws.Util.option_map (Aws.Json.lookup j "createTime") DateTime.of_json
  ; created_by = Aws.Util.option_map (Aws.Json.lookup j "createdBy") String.of_json
  ; default_version_number =
      Aws.Util.option_map (Aws.Json.lookup j "defaultVersionNumber") Long.of_json
  ; latest_version_number =
      Aws.Util.option_map (Aws.Json.lookup j "latestVersionNumber") Long.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; operator = Aws.Util.option_map (Aws.Json.lookup j "operator") OperatorResponse.of_json
  }
