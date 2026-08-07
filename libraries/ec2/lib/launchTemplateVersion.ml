open Aws.BaseTypes

type t =
  { launch_template_id : String.t option
  ; launch_template_name : String.t option
  ; version_number : Long.t option
  ; version_description : String.t option
  ; create_time : DateTime.t option
  ; created_by : String.t option
  ; default_version : Boolean.t option
  ; launch_template_data : ResponseLaunchTemplateData.t option
  ; operator : OperatorResponse.t option
  }

let make
    ?launch_template_id
    ?launch_template_name
    ?version_number
    ?version_description
    ?create_time
    ?created_by
    ?default_version
    ?launch_template_data
    ?operator
    () =
  { launch_template_id
  ; launch_template_name
  ; version_number
  ; version_description
  ; create_time
  ; created_by
  ; default_version
  ; launch_template_data
  ; operator
  }

let parse xml =
  Some
    { launch_template_id =
        Aws.Util.option_bind (Aws.Xml.member "launchTemplateId" xml) String.parse
    ; launch_template_name =
        Aws.Util.option_bind (Aws.Xml.member "launchTemplateName" xml) String.parse
    ; version_number =
        Aws.Util.option_bind (Aws.Xml.member "versionNumber" xml) Long.parse
    ; version_description =
        Aws.Util.option_bind (Aws.Xml.member "versionDescription" xml) String.parse
    ; create_time = Aws.Util.option_bind (Aws.Xml.member "createTime" xml) DateTime.parse
    ; created_by = Aws.Util.option_bind (Aws.Xml.member "createdBy" xml) String.parse
    ; default_version =
        Aws.Util.option_bind (Aws.Xml.member "defaultVersion" xml) Boolean.parse
    ; launch_template_data =
        Aws.Util.option_bind
          (Aws.Xml.member "launchTemplateData" xml)
          ResponseLaunchTemplateData.parse
    ; operator =
        Aws.Util.option_bind (Aws.Xml.member "operator" xml) OperatorResponse.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.operator (fun f ->
             Aws.Query.Pair ("Operator", OperatorResponse.to_query f))
       ; Aws.Util.option_map v.launch_template_data (fun f ->
             Aws.Query.Pair ("LaunchTemplateData", ResponseLaunchTemplateData.to_query f))
       ; Aws.Util.option_map v.default_version (fun f ->
             Aws.Query.Pair ("DefaultVersion", Boolean.to_query f))
       ; Aws.Util.option_map v.created_by (fun f ->
             Aws.Query.Pair ("CreatedBy", String.to_query f))
       ; Aws.Util.option_map v.create_time (fun f ->
             Aws.Query.Pair ("CreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.version_description (fun f ->
             Aws.Query.Pair ("VersionDescription", String.to_query f))
       ; Aws.Util.option_map v.version_number (fun f ->
             Aws.Query.Pair ("VersionNumber", Long.to_query f))
       ; Aws.Util.option_map v.launch_template_name (fun f ->
             Aws.Query.Pair ("LaunchTemplateName", String.to_query f))
       ; Aws.Util.option_map v.launch_template_id (fun f ->
             Aws.Query.Pair ("LaunchTemplateId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.operator (fun f -> "operator", OperatorResponse.to_json f)
       ; Aws.Util.option_map v.launch_template_data (fun f ->
             "launchTemplateData", ResponseLaunchTemplateData.to_json f)
       ; Aws.Util.option_map v.default_version (fun f ->
             "defaultVersion", Boolean.to_json f)
       ; Aws.Util.option_map v.created_by (fun f -> "createdBy", String.to_json f)
       ; Aws.Util.option_map v.create_time (fun f -> "createTime", DateTime.to_json f)
       ; Aws.Util.option_map v.version_description (fun f ->
             "versionDescription", String.to_json f)
       ; Aws.Util.option_map v.version_number (fun f -> "versionNumber", Long.to_json f)
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
  ; version_number = Aws.Util.option_map (Aws.Json.lookup j "versionNumber") Long.of_json
  ; version_description =
      Aws.Util.option_map (Aws.Json.lookup j "versionDescription") String.of_json
  ; create_time = Aws.Util.option_map (Aws.Json.lookup j "createTime") DateTime.of_json
  ; created_by = Aws.Util.option_map (Aws.Json.lookup j "createdBy") String.of_json
  ; default_version =
      Aws.Util.option_map (Aws.Json.lookup j "defaultVersion") Boolean.of_json
  ; launch_template_data =
      Aws.Util.option_map
        (Aws.Json.lookup j "launchTemplateData")
        ResponseLaunchTemplateData.of_json
  ; operator = Aws.Util.option_map (Aws.Json.lookup j "operator") OperatorResponse.of_json
  }
