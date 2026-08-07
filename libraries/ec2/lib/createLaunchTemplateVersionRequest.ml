open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; client_token : String.t option
  ; launch_template_id : String.t option
  ; launch_template_name : String.t option
  ; source_version : String.t option
  ; version_description : String.t option
  ; launch_template_data : RequestLaunchTemplateData.t
  ; resolve_alias : Boolean.t option
  }

let make
    ?dry_run
    ?client_token
    ?launch_template_id
    ?launch_template_name
    ?source_version
    ?version_description
    ~launch_template_data
    ?resolve_alias
    () =
  { dry_run
  ; client_token
  ; launch_template_id
  ; launch_template_name
  ; source_version
  ; version_description
  ; launch_template_data
  ; resolve_alias
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; launch_template_id =
        Aws.Util.option_bind (Aws.Xml.member "LaunchTemplateId" xml) String.parse
    ; launch_template_name =
        Aws.Util.option_bind (Aws.Xml.member "LaunchTemplateName" xml) String.parse
    ; source_version =
        Aws.Util.option_bind (Aws.Xml.member "SourceVersion" xml) String.parse
    ; version_description =
        Aws.Util.option_bind (Aws.Xml.member "VersionDescription" xml) String.parse
    ; launch_template_data =
        Aws.Xml.required
          "LaunchTemplateData"
          (Aws.Util.option_bind
             (Aws.Xml.member "LaunchTemplateData" xml)
             RequestLaunchTemplateData.parse)
    ; resolve_alias =
        Aws.Util.option_bind (Aws.Xml.member "ResolveAlias" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resolve_alias (fun f ->
             Aws.Query.Pair ("ResolveAlias", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LaunchTemplateData"
              , RequestLaunchTemplateData.to_query v.launch_template_data ))
       ; Aws.Util.option_map v.version_description (fun f ->
             Aws.Query.Pair ("VersionDescription", String.to_query f))
       ; Aws.Util.option_map v.source_version (fun f ->
             Aws.Query.Pair ("SourceVersion", String.to_query f))
       ; Aws.Util.option_map v.launch_template_name (fun f ->
             Aws.Query.Pair ("LaunchTemplateName", String.to_query f))
       ; Aws.Util.option_map v.launch_template_id (fun f ->
             Aws.Query.Pair ("LaunchTemplateId", String.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resolve_alias (fun f -> "ResolveAlias", Boolean.to_json f)
       ; Some
           ("LaunchTemplateData", RequestLaunchTemplateData.to_json v.launch_template_data)
       ; Aws.Util.option_map v.version_description (fun f ->
             "VersionDescription", String.to_json f)
       ; Aws.Util.option_map v.source_version (fun f -> "SourceVersion", String.to_json f)
       ; Aws.Util.option_map v.launch_template_name (fun f ->
             "LaunchTemplateName", String.to_json f)
       ; Aws.Util.option_map v.launch_template_id (fun f ->
             "LaunchTemplateId", String.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; launch_template_id =
      Aws.Util.option_map (Aws.Json.lookup j "LaunchTemplateId") String.of_json
  ; launch_template_name =
      Aws.Util.option_map (Aws.Json.lookup j "LaunchTemplateName") String.of_json
  ; source_version =
      Aws.Util.option_map (Aws.Json.lookup j "SourceVersion") String.of_json
  ; version_description =
      Aws.Util.option_map (Aws.Json.lookup j "VersionDescription") String.of_json
  ; launch_template_data =
      RequestLaunchTemplateData.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LaunchTemplateData"))
  ; resolve_alias = Aws.Util.option_map (Aws.Json.lookup j "ResolveAlias") Boolean.of_json
  }
