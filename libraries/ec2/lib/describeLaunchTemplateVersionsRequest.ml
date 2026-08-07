open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; launch_template_id : String.t option
  ; launch_template_name : String.t option
  ; versions : VersionStringList.t
  ; min_version : String.t option
  ; max_version : String.t option
  ; next_token : String.t option
  ; max_results : Integer.t option
  ; filters : FilterList.t
  ; resolve_alias : Boolean.t option
  ; include_managed_resources : Boolean.t option
  }

let make
    ?dry_run
    ?launch_template_id
    ?launch_template_name
    ?(versions = [])
    ?min_version
    ?max_version
    ?next_token
    ?max_results
    ?(filters = [])
    ?resolve_alias
    ?include_managed_resources
    () =
  { dry_run
  ; launch_template_id
  ; launch_template_name
  ; versions
  ; min_version
  ; max_version
  ; next_token
  ; max_results
  ; filters
  ; resolve_alias
  ; include_managed_resources
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; launch_template_id =
        Aws.Util.option_bind (Aws.Xml.member "LaunchTemplateId" xml) String.parse
    ; launch_template_name =
        Aws.Util.option_bind (Aws.Xml.member "LaunchTemplateName" xml) String.parse
    ; versions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LaunchTemplateVersion" xml)
             VersionStringList.parse)
    ; min_version = Aws.Util.option_bind (Aws.Xml.member "MinVersion" xml) String.parse
    ; max_version = Aws.Util.option_bind (Aws.Xml.member "MaxVersion" xml) String.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; resolve_alias =
        Aws.Util.option_bind (Aws.Xml.member "ResolveAlias" xml) Boolean.parse
    ; include_managed_resources =
        Aws.Util.option_bind (Aws.Xml.member "IncludeManagedResources" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.include_managed_resources (fun f ->
             Aws.Query.Pair ("IncludeManagedResources", Boolean.to_query f))
       ; Aws.Util.option_map v.resolve_alias (fun f ->
             Aws.Query.Pair ("ResolveAlias", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_version (fun f ->
             Aws.Query.Pair ("MaxVersion", String.to_query f))
       ; Aws.Util.option_map v.min_version (fun f ->
             Aws.Query.Pair ("MinVersion", String.to_query f))
       ; Some
           (Aws.Query.Pair ("LaunchTemplateVersion", VersionStringList.to_query v.versions))
       ; Aws.Util.option_map v.launch_template_name (fun f ->
             Aws.Query.Pair ("LaunchTemplateName", String.to_query f))
       ; Aws.Util.option_map v.launch_template_id (fun f ->
             Aws.Query.Pair ("LaunchTemplateId", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.include_managed_resources (fun f ->
             "IncludeManagedResources", Boolean.to_json f)
       ; Aws.Util.option_map v.resolve_alias (fun f -> "ResolveAlias", Boolean.to_json f)
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_version (fun f -> "MaxVersion", String.to_json f)
       ; Aws.Util.option_map v.min_version (fun f -> "MinVersion", String.to_json f)
       ; Some ("LaunchTemplateVersion", VersionStringList.to_json v.versions)
       ; Aws.Util.option_map v.launch_template_name (fun f ->
             "LaunchTemplateName", String.to_json f)
       ; Aws.Util.option_map v.launch_template_id (fun f ->
             "LaunchTemplateId", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; launch_template_id =
      Aws.Util.option_map (Aws.Json.lookup j "LaunchTemplateId") String.of_json
  ; launch_template_name =
      Aws.Util.option_map (Aws.Json.lookup j "LaunchTemplateName") String.of_json
  ; versions =
      VersionStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LaunchTemplateVersion"))
  ; min_version = Aws.Util.option_map (Aws.Json.lookup j "MinVersion") String.of_json
  ; max_version = Aws.Util.option_map (Aws.Json.lookup j "MaxVersion") String.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; resolve_alias = Aws.Util.option_map (Aws.Json.lookup j "ResolveAlias") Boolean.of_json
  ; include_managed_resources =
      Aws.Util.option_map (Aws.Json.lookup j "IncludeManagedResources") Boolean.of_json
  }
