open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; launch_template_ids : LaunchTemplateIdStringList.t
  ; launch_template_names : LaunchTemplateNameStringList.t
  ; filters : FilterList.t
  ; next_token : String.t option
  ; max_results : Integer.t option
  ; include_managed_resources : Boolean.t option
  }

let make
    ?dry_run
    ?(launch_template_ids = [])
    ?(launch_template_names = [])
    ?(filters = [])
    ?next_token
    ?max_results
    ?include_managed_resources
    () =
  { dry_run
  ; launch_template_ids
  ; launch_template_names
  ; filters
  ; next_token
  ; max_results
  ; include_managed_resources
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; launch_template_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LaunchTemplateId" xml)
             LaunchTemplateIdStringList.parse)
    ; launch_template_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LaunchTemplateName" xml)
             LaunchTemplateNameStringList.parse)
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; include_managed_resources =
        Aws.Util.option_bind (Aws.Xml.member "IncludeManagedResources" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.include_managed_resources (fun f ->
             Aws.Query.Pair ("IncludeManagedResources", Boolean.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Some
           (Aws.Query.Pair
              ( "LaunchTemplateName"
              , LaunchTemplateNameStringList.to_query v.launch_template_names ))
       ; Some
           (Aws.Query.Pair
              ( "LaunchTemplateId"
              , LaunchTemplateIdStringList.to_query v.launch_template_ids ))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.include_managed_resources (fun f ->
             "IncludeManagedResources", Boolean.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Some
           ( "LaunchTemplateName"
           , LaunchTemplateNameStringList.to_json v.launch_template_names )
       ; Some
           ("LaunchTemplateId", LaunchTemplateIdStringList.to_json v.launch_template_ids)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; launch_template_ids =
      LaunchTemplateIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LaunchTemplateId"))
  ; launch_template_names =
      LaunchTemplateNameStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LaunchTemplateName"))
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; include_managed_resources =
      Aws.Util.option_map (Aws.Json.lookup j "IncludeManagedResources") Boolean.of_json
  }
