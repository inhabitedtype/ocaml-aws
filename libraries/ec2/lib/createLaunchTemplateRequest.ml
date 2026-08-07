open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; client_token : String.t option
  ; launch_template_name : String.t
  ; version_description : String.t option
  ; launch_template_data : RequestLaunchTemplateData.t
  ; operator : OperatorRequest.t option
  ; tag_specifications : TagSpecificationList.t
  }

let make
    ?dry_run
    ?client_token
    ~launch_template_name
    ?version_description
    ~launch_template_data
    ?operator
    ?(tag_specifications = [])
    () =
  { dry_run
  ; client_token
  ; launch_template_name
  ; version_description
  ; launch_template_data
  ; operator
  ; tag_specifications
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; launch_template_name =
        Aws.Xml.required
          "LaunchTemplateName"
          (Aws.Util.option_bind (Aws.Xml.member "LaunchTemplateName" xml) String.parse)
    ; version_description =
        Aws.Util.option_bind (Aws.Xml.member "VersionDescription" xml) String.parse
    ; launch_template_data =
        Aws.Xml.required
          "LaunchTemplateData"
          (Aws.Util.option_bind
             (Aws.Xml.member "LaunchTemplateData" xml)
             RequestLaunchTemplateData.parse)
    ; operator =
        Aws.Util.option_bind (Aws.Xml.member "Operator" xml) OperatorRequest.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.operator (fun f ->
             Aws.Query.Pair ("Operator", OperatorRequest.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LaunchTemplateData"
              , RequestLaunchTemplateData.to_query v.launch_template_data ))
       ; Aws.Util.option_map v.version_description (fun f ->
             Aws.Query.Pair ("VersionDescription", String.to_query f))
       ; Some
           (Aws.Query.Pair ("LaunchTemplateName", String.to_query v.launch_template_name))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.operator (fun f -> "Operator", OperatorRequest.to_json f)
       ; Some
           ("LaunchTemplateData", RequestLaunchTemplateData.to_json v.launch_template_data)
       ; Aws.Util.option_map v.version_description (fun f ->
             "VersionDescription", String.to_json f)
       ; Some ("LaunchTemplateName", String.to_json v.launch_template_name)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; launch_template_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LaunchTemplateName"))
  ; version_description =
      Aws.Util.option_map (Aws.Json.lookup j "VersionDescription") String.of_json
  ; launch_template_data =
      RequestLaunchTemplateData.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LaunchTemplateData"))
  ; operator = Aws.Util.option_map (Aws.Json.lookup j "Operator") OperatorRequest.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  }
