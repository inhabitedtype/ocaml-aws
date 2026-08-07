open Aws.BaseTypes

type t =
  { option_group_name : String.t option
  ; option_group_description : String.t option
  ; engine_name : String.t option
  ; major_engine_version : String.t option
  ; options : OptionsList.t
  ; allows_vpc_and_non_vpc_instance_memberships : Boolean.t option
  ; vpc_id : String.t option
  ; option_group_arn : String.t option
  ; source_option_group : String.t option
  ; source_account_id : String.t option
  ; copy_timestamp : DateTime.t option
  }

let make
    ?option_group_name
    ?option_group_description
    ?engine_name
    ?major_engine_version
    ?(options = [])
    ?allows_vpc_and_non_vpc_instance_memberships
    ?vpc_id
    ?option_group_arn
    ?source_option_group
    ?source_account_id
    ?copy_timestamp
    () =
  { option_group_name
  ; option_group_description
  ; engine_name
  ; major_engine_version
  ; options
  ; allows_vpc_and_non_vpc_instance_memberships
  ; vpc_id
  ; option_group_arn
  ; source_option_group
  ; source_account_id
  ; copy_timestamp
  }

let parse xml =
  Some
    { option_group_name =
        Aws.Util.option_bind (Aws.Xml.member "OptionGroupName" xml) String.parse
    ; option_group_description =
        Aws.Util.option_bind (Aws.Xml.member "OptionGroupDescription" xml) String.parse
    ; engine_name = Aws.Util.option_bind (Aws.Xml.member "EngineName" xml) String.parse
    ; major_engine_version =
        Aws.Util.option_bind (Aws.Xml.member "MajorEngineVersion" xml) String.parse
    ; options =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Options" xml) OptionsList.parse)
    ; allows_vpc_and_non_vpc_instance_memberships =
        Aws.Util.option_bind
          (Aws.Xml.member "AllowsVpcAndNonVpcInstanceMemberships" xml)
          Boolean.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) String.parse
    ; option_group_arn =
        Aws.Util.option_bind (Aws.Xml.member "OptionGroupArn" xml) String.parse
    ; source_option_group =
        Aws.Util.option_bind (Aws.Xml.member "SourceOptionGroup" xml) String.parse
    ; source_account_id =
        Aws.Util.option_bind (Aws.Xml.member "SourceAccountId" xml) String.parse
    ; copy_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "CopyTimestamp" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.copy_timestamp (fun f ->
             Aws.Query.Pair ("CopyTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.source_account_id (fun f ->
             Aws.Query.Pair ("SourceAccountId", String.to_query f))
       ; Aws.Util.option_map v.source_option_group (fun f ->
             Aws.Query.Pair ("SourceOptionGroup", String.to_query f))
       ; Aws.Util.option_map v.option_group_arn (fun f ->
             Aws.Query.Pair ("OptionGroupArn", String.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.allows_vpc_and_non_vpc_instance_memberships (fun f ->
             Aws.Query.Pair ("AllowsVpcAndNonVpcInstanceMemberships", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("Options.member", OptionsList.to_query v.options))
       ; Aws.Util.option_map v.major_engine_version (fun f ->
             Aws.Query.Pair ("MajorEngineVersion", String.to_query f))
       ; Aws.Util.option_map v.engine_name (fun f ->
             Aws.Query.Pair ("EngineName", String.to_query f))
       ; Aws.Util.option_map v.option_group_description (fun f ->
             Aws.Query.Pair ("OptionGroupDescription", String.to_query f))
       ; Aws.Util.option_map v.option_group_name (fun f ->
             Aws.Query.Pair ("OptionGroupName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.copy_timestamp (fun f ->
             "CopyTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.source_account_id (fun f ->
             "SourceAccountId", String.to_json f)
       ; Aws.Util.option_map v.source_option_group (fun f ->
             "SourceOptionGroup", String.to_json f)
       ; Aws.Util.option_map v.option_group_arn (fun f ->
             "OptionGroupArn", String.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "VpcId", String.to_json f)
       ; Aws.Util.option_map v.allows_vpc_and_non_vpc_instance_memberships (fun f ->
             "AllowsVpcAndNonVpcInstanceMemberships", Boolean.to_json f)
       ; Some ("Options", OptionsList.to_json v.options)
       ; Aws.Util.option_map v.major_engine_version (fun f ->
             "MajorEngineVersion", String.to_json f)
       ; Aws.Util.option_map v.engine_name (fun f -> "EngineName", String.to_json f)
       ; Aws.Util.option_map v.option_group_description (fun f ->
             "OptionGroupDescription", String.to_json f)
       ; Aws.Util.option_map v.option_group_name (fun f ->
             "OptionGroupName", String.to_json f)
       ])

let of_json j =
  { option_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "OptionGroupName") String.of_json
  ; option_group_description =
      Aws.Util.option_map (Aws.Json.lookup j "OptionGroupDescription") String.of_json
  ; engine_name = Aws.Util.option_map (Aws.Json.lookup j "EngineName") String.of_json
  ; major_engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "MajorEngineVersion") String.of_json
  ; options = OptionsList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Options"))
  ; allows_vpc_and_non_vpc_instance_memberships =
      Aws.Util.option_map
        (Aws.Json.lookup j "AllowsVpcAndNonVpcInstanceMemberships")
        Boolean.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "VpcId") String.of_json
  ; option_group_arn =
      Aws.Util.option_map (Aws.Json.lookup j "OptionGroupArn") String.of_json
  ; source_option_group =
      Aws.Util.option_map (Aws.Json.lookup j "SourceOptionGroup") String.of_json
  ; source_account_id =
      Aws.Util.option_map (Aws.Json.lookup j "SourceAccountId") String.of_json
  ; copy_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "CopyTimestamp") DateTime.of_json
  }
