open Aws.BaseTypes

type t =
  { name : String.t option
  ; description : String.t option
  ; engine_name : String.t option
  ; major_engine_version : String.t option
  ; minimum_required_minor_engine_version : String.t option
  ; port_required : Boolean.t option
  ; default_port : Integer.t option
  ; options_depended_on : OptionsDependedOn.t
  ; options_conflicts_with : OptionsConflictsWith.t
  ; persistent : Boolean.t option
  ; permanent : Boolean.t option
  ; requires_auto_minor_engine_version_upgrade : Boolean.t option
  ; vpc_only : Boolean.t option
  ; supports_option_version_downgrade : Boolean.t option
  ; option_group_option_settings : OptionGroupOptionSettingsList.t
  ; option_group_option_versions : OptionGroupOptionVersionsList.t
  ; copyable_cross_account : Boolean.t option
  }

let make
    ?name
    ?description
    ?engine_name
    ?major_engine_version
    ?minimum_required_minor_engine_version
    ?port_required
    ?default_port
    ?(options_depended_on = [])
    ?(options_conflicts_with = [])
    ?persistent
    ?permanent
    ?requires_auto_minor_engine_version_upgrade
    ?vpc_only
    ?supports_option_version_downgrade
    ?(option_group_option_settings = [])
    ?(option_group_option_versions = [])
    ?copyable_cross_account
    () =
  { name
  ; description
  ; engine_name
  ; major_engine_version
  ; minimum_required_minor_engine_version
  ; port_required
  ; default_port
  ; options_depended_on
  ; options_conflicts_with
  ; persistent
  ; permanent
  ; requires_auto_minor_engine_version_upgrade
  ; vpc_only
  ; supports_option_version_downgrade
  ; option_group_option_settings
  ; option_group_option_versions
  ; copyable_cross_account
  }

let parse xml =
  Some
    { name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; engine_name = Aws.Util.option_bind (Aws.Xml.member "EngineName" xml) String.parse
    ; major_engine_version =
        Aws.Util.option_bind (Aws.Xml.member "MajorEngineVersion" xml) String.parse
    ; minimum_required_minor_engine_version =
        Aws.Util.option_bind
          (Aws.Xml.member "MinimumRequiredMinorEngineVersion" xml)
          String.parse
    ; port_required =
        Aws.Util.option_bind (Aws.Xml.member "PortRequired" xml) Boolean.parse
    ; default_port = Aws.Util.option_bind (Aws.Xml.member "DefaultPort" xml) Integer.parse
    ; options_depended_on =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "OptionsDependedOn" xml)
             OptionsDependedOn.parse)
    ; options_conflicts_with =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "OptionsConflictsWith" xml)
             OptionsConflictsWith.parse)
    ; persistent = Aws.Util.option_bind (Aws.Xml.member "Persistent" xml) Boolean.parse
    ; permanent = Aws.Util.option_bind (Aws.Xml.member "Permanent" xml) Boolean.parse
    ; requires_auto_minor_engine_version_upgrade =
        Aws.Util.option_bind
          (Aws.Xml.member "RequiresAutoMinorEngineVersionUpgrade" xml)
          Boolean.parse
    ; vpc_only = Aws.Util.option_bind (Aws.Xml.member "VpcOnly" xml) Boolean.parse
    ; supports_option_version_downgrade =
        Aws.Util.option_bind
          (Aws.Xml.member "SupportsOptionVersionDowngrade" xml)
          Boolean.parse
    ; option_group_option_settings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "OptionGroupOptionSettings" xml)
             OptionGroupOptionSettingsList.parse)
    ; option_group_option_versions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "OptionGroupOptionVersions" xml)
             OptionGroupOptionVersionsList.parse)
    ; copyable_cross_account =
        Aws.Util.option_bind (Aws.Xml.member "CopyableCrossAccount" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.copyable_cross_account (fun f ->
             Aws.Query.Pair ("CopyableCrossAccount", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "OptionGroupOptionVersions.member"
              , OptionGroupOptionVersionsList.to_query v.option_group_option_versions ))
       ; Some
           (Aws.Query.Pair
              ( "OptionGroupOptionSettings.member"
              , OptionGroupOptionSettingsList.to_query v.option_group_option_settings ))
       ; Aws.Util.option_map v.supports_option_version_downgrade (fun f ->
             Aws.Query.Pair ("SupportsOptionVersionDowngrade", Boolean.to_query f))
       ; Aws.Util.option_map v.vpc_only (fun f ->
             Aws.Query.Pair ("VpcOnly", Boolean.to_query f))
       ; Aws.Util.option_map v.requires_auto_minor_engine_version_upgrade (fun f ->
             Aws.Query.Pair ("RequiresAutoMinorEngineVersionUpgrade", Boolean.to_query f))
       ; Aws.Util.option_map v.permanent (fun f ->
             Aws.Query.Pair ("Permanent", Boolean.to_query f))
       ; Aws.Util.option_map v.persistent (fun f ->
             Aws.Query.Pair ("Persistent", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "OptionsConflictsWith.member"
              , OptionsConflictsWith.to_query v.options_conflicts_with ))
       ; Some
           (Aws.Query.Pair
              ( "OptionsDependedOn.member"
              , OptionsDependedOn.to_query v.options_depended_on ))
       ; Aws.Util.option_map v.default_port (fun f ->
             Aws.Query.Pair ("DefaultPort", Integer.to_query f))
       ; Aws.Util.option_map v.port_required (fun f ->
             Aws.Query.Pair ("PortRequired", Boolean.to_query f))
       ; Aws.Util.option_map v.minimum_required_minor_engine_version (fun f ->
             Aws.Query.Pair ("MinimumRequiredMinorEngineVersion", String.to_query f))
       ; Aws.Util.option_map v.major_engine_version (fun f ->
             Aws.Query.Pair ("MajorEngineVersion", String.to_query f))
       ; Aws.Util.option_map v.engine_name (fun f ->
             Aws.Query.Pair ("EngineName", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.copyable_cross_account (fun f ->
             "CopyableCrossAccount", Boolean.to_json f)
       ; Some
           ( "OptionGroupOptionVersions"
           , OptionGroupOptionVersionsList.to_json v.option_group_option_versions )
       ; Some
           ( "OptionGroupOptionSettings"
           , OptionGroupOptionSettingsList.to_json v.option_group_option_settings )
       ; Aws.Util.option_map v.supports_option_version_downgrade (fun f ->
             "SupportsOptionVersionDowngrade", Boolean.to_json f)
       ; Aws.Util.option_map v.vpc_only (fun f -> "VpcOnly", Boolean.to_json f)
       ; Aws.Util.option_map v.requires_auto_minor_engine_version_upgrade (fun f ->
             "RequiresAutoMinorEngineVersionUpgrade", Boolean.to_json f)
       ; Aws.Util.option_map v.permanent (fun f -> "Permanent", Boolean.to_json f)
       ; Aws.Util.option_map v.persistent (fun f -> "Persistent", Boolean.to_json f)
       ; Some
           ("OptionsConflictsWith", OptionsConflictsWith.to_json v.options_conflicts_with)
       ; Some ("OptionsDependedOn", OptionsDependedOn.to_json v.options_depended_on)
       ; Aws.Util.option_map v.default_port (fun f -> "DefaultPort", Integer.to_json f)
       ; Aws.Util.option_map v.port_required (fun f -> "PortRequired", Boolean.to_json f)
       ; Aws.Util.option_map v.minimum_required_minor_engine_version (fun f ->
             "MinimumRequiredMinorEngineVersion", String.to_json f)
       ; Aws.Util.option_map v.major_engine_version (fun f ->
             "MajorEngineVersion", String.to_json f)
       ; Aws.Util.option_map v.engine_name (fun f -> "EngineName", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ])

let of_json j =
  { name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; engine_name = Aws.Util.option_map (Aws.Json.lookup j "EngineName") String.of_json
  ; major_engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "MajorEngineVersion") String.of_json
  ; minimum_required_minor_engine_version =
      Aws.Util.option_map
        (Aws.Json.lookup j "MinimumRequiredMinorEngineVersion")
        String.of_json
  ; port_required = Aws.Util.option_map (Aws.Json.lookup j "PortRequired") Boolean.of_json
  ; default_port = Aws.Util.option_map (Aws.Json.lookup j "DefaultPort") Integer.of_json
  ; options_depended_on =
      OptionsDependedOn.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "OptionsDependedOn"))
  ; options_conflicts_with =
      OptionsConflictsWith.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "OptionsConflictsWith"))
  ; persistent = Aws.Util.option_map (Aws.Json.lookup j "Persistent") Boolean.of_json
  ; permanent = Aws.Util.option_map (Aws.Json.lookup j "Permanent") Boolean.of_json
  ; requires_auto_minor_engine_version_upgrade =
      Aws.Util.option_map
        (Aws.Json.lookup j "RequiresAutoMinorEngineVersionUpgrade")
        Boolean.of_json
  ; vpc_only = Aws.Util.option_map (Aws.Json.lookup j "VpcOnly") Boolean.of_json
  ; supports_option_version_downgrade =
      Aws.Util.option_map
        (Aws.Json.lookup j "SupportsOptionVersionDowngrade")
        Boolean.of_json
  ; option_group_option_settings =
      OptionGroupOptionSettingsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "OptionGroupOptionSettings"))
  ; option_group_option_versions =
      OptionGroupOptionVersionsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "OptionGroupOptionVersions"))
  ; copyable_cross_account =
      Aws.Util.option_map (Aws.Json.lookup j "CopyableCrossAccount") Boolean.of_json
  }
