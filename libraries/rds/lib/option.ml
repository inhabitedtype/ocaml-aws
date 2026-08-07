open Aws.BaseTypes

type t =
  { option_name : String.t option
  ; option_description : String.t option
  ; persistent : Boolean.t option
  ; permanent : Boolean.t option
  ; port : Integer.t option
  ; option_version : String.t option
  ; option_settings : OptionSettingConfigurationList.t
  ; d_b_security_group_memberships : DBSecurityGroupMembershipList.t
  ; vpc_security_group_memberships : VpcSecurityGroupMembershipList.t
  }

let make
    ?option_name
    ?option_description
    ?persistent
    ?permanent
    ?port
    ?option_version
    ?(option_settings = [])
    ?(d_b_security_group_memberships = [])
    ?(vpc_security_group_memberships = [])
    () =
  { option_name
  ; option_description
  ; persistent
  ; permanent
  ; port
  ; option_version
  ; option_settings
  ; d_b_security_group_memberships
  ; vpc_security_group_memberships
  }

let parse xml =
  Some
    { option_name = Aws.Util.option_bind (Aws.Xml.member "OptionName" xml) String.parse
    ; option_description =
        Aws.Util.option_bind (Aws.Xml.member "OptionDescription" xml) String.parse
    ; persistent = Aws.Util.option_bind (Aws.Xml.member "Persistent" xml) Boolean.parse
    ; permanent = Aws.Util.option_bind (Aws.Xml.member "Permanent" xml) Boolean.parse
    ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
    ; option_version =
        Aws.Util.option_bind (Aws.Xml.member "OptionVersion" xml) String.parse
    ; option_settings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "OptionSettings" xml)
             OptionSettingConfigurationList.parse)
    ; d_b_security_group_memberships =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DBSecurityGroupMemberships" xml)
             DBSecurityGroupMembershipList.parse)
    ; vpc_security_group_memberships =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "VpcSecurityGroupMemberships" xml)
             VpcSecurityGroupMembershipList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "VpcSecurityGroupMemberships.member"
              , VpcSecurityGroupMembershipList.to_query v.vpc_security_group_memberships
              ))
       ; Some
           (Aws.Query.Pair
              ( "DBSecurityGroupMemberships.member"
              , DBSecurityGroupMembershipList.to_query v.d_b_security_group_memberships ))
       ; Some
           (Aws.Query.Pair
              ( "OptionSettings.member"
              , OptionSettingConfigurationList.to_query v.option_settings ))
       ; Aws.Util.option_map v.option_version (fun f ->
             Aws.Query.Pair ("OptionVersion", String.to_query f))
       ; Aws.Util.option_map v.port (fun f -> Aws.Query.Pair ("Port", Integer.to_query f))
       ; Aws.Util.option_map v.permanent (fun f ->
             Aws.Query.Pair ("Permanent", Boolean.to_query f))
       ; Aws.Util.option_map v.persistent (fun f ->
             Aws.Query.Pair ("Persistent", Boolean.to_query f))
       ; Aws.Util.option_map v.option_description (fun f ->
             Aws.Query.Pair ("OptionDescription", String.to_query f))
       ; Aws.Util.option_map v.option_name (fun f ->
             Aws.Query.Pair ("OptionName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "VpcSecurityGroupMemberships"
           , VpcSecurityGroupMembershipList.to_json v.vpc_security_group_memberships )
       ; Some
           ( "DBSecurityGroupMemberships"
           , DBSecurityGroupMembershipList.to_json v.d_b_security_group_memberships )
       ; Some ("OptionSettings", OptionSettingConfigurationList.to_json v.option_settings)
       ; Aws.Util.option_map v.option_version (fun f -> "OptionVersion", String.to_json f)
       ; Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
       ; Aws.Util.option_map v.permanent (fun f -> "Permanent", Boolean.to_json f)
       ; Aws.Util.option_map v.persistent (fun f -> "Persistent", Boolean.to_json f)
       ; Aws.Util.option_map v.option_description (fun f ->
             "OptionDescription", String.to_json f)
       ; Aws.Util.option_map v.option_name (fun f -> "OptionName", String.to_json f)
       ])

let of_json j =
  { option_name = Aws.Util.option_map (Aws.Json.lookup j "OptionName") String.of_json
  ; option_description =
      Aws.Util.option_map (Aws.Json.lookup j "OptionDescription") String.of_json
  ; persistent = Aws.Util.option_map (Aws.Json.lookup j "Persistent") Boolean.of_json
  ; permanent = Aws.Util.option_map (Aws.Json.lookup j "Permanent") Boolean.of_json
  ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
  ; option_version =
      Aws.Util.option_map (Aws.Json.lookup j "OptionVersion") String.of_json
  ; option_settings =
      OptionSettingConfigurationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "OptionSettings"))
  ; d_b_security_group_memberships =
      DBSecurityGroupMembershipList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DBSecurityGroupMemberships"))
  ; vpc_security_group_memberships =
      VpcSecurityGroupMembershipList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcSecurityGroupMemberships"))
  }
