open Aws.BaseTypes

type t =
  { option_name : String.t
  ; port : Integer.t option
  ; option_version : String.t option
  ; d_b_security_group_memberships : DBSecurityGroupNameList.t
  ; vpc_security_group_memberships : VpcSecurityGroupIdList.t
  ; option_settings : OptionSettingsList.t
  }

let make
    ~option_name
    ?port
    ?option_version
    ?(d_b_security_group_memberships = [])
    ?(vpc_security_group_memberships = [])
    ?(option_settings = [])
    () =
  { option_name
  ; port
  ; option_version
  ; d_b_security_group_memberships
  ; vpc_security_group_memberships
  ; option_settings
  }

let parse xml =
  Some
    { option_name =
        Aws.Xml.required
          "OptionName"
          (Aws.Util.option_bind (Aws.Xml.member "OptionName" xml) String.parse)
    ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
    ; option_version =
        Aws.Util.option_bind (Aws.Xml.member "OptionVersion" xml) String.parse
    ; d_b_security_group_memberships =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DBSecurityGroupMemberships" xml)
             DBSecurityGroupNameList.parse)
    ; vpc_security_group_memberships =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "VpcSecurityGroupMemberships" xml)
             VpcSecurityGroupIdList.parse)
    ; option_settings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "OptionSettings" xml)
             OptionSettingsList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("OptionSettings.member", OptionSettingsList.to_query v.option_settings))
       ; Some
           (Aws.Query.Pair
              ( "VpcSecurityGroupMemberships.member"
              , VpcSecurityGroupIdList.to_query v.vpc_security_group_memberships ))
       ; Some
           (Aws.Query.Pair
              ( "DBSecurityGroupMemberships.member"
              , DBSecurityGroupNameList.to_query v.d_b_security_group_memberships ))
       ; Aws.Util.option_map v.option_version (fun f ->
             Aws.Query.Pair ("OptionVersion", String.to_query f))
       ; Aws.Util.option_map v.port (fun f -> Aws.Query.Pair ("Port", Integer.to_query f))
       ; Some (Aws.Query.Pair ("OptionName", String.to_query v.option_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("OptionSettings", OptionSettingsList.to_json v.option_settings)
       ; Some
           ( "VpcSecurityGroupMemberships"
           , VpcSecurityGroupIdList.to_json v.vpc_security_group_memberships )
       ; Some
           ( "DBSecurityGroupMemberships"
           , DBSecurityGroupNameList.to_json v.d_b_security_group_memberships )
       ; Aws.Util.option_map v.option_version (fun f -> "OptionVersion", String.to_json f)
       ; Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
       ; Some ("OptionName", String.to_json v.option_name)
       ])

let of_json j =
  { option_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OptionName"))
  ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
  ; option_version =
      Aws.Util.option_map (Aws.Json.lookup j "OptionVersion") String.of_json
  ; d_b_security_group_memberships =
      DBSecurityGroupNameList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DBSecurityGroupMemberships"))
  ; vpc_security_group_memberships =
      VpcSecurityGroupIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcSecurityGroupMemberships"))
  ; option_settings =
      OptionSettingsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "OptionSettings"))
  }
