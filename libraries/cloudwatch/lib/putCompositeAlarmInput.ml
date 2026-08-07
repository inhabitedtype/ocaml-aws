open Aws.BaseTypes

type t =
  { actions_enabled : Boolean.t option
  ; alarm_actions : ResourceList.t
  ; alarm_description : String.t option
  ; alarm_name : String.t
  ; alarm_rule : String.t
  ; insufficient_data_actions : ResourceList.t
  ; o_k_actions : ResourceList.t
  ; tags : TagList.t
  ; actions_suppressor : String.t option
  ; actions_suppressor_wait_period : Integer.t option
  ; actions_suppressor_extension_period : Integer.t option
  }

let make
    ?actions_enabled
    ?(alarm_actions = [])
    ?alarm_description
    ~alarm_name
    ~alarm_rule
    ?(insufficient_data_actions = [])
    ?(o_k_actions = [])
    ?(tags = [])
    ?actions_suppressor
    ?actions_suppressor_wait_period
    ?actions_suppressor_extension_period
    () =
  { actions_enabled
  ; alarm_actions
  ; alarm_description
  ; alarm_name
  ; alarm_rule
  ; insufficient_data_actions
  ; o_k_actions
  ; tags
  ; actions_suppressor
  ; actions_suppressor_wait_period
  ; actions_suppressor_extension_period
  }

let parse xml =
  Some
    { actions_enabled =
        Aws.Util.option_bind (Aws.Xml.member "ActionsEnabled" xml) Boolean.parse
    ; alarm_actions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "AlarmActions" xml) ResourceList.parse)
    ; alarm_description =
        Aws.Util.option_bind (Aws.Xml.member "AlarmDescription" xml) String.parse
    ; alarm_name =
        Aws.Xml.required
          "AlarmName"
          (Aws.Util.option_bind (Aws.Xml.member "AlarmName" xml) String.parse)
    ; alarm_rule =
        Aws.Xml.required
          "AlarmRule"
          (Aws.Util.option_bind (Aws.Xml.member "AlarmRule" xml) String.parse)
    ; insufficient_data_actions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "InsufficientDataActions" xml)
             ResourceList.parse)
    ; o_k_actions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "OKActions" xml) ResourceList.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    ; actions_suppressor =
        Aws.Util.option_bind (Aws.Xml.member "ActionsSuppressor" xml) String.parse
    ; actions_suppressor_wait_period =
        Aws.Util.option_bind
          (Aws.Xml.member "ActionsSuppressorWaitPeriod" xml)
          Integer.parse
    ; actions_suppressor_extension_period =
        Aws.Util.option_bind
          (Aws.Xml.member "ActionsSuppressorExtensionPeriod" xml)
          Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.actions_suppressor_extension_period (fun f ->
             Aws.Query.Pair ("ActionsSuppressorExtensionPeriod", Integer.to_query f))
       ; Aws.Util.option_map v.actions_suppressor_wait_period (fun f ->
             Aws.Query.Pair ("ActionsSuppressorWaitPeriod", Integer.to_query f))
       ; Aws.Util.option_map v.actions_suppressor (fun f ->
             Aws.Query.Pair ("ActionsSuppressor", String.to_query f))
       ; Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Some (Aws.Query.Pair ("OKActions.member", ResourceList.to_query v.o_k_actions))
       ; Some
           (Aws.Query.Pair
              ( "InsufficientDataActions.member"
              , ResourceList.to_query v.insufficient_data_actions ))
       ; Some (Aws.Query.Pair ("AlarmRule", String.to_query v.alarm_rule))
       ; Some (Aws.Query.Pair ("AlarmName", String.to_query v.alarm_name))
       ; Aws.Util.option_map v.alarm_description (fun f ->
             Aws.Query.Pair ("AlarmDescription", String.to_query f))
       ; Some
           (Aws.Query.Pair ("AlarmActions.member", ResourceList.to_query v.alarm_actions))
       ; Aws.Util.option_map v.actions_enabled (fun f ->
             Aws.Query.Pair ("ActionsEnabled", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.actions_suppressor_extension_period (fun f ->
             "ActionsSuppressorExtensionPeriod", Integer.to_json f)
       ; Aws.Util.option_map v.actions_suppressor_wait_period (fun f ->
             "ActionsSuppressorWaitPeriod", Integer.to_json f)
       ; Aws.Util.option_map v.actions_suppressor (fun f ->
             "ActionsSuppressor", String.to_json f)
       ; Some ("Tags", TagList.to_json v.tags)
       ; Some ("OKActions", ResourceList.to_json v.o_k_actions)
       ; Some ("InsufficientDataActions", ResourceList.to_json v.insufficient_data_actions)
       ; Some ("AlarmRule", String.to_json v.alarm_rule)
       ; Some ("AlarmName", String.to_json v.alarm_name)
       ; Aws.Util.option_map v.alarm_description (fun f ->
             "AlarmDescription", String.to_json f)
       ; Some ("AlarmActions", ResourceList.to_json v.alarm_actions)
       ; Aws.Util.option_map v.actions_enabled (fun f ->
             "ActionsEnabled", Boolean.to_json f)
       ])

let of_json j =
  { actions_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "ActionsEnabled") Boolean.of_json
  ; alarm_actions =
      ResourceList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmActions"))
  ; alarm_description =
      Aws.Util.option_map (Aws.Json.lookup j "AlarmDescription") String.of_json
  ; alarm_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmName"))
  ; alarm_rule = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmRule"))
  ; insufficient_data_actions =
      ResourceList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InsufficientDataActions"))
  ; o_k_actions =
      ResourceList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OKActions"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; actions_suppressor =
      Aws.Util.option_map (Aws.Json.lookup j "ActionsSuppressor") String.of_json
  ; actions_suppressor_wait_period =
      Aws.Util.option_map
        (Aws.Json.lookup j "ActionsSuppressorWaitPeriod")
        Integer.of_json
  ; actions_suppressor_extension_period =
      Aws.Util.option_map
        (Aws.Json.lookup j "ActionsSuppressorExtensionPeriod")
        Integer.of_json
  }
