open Aws.BaseTypes

type t =
  { stack_set_name : String.t
  ; accounts : AccountList.t
  ; deployment_targets : DeploymentTargets.t option
  ; regions : RegionList.t
  ; operation_preferences : StackSetOperationPreferences.t option
  ; retain_stacks : Boolean.t
  ; operation_id : String.t option
  ; call_as : CallAs.t option
  }

let make
    ~stack_set_name
    ?(accounts = [])
    ?deployment_targets
    ~regions
    ?operation_preferences
    ~retain_stacks
    ?operation_id
    ?call_as
    () =
  { stack_set_name
  ; accounts
  ; deployment_targets
  ; regions
  ; operation_preferences
  ; retain_stacks
  ; operation_id
  ; call_as
  }

let parse xml =
  Some
    { stack_set_name =
        Aws.Xml.required
          "StackSetName"
          (Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse)
    ; accounts =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Accounts" xml) AccountList.parse)
    ; deployment_targets =
        Aws.Util.option_bind
          (Aws.Xml.member "DeploymentTargets" xml)
          DeploymentTargets.parse
    ; regions =
        Aws.Xml.required
          "Regions"
          (Aws.Util.option_bind (Aws.Xml.member "Regions" xml) RegionList.parse)
    ; operation_preferences =
        Aws.Util.option_bind
          (Aws.Xml.member "OperationPreferences" xml)
          StackSetOperationPreferences.parse
    ; retain_stacks =
        Aws.Xml.required
          "RetainStacks"
          (Aws.Util.option_bind (Aws.Xml.member "RetainStacks" xml) Boolean.parse)
    ; operation_id = Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse
    ; call_as = Aws.Util.option_bind (Aws.Xml.member "CallAs" xml) CallAs.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.call_as (fun f ->
             Aws.Query.Pair ("CallAs", CallAs.to_query f))
       ; Aws.Util.option_map v.operation_id (fun f ->
             Aws.Query.Pair ("OperationId", String.to_query f))
       ; Some (Aws.Query.Pair ("RetainStacks", Boolean.to_query v.retain_stacks))
       ; Aws.Util.option_map v.operation_preferences (fun f ->
             Aws.Query.Pair
               ("OperationPreferences", StackSetOperationPreferences.to_query f))
       ; Some (Aws.Query.Pair ("Regions.member", RegionList.to_query v.regions))
       ; Aws.Util.option_map v.deployment_targets (fun f ->
             Aws.Query.Pair ("DeploymentTargets", DeploymentTargets.to_query f))
       ; Some (Aws.Query.Pair ("Accounts.member", AccountList.to_query v.accounts))
       ; Some (Aws.Query.Pair ("StackSetName", String.to_query v.stack_set_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.call_as (fun f -> "CallAs", CallAs.to_json f)
       ; Aws.Util.option_map v.operation_id (fun f -> "OperationId", String.to_json f)
       ; Some ("RetainStacks", Boolean.to_json v.retain_stacks)
       ; Aws.Util.option_map v.operation_preferences (fun f ->
             "OperationPreferences", StackSetOperationPreferences.to_json f)
       ; Some ("Regions", RegionList.to_json v.regions)
       ; Aws.Util.option_map v.deployment_targets (fun f ->
             "DeploymentTargets", DeploymentTargets.to_json f)
       ; Some ("Accounts", AccountList.to_json v.accounts)
       ; Some ("StackSetName", String.to_json v.stack_set_name)
       ])

let of_json j =
  { stack_set_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackSetName"))
  ; accounts = AccountList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Accounts"))
  ; deployment_targets =
      Aws.Util.option_map
        (Aws.Json.lookup j "DeploymentTargets")
        DeploymentTargets.of_json
  ; regions = RegionList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Regions"))
  ; operation_preferences =
      Aws.Util.option_map
        (Aws.Json.lookup j "OperationPreferences")
        StackSetOperationPreferences.of_json
  ; retain_stacks =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RetainStacks"))
  ; operation_id = Aws.Util.option_map (Aws.Json.lookup j "OperationId") String.of_json
  ; call_as = Aws.Util.option_map (Aws.Json.lookup j "CallAs") CallAs.of_json
  }
