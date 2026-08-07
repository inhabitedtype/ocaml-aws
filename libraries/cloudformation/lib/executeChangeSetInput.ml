open Aws.BaseTypes

type t =
  { change_set_name : String.t
  ; stack_name : String.t option
  ; client_request_token : String.t option
  ; disable_rollback : Boolean.t option
  ; retain_except_on_create : Boolean.t option
  }

let make
    ~change_set_name
    ?stack_name
    ?client_request_token
    ?disable_rollback
    ?retain_except_on_create
    () =
  { change_set_name
  ; stack_name
  ; client_request_token
  ; disable_rollback
  ; retain_except_on_create
  }

let parse xml =
  Some
    { change_set_name =
        Aws.Xml.required
          "ChangeSetName"
          (Aws.Util.option_bind (Aws.Xml.member "ChangeSetName" xml) String.parse)
    ; stack_name = Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse
    ; client_request_token =
        Aws.Util.option_bind (Aws.Xml.member "ClientRequestToken" xml) String.parse
    ; disable_rollback =
        Aws.Util.option_bind (Aws.Xml.member "DisableRollback" xml) Boolean.parse
    ; retain_except_on_create =
        Aws.Util.option_bind (Aws.Xml.member "RetainExceptOnCreate" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.retain_except_on_create (fun f ->
             Aws.Query.Pair ("RetainExceptOnCreate", Boolean.to_query f))
       ; Aws.Util.option_map v.disable_rollback (fun f ->
             Aws.Query.Pair ("DisableRollback", Boolean.to_query f))
       ; Aws.Util.option_map v.client_request_token (fun f ->
             Aws.Query.Pair ("ClientRequestToken", String.to_query f))
       ; Aws.Util.option_map v.stack_name (fun f ->
             Aws.Query.Pair ("StackName", String.to_query f))
       ; Some (Aws.Query.Pair ("ChangeSetName", String.to_query v.change_set_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.retain_except_on_create (fun f ->
             "RetainExceptOnCreate", Boolean.to_json f)
       ; Aws.Util.option_map v.disable_rollback (fun f ->
             "DisableRollback", Boolean.to_json f)
       ; Aws.Util.option_map v.client_request_token (fun f ->
             "ClientRequestToken", String.to_json f)
       ; Aws.Util.option_map v.stack_name (fun f -> "StackName", String.to_json f)
       ; Some ("ChangeSetName", String.to_json v.change_set_name)
       ])

let of_json j =
  { change_set_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ChangeSetName"))
  ; stack_name = Aws.Util.option_map (Aws.Json.lookup j "StackName") String.of_json
  ; client_request_token =
      Aws.Util.option_map (Aws.Json.lookup j "ClientRequestToken") String.of_json
  ; disable_rollback =
      Aws.Util.option_map (Aws.Json.lookup j "DisableRollback") Boolean.of_json
  ; retain_except_on_create =
      Aws.Util.option_map (Aws.Json.lookup j "RetainExceptOnCreate") Boolean.of_json
  }
