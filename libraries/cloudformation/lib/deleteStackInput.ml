open Aws.BaseTypes

type t =
  { stack_name : String.t
  ; retain_resources : RetainResources.t
  ; role_a_r_n : String.t option
  ; client_request_token : String.t option
  ; deletion_mode : DeletionMode.t option
  }

let make
    ~stack_name
    ?(retain_resources = [])
    ?role_a_r_n
    ?client_request_token
    ?deletion_mode
    () =
  { stack_name; retain_resources; role_a_r_n; client_request_token; deletion_mode }

let parse xml =
  Some
    { stack_name =
        Aws.Xml.required
          "StackName"
          (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
    ; retain_resources =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RetainResources" xml)
             RetainResources.parse)
    ; role_a_r_n = Aws.Util.option_bind (Aws.Xml.member "RoleARN" xml) String.parse
    ; client_request_token =
        Aws.Util.option_bind (Aws.Xml.member "ClientRequestToken" xml) String.parse
    ; deletion_mode =
        Aws.Util.option_bind (Aws.Xml.member "DeletionMode" xml) DeletionMode.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.deletion_mode (fun f ->
             Aws.Query.Pair ("DeletionMode", DeletionMode.to_query f))
       ; Aws.Util.option_map v.client_request_token (fun f ->
             Aws.Query.Pair ("ClientRequestToken", String.to_query f))
       ; Aws.Util.option_map v.role_a_r_n (fun f ->
             Aws.Query.Pair ("RoleARN", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("RetainResources.member", RetainResources.to_query v.retain_resources))
       ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.deletion_mode (fun f ->
             "DeletionMode", DeletionMode.to_json f)
       ; Aws.Util.option_map v.client_request_token (fun f ->
             "ClientRequestToken", String.to_json f)
       ; Aws.Util.option_map v.role_a_r_n (fun f -> "RoleARN", String.to_json f)
       ; Some ("RetainResources", RetainResources.to_json v.retain_resources)
       ; Some ("StackName", String.to_json v.stack_name)
       ])

let of_json j =
  { stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
  ; retain_resources =
      RetainResources.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RetainResources"))
  ; role_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "RoleARN") String.of_json
  ; client_request_token =
      Aws.Util.option_map (Aws.Json.lookup j "ClientRequestToken") String.of_json
  ; deletion_mode =
      Aws.Util.option_map (Aws.Json.lookup j "DeletionMode") DeletionMode.of_json
  }
