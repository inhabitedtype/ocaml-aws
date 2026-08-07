open Aws.BaseTypes

type t =
  { stack_name : String.t
  ; role_a_r_n : String.t option
  ; client_request_token : String.t option
  ; retain_except_on_create : Boolean.t option
  }

let make ~stack_name ?role_a_r_n ?client_request_token ?retain_except_on_create () =
  { stack_name; role_a_r_n; client_request_token; retain_except_on_create }

let parse xml =
  Some
    { stack_name =
        Aws.Xml.required
          "StackName"
          (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
    ; role_a_r_n = Aws.Util.option_bind (Aws.Xml.member "RoleARN" xml) String.parse
    ; client_request_token =
        Aws.Util.option_bind (Aws.Xml.member "ClientRequestToken" xml) String.parse
    ; retain_except_on_create =
        Aws.Util.option_bind (Aws.Xml.member "RetainExceptOnCreate" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.retain_except_on_create (fun f ->
             Aws.Query.Pair ("RetainExceptOnCreate", Boolean.to_query f))
       ; Aws.Util.option_map v.client_request_token (fun f ->
             Aws.Query.Pair ("ClientRequestToken", String.to_query f))
       ; Aws.Util.option_map v.role_a_r_n (fun f ->
             Aws.Query.Pair ("RoleARN", String.to_query f))
       ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.retain_except_on_create (fun f ->
             "RetainExceptOnCreate", Boolean.to_json f)
       ; Aws.Util.option_map v.client_request_token (fun f ->
             "ClientRequestToken", String.to_json f)
       ; Aws.Util.option_map v.role_a_r_n (fun f -> "RoleARN", String.to_json f)
       ; Some ("StackName", String.to_json v.stack_name)
       ])

let of_json j =
  { stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
  ; role_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "RoleARN") String.of_json
  ; client_request_token =
      Aws.Util.option_map (Aws.Json.lookup j "ClientRequestToken") String.of_json
  ; retain_except_on_create =
      Aws.Util.option_map (Aws.Json.lookup j "RetainExceptOnCreate") Boolean.of_json
  }
