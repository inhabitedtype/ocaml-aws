open Aws.BaseTypes

type t =
  { stack_name : String.t
  ; role_a_r_n : String.t option
  ; resources_to_skip : ResourcesToSkip.t
  ; client_request_token : String.t option
  }

let make ~stack_name ?role_a_r_n ?(resources_to_skip = []) ?client_request_token () =
  { stack_name; role_a_r_n; resources_to_skip; client_request_token }

let parse xml =
  Some
    { stack_name =
        Aws.Xml.required
          "StackName"
          (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
    ; role_a_r_n = Aws.Util.option_bind (Aws.Xml.member "RoleARN" xml) String.parse
    ; resources_to_skip =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ResourcesToSkip" xml)
             ResourcesToSkip.parse)
    ; client_request_token =
        Aws.Util.option_bind (Aws.Xml.member "ClientRequestToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_request_token (fun f ->
             Aws.Query.Pair ("ClientRequestToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ResourcesToSkip.member", ResourcesToSkip.to_query v.resources_to_skip))
       ; Aws.Util.option_map v.role_a_r_n (fun f ->
             Aws.Query.Pair ("RoleARN", String.to_query f))
       ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_request_token (fun f ->
             "ClientRequestToken", String.to_json f)
       ; Some ("ResourcesToSkip", ResourcesToSkip.to_json v.resources_to_skip)
       ; Aws.Util.option_map v.role_a_r_n (fun f -> "RoleARN", String.to_json f)
       ; Some ("StackName", String.to_json v.stack_name)
       ])

let of_json j =
  { stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
  ; role_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "RoleARN") String.of_json
  ; resources_to_skip =
      ResourcesToSkip.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourcesToSkip"))
  ; client_request_token =
      Aws.Util.option_map (Aws.Json.lookup j "ClientRequestToken") String.of_json
  }
