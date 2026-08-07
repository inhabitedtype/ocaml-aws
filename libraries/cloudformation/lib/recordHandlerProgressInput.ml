open Aws.BaseTypes

type t =
  { bearer_token : String.t
  ; operation_status : OperationStatus.t
  ; current_operation_status : OperationStatus.t option
  ; status_message : String.t option
  ; error_code : HandlerErrorCode.t option
  ; resource_model : String.t option
  ; client_request_token : String.t option
  }

let make
    ~bearer_token
    ~operation_status
    ?current_operation_status
    ?status_message
    ?error_code
    ?resource_model
    ?client_request_token
    () =
  { bearer_token
  ; operation_status
  ; current_operation_status
  ; status_message
  ; error_code
  ; resource_model
  ; client_request_token
  }

let parse xml =
  Some
    { bearer_token =
        Aws.Xml.required
          "BearerToken"
          (Aws.Util.option_bind (Aws.Xml.member "BearerToken" xml) String.parse)
    ; operation_status =
        Aws.Xml.required
          "OperationStatus"
          (Aws.Util.option_bind
             (Aws.Xml.member "OperationStatus" xml)
             OperationStatus.parse)
    ; current_operation_status =
        Aws.Util.option_bind
          (Aws.Xml.member "CurrentOperationStatus" xml)
          OperationStatus.parse
    ; status_message =
        Aws.Util.option_bind (Aws.Xml.member "StatusMessage" xml) String.parse
    ; error_code =
        Aws.Util.option_bind (Aws.Xml.member "ErrorCode" xml) HandlerErrorCode.parse
    ; resource_model =
        Aws.Util.option_bind (Aws.Xml.member "ResourceModel" xml) String.parse
    ; client_request_token =
        Aws.Util.option_bind (Aws.Xml.member "ClientRequestToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_request_token (fun f ->
             Aws.Query.Pair ("ClientRequestToken", String.to_query f))
       ; Aws.Util.option_map v.resource_model (fun f ->
             Aws.Query.Pair ("ResourceModel", String.to_query f))
       ; Aws.Util.option_map v.error_code (fun f ->
             Aws.Query.Pair ("ErrorCode", HandlerErrorCode.to_query f))
       ; Aws.Util.option_map v.status_message (fun f ->
             Aws.Query.Pair ("StatusMessage", String.to_query f))
       ; Aws.Util.option_map v.current_operation_status (fun f ->
             Aws.Query.Pair ("CurrentOperationStatus", OperationStatus.to_query f))
       ; Some
           (Aws.Query.Pair ("OperationStatus", OperationStatus.to_query v.operation_status))
       ; Some (Aws.Query.Pair ("BearerToken", String.to_query v.bearer_token))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_request_token (fun f ->
             "ClientRequestToken", String.to_json f)
       ; Aws.Util.option_map v.resource_model (fun f -> "ResourceModel", String.to_json f)
       ; Aws.Util.option_map v.error_code (fun f ->
             "ErrorCode", HandlerErrorCode.to_json f)
       ; Aws.Util.option_map v.status_message (fun f -> "StatusMessage", String.to_json f)
       ; Aws.Util.option_map v.current_operation_status (fun f ->
             "CurrentOperationStatus", OperationStatus.to_json f)
       ; Some ("OperationStatus", OperationStatus.to_json v.operation_status)
       ; Some ("BearerToken", String.to_json v.bearer_token)
       ])

let of_json j =
  { bearer_token =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "BearerToken"))
  ; operation_status =
      OperationStatus.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "OperationStatus"))
  ; current_operation_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "CurrentOperationStatus")
        OperationStatus.of_json
  ; status_message =
      Aws.Util.option_map (Aws.Json.lookup j "StatusMessage") String.of_json
  ; error_code =
      Aws.Util.option_map (Aws.Json.lookup j "ErrorCode") HandlerErrorCode.of_json
  ; resource_model =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceModel") String.of_json
  ; client_request_token =
      Aws.Util.option_map (Aws.Json.lookup j "ClientRequestToken") String.of_json
  }
