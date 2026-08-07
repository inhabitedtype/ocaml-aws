open Aws.BaseTypes

type t =
  { location : String.t option
  ; status : ImportFailureStatus.t option
  ; error_type : String.t option
  ; error_message : String.t option
  ; last_updated_time : DateTime.t option
  }

let make ?location ?status ?error_type ?error_message ?last_updated_time () =
  { location; status; error_type; error_message; last_updated_time }

let parse xml =
  Some
    { location = Aws.Util.option_bind (Aws.Xml.member "Location" xml) String.parse
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "Status" xml) ImportFailureStatus.parse
    ; error_type = Aws.Util.option_bind (Aws.Xml.member "ErrorType" xml) String.parse
    ; error_message =
        Aws.Util.option_bind (Aws.Xml.member "ErrorMessage" xml) String.parse
    ; last_updated_time =
        Aws.Util.option_bind (Aws.Xml.member "LastUpdatedTime" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.last_updated_time (fun f ->
             Aws.Query.Pair ("LastUpdatedTime", DateTime.to_query f))
       ; Aws.Util.option_map v.error_message (fun f ->
             Aws.Query.Pair ("ErrorMessage", String.to_query f))
       ; Aws.Util.option_map v.error_type (fun f ->
             Aws.Query.Pair ("ErrorType", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", ImportFailureStatus.to_query f))
       ; Aws.Util.option_map v.location (fun f ->
             Aws.Query.Pair ("Location", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.last_updated_time (fun f ->
             "LastUpdatedTime", DateTime.to_json f)
       ; Aws.Util.option_map v.error_message (fun f -> "ErrorMessage", String.to_json f)
       ; Aws.Util.option_map v.error_type (fun f -> "ErrorType", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", ImportFailureStatus.to_json f)
       ; Aws.Util.option_map v.location (fun f -> "Location", String.to_json f)
       ])

let of_json j =
  { location = Aws.Util.option_map (Aws.Json.lookup j "Location") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") ImportFailureStatus.of_json
  ; error_type = Aws.Util.option_map (Aws.Json.lookup j "ErrorType") String.of_json
  ; error_message = Aws.Util.option_map (Aws.Json.lookup j "ErrorMessage") String.of_json
  ; last_updated_time =
      Aws.Util.option_map (Aws.Json.lookup j "LastUpdatedTime") DateTime.of_json
  }
