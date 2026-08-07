open Aws.BaseTypes

type t =
  { failure_resource : String.t option
  ; exception_type : String.t option
  ; failure_code : String.t option
  ; failure_description : String.t option
  }

let make ?failure_resource ?exception_type ?failure_code ?failure_description () =
  { failure_resource; exception_type; failure_code; failure_description }

let parse xml =
  Some
    { failure_resource =
        Aws.Util.option_bind (Aws.Xml.member "FailureResource" xml) String.parse
    ; exception_type =
        Aws.Util.option_bind (Aws.Xml.member "ExceptionType" xml) String.parse
    ; failure_code = Aws.Util.option_bind (Aws.Xml.member "FailureCode" xml) String.parse
    ; failure_description =
        Aws.Util.option_bind (Aws.Xml.member "FailureDescription" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.failure_description (fun f ->
             Aws.Query.Pair ("FailureDescription", String.to_query f))
       ; Aws.Util.option_map v.failure_code (fun f ->
             Aws.Query.Pair ("FailureCode", String.to_query f))
       ; Aws.Util.option_map v.exception_type (fun f ->
             Aws.Query.Pair ("ExceptionType", String.to_query f))
       ; Aws.Util.option_map v.failure_resource (fun f ->
             Aws.Query.Pair ("FailureResource", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.failure_description (fun f ->
             "FailureDescription", String.to_json f)
       ; Aws.Util.option_map v.failure_code (fun f -> "FailureCode", String.to_json f)
       ; Aws.Util.option_map v.exception_type (fun f -> "ExceptionType", String.to_json f)
       ; Aws.Util.option_map v.failure_resource (fun f ->
             "FailureResource", String.to_json f)
       ])

let of_json j =
  { failure_resource =
      Aws.Util.option_map (Aws.Json.lookup j "FailureResource") String.of_json
  ; exception_type =
      Aws.Util.option_map (Aws.Json.lookup j "ExceptionType") String.of_json
  ; failure_code = Aws.Util.option_map (Aws.Json.lookup j "FailureCode") String.of_json
  ; failure_description =
      Aws.Util.option_map (Aws.Json.lookup j "FailureDescription") String.of_json
  }
