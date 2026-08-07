open Aws.BaseTypes

type t =
  { error_code : String.t
  ; error_message : String.t option
  }

let make ~error_code ?error_message () = { error_code; error_message }

let parse xml =
  Some
    { error_code =
        Aws.Xml.required
          "ErrorCode"
          (Aws.Util.option_bind (Aws.Xml.member "ErrorCode" xml) String.parse)
    ; error_message =
        Aws.Util.option_bind (Aws.Xml.member "ErrorMessage" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.error_message (fun f ->
             Aws.Query.Pair ("ErrorMessage", String.to_query f))
       ; Some (Aws.Query.Pair ("ErrorCode", String.to_query v.error_code))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.error_message (fun f -> "ErrorMessage", String.to_json f)
       ; Some ("ErrorCode", String.to_json v.error_code)
       ])

let of_json j =
  { error_code = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ErrorCode"))
  ; error_message = Aws.Util.option_map (Aws.Json.lookup j "ErrorMessage") String.of_json
  }
