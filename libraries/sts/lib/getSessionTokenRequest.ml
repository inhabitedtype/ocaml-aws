open Aws.BaseTypes

type t =
  { duration_seconds : Integer.t option
  ; serial_number : String.t option
  ; token_code : String.t option
  }

let make ?duration_seconds ?serial_number ?token_code () =
  { duration_seconds; serial_number; token_code }

let parse xml =
  Some
    { duration_seconds =
        Aws.Util.option_bind (Aws.Xml.member "DurationSeconds" xml) Integer.parse
    ; serial_number =
        Aws.Util.option_bind (Aws.Xml.member "SerialNumber" xml) String.parse
    ; token_code = Aws.Util.option_bind (Aws.Xml.member "TokenCode" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.token_code (fun f ->
             Aws.Query.Pair ("TokenCode", String.to_query f))
       ; Aws.Util.option_map v.serial_number (fun f ->
             Aws.Query.Pair ("SerialNumber", String.to_query f))
       ; Aws.Util.option_map v.duration_seconds (fun f ->
             Aws.Query.Pair ("DurationSeconds", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.token_code (fun f -> "TokenCode", String.to_json f)
       ; Aws.Util.option_map v.serial_number (fun f -> "SerialNumber", String.to_json f)
       ; Aws.Util.option_map v.duration_seconds (fun f ->
             "DurationSeconds", Integer.to_json f)
       ])

let of_json j =
  { duration_seconds =
      Aws.Util.option_map (Aws.Json.lookup j "DurationSeconds") Integer.of_json
  ; serial_number = Aws.Util.option_map (Aws.Json.lookup j "SerialNumber") String.of_json
  ; token_code = Aws.Util.option_map (Aws.Json.lookup j "TokenCode") String.of_json
  }
