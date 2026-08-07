open Aws.BaseTypes

type t =
  { log_file_name : String.t option
  ; last_written : Long.t option
  ; size : Long.t option
  }

let make ?log_file_name ?last_written ?size () = { log_file_name; last_written; size }

let parse xml =
  Some
    { log_file_name = Aws.Util.option_bind (Aws.Xml.member "LogFileName" xml) String.parse
    ; last_written = Aws.Util.option_bind (Aws.Xml.member "LastWritten" xml) Long.parse
    ; size = Aws.Util.option_bind (Aws.Xml.member "Size" xml) Long.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.size (fun f -> Aws.Query.Pair ("Size", Long.to_query f))
       ; Aws.Util.option_map v.last_written (fun f ->
             Aws.Query.Pair ("LastWritten", Long.to_query f))
       ; Aws.Util.option_map v.log_file_name (fun f ->
             Aws.Query.Pair ("LogFileName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.size (fun f -> "Size", Long.to_json f)
       ; Aws.Util.option_map v.last_written (fun f -> "LastWritten", Long.to_json f)
       ; Aws.Util.option_map v.log_file_name (fun f -> "LogFileName", String.to_json f)
       ])

let of_json j =
  { log_file_name = Aws.Util.option_map (Aws.Json.lookup j "LogFileName") String.of_json
  ; last_written = Aws.Util.option_map (Aws.Json.lookup j "LastWritten") Long.of_json
  ; size = Aws.Util.option_map (Aws.Json.lookup j "Size") Long.of_json
  }
