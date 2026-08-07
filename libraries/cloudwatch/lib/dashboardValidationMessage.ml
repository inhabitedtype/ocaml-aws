open Aws.BaseTypes

type t =
  { data_path : String.t option
  ; message : String.t option
  }

let make ?data_path ?message () = { data_path; message }

let parse xml =
  Some
    { data_path = Aws.Util.option_bind (Aws.Xml.member "DataPath" xml) String.parse
    ; message = Aws.Util.option_bind (Aws.Xml.member "Message" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.message (fun f ->
             Aws.Query.Pair ("Message", String.to_query f))
       ; Aws.Util.option_map v.data_path (fun f ->
             Aws.Query.Pair ("DataPath", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.message (fun f -> "Message", String.to_json f)
       ; Aws.Util.option_map v.data_path (fun f -> "DataPath", String.to_json f)
       ])

let of_json j =
  { data_path = Aws.Util.option_map (Aws.Json.lookup j "DataPath") String.of_json
  ; message = Aws.Util.option_map (Aws.Json.lookup j "Message") String.of_json
  }
