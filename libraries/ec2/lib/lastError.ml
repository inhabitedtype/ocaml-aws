open Aws.BaseTypes

type t =
  { message : String.t option
  ; code : String.t option
  }

let make ?message ?code () = { message; code }

let parse xml =
  Some
    { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse
    ; code = Aws.Util.option_bind (Aws.Xml.member "code" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.code (fun f -> Aws.Query.Pair ("Code", String.to_query f))
       ; Aws.Util.option_map v.message (fun f ->
             Aws.Query.Pair ("Message", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.code (fun f -> "code", String.to_json f)
       ; Aws.Util.option_map v.message (fun f -> "message", String.to_json f)
       ])

let of_json j =
  { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json
  ; code = Aws.Util.option_map (Aws.Json.lookup j "code") String.of_json
  }
