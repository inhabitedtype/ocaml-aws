open Aws.BaseTypes

type t =
  { message : String.t option
  ; signature : String.t option
  }

let make ?message ?signature () = { message; signature }

let parse xml =
  Some
    { message = Aws.Util.option_bind (Aws.Xml.member "Message" xml) String.parse
    ; signature = Aws.Util.option_bind (Aws.Xml.member "Signature" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.signature (fun f ->
             Aws.Query.Pair ("Signature", String.to_query f))
       ; Aws.Util.option_map v.message (fun f ->
             Aws.Query.Pair ("Message", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.signature (fun f -> "Signature", String.to_json f)
       ; Aws.Util.option_map v.message (fun f -> "Message", String.to_json f)
       ])

let of_json j =
  { message = Aws.Util.option_map (Aws.Json.lookup j "Message") String.of_json
  ; signature = Aws.Util.option_map (Aws.Json.lookup j "Signature") String.of_json
  }
