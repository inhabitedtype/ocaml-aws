open Aws.BaseTypes

type t =
  { text : String.t option
  ; url : String.t option
  }

let make ?text ?url () = { text; url }

let parse xml =
  Some
    { text = Aws.Util.option_bind (Aws.Xml.member "Text" xml) String.parse
    ; url = Aws.Util.option_bind (Aws.Xml.member "Url" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.url (fun f -> Aws.Query.Pair ("Url", String.to_query f))
       ; Aws.Util.option_map v.text (fun f -> Aws.Query.Pair ("Text", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.url (fun f -> "Url", String.to_json f)
       ; Aws.Util.option_map v.text (fun f -> "Text", String.to_json f)
       ])

let of_json j =
  { text = Aws.Util.option_map (Aws.Json.lookup j "Text") String.of_json
  ; url = Aws.Util.option_map (Aws.Json.lookup j "Url") String.of_json
  }
