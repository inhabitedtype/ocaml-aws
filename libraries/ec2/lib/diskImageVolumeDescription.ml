open Aws.BaseTypes

type t =
  { id : String.t option
  ; size : Long.t option
  }

let make ?id ?size () = { id; size }

let parse xml =
  Some
    { id = Aws.Util.option_bind (Aws.Xml.member "id" xml) String.parse
    ; size = Aws.Util.option_bind (Aws.Xml.member "size" xml) Long.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.size (fun f -> Aws.Query.Pair ("Size", Long.to_query f))
       ; Aws.Util.option_map v.id (fun f -> Aws.Query.Pair ("Id", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.size (fun f -> "size", Long.to_json f)
       ; Aws.Util.option_map v.id (fun f -> "id", String.to_json f)
       ])

let of_json j =
  { id = Aws.Util.option_map (Aws.Json.lookup j "id") String.of_json
  ; size = Aws.Util.option_map (Aws.Json.lookup j "size") Long.of_json
  }
