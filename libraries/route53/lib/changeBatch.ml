open Aws.BaseTypes

type t =
  { comment : String.t option
  ; changes : Changes.t
  }

let make ?comment ~changes () = { comment; changes }

let parse xml =
  Some
    { comment = Aws.Util.option_bind (Aws.Xml.member "Comment" xml) String.parse
    ; changes =
        Aws.Xml.required
          "Changes"
          (Aws.Util.option_bind (Aws.Xml.member "Changes" xml) Changes.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Changes.member", Changes.to_query v.changes))
       ; Aws.Util.option_map v.comment (fun f ->
             Aws.Query.Pair ("Comment", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Changes", Changes.to_json v.changes)
       ; Aws.Util.option_map v.comment (fun f -> "Comment", String.to_json f)
       ])

let of_json j =
  { comment = Aws.Util.option_map (Aws.Json.lookup j "Comment") String.of_json
  ; changes = Changes.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Changes"))
  }
