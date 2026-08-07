open Aws.BaseTypes

type t =
  { id : String.t
  ; comment : String.t option
  }

let make ~id ?comment () = { id; comment }

let parse xml =
  Some
    { id =
        Aws.Xml.required
          "Id"
          (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
    ; comment = Aws.Util.option_bind (Aws.Xml.member "Comment" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.comment (fun f ->
             Aws.Query.Pair ("Comment", String.to_query f))
       ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.comment (fun f -> "Comment", String.to_json f)
       ; Some ("Id", String.to_json v.id)
       ])

let of_json j =
  { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
  ; comment = Aws.Util.option_map (Aws.Json.lookup j "Comment") String.of_json
  }
