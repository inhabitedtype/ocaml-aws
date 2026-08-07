open Aws.BaseTypes

type t =
  { name : String.t
  ; document : String.t
  ; comment : String.t option
  }

let make ~name ~document ?comment () = { name; document; comment }

let parse xml =
  Some
    { name =
        Aws.Xml.required
          "Name"
          (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
    ; document =
        Aws.Xml.required
          "Document"
          (Aws.Util.option_bind (Aws.Xml.member "Document" xml) String.parse)
    ; comment = Aws.Util.option_bind (Aws.Xml.member "Comment" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.comment (fun f ->
             Aws.Query.Pair ("Comment", String.to_query f))
       ; Some (Aws.Query.Pair ("Document", String.to_query v.document))
       ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.comment (fun f -> "Comment", String.to_json f)
       ; Some ("Document", String.to_json v.document)
       ; Some ("Name", String.to_json v.name)
       ])

let of_json j =
  { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
  ; document = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Document"))
  ; comment = Aws.Util.option_map (Aws.Json.lookup j "Comment") String.of_json
  }
