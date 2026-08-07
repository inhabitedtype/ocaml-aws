open Aws.BaseTypes

type t =
  { id : String.t
  ; version : Integer.t
  ; name : String.t
  ; type_ : RRType.t
  ; document : String.t
  ; comment : String.t option
  }

let make ~id ~version ~name ~type_ ~document ?comment () =
  { id; version; name; type_; document; comment }

let parse xml =
  Some
    { id =
        Aws.Xml.required
          "Id"
          (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
    ; version =
        Aws.Xml.required
          "Version"
          (Aws.Util.option_bind (Aws.Xml.member "Version" xml) Integer.parse)
    ; name =
        Aws.Xml.required
          "Name"
          (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
    ; type_ =
        Aws.Xml.required
          "Type"
          (Aws.Util.option_bind (Aws.Xml.member "Type" xml) RRType.parse)
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
       ; Some (Aws.Query.Pair ("Type", RRType.to_query v.type_))
       ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
       ; Some (Aws.Query.Pair ("Version", Integer.to_query v.version))
       ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.comment (fun f -> "Comment", String.to_json f)
       ; Some ("Document", String.to_json v.document)
       ; Some ("Type", RRType.to_json v.type_)
       ; Some ("Name", String.to_json v.name)
       ; Some ("Version", Integer.to_json v.version)
       ; Some ("Id", String.to_json v.id)
       ])

let of_json j =
  { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
  ; version = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Version"))
  ; name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
  ; type_ = RRType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Type"))
  ; document = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Document"))
  ; comment = Aws.Util.option_map (Aws.Json.lookup j "Comment") String.of_json
  }
