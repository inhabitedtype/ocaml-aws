open Aws.BaseTypes

type t =
  { id : String.t
  ; version : Integer.t
  ; comment : String.t
  }

let make ~id ~version ~comment () = { id; version; comment }

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
    ; comment =
        Aws.Xml.required
          "Comment"
          (Aws.Util.option_bind (Aws.Xml.member "Comment" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Comment", String.to_query v.comment))
       ; Some (Aws.Query.Pair ("Version", Integer.to_query v.version))
       ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Comment", String.to_json v.comment)
       ; Some ("Version", Integer.to_json v.version)
       ; Some ("Id", String.to_json v.id)
       ])

let of_json j =
  { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
  ; version = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Version"))
  ; comment = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Comment"))
  }
