open Aws.BaseTypes

type t =
  { id : String.t
  ; status : ChangeStatus.t
  ; submitted_at : DateTime.t
  ; comment : String.t option
  }

let make ~id ~status ~submitted_at ?comment () = { id; status; submitted_at; comment }

let parse xml =
  Some
    { id =
        Aws.Xml.required
          "Id"
          (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
    ; status =
        Aws.Xml.required
          "Status"
          (Aws.Util.option_bind (Aws.Xml.member "Status" xml) ChangeStatus.parse)
    ; submitted_at =
        Aws.Xml.required
          "SubmittedAt"
          (Aws.Util.option_bind (Aws.Xml.member "SubmittedAt" xml) DateTime.parse)
    ; comment = Aws.Util.option_bind (Aws.Xml.member "Comment" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.comment (fun f ->
             Aws.Query.Pair ("Comment", String.to_query f))
       ; Some (Aws.Query.Pair ("SubmittedAt", DateTime.to_query v.submitted_at))
       ; Some (Aws.Query.Pair ("Status", ChangeStatus.to_query v.status))
       ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.comment (fun f -> "Comment", String.to_json f)
       ; Some ("SubmittedAt", DateTime.to_json v.submitted_at)
       ; Some ("Status", ChangeStatus.to_json v.status)
       ; Some ("Id", String.to_json v.id)
       ])

let of_json j =
  { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
  ; status = ChangeStatus.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Status"))
  ; submitted_at =
      DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SubmittedAt"))
  ; comment = Aws.Util.option_map (Aws.Json.lookup j "Comment") String.of_json
  }
