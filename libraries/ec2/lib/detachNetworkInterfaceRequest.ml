open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; attachment_id : String.t
  ; force : Boolean.t option
  }

let make ?dry_run ~attachment_id ?force () = { dry_run; attachment_id; force }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; attachment_id =
        Aws.Xml.required
          "attachmentId"
          (Aws.Util.option_bind (Aws.Xml.member "attachmentId" xml) String.parse)
    ; force = Aws.Util.option_bind (Aws.Xml.member "force" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.force (fun f ->
             Aws.Query.Pair ("Force", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("AttachmentId", String.to_query v.attachment_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.force (fun f -> "force", Boolean.to_json f)
       ; Some ("attachmentId", String.to_json v.attachment_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; attachment_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "attachmentId"))
  ; force = Aws.Util.option_map (Aws.Json.lookup j "force") Boolean.of_json
  }
