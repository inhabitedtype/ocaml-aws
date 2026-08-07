open Aws.BaseTypes

type t =
  { attribute : ResetImageAttributeName.t
  ; image_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~attribute ~image_id ?dry_run () = { attribute; image_id; dry_run }

let parse xml =
  Some
    { attribute =
        Aws.Xml.required
          "Attribute"
          (Aws.Util.option_bind
             (Aws.Xml.member "Attribute" xml)
             ResetImageAttributeName.parse)
    ; image_id =
        Aws.Xml.required
          "ImageId"
          (Aws.Util.option_bind (Aws.Xml.member "ImageId" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("ImageId", String.to_query v.image_id))
       ; Some (Aws.Query.Pair ("Attribute", ResetImageAttributeName.to_query v.attribute))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some ("ImageId", String.to_json v.image_id)
       ; Some ("Attribute", ResetImageAttributeName.to_json v.attribute)
       ])

let of_json j =
  { attribute =
      ResetImageAttributeName.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Attribute"))
  ; image_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ImageId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
