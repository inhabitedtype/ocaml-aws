open Aws.BaseTypes

type t =
  { image_id : String.t
  ; deprecate_at : DateTime.t
  ; dry_run : Boolean.t option
  }

let make ~image_id ~deprecate_at ?dry_run () = { image_id; deprecate_at; dry_run }

let parse xml =
  Some
    { image_id =
        Aws.Xml.required
          "ImageId"
          (Aws.Util.option_bind (Aws.Xml.member "ImageId" xml) String.parse)
    ; deprecate_at =
        Aws.Xml.required
          "DeprecateAt"
          (Aws.Util.option_bind (Aws.Xml.member "DeprecateAt" xml) DateTime.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("DeprecateAt", DateTime.to_query v.deprecate_at))
       ; Some (Aws.Query.Pair ("ImageId", String.to_query v.image_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("DeprecateAt", DateTime.to_json v.deprecate_at)
       ; Some ("ImageId", String.to_json v.image_id)
       ])

let of_json j =
  { image_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ImageId"))
  ; deprecate_at =
      DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DeprecateAt"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
