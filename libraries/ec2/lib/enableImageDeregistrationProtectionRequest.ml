open Aws.BaseTypes

type t =
  { image_id : String.t
  ; with_cooldown : Boolean.t option
  ; dry_run : Boolean.t option
  }

let make ~image_id ?with_cooldown ?dry_run () = { image_id; with_cooldown; dry_run }

let parse xml =
  Some
    { image_id =
        Aws.Xml.required
          "ImageId"
          (Aws.Util.option_bind (Aws.Xml.member "ImageId" xml) String.parse)
    ; with_cooldown =
        Aws.Util.option_bind (Aws.Xml.member "WithCooldown" xml) Boolean.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.with_cooldown (fun f ->
             Aws.Query.Pair ("WithCooldown", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("ImageId", String.to_query v.image_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.with_cooldown (fun f -> "WithCooldown", Boolean.to_json f)
       ; Some ("ImageId", String.to_json v.image_id)
       ])

let of_json j =
  { image_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ImageId"))
  ; with_cooldown = Aws.Util.option_map (Aws.Json.lookup j "WithCooldown") Boolean.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
