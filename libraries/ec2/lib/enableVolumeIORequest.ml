open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; volume_id : String.t
  }

let make ?dry_run ~volume_id () = { dry_run; volume_id }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; volume_id =
        Aws.Xml.required
          "volumeId"
          (Aws.Util.option_bind (Aws.Xml.member "volumeId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("VolumeId", String.to_query v.volume_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("volumeId", String.to_json v.volume_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; volume_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "volumeId"))
  }
