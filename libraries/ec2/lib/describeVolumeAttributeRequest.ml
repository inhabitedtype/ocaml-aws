open Aws.BaseTypes

type t =
  { attribute : VolumeAttributeName.t
  ; volume_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~attribute ~volume_id ?dry_run () = { attribute; volume_id; dry_run }

let parse xml =
  Some
    { attribute =
        Aws.Xml.required
          "Attribute"
          (Aws.Util.option_bind
             (Aws.Xml.member "Attribute" xml)
             VolumeAttributeName.parse)
    ; volume_id =
        Aws.Xml.required
          "VolumeId"
          (Aws.Util.option_bind (Aws.Xml.member "VolumeId" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("VolumeId", String.to_query v.volume_id))
       ; Some (Aws.Query.Pair ("Attribute", VolumeAttributeName.to_query v.attribute))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some ("VolumeId", String.to_json v.volume_id)
       ; Some ("Attribute", VolumeAttributeName.to_json v.attribute)
       ])

let of_json j =
  { attribute =
      VolumeAttributeName.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Attribute"))
  ; volume_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VolumeId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
