open Aws.BaseTypes

type t =
  { auto_enable_i_o : AttributeBooleanValue.t option
  ; volume_id : String.t
  ; dry_run : Boolean.t option
  }

let make ?auto_enable_i_o ~volume_id ?dry_run () = { auto_enable_i_o; volume_id; dry_run }

let parse xml =
  Some
    { auto_enable_i_o =
        Aws.Util.option_bind
          (Aws.Xml.member "AutoEnableIO" xml)
          AttributeBooleanValue.parse
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
       ; Aws.Util.option_map v.auto_enable_i_o (fun f ->
             Aws.Query.Pair ("AutoEnableIO", AttributeBooleanValue.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some ("VolumeId", String.to_json v.volume_id)
       ; Aws.Util.option_map v.auto_enable_i_o (fun f ->
             "AutoEnableIO", AttributeBooleanValue.to_json f)
       ])

let of_json j =
  { auto_enable_i_o =
      Aws.Util.option_map (Aws.Json.lookup j "AutoEnableIO") AttributeBooleanValue.of_json
  ; volume_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VolumeId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
