open Aws.BaseTypes

type t =
  { device : String.t option
  ; force : Boolean.t option
  ; instance_id : String.t option
  ; volume_id : String.t
  ; dry_run : Boolean.t option
  }

let make ?device ?force ?instance_id ~volume_id ?dry_run () =
  { device; force; instance_id; volume_id; dry_run }

let parse xml =
  Some
    { device = Aws.Util.option_bind (Aws.Xml.member "Device" xml) String.parse
    ; force = Aws.Util.option_bind (Aws.Xml.member "Force" xml) Boolean.parse
    ; instance_id = Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse
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
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ; Aws.Util.option_map v.force (fun f ->
             Aws.Query.Pair ("Force", Boolean.to_query f))
       ; Aws.Util.option_map v.device (fun f ->
             Aws.Query.Pair ("Device", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some ("VolumeId", String.to_json v.volume_id)
       ; Aws.Util.option_map v.instance_id (fun f -> "InstanceId", String.to_json f)
       ; Aws.Util.option_map v.force (fun f -> "Force", Boolean.to_json f)
       ; Aws.Util.option_map v.device (fun f -> "Device", String.to_json f)
       ])

let of_json j =
  { device = Aws.Util.option_map (Aws.Json.lookup j "Device") String.of_json
  ; force = Aws.Util.option_map (Aws.Json.lookup j "Force") Boolean.of_json
  ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "InstanceId") String.of_json
  ; volume_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VolumeId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
