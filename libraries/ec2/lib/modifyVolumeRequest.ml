open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; volume_id : String.t
  ; size : Integer.t option
  ; volume_type : VolumeType.t option
  ; iops : Integer.t option
  ; throughput : Integer.t option
  ; multi_attach_enabled : Boolean.t option
  }

let make ?dry_run ~volume_id ?size ?volume_type ?iops ?throughput ?multi_attach_enabled ()
    =
  { dry_run; volume_id; size; volume_type; iops; throughput; multi_attach_enabled }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; volume_id =
        Aws.Xml.required
          "VolumeId"
          (Aws.Util.option_bind (Aws.Xml.member "VolumeId" xml) String.parse)
    ; size = Aws.Util.option_bind (Aws.Xml.member "Size" xml) Integer.parse
    ; volume_type =
        Aws.Util.option_bind (Aws.Xml.member "VolumeType" xml) VolumeType.parse
    ; iops = Aws.Util.option_bind (Aws.Xml.member "Iops" xml) Integer.parse
    ; throughput = Aws.Util.option_bind (Aws.Xml.member "Throughput" xml) Integer.parse
    ; multi_attach_enabled =
        Aws.Util.option_bind (Aws.Xml.member "MultiAttachEnabled" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.multi_attach_enabled (fun f ->
             Aws.Query.Pair ("MultiAttachEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.throughput (fun f ->
             Aws.Query.Pair ("Throughput", Integer.to_query f))
       ; Aws.Util.option_map v.iops (fun f -> Aws.Query.Pair ("Iops", Integer.to_query f))
       ; Aws.Util.option_map v.volume_type (fun f ->
             Aws.Query.Pair ("VolumeType", VolumeType.to_query f))
       ; Aws.Util.option_map v.size (fun f -> Aws.Query.Pair ("Size", Integer.to_query f))
       ; Some (Aws.Query.Pair ("VolumeId", String.to_query v.volume_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.multi_attach_enabled (fun f ->
             "MultiAttachEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.throughput (fun f -> "Throughput", Integer.to_json f)
       ; Aws.Util.option_map v.iops (fun f -> "Iops", Integer.to_json f)
       ; Aws.Util.option_map v.volume_type (fun f -> "VolumeType", VolumeType.to_json f)
       ; Aws.Util.option_map v.size (fun f -> "Size", Integer.to_json f)
       ; Some ("VolumeId", String.to_json v.volume_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; volume_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VolumeId"))
  ; size = Aws.Util.option_map (Aws.Json.lookup j "Size") Integer.of_json
  ; volume_type = Aws.Util.option_map (Aws.Json.lookup j "VolumeType") VolumeType.of_json
  ; iops = Aws.Util.option_map (Aws.Json.lookup j "Iops") Integer.of_json
  ; throughput = Aws.Util.option_map (Aws.Json.lookup j "Throughput") Integer.of_json
  ; multi_attach_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "MultiAttachEnabled") Boolean.of_json
  }
