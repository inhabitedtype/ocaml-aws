open Aws.BaseTypes

type t =
  { source_volume_id : String.t
  ; iops : Integer.t option
  ; size : Integer.t option
  ; volume_type : VolumeType.t option
  ; dry_run : Boolean.t option
  ; tag_specifications : TagSpecificationList.t
  ; multi_attach_enabled : Boolean.t option
  ; throughput : Integer.t option
  ; client_token : String.t option
  }

let make
    ~source_volume_id
    ?iops
    ?size
    ?volume_type
    ?dry_run
    ?(tag_specifications = [])
    ?multi_attach_enabled
    ?throughput
    ?client_token
    () =
  { source_volume_id
  ; iops
  ; size
  ; volume_type
  ; dry_run
  ; tag_specifications
  ; multi_attach_enabled
  ; throughput
  ; client_token
  }

let parse xml =
  Some
    { source_volume_id =
        Aws.Xml.required
          "SourceVolumeId"
          (Aws.Util.option_bind (Aws.Xml.member "SourceVolumeId" xml) String.parse)
    ; iops = Aws.Util.option_bind (Aws.Xml.member "Iops" xml) Integer.parse
    ; size = Aws.Util.option_bind (Aws.Xml.member "Size" xml) Integer.parse
    ; volume_type =
        Aws.Util.option_bind (Aws.Xml.member "VolumeType" xml) VolumeType.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; multi_attach_enabled =
        Aws.Util.option_bind (Aws.Xml.member "MultiAttachEnabled" xml) Boolean.parse
    ; throughput = Aws.Util.option_bind (Aws.Xml.member "Throughput" xml) Integer.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.throughput (fun f ->
             Aws.Query.Pair ("Throughput", Integer.to_query f))
       ; Aws.Util.option_map v.multi_attach_enabled (fun f ->
             Aws.Query.Pair ("MultiAttachEnabled", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.volume_type (fun f ->
             Aws.Query.Pair ("VolumeType", VolumeType.to_query f))
       ; Aws.Util.option_map v.size (fun f -> Aws.Query.Pair ("Size", Integer.to_query f))
       ; Aws.Util.option_map v.iops (fun f -> Aws.Query.Pair ("Iops", Integer.to_query f))
       ; Some (Aws.Query.Pair ("SourceVolumeId", String.to_query v.source_volume_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.throughput (fun f -> "Throughput", Integer.to_json f)
       ; Aws.Util.option_map v.multi_attach_enabled (fun f ->
             "MultiAttachEnabled", Boolean.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.volume_type (fun f -> "VolumeType", VolumeType.to_json f)
       ; Aws.Util.option_map v.size (fun f -> "Size", Integer.to_json f)
       ; Aws.Util.option_map v.iops (fun f -> "Iops", Integer.to_json f)
       ; Some ("SourceVolumeId", String.to_json v.source_volume_id)
       ])

let of_json j =
  { source_volume_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceVolumeId"))
  ; iops = Aws.Util.option_map (Aws.Json.lookup j "Iops") Integer.of_json
  ; size = Aws.Util.option_map (Aws.Json.lookup j "Size") Integer.of_json
  ; volume_type = Aws.Util.option_map (Aws.Json.lookup j "VolumeType") VolumeType.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; multi_attach_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "MultiAttachEnabled") Boolean.of_json
  ; throughput = Aws.Util.option_map (Aws.Json.lookup j "Throughput") Integer.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  }
