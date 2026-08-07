open Aws.BaseTypes

type t =
  { device : String.t
  ; instance_id : String.t
  ; volume_id : String.t
  ; ebs_card_index : Integer.t option
  ; dry_run : Boolean.t option
  }

let make ~device ~instance_id ~volume_id ?ebs_card_index ?dry_run () =
  { device; instance_id; volume_id; ebs_card_index; dry_run }

let parse xml =
  Some
    { device =
        Aws.Xml.required
          "Device"
          (Aws.Util.option_bind (Aws.Xml.member "Device" xml) String.parse)
    ; instance_id =
        Aws.Xml.required
          "InstanceId"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
    ; volume_id =
        Aws.Xml.required
          "VolumeId"
          (Aws.Util.option_bind (Aws.Xml.member "VolumeId" xml) String.parse)
    ; ebs_card_index =
        Aws.Util.option_bind (Aws.Xml.member "EbsCardIndex" xml) Integer.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.ebs_card_index (fun f ->
             Aws.Query.Pair ("EbsCardIndex", Integer.to_query f))
       ; Some (Aws.Query.Pair ("VolumeId", String.to_query v.volume_id))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ; Some (Aws.Query.Pair ("Device", String.to_query v.device))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.ebs_card_index (fun f -> "EbsCardIndex", Integer.to_json f)
       ; Some ("VolumeId", String.to_json v.volume_id)
       ; Some ("InstanceId", String.to_json v.instance_id)
       ; Some ("Device", String.to_json v.device)
       ])

let of_json j =
  { device = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Device"))
  ; instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  ; volume_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VolumeId"))
  ; ebs_card_index =
      Aws.Util.option_map (Aws.Json.lookup j "EbsCardIndex") Integer.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
