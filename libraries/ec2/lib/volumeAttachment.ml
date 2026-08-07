open Aws.BaseTypes

type t =
  { delete_on_termination : Boolean.t option
  ; associated_resource : String.t option
  ; instance_owning_service : String.t option
  ; ebs_card_index : Integer.t option
  ; volume_id : String.t option
  ; instance_id : String.t option
  ; device : String.t option
  ; state : VolumeAttachmentState.t option
  ; attach_time : DateTime.t option
  }

let make
    ?delete_on_termination
    ?associated_resource
    ?instance_owning_service
    ?ebs_card_index
    ?volume_id
    ?instance_id
    ?device
    ?state
    ?attach_time
    () =
  { delete_on_termination
  ; associated_resource
  ; instance_owning_service
  ; ebs_card_index
  ; volume_id
  ; instance_id
  ; device
  ; state
  ; attach_time
  }

let parse xml =
  Some
    { delete_on_termination =
        Aws.Util.option_bind (Aws.Xml.member "deleteOnTermination" xml) Boolean.parse
    ; associated_resource =
        Aws.Util.option_bind (Aws.Xml.member "associatedResource" xml) String.parse
    ; instance_owning_service =
        Aws.Util.option_bind (Aws.Xml.member "instanceOwningService" xml) String.parse
    ; ebs_card_index =
        Aws.Util.option_bind (Aws.Xml.member "ebsCardIndex" xml) Integer.parse
    ; volume_id = Aws.Util.option_bind (Aws.Xml.member "volumeId" xml) String.parse
    ; instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; device = Aws.Util.option_bind (Aws.Xml.member "device" xml) String.parse
    ; state =
        Aws.Util.option_bind (Aws.Xml.member "status" xml) VolumeAttachmentState.parse
    ; attach_time = Aws.Util.option_bind (Aws.Xml.member "attachTime" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.attach_time (fun f ->
             Aws.Query.Pair ("AttachTime", DateTime.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("Status", VolumeAttachmentState.to_query f))
       ; Aws.Util.option_map v.device (fun f ->
             Aws.Query.Pair ("Device", String.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ; Aws.Util.option_map v.volume_id (fun f ->
             Aws.Query.Pair ("VolumeId", String.to_query f))
       ; Aws.Util.option_map v.ebs_card_index (fun f ->
             Aws.Query.Pair ("EbsCardIndex", Integer.to_query f))
       ; Aws.Util.option_map v.instance_owning_service (fun f ->
             Aws.Query.Pair ("InstanceOwningService", String.to_query f))
       ; Aws.Util.option_map v.associated_resource (fun f ->
             Aws.Query.Pair ("AssociatedResource", String.to_query f))
       ; Aws.Util.option_map v.delete_on_termination (fun f ->
             Aws.Query.Pair ("DeleteOnTermination", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.attach_time (fun f -> "attachTime", DateTime.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "status", VolumeAttachmentState.to_json f)
       ; Aws.Util.option_map v.device (fun f -> "device", String.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ; Aws.Util.option_map v.volume_id (fun f -> "volumeId", String.to_json f)
       ; Aws.Util.option_map v.ebs_card_index (fun f -> "ebsCardIndex", Integer.to_json f)
       ; Aws.Util.option_map v.instance_owning_service (fun f ->
             "instanceOwningService", String.to_json f)
       ; Aws.Util.option_map v.associated_resource (fun f ->
             "associatedResource", String.to_json f)
       ; Aws.Util.option_map v.delete_on_termination (fun f ->
             "deleteOnTermination", Boolean.to_json f)
       ])

let of_json j =
  { delete_on_termination =
      Aws.Util.option_map (Aws.Json.lookup j "deleteOnTermination") Boolean.of_json
  ; associated_resource =
      Aws.Util.option_map (Aws.Json.lookup j "associatedResource") String.of_json
  ; instance_owning_service =
      Aws.Util.option_map (Aws.Json.lookup j "instanceOwningService") String.of_json
  ; ebs_card_index =
      Aws.Util.option_map (Aws.Json.lookup j "ebsCardIndex") Integer.of_json
  ; volume_id = Aws.Util.option_map (Aws.Json.lookup j "volumeId") String.of_json
  ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; device = Aws.Util.option_map (Aws.Json.lookup j "device") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "status") VolumeAttachmentState.of_json
  ; attach_time = Aws.Util.option_map (Aws.Json.lookup j "attachTime") DateTime.of_json
  }
