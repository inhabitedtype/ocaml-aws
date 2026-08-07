open Aws.BaseTypes

type t =
  { attach_time : DateTime.t option
  ; attachment_id : String.t option
  ; delete_on_termination : Boolean.t option
  ; device_index : Integer.t option
  ; network_card_index : Integer.t option
  ; instance_id : String.t option
  ; instance_owner_id : String.t option
  ; status : AttachmentStatus.t option
  ; ena_srd_specification : AttachmentEnaSrdSpecification.t option
  ; ena_queue_count : Integer.t option
  }

let make
    ?attach_time
    ?attachment_id
    ?delete_on_termination
    ?device_index
    ?network_card_index
    ?instance_id
    ?instance_owner_id
    ?status
    ?ena_srd_specification
    ?ena_queue_count
    () =
  { attach_time
  ; attachment_id
  ; delete_on_termination
  ; device_index
  ; network_card_index
  ; instance_id
  ; instance_owner_id
  ; status
  ; ena_srd_specification
  ; ena_queue_count
  }

let parse xml =
  Some
    { attach_time = Aws.Util.option_bind (Aws.Xml.member "attachTime" xml) DateTime.parse
    ; attachment_id =
        Aws.Util.option_bind (Aws.Xml.member "attachmentId" xml) String.parse
    ; delete_on_termination =
        Aws.Util.option_bind (Aws.Xml.member "deleteOnTermination" xml) Boolean.parse
    ; device_index = Aws.Util.option_bind (Aws.Xml.member "deviceIndex" xml) Integer.parse
    ; network_card_index =
        Aws.Util.option_bind (Aws.Xml.member "networkCardIndex" xml) Integer.parse
    ; instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; instance_owner_id =
        Aws.Util.option_bind (Aws.Xml.member "instanceOwnerId" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "status" xml) AttachmentStatus.parse
    ; ena_srd_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "enaSrdSpecification" xml)
          AttachmentEnaSrdSpecification.parse
    ; ena_queue_count =
        Aws.Util.option_bind (Aws.Xml.member "enaQueueCount" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ena_queue_count (fun f ->
             Aws.Query.Pair ("EnaQueueCount", Integer.to_query f))
       ; Aws.Util.option_map v.ena_srd_specification (fun f ->
             Aws.Query.Pair
               ("EnaSrdSpecification", AttachmentEnaSrdSpecification.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", AttachmentStatus.to_query f))
       ; Aws.Util.option_map v.instance_owner_id (fun f ->
             Aws.Query.Pair ("InstanceOwnerId", String.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ; Aws.Util.option_map v.network_card_index (fun f ->
             Aws.Query.Pair ("NetworkCardIndex", Integer.to_query f))
       ; Aws.Util.option_map v.device_index (fun f ->
             Aws.Query.Pair ("DeviceIndex", Integer.to_query f))
       ; Aws.Util.option_map v.delete_on_termination (fun f ->
             Aws.Query.Pair ("DeleteOnTermination", Boolean.to_query f))
       ; Aws.Util.option_map v.attachment_id (fun f ->
             Aws.Query.Pair ("AttachmentId", String.to_query f))
       ; Aws.Util.option_map v.attach_time (fun f ->
             Aws.Query.Pair ("AttachTime", DateTime.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ena_queue_count (fun f ->
             "enaQueueCount", Integer.to_json f)
       ; Aws.Util.option_map v.ena_srd_specification (fun f ->
             "enaSrdSpecification", AttachmentEnaSrdSpecification.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "status", AttachmentStatus.to_json f)
       ; Aws.Util.option_map v.instance_owner_id (fun f ->
             "instanceOwnerId", String.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ; Aws.Util.option_map v.network_card_index (fun f ->
             "networkCardIndex", Integer.to_json f)
       ; Aws.Util.option_map v.device_index (fun f -> "deviceIndex", Integer.to_json f)
       ; Aws.Util.option_map v.delete_on_termination (fun f ->
             "deleteOnTermination", Boolean.to_json f)
       ; Aws.Util.option_map v.attachment_id (fun f -> "attachmentId", String.to_json f)
       ; Aws.Util.option_map v.attach_time (fun f -> "attachTime", DateTime.to_json f)
       ])

let of_json j =
  { attach_time = Aws.Util.option_map (Aws.Json.lookup j "attachTime") DateTime.of_json
  ; attachment_id = Aws.Util.option_map (Aws.Json.lookup j "attachmentId") String.of_json
  ; delete_on_termination =
      Aws.Util.option_map (Aws.Json.lookup j "deleteOnTermination") Boolean.of_json
  ; device_index = Aws.Util.option_map (Aws.Json.lookup j "deviceIndex") Integer.of_json
  ; network_card_index =
      Aws.Util.option_map (Aws.Json.lookup j "networkCardIndex") Integer.of_json
  ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; instance_owner_id =
      Aws.Util.option_map (Aws.Json.lookup j "instanceOwnerId") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") AttachmentStatus.of_json
  ; ena_srd_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "enaSrdSpecification")
        AttachmentEnaSrdSpecification.of_json
  ; ena_queue_count =
      Aws.Util.option_map (Aws.Json.lookup j "enaQueueCount") Integer.of_json
  }
