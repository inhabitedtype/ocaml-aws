open Aws.BaseTypes

type t =
  { attach_time : DateTime.t option
  ; attachment_id : String.t option
  ; delete_on_termination : Boolean.t option
  ; device_index : Integer.t option
  ; status : AttachmentStatus.t option
  ; network_card_index : Integer.t option
  }

let make
    ?attach_time
    ?attachment_id
    ?delete_on_termination
    ?device_index
    ?status
    ?network_card_index
    () =
  { attach_time
  ; attachment_id
  ; delete_on_termination
  ; device_index
  ; status
  ; network_card_index
  }

let parse xml =
  Some
    { attach_time = Aws.Util.option_bind (Aws.Xml.member "attachTime" xml) DateTime.parse
    ; attachment_id =
        Aws.Util.option_bind (Aws.Xml.member "attachmentId" xml) String.parse
    ; delete_on_termination =
        Aws.Util.option_bind (Aws.Xml.member "deleteOnTermination" xml) Boolean.parse
    ; device_index = Aws.Util.option_bind (Aws.Xml.member "deviceIndex" xml) Integer.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "status" xml) AttachmentStatus.parse
    ; network_card_index =
        Aws.Util.option_bind (Aws.Xml.member "networkCardIndex" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_card_index (fun f ->
             Aws.Query.Pair ("NetworkCardIndex", Integer.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", AttachmentStatus.to_query f))
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
       [ Aws.Util.option_map v.network_card_index (fun f ->
             "networkCardIndex", Integer.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "status", AttachmentStatus.to_json f)
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
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") AttachmentStatus.of_json
  ; network_card_index =
      Aws.Util.option_map (Aws.Json.lookup j "networkCardIndex") Integer.of_json
  }
