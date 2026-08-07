open Aws.BaseTypes

type t =
  { attach_time : DateTime.t option
  ; delete_on_termination : Boolean.t option
  ; status : AttachmentStatus.t option
  ; volume_id : String.t option
  ; associated_resource : String.t option
  ; volume_owner_id : String.t option
  ; operator : OperatorResponse.t option
  ; ebs_card_index : Integer.t option
  }

let make
    ?attach_time
    ?delete_on_termination
    ?status
    ?volume_id
    ?associated_resource
    ?volume_owner_id
    ?operator
    ?ebs_card_index
    () =
  { attach_time
  ; delete_on_termination
  ; status
  ; volume_id
  ; associated_resource
  ; volume_owner_id
  ; operator
  ; ebs_card_index
  }

let parse xml =
  Some
    { attach_time = Aws.Util.option_bind (Aws.Xml.member "attachTime" xml) DateTime.parse
    ; delete_on_termination =
        Aws.Util.option_bind (Aws.Xml.member "deleteOnTermination" xml) Boolean.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "status" xml) AttachmentStatus.parse
    ; volume_id = Aws.Util.option_bind (Aws.Xml.member "volumeId" xml) String.parse
    ; associated_resource =
        Aws.Util.option_bind (Aws.Xml.member "associatedResource" xml) String.parse
    ; volume_owner_id =
        Aws.Util.option_bind (Aws.Xml.member "volumeOwnerId" xml) String.parse
    ; operator =
        Aws.Util.option_bind (Aws.Xml.member "operator" xml) OperatorResponse.parse
    ; ebs_card_index =
        Aws.Util.option_bind (Aws.Xml.member "ebsCardIndex" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ebs_card_index (fun f ->
             Aws.Query.Pair ("EbsCardIndex", Integer.to_query f))
       ; Aws.Util.option_map v.operator (fun f ->
             Aws.Query.Pair ("Operator", OperatorResponse.to_query f))
       ; Aws.Util.option_map v.volume_owner_id (fun f ->
             Aws.Query.Pair ("VolumeOwnerId", String.to_query f))
       ; Aws.Util.option_map v.associated_resource (fun f ->
             Aws.Query.Pair ("AssociatedResource", String.to_query f))
       ; Aws.Util.option_map v.volume_id (fun f ->
             Aws.Query.Pair ("VolumeId", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", AttachmentStatus.to_query f))
       ; Aws.Util.option_map v.delete_on_termination (fun f ->
             Aws.Query.Pair ("DeleteOnTermination", Boolean.to_query f))
       ; Aws.Util.option_map v.attach_time (fun f ->
             Aws.Query.Pair ("AttachTime", DateTime.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ebs_card_index (fun f -> "ebsCardIndex", Integer.to_json f)
       ; Aws.Util.option_map v.operator (fun f -> "operator", OperatorResponse.to_json f)
       ; Aws.Util.option_map v.volume_owner_id (fun f ->
             "volumeOwnerId", String.to_json f)
       ; Aws.Util.option_map v.associated_resource (fun f ->
             "associatedResource", String.to_json f)
       ; Aws.Util.option_map v.volume_id (fun f -> "volumeId", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "status", AttachmentStatus.to_json f)
       ; Aws.Util.option_map v.delete_on_termination (fun f ->
             "deleteOnTermination", Boolean.to_json f)
       ; Aws.Util.option_map v.attach_time (fun f -> "attachTime", DateTime.to_json f)
       ])

let of_json j =
  { attach_time = Aws.Util.option_map (Aws.Json.lookup j "attachTime") DateTime.of_json
  ; delete_on_termination =
      Aws.Util.option_map (Aws.Json.lookup j "deleteOnTermination") Boolean.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") AttachmentStatus.of_json
  ; volume_id = Aws.Util.option_map (Aws.Json.lookup j "volumeId") String.of_json
  ; associated_resource =
      Aws.Util.option_map (Aws.Json.lookup j "associatedResource") String.of_json
  ; volume_owner_id =
      Aws.Util.option_map (Aws.Json.lookup j "volumeOwnerId") String.of_json
  ; operator = Aws.Util.option_map (Aws.Json.lookup j "operator") OperatorResponse.of_json
  ; ebs_card_index =
      Aws.Util.option_map (Aws.Json.lookup j "ebsCardIndex") Integer.of_json
  }
