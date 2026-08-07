open Aws.BaseTypes

type t =
  { attachment : NetworkInterfaceAttachment.t option
  ; description : AttributeValue.t option
  ; groups : GroupIdentifierList.t
  ; network_interface_id : String.t option
  ; source_dest_check : AttributeBooleanValue.t option
  ; associate_public_ip_address : Boolean.t option
  }

let make
    ?attachment
    ?description
    ?(groups = [])
    ?network_interface_id
    ?source_dest_check
    ?associate_public_ip_address
    () =
  { attachment
  ; description
  ; groups
  ; network_interface_id
  ; source_dest_check
  ; associate_public_ip_address
  }

let parse xml =
  Some
    { attachment =
        Aws.Util.option_bind
          (Aws.Xml.member "attachment" xml)
          NetworkInterfaceAttachment.parse
    ; description =
        Aws.Util.option_bind (Aws.Xml.member "description" xml) AttributeValue.parse
    ; groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "groupSet" xml) GroupIdentifierList.parse)
    ; network_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "networkInterfaceId" xml) String.parse
    ; source_dest_check =
        Aws.Util.option_bind
          (Aws.Xml.member "sourceDestCheck" xml)
          AttributeBooleanValue.parse
    ; associate_public_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "associatePublicIpAddress" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.associate_public_ip_address (fun f ->
             Aws.Query.Pair ("AssociatePublicIpAddress", Boolean.to_query f))
       ; Aws.Util.option_map v.source_dest_check (fun f ->
             Aws.Query.Pair ("SourceDestCheck", AttributeBooleanValue.to_query f))
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             Aws.Query.Pair ("NetworkInterfaceId", String.to_query f))
       ; Some (Aws.Query.Pair ("GroupSet", GroupIdentifierList.to_query v.groups))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", AttributeValue.to_query f))
       ; Aws.Util.option_map v.attachment (fun f ->
             Aws.Query.Pair ("Attachment", NetworkInterfaceAttachment.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.associate_public_ip_address (fun f ->
             "associatePublicIpAddress", Boolean.to_json f)
       ; Aws.Util.option_map v.source_dest_check (fun f ->
             "sourceDestCheck", AttributeBooleanValue.to_json f)
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             "networkInterfaceId", String.to_json f)
       ; Some ("groupSet", GroupIdentifierList.to_json v.groups)
       ; Aws.Util.option_map v.description (fun f ->
             "description", AttributeValue.to_json f)
       ; Aws.Util.option_map v.attachment (fun f ->
             "attachment", NetworkInterfaceAttachment.to_json f)
       ])

let of_json j =
  { attachment =
      Aws.Util.option_map
        (Aws.Json.lookup j "attachment")
        NetworkInterfaceAttachment.of_json
  ; description =
      Aws.Util.option_map (Aws.Json.lookup j "description") AttributeValue.of_json
  ; groups =
      GroupIdentifierList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "groupSet"))
  ; network_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "networkInterfaceId") String.of_json
  ; source_dest_check =
      Aws.Util.option_map
        (Aws.Json.lookup j "sourceDestCheck")
        AttributeBooleanValue.of_json
  ; associate_public_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "associatePublicIpAddress") Boolean.of_json
  }
