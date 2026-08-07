open Aws.BaseTypes

type t =
  { traffic_mirror_session_id : String.t option
  ; traffic_mirror_target_id : String.t option
  ; traffic_mirror_filter_id : String.t option
  ; network_interface_id : String.t option
  ; owner_id : String.t option
  ; packet_length : Integer.t option
  ; session_number : Integer.t option
  ; virtual_network_id : Integer.t option
  ; description : String.t option
  ; tags : TagList.t
  }

let make
    ?traffic_mirror_session_id
    ?traffic_mirror_target_id
    ?traffic_mirror_filter_id
    ?network_interface_id
    ?owner_id
    ?packet_length
    ?session_number
    ?virtual_network_id
    ?description
    ?(tags = [])
    () =
  { traffic_mirror_session_id
  ; traffic_mirror_target_id
  ; traffic_mirror_filter_id
  ; network_interface_id
  ; owner_id
  ; packet_length
  ; session_number
  ; virtual_network_id
  ; description
  ; tags
  }

let parse xml =
  Some
    { traffic_mirror_session_id =
        Aws.Util.option_bind (Aws.Xml.member "trafficMirrorSessionId" xml) String.parse
    ; traffic_mirror_target_id =
        Aws.Util.option_bind (Aws.Xml.member "trafficMirrorTargetId" xml) String.parse
    ; traffic_mirror_filter_id =
        Aws.Util.option_bind (Aws.Xml.member "trafficMirrorFilterId" xml) String.parse
    ; network_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "networkInterfaceId" xml) String.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; packet_length =
        Aws.Util.option_bind (Aws.Xml.member "packetLength" xml) Integer.parse
    ; session_number =
        Aws.Util.option_bind (Aws.Xml.member "sessionNumber" xml) Integer.parse
    ; virtual_network_id =
        Aws.Util.option_bind (Aws.Xml.member "virtualNetworkId" xml) Integer.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.virtual_network_id (fun f ->
             Aws.Query.Pair ("VirtualNetworkId", Integer.to_query f))
       ; Aws.Util.option_map v.session_number (fun f ->
             Aws.Query.Pair ("SessionNumber", Integer.to_query f))
       ; Aws.Util.option_map v.packet_length (fun f ->
             Aws.Query.Pair ("PacketLength", Integer.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             Aws.Query.Pair ("NetworkInterfaceId", String.to_query f))
       ; Aws.Util.option_map v.traffic_mirror_filter_id (fun f ->
             Aws.Query.Pair ("TrafficMirrorFilterId", String.to_query f))
       ; Aws.Util.option_map v.traffic_mirror_target_id (fun f ->
             Aws.Query.Pair ("TrafficMirrorTargetId", String.to_query f))
       ; Aws.Util.option_map v.traffic_mirror_session_id (fun f ->
             Aws.Query.Pair ("TrafficMirrorSessionId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.virtual_network_id (fun f ->
             "virtualNetworkId", Integer.to_json f)
       ; Aws.Util.option_map v.session_number (fun f ->
             "sessionNumber", Integer.to_json f)
       ; Aws.Util.option_map v.packet_length (fun f -> "packetLength", Integer.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             "networkInterfaceId", String.to_json f)
       ; Aws.Util.option_map v.traffic_mirror_filter_id (fun f ->
             "trafficMirrorFilterId", String.to_json f)
       ; Aws.Util.option_map v.traffic_mirror_target_id (fun f ->
             "trafficMirrorTargetId", String.to_json f)
       ; Aws.Util.option_map v.traffic_mirror_session_id (fun f ->
             "trafficMirrorSessionId", String.to_json f)
       ])

let of_json j =
  { traffic_mirror_session_id =
      Aws.Util.option_map (Aws.Json.lookup j "trafficMirrorSessionId") String.of_json
  ; traffic_mirror_target_id =
      Aws.Util.option_map (Aws.Json.lookup j "trafficMirrorTargetId") String.of_json
  ; traffic_mirror_filter_id =
      Aws.Util.option_map (Aws.Json.lookup j "trafficMirrorFilterId") String.of_json
  ; network_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "networkInterfaceId") String.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; packet_length = Aws.Util.option_map (Aws.Json.lookup j "packetLength") Integer.of_json
  ; session_number =
      Aws.Util.option_map (Aws.Json.lookup j "sessionNumber") Integer.of_json
  ; virtual_network_id =
      Aws.Util.option_map (Aws.Json.lookup j "virtualNetworkId") Integer.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
