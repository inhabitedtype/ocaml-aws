open Aws.BaseTypes

type t =
  { traffic_mirror_session_id : String.t
  ; traffic_mirror_target_id : String.t option
  ; traffic_mirror_filter_id : String.t option
  ; packet_length : Integer.t option
  ; session_number : Integer.t option
  ; virtual_network_id : Integer.t option
  ; description : String.t option
  ; remove_fields : TrafficMirrorSessionFieldList.t
  ; dry_run : Boolean.t option
  }

let make
    ~traffic_mirror_session_id
    ?traffic_mirror_target_id
    ?traffic_mirror_filter_id
    ?packet_length
    ?session_number
    ?virtual_network_id
    ?description
    ?(remove_fields = [])
    ?dry_run
    () =
  { traffic_mirror_session_id
  ; traffic_mirror_target_id
  ; traffic_mirror_filter_id
  ; packet_length
  ; session_number
  ; virtual_network_id
  ; description
  ; remove_fields
  ; dry_run
  }

let parse xml =
  Some
    { traffic_mirror_session_id =
        Aws.Xml.required
          "TrafficMirrorSessionId"
          (Aws.Util.option_bind
             (Aws.Xml.member "TrafficMirrorSessionId" xml)
             String.parse)
    ; traffic_mirror_target_id =
        Aws.Util.option_bind (Aws.Xml.member "TrafficMirrorTargetId" xml) String.parse
    ; traffic_mirror_filter_id =
        Aws.Util.option_bind (Aws.Xml.member "TrafficMirrorFilterId" xml) String.parse
    ; packet_length =
        Aws.Util.option_bind (Aws.Xml.member "PacketLength" xml) Integer.parse
    ; session_number =
        Aws.Util.option_bind (Aws.Xml.member "SessionNumber" xml) Integer.parse
    ; virtual_network_id =
        Aws.Util.option_bind (Aws.Xml.member "VirtualNetworkId" xml) Integer.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; remove_fields =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RemoveField" xml)
             TrafficMirrorSessionFieldList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("RemoveField", TrafficMirrorSessionFieldList.to_query v.remove_fields))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.virtual_network_id (fun f ->
             Aws.Query.Pair ("VirtualNetworkId", Integer.to_query f))
       ; Aws.Util.option_map v.session_number (fun f ->
             Aws.Query.Pair ("SessionNumber", Integer.to_query f))
       ; Aws.Util.option_map v.packet_length (fun f ->
             Aws.Query.Pair ("PacketLength", Integer.to_query f))
       ; Aws.Util.option_map v.traffic_mirror_filter_id (fun f ->
             Aws.Query.Pair ("TrafficMirrorFilterId", String.to_query f))
       ; Aws.Util.option_map v.traffic_mirror_target_id (fun f ->
             Aws.Query.Pair ("TrafficMirrorTargetId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TrafficMirrorSessionId", String.to_query v.traffic_mirror_session_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("RemoveField", TrafficMirrorSessionFieldList.to_json v.remove_fields)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.virtual_network_id (fun f ->
             "VirtualNetworkId", Integer.to_json f)
       ; Aws.Util.option_map v.session_number (fun f ->
             "SessionNumber", Integer.to_json f)
       ; Aws.Util.option_map v.packet_length (fun f -> "PacketLength", Integer.to_json f)
       ; Aws.Util.option_map v.traffic_mirror_filter_id (fun f ->
             "TrafficMirrorFilterId", String.to_json f)
       ; Aws.Util.option_map v.traffic_mirror_target_id (fun f ->
             "TrafficMirrorTargetId", String.to_json f)
       ; Some ("TrafficMirrorSessionId", String.to_json v.traffic_mirror_session_id)
       ])

let of_json j =
  { traffic_mirror_session_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficMirrorSessionId"))
  ; traffic_mirror_target_id =
      Aws.Util.option_map (Aws.Json.lookup j "TrafficMirrorTargetId") String.of_json
  ; traffic_mirror_filter_id =
      Aws.Util.option_map (Aws.Json.lookup j "TrafficMirrorFilterId") String.of_json
  ; packet_length = Aws.Util.option_map (Aws.Json.lookup j "PacketLength") Integer.of_json
  ; session_number =
      Aws.Util.option_map (Aws.Json.lookup j "SessionNumber") Integer.of_json
  ; virtual_network_id =
      Aws.Util.option_map (Aws.Json.lookup j "VirtualNetworkId") Integer.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; remove_fields =
      TrafficMirrorSessionFieldList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RemoveField"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
