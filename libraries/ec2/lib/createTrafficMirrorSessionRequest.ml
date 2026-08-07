open Aws.BaseTypes

type t =
  { network_interface_id : String.t
  ; traffic_mirror_target_id : String.t
  ; traffic_mirror_filter_id : String.t
  ; packet_length : Integer.t option
  ; session_number : Integer.t
  ; virtual_network_id : Integer.t option
  ; description : String.t option
  ; tag_specifications : TagSpecificationList.t
  ; dry_run : Boolean.t option
  ; client_token : String.t option
  }

let make
    ~network_interface_id
    ~traffic_mirror_target_id
    ~traffic_mirror_filter_id
    ?packet_length
    ~session_number
    ?virtual_network_id
    ?description
    ?(tag_specifications = [])
    ?dry_run
    ?client_token
    () =
  { network_interface_id
  ; traffic_mirror_target_id
  ; traffic_mirror_filter_id
  ; packet_length
  ; session_number
  ; virtual_network_id
  ; description
  ; tag_specifications
  ; dry_run
  ; client_token
  }

let parse xml =
  Some
    { network_interface_id =
        Aws.Xml.required
          "NetworkInterfaceId"
          (Aws.Util.option_bind (Aws.Xml.member "NetworkInterfaceId" xml) String.parse)
    ; traffic_mirror_target_id =
        Aws.Xml.required
          "TrafficMirrorTargetId"
          (Aws.Util.option_bind (Aws.Xml.member "TrafficMirrorTargetId" xml) String.parse)
    ; traffic_mirror_filter_id =
        Aws.Xml.required
          "TrafficMirrorFilterId"
          (Aws.Util.option_bind (Aws.Xml.member "TrafficMirrorFilterId" xml) String.parse)
    ; packet_length =
        Aws.Util.option_bind (Aws.Xml.member "PacketLength" xml) Integer.parse
    ; session_number =
        Aws.Xml.required
          "SessionNumber"
          (Aws.Util.option_bind (Aws.Xml.member "SessionNumber" xml) Integer.parse)
    ; virtual_network_id =
        Aws.Util.option_bind (Aws.Xml.member "VirtualNetworkId" xml) Integer.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.virtual_network_id (fun f ->
             Aws.Query.Pair ("VirtualNetworkId", Integer.to_query f))
       ; Some (Aws.Query.Pair ("SessionNumber", Integer.to_query v.session_number))
       ; Aws.Util.option_map v.packet_length (fun f ->
             Aws.Query.Pair ("PacketLength", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TrafficMirrorFilterId", String.to_query v.traffic_mirror_filter_id))
       ; Some
           (Aws.Query.Pair
              ("TrafficMirrorTargetId", String.to_query v.traffic_mirror_target_id))
       ; Some
           (Aws.Query.Pair ("NetworkInterfaceId", String.to_query v.network_interface_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.virtual_network_id (fun f ->
             "VirtualNetworkId", Integer.to_json f)
       ; Some ("SessionNumber", Integer.to_json v.session_number)
       ; Aws.Util.option_map v.packet_length (fun f -> "PacketLength", Integer.to_json f)
       ; Some ("TrafficMirrorFilterId", String.to_json v.traffic_mirror_filter_id)
       ; Some ("TrafficMirrorTargetId", String.to_json v.traffic_mirror_target_id)
       ; Some ("NetworkInterfaceId", String.to_json v.network_interface_id)
       ])

let of_json j =
  { network_interface_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NetworkInterfaceId"))
  ; traffic_mirror_target_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficMirrorTargetId"))
  ; traffic_mirror_filter_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficMirrorFilterId"))
  ; packet_length = Aws.Util.option_map (Aws.Json.lookup j "PacketLength") Integer.of_json
  ; session_number =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SessionNumber"))
  ; virtual_network_id =
      Aws.Util.option_map (Aws.Json.lookup j "VirtualNetworkId") Integer.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  }
