open Aws.BaseTypes

type t =
  { network_interface_id : String.t option
  ; protocol : VerifiedAccessEndpointProtocol.t option
  ; port : Integer.t option
  ; port_ranges : VerifiedAccessEndpointPortRangeList.t
  }

let make ?network_interface_id ?protocol ?port ?(port_ranges = []) () =
  { network_interface_id; protocol; port; port_ranges }

let parse xml =
  Some
    { network_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "networkInterfaceId" xml) String.parse
    ; protocol =
        Aws.Util.option_bind
          (Aws.Xml.member "protocol" xml)
          VerifiedAccessEndpointProtocol.parse
    ; port = Aws.Util.option_bind (Aws.Xml.member "port" xml) Integer.parse
    ; port_ranges =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "portRangeSet" xml)
             VerifiedAccessEndpointPortRangeList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("PortRangeSet", VerifiedAccessEndpointPortRangeList.to_query v.port_ranges))
       ; Aws.Util.option_map v.port (fun f -> Aws.Query.Pair ("Port", Integer.to_query f))
       ; Aws.Util.option_map v.protocol (fun f ->
             Aws.Query.Pair ("Protocol", VerifiedAccessEndpointProtocol.to_query f))
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             Aws.Query.Pair ("NetworkInterfaceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("portRangeSet", VerifiedAccessEndpointPortRangeList.to_json v.port_ranges)
       ; Aws.Util.option_map v.port (fun f -> "port", Integer.to_json f)
       ; Aws.Util.option_map v.protocol (fun f ->
             "protocol", VerifiedAccessEndpointProtocol.to_json f)
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             "networkInterfaceId", String.to_json f)
       ])

let of_json j =
  { network_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "networkInterfaceId") String.of_json
  ; protocol =
      Aws.Util.option_map
        (Aws.Json.lookup j "protocol")
        VerifiedAccessEndpointProtocol.of_json
  ; port = Aws.Util.option_map (Aws.Json.lookup j "port") Integer.of_json
  ; port_ranges =
      VerifiedAccessEndpointPortRangeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "portRangeSet"))
  }
