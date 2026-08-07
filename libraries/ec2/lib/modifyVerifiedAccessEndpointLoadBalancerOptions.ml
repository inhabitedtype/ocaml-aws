open Aws.BaseTypes

type t =
  { subnet_ids : ModifyVerifiedAccessEndpointSubnetIdList.t
  ; protocol : VerifiedAccessEndpointProtocol.t option
  ; port : Integer.t option
  ; port_ranges : ModifyVerifiedAccessEndpointPortRangeList.t
  }

let make ?(subnet_ids = []) ?protocol ?port ?(port_ranges = []) () =
  { subnet_ids; protocol; port; port_ranges }

let parse xml =
  Some
    { subnet_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SubnetId" xml)
             ModifyVerifiedAccessEndpointSubnetIdList.parse)
    ; protocol =
        Aws.Util.option_bind
          (Aws.Xml.member "Protocol" xml)
          VerifiedAccessEndpointProtocol.parse
    ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
    ; port_ranges =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "PortRange" xml)
             ModifyVerifiedAccessEndpointPortRangeList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "PortRange"
              , ModifyVerifiedAccessEndpointPortRangeList.to_query v.port_ranges ))
       ; Aws.Util.option_map v.port (fun f -> Aws.Query.Pair ("Port", Integer.to_query f))
       ; Aws.Util.option_map v.protocol (fun f ->
             Aws.Query.Pair ("Protocol", VerifiedAccessEndpointProtocol.to_query f))
       ; Some
           (Aws.Query.Pair
              ("SubnetId", ModifyVerifiedAccessEndpointSubnetIdList.to_query v.subnet_ids))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ("PortRange", ModifyVerifiedAccessEndpointPortRangeList.to_json v.port_ranges)
       ; Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
       ; Aws.Util.option_map v.protocol (fun f ->
             "Protocol", VerifiedAccessEndpointProtocol.to_json f)
       ; Some ("SubnetId", ModifyVerifiedAccessEndpointSubnetIdList.to_json v.subnet_ids)
       ])

let of_json j =
  { subnet_ids =
      ModifyVerifiedAccessEndpointSubnetIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SubnetId"))
  ; protocol =
      Aws.Util.option_map
        (Aws.Json.lookup j "Protocol")
        VerifiedAccessEndpointProtocol.of_json
  ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
  ; port_ranges =
      ModifyVerifiedAccessEndpointPortRangeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PortRange"))
  }
