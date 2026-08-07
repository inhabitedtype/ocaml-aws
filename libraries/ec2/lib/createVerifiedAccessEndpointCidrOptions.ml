open Aws.BaseTypes

type t =
  { protocol : VerifiedAccessEndpointProtocol.t option
  ; subnet_ids : CreateVerifiedAccessEndpointSubnetIdList.t
  ; cidr : String.t option
  ; port_ranges : CreateVerifiedAccessEndpointPortRangeList.t
  }

let make ?protocol ?(subnet_ids = []) ?cidr ?(port_ranges = []) () =
  { protocol; subnet_ids; cidr; port_ranges }

let parse xml =
  Some
    { protocol =
        Aws.Util.option_bind
          (Aws.Xml.member "Protocol" xml)
          VerifiedAccessEndpointProtocol.parse
    ; subnet_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SubnetId" xml)
             CreateVerifiedAccessEndpointSubnetIdList.parse)
    ; cidr = Aws.Util.option_bind (Aws.Xml.member "Cidr" xml) String.parse
    ; port_ranges =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "PortRange" xml)
             CreateVerifiedAccessEndpointPortRangeList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "PortRange"
              , CreateVerifiedAccessEndpointPortRangeList.to_query v.port_ranges ))
       ; Aws.Util.option_map v.cidr (fun f -> Aws.Query.Pair ("Cidr", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("SubnetId", CreateVerifiedAccessEndpointSubnetIdList.to_query v.subnet_ids))
       ; Aws.Util.option_map v.protocol (fun f ->
             Aws.Query.Pair ("Protocol", VerifiedAccessEndpointProtocol.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ("PortRange", CreateVerifiedAccessEndpointPortRangeList.to_json v.port_ranges)
       ; Aws.Util.option_map v.cidr (fun f -> "Cidr", String.to_json f)
       ; Some ("SubnetId", CreateVerifiedAccessEndpointSubnetIdList.to_json v.subnet_ids)
       ; Aws.Util.option_map v.protocol (fun f ->
             "Protocol", VerifiedAccessEndpointProtocol.to_json f)
       ])

let of_json j =
  { protocol =
      Aws.Util.option_map
        (Aws.Json.lookup j "Protocol")
        VerifiedAccessEndpointProtocol.of_json
  ; subnet_ids =
      CreateVerifiedAccessEndpointSubnetIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SubnetId"))
  ; cidr = Aws.Util.option_map (Aws.Json.lookup j "Cidr") String.of_json
  ; port_ranges =
      CreateVerifiedAccessEndpointPortRangeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PortRange"))
  }
