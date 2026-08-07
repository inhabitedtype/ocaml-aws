open Aws.BaseTypes

type t =
  { cidr : String.t option
  ; port_ranges : VerifiedAccessEndpointPortRangeList.t
  ; protocol : VerifiedAccessEndpointProtocol.t option
  ; subnet_ids : VerifiedAccessEndpointSubnetIdList.t
  }

let make ?cidr ?(port_ranges = []) ?protocol ?(subnet_ids = []) () =
  { cidr; port_ranges; protocol; subnet_ids }

let parse xml =
  Some
    { cidr = Aws.Util.option_bind (Aws.Xml.member "cidr" xml) String.parse
    ; port_ranges =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "portRangeSet" xml)
             VerifiedAccessEndpointPortRangeList.parse)
    ; protocol =
        Aws.Util.option_bind
          (Aws.Xml.member "protocol" xml)
          VerifiedAccessEndpointProtocol.parse
    ; subnet_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "subnetIdSet" xml)
             VerifiedAccessEndpointSubnetIdList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("SubnetIdSet", VerifiedAccessEndpointSubnetIdList.to_query v.subnet_ids))
       ; Aws.Util.option_map v.protocol (fun f ->
             Aws.Query.Pair ("Protocol", VerifiedAccessEndpointProtocol.to_query f))
       ; Some
           (Aws.Query.Pair
              ("PortRangeSet", VerifiedAccessEndpointPortRangeList.to_query v.port_ranges))
       ; Aws.Util.option_map v.cidr (fun f -> Aws.Query.Pair ("Cidr", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("subnetIdSet", VerifiedAccessEndpointSubnetIdList.to_json v.subnet_ids)
       ; Aws.Util.option_map v.protocol (fun f ->
             "protocol", VerifiedAccessEndpointProtocol.to_json f)
       ; Some ("portRangeSet", VerifiedAccessEndpointPortRangeList.to_json v.port_ranges)
       ; Aws.Util.option_map v.cidr (fun f -> "cidr", String.to_json f)
       ])

let of_json j =
  { cidr = Aws.Util.option_map (Aws.Json.lookup j "cidr") String.of_json
  ; port_ranges =
      VerifiedAccessEndpointPortRangeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "portRangeSet"))
  ; protocol =
      Aws.Util.option_map
        (Aws.Json.lookup j "protocol")
        VerifiedAccessEndpointProtocol.of_json
  ; subnet_ids =
      VerifiedAccessEndpointSubnetIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "subnetIdSet"))
  }
