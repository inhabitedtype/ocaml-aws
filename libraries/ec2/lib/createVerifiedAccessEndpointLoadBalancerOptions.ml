open Aws.BaseTypes

type t =
  { protocol : VerifiedAccessEndpointProtocol.t option
  ; port : Integer.t option
  ; load_balancer_arn : String.t option
  ; subnet_ids : CreateVerifiedAccessEndpointSubnetIdList.t
  ; port_ranges : CreateVerifiedAccessEndpointPortRangeList.t
  }

let make ?protocol ?port ?load_balancer_arn ?(subnet_ids = []) ?(port_ranges = []) () =
  { protocol; port; load_balancer_arn; subnet_ids; port_ranges }

let parse xml =
  Some
    { protocol =
        Aws.Util.option_bind
          (Aws.Xml.member "Protocol" xml)
          VerifiedAccessEndpointProtocol.parse
    ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
    ; load_balancer_arn =
        Aws.Util.option_bind (Aws.Xml.member "LoadBalancerArn" xml) String.parse
    ; subnet_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SubnetId" xml)
             CreateVerifiedAccessEndpointSubnetIdList.parse)
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
       ; Some
           (Aws.Query.Pair
              ("SubnetId", CreateVerifiedAccessEndpointSubnetIdList.to_query v.subnet_ids))
       ; Aws.Util.option_map v.load_balancer_arn (fun f ->
             Aws.Query.Pair ("LoadBalancerArn", String.to_query f))
       ; Aws.Util.option_map v.port (fun f -> Aws.Query.Pair ("Port", Integer.to_query f))
       ; Aws.Util.option_map v.protocol (fun f ->
             Aws.Query.Pair ("Protocol", VerifiedAccessEndpointProtocol.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ("PortRange", CreateVerifiedAccessEndpointPortRangeList.to_json v.port_ranges)
       ; Some ("SubnetId", CreateVerifiedAccessEndpointSubnetIdList.to_json v.subnet_ids)
       ; Aws.Util.option_map v.load_balancer_arn (fun f ->
             "LoadBalancerArn", String.to_json f)
       ; Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
       ; Aws.Util.option_map v.protocol (fun f ->
             "Protocol", VerifiedAccessEndpointProtocol.to_json f)
       ])

let of_json j =
  { protocol =
      Aws.Util.option_map
        (Aws.Json.lookup j "Protocol")
        VerifiedAccessEndpointProtocol.of_json
  ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
  ; load_balancer_arn =
      Aws.Util.option_map (Aws.Json.lookup j "LoadBalancerArn") String.of_json
  ; subnet_ids =
      CreateVerifiedAccessEndpointSubnetIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SubnetId"))
  ; port_ranges =
      CreateVerifiedAccessEndpointPortRangeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PortRange"))
  }
