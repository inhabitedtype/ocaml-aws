open Aws.BaseTypes

type t =
  { protocol : VerifiedAccessEndpointProtocol.t option
  ; port : Integer.t option
  ; load_balancer_arn : String.t option
  ; subnet_ids : VerifiedAccessEndpointSubnetIdList.t
  ; port_ranges : VerifiedAccessEndpointPortRangeList.t
  }

let make ?protocol ?port ?load_balancer_arn ?(subnet_ids = []) ?(port_ranges = []) () =
  { protocol; port; load_balancer_arn; subnet_ids; port_ranges }

let parse xml =
  Some
    { protocol =
        Aws.Util.option_bind
          (Aws.Xml.member "protocol" xml)
          VerifiedAccessEndpointProtocol.parse
    ; port = Aws.Util.option_bind (Aws.Xml.member "port" xml) Integer.parse
    ; load_balancer_arn =
        Aws.Util.option_bind (Aws.Xml.member "loadBalancerArn" xml) String.parse
    ; subnet_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "subnetIdSet" xml)
             VerifiedAccessEndpointSubnetIdList.parse)
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
       ; Some
           (Aws.Query.Pair
              ("SubnetIdSet", VerifiedAccessEndpointSubnetIdList.to_query v.subnet_ids))
       ; Aws.Util.option_map v.load_balancer_arn (fun f ->
             Aws.Query.Pair ("LoadBalancerArn", String.to_query f))
       ; Aws.Util.option_map v.port (fun f -> Aws.Query.Pair ("Port", Integer.to_query f))
       ; Aws.Util.option_map v.protocol (fun f ->
             Aws.Query.Pair ("Protocol", VerifiedAccessEndpointProtocol.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("portRangeSet", VerifiedAccessEndpointPortRangeList.to_json v.port_ranges)
       ; Some ("subnetIdSet", VerifiedAccessEndpointSubnetIdList.to_json v.subnet_ids)
       ; Aws.Util.option_map v.load_balancer_arn (fun f ->
             "loadBalancerArn", String.to_json f)
       ; Aws.Util.option_map v.port (fun f -> "port", Integer.to_json f)
       ; Aws.Util.option_map v.protocol (fun f ->
             "protocol", VerifiedAccessEndpointProtocol.to_json f)
       ])

let of_json j =
  { protocol =
      Aws.Util.option_map
        (Aws.Json.lookup j "protocol")
        VerifiedAccessEndpointProtocol.of_json
  ; port = Aws.Util.option_map (Aws.Json.lookup j "port") Integer.of_json
  ; load_balancer_arn =
      Aws.Util.option_map (Aws.Json.lookup j "loadBalancerArn") String.of_json
  ; subnet_ids =
      VerifiedAccessEndpointSubnetIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "subnetIdSet"))
  ; port_ranges =
      VerifiedAccessEndpointPortRangeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "portRangeSet"))
  }
