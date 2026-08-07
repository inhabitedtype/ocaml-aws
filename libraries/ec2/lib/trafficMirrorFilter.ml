open Aws.BaseTypes

type t =
  { traffic_mirror_filter_id : String.t option
  ; ingress_filter_rules : TrafficMirrorFilterRuleList.t
  ; egress_filter_rules : TrafficMirrorFilterRuleList.t
  ; network_services : TrafficMirrorNetworkServiceList.t
  ; description : String.t option
  ; tags : TagList.t
  }

let make
    ?traffic_mirror_filter_id
    ?(ingress_filter_rules = [])
    ?(egress_filter_rules = [])
    ?(network_services = [])
    ?description
    ?(tags = [])
    () =
  { traffic_mirror_filter_id
  ; ingress_filter_rules
  ; egress_filter_rules
  ; network_services
  ; description
  ; tags
  }

let parse xml =
  Some
    { traffic_mirror_filter_id =
        Aws.Util.option_bind (Aws.Xml.member "trafficMirrorFilterId" xml) String.parse
    ; ingress_filter_rules =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ingressFilterRuleSet" xml)
             TrafficMirrorFilterRuleList.parse)
    ; egress_filter_rules =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "egressFilterRuleSet" xml)
             TrafficMirrorFilterRuleList.parse)
    ; network_services =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "networkServiceSet" xml)
             TrafficMirrorNetworkServiceList.parse)
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
       ; Some
           (Aws.Query.Pair
              ( "NetworkServiceSet"
              , TrafficMirrorNetworkServiceList.to_query v.network_services ))
       ; Some
           (Aws.Query.Pair
              ( "EgressFilterRuleSet"
              , TrafficMirrorFilterRuleList.to_query v.egress_filter_rules ))
       ; Some
           (Aws.Query.Pair
              ( "IngressFilterRuleSet"
              , TrafficMirrorFilterRuleList.to_query v.ingress_filter_rules ))
       ; Aws.Util.option_map v.traffic_mirror_filter_id (fun f ->
             Aws.Query.Pair ("TrafficMirrorFilterId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Some
           ( "networkServiceSet"
           , TrafficMirrorNetworkServiceList.to_json v.network_services )
       ; Some
           ( "egressFilterRuleSet"
           , TrafficMirrorFilterRuleList.to_json v.egress_filter_rules )
       ; Some
           ( "ingressFilterRuleSet"
           , TrafficMirrorFilterRuleList.to_json v.ingress_filter_rules )
       ; Aws.Util.option_map v.traffic_mirror_filter_id (fun f ->
             "trafficMirrorFilterId", String.to_json f)
       ])

let of_json j =
  { traffic_mirror_filter_id =
      Aws.Util.option_map (Aws.Json.lookup j "trafficMirrorFilterId") String.of_json
  ; ingress_filter_rules =
      TrafficMirrorFilterRuleList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ingressFilterRuleSet"))
  ; egress_filter_rules =
      TrafficMirrorFilterRuleList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "egressFilterRuleSet"))
  ; network_services =
      TrafficMirrorNetworkServiceList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "networkServiceSet"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
