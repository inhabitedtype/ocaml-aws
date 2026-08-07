open Aws.BaseTypes

type t =
  { traffic_mirror_filter_id : String.t
  ; add_network_services : TrafficMirrorNetworkServiceList.t
  ; remove_network_services : TrafficMirrorNetworkServiceList.t
  ; dry_run : Boolean.t option
  }

let make
    ~traffic_mirror_filter_id
    ?(add_network_services = [])
    ?(remove_network_services = [])
    ?dry_run
    () =
  { traffic_mirror_filter_id; add_network_services; remove_network_services; dry_run }

let parse xml =
  Some
    { traffic_mirror_filter_id =
        Aws.Xml.required
          "TrafficMirrorFilterId"
          (Aws.Util.option_bind (Aws.Xml.member "TrafficMirrorFilterId" xml) String.parse)
    ; add_network_services =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AddNetworkService" xml)
             TrafficMirrorNetworkServiceList.parse)
    ; remove_network_services =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RemoveNetworkService" xml)
             TrafficMirrorNetworkServiceList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "RemoveNetworkService"
              , TrafficMirrorNetworkServiceList.to_query v.remove_network_services ))
       ; Some
           (Aws.Query.Pair
              ( "AddNetworkService"
              , TrafficMirrorNetworkServiceList.to_query v.add_network_services ))
       ; Some
           (Aws.Query.Pair
              ("TrafficMirrorFilterId", String.to_query v.traffic_mirror_filter_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some
           ( "RemoveNetworkService"
           , TrafficMirrorNetworkServiceList.to_json v.remove_network_services )
       ; Some
           ( "AddNetworkService"
           , TrafficMirrorNetworkServiceList.to_json v.add_network_services )
       ; Some ("TrafficMirrorFilterId", String.to_json v.traffic_mirror_filter_id)
       ])

let of_json j =
  { traffic_mirror_filter_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficMirrorFilterId"))
  ; add_network_services =
      TrafficMirrorNetworkServiceList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AddNetworkService"))
  ; remove_network_services =
      TrafficMirrorNetworkServiceList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RemoveNetworkService"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
