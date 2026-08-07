open Aws.BaseTypes

type t =
  { peer_asn : Long.t
  ; peer_liveness_detection : RouteServerPeerLivenessMode.t option
  }

let make ~peer_asn ?peer_liveness_detection () = { peer_asn; peer_liveness_detection }

let parse xml =
  Some
    { peer_asn =
        Aws.Xml.required
          "PeerAsn"
          (Aws.Util.option_bind (Aws.Xml.member "PeerAsn" xml) Long.parse)
    ; peer_liveness_detection =
        Aws.Util.option_bind
          (Aws.Xml.member "PeerLivenessDetection" xml)
          RouteServerPeerLivenessMode.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.peer_liveness_detection (fun f ->
             Aws.Query.Pair
               ("PeerLivenessDetection", RouteServerPeerLivenessMode.to_query f))
       ; Some (Aws.Query.Pair ("PeerAsn", Long.to_query v.peer_asn))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.peer_liveness_detection (fun f ->
             "PeerLivenessDetection", RouteServerPeerLivenessMode.to_json f)
       ; Some ("PeerAsn", Long.to_json v.peer_asn)
       ])

let of_json j =
  { peer_asn = Long.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PeerAsn"))
  ; peer_liveness_detection =
      Aws.Util.option_map
        (Aws.Json.lookup j "PeerLivenessDetection")
        RouteServerPeerLivenessMode.of_json
  }
