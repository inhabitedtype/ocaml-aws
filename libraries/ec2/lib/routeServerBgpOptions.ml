open Aws.BaseTypes

type t =
  { peer_asn : Long.t option
  ; peer_liveness_detection : RouteServerPeerLivenessMode.t option
  }

let make ?peer_asn ?peer_liveness_detection () = { peer_asn; peer_liveness_detection }

let parse xml =
  Some
    { peer_asn = Aws.Util.option_bind (Aws.Xml.member "peerAsn" xml) Long.parse
    ; peer_liveness_detection =
        Aws.Util.option_bind
          (Aws.Xml.member "peerLivenessDetection" xml)
          RouteServerPeerLivenessMode.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.peer_liveness_detection (fun f ->
             Aws.Query.Pair
               ("PeerLivenessDetection", RouteServerPeerLivenessMode.to_query f))
       ; Aws.Util.option_map v.peer_asn (fun f ->
             Aws.Query.Pair ("PeerAsn", Long.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.peer_liveness_detection (fun f ->
             "peerLivenessDetection", RouteServerPeerLivenessMode.to_json f)
       ; Aws.Util.option_map v.peer_asn (fun f -> "peerAsn", Long.to_json f)
       ])

let of_json j =
  { peer_asn = Aws.Util.option_map (Aws.Json.lookup j "peerAsn") Long.of_json
  ; peer_liveness_detection =
      Aws.Util.option_map
        (Aws.Json.lookup j "peerLivenessDetection")
        RouteServerPeerLivenessMode.of_json
  }
