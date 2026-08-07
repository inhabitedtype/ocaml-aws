open Aws.BaseTypes

type t = { peer_asn : Long.t option }

let make ?peer_asn () = { peer_asn }

let parse xml =
  Some { peer_asn = Aws.Util.option_bind (Aws.Xml.member "PeerAsn" xml) Long.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.peer_asn (fun f ->
             Aws.Query.Pair ("PeerAsn", Long.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.peer_asn (fun f -> "PeerAsn", Long.to_json f) ])

let of_json j =
  { peer_asn = Aws.Util.option_map (Aws.Json.lookup j "PeerAsn") Long.of_json }
