open Aws.BaseTypes

type t = { ena_srd_udp_enabled : Boolean.t option }

let make ?ena_srd_udp_enabled () = { ena_srd_udp_enabled }

let parse xml =
  Some
    { ena_srd_udp_enabled =
        Aws.Util.option_bind (Aws.Xml.member "EnaSrdUdpEnabled" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ena_srd_udp_enabled (fun f ->
             Aws.Query.Pair ("EnaSrdUdpEnabled", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ena_srd_udp_enabled (fun f ->
             "EnaSrdUdpEnabled", Boolean.to_json f)
       ])

let of_json j =
  { ena_srd_udp_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "EnaSrdUdpEnabled") Boolean.of_json
  }
