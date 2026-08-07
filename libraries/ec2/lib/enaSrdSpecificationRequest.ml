open Aws.BaseTypes

type t =
  { ena_srd_enabled : Boolean.t option
  ; ena_srd_udp_specification : EnaSrdUdpSpecificationRequest.t option
  }

let make ?ena_srd_enabled ?ena_srd_udp_specification () =
  { ena_srd_enabled; ena_srd_udp_specification }

let parse xml =
  Some
    { ena_srd_enabled =
        Aws.Util.option_bind (Aws.Xml.member "EnaSrdEnabled" xml) Boolean.parse
    ; ena_srd_udp_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "EnaSrdUdpSpecification" xml)
          EnaSrdUdpSpecificationRequest.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ena_srd_udp_specification (fun f ->
             Aws.Query.Pair
               ("EnaSrdUdpSpecification", EnaSrdUdpSpecificationRequest.to_query f))
       ; Aws.Util.option_map v.ena_srd_enabled (fun f ->
             Aws.Query.Pair ("EnaSrdEnabled", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ena_srd_udp_specification (fun f ->
             "EnaSrdUdpSpecification", EnaSrdUdpSpecificationRequest.to_json f)
       ; Aws.Util.option_map v.ena_srd_enabled (fun f ->
             "EnaSrdEnabled", Boolean.to_json f)
       ])

let of_json j =
  { ena_srd_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "EnaSrdEnabled") Boolean.of_json
  ; ena_srd_udp_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "EnaSrdUdpSpecification")
        EnaSrdUdpSpecificationRequest.of_json
  }
