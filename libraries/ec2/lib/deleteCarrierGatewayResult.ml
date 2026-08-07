type t = { carrier_gateway : CarrierGateway.t option }

let make ?carrier_gateway () = { carrier_gateway }

let parse xml =
  Some
    { carrier_gateway =
        Aws.Util.option_bind (Aws.Xml.member "carrierGateway" xml) CarrierGateway.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.carrier_gateway (fun f ->
             Aws.Query.Pair ("CarrierGateway", CarrierGateway.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.carrier_gateway (fun f ->
             "carrierGateway", CarrierGateway.to_json f)
       ])

let of_json j =
  { carrier_gateway =
      Aws.Util.option_map (Aws.Json.lookup j "carrierGateway") CarrierGateway.of_json
  }
