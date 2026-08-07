type t = { propagation : TransitGatewayPropagation.t option }

let make ?propagation () = { propagation }

let parse xml =
  Some
    { propagation =
        Aws.Util.option_bind
          (Aws.Xml.member "propagation" xml)
          TransitGatewayPropagation.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.propagation (fun f ->
             Aws.Query.Pair ("Propagation", TransitGatewayPropagation.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.propagation (fun f ->
             "propagation", TransitGatewayPropagation.to_json f)
       ])

let of_json j =
  { propagation =
      Aws.Util.option_map
        (Aws.Json.lookup j "propagation")
        TransitGatewayPropagation.of_json
  }
