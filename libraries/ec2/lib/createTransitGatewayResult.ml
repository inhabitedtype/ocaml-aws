type t = { transit_gateway : TransitGateway.t option }

let make ?transit_gateway () = { transit_gateway }

let parse xml =
  Some
    { transit_gateway =
        Aws.Util.option_bind (Aws.Xml.member "transitGateway" xml) TransitGateway.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway (fun f ->
             Aws.Query.Pair ("TransitGateway", TransitGateway.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway (fun f ->
             "transitGateway", TransitGateway.to_json f)
       ])

let of_json j =
  { transit_gateway =
      Aws.Util.option_map (Aws.Json.lookup j "transitGateway") TransitGateway.of_json
  }
