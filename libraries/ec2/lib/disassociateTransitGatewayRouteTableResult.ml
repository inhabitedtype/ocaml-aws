type t = { association : TransitGatewayAssociation.t option }

let make ?association () = { association }

let parse xml =
  Some
    { association =
        Aws.Util.option_bind
          (Aws.Xml.member "association" xml)
          TransitGatewayAssociation.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.association (fun f ->
             Aws.Query.Pair ("Association", TransitGatewayAssociation.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.association (fun f ->
             "association", TransitGatewayAssociation.to_json f)
       ])

let of_json j =
  { association =
      Aws.Util.option_map
        (Aws.Json.lookup j "association")
        TransitGatewayAssociation.of_json
  }
