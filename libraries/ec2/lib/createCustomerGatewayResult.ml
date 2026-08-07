type t = { customer_gateway : CustomerGateway.t option }

let make ?customer_gateway () = { customer_gateway }

let parse xml =
  Some
    { customer_gateway =
        Aws.Util.option_bind (Aws.Xml.member "customerGateway" xml) CustomerGateway.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.customer_gateway (fun f ->
             Aws.Query.Pair ("CustomerGateway", CustomerGateway.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.customer_gateway (fun f ->
             "customerGateway", CustomerGateway.to_json f)
       ])

let of_json j =
  { customer_gateway =
      Aws.Util.option_map (Aws.Json.lookup j "customerGateway") CustomerGateway.of_json
  }
