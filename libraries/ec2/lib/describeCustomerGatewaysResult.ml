type t = { customer_gateways : CustomerGatewayList.t }

let make ?(customer_gateways = []) () = { customer_gateways }

let parse xml =
  Some
    { customer_gateways =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "customerGatewaySet" xml)
             CustomerGatewayList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("CustomerGatewaySet", CustomerGatewayList.to_query v.customer_gateways))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("customerGatewaySet", CustomerGatewayList.to_json v.customer_gateways) ])

let of_json j =
  { customer_gateways =
      CustomerGatewayList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "customerGatewaySet"))
  }
