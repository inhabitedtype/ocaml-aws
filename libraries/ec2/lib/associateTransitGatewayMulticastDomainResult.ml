type t = { associations : TransitGatewayMulticastDomainAssociations.t option }

let make ?associations () = { associations }

let parse xml =
  Some
    { associations =
        Aws.Util.option_bind
          (Aws.Xml.member "associations" xml)
          TransitGatewayMulticastDomainAssociations.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.associations (fun f ->
             Aws.Query.Pair
               ("Associations", TransitGatewayMulticastDomainAssociations.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.associations (fun f ->
             "associations", TransitGatewayMulticastDomainAssociations.to_json f)
       ])

let of_json j =
  { associations =
      Aws.Util.option_map
        (Aws.Json.lookup j "associations")
        TransitGatewayMulticastDomainAssociations.of_json
  }
