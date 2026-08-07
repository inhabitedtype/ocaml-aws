open Aws.BaseTypes

type t =
  { multicast_domain_associations : TransitGatewayMulticastDomainAssociationList.t
  ; next_token : String.t option
  }

let make ?(multicast_domain_associations = []) ?next_token () =
  { multicast_domain_associations; next_token }

let parse xml =
  Some
    { multicast_domain_associations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "multicastDomainAssociations" xml)
             TransitGatewayMulticastDomainAssociationList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "MulticastDomainAssociations"
              , TransitGatewayMulticastDomainAssociationList.to_query
                  v.multicast_domain_associations ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "multicastDomainAssociations"
           , TransitGatewayMulticastDomainAssociationList.to_json
               v.multicast_domain_associations )
       ])

let of_json j =
  { multicast_domain_associations =
      TransitGatewayMulticastDomainAssociationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "multicastDomainAssociations"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
