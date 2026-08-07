open Aws.BaseTypes

type t =
  { associations : TransitGatewayRouteTableAssociationList.t
  ; next_token : String.t option
  }

let make ?(associations = []) ?next_token () = { associations; next_token }

let parse xml =
  Some
    { associations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "associations" xml)
             TransitGatewayRouteTableAssociationList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "Associations"
              , TransitGatewayRouteTableAssociationList.to_query v.associations ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ("associations", TransitGatewayRouteTableAssociationList.to_json v.associations)
       ])

let of_json j =
  { associations =
      TransitGatewayRouteTableAssociationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "associations"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
