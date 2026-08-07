open Aws.BaseTypes

type t =
  { subnet_id : String.t option
  ; state : TransitGatewayMulitcastDomainAssociationState.t option
  }

let make ?subnet_id ?state () = { subnet_id; state }

let parse xml =
  Some
    { subnet_id = Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          TransitGatewayMulitcastDomainAssociationState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair
               ("State", TransitGatewayMulitcastDomainAssociationState.to_query f))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             "state", TransitGatewayMulitcastDomainAssociationState.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "subnetId", String.to_json f)
       ])

let of_json j =
  { subnet_id = Aws.Util.option_map (Aws.Json.lookup j "subnetId") String.of_json
  ; state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        TransitGatewayMulitcastDomainAssociationState.of_json
  }
