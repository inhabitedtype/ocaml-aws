open Aws.BaseTypes

type t =
  { transit_gateways : TransitGatewayList.t
  ; next_token : String.t option
  }

let make ?(transit_gateways = []) ?next_token () = { transit_gateways; next_token }

let parse xml =
  Some
    { transit_gateways =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "transitGatewaySet" xml)
             TransitGatewayList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TransitGatewaySet", TransitGatewayList.to_query v.transit_gateways))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("transitGatewaySet", TransitGatewayList.to_json v.transit_gateways)
       ])

let of_json j =
  { transit_gateways =
      TransitGatewayList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "transitGatewaySet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
