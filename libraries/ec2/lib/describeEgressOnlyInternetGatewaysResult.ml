open Aws.BaseTypes

type t =
  { egress_only_internet_gateways : EgressOnlyInternetGatewayList.t
  ; next_token : String.t option
  }

let make ?(egress_only_internet_gateways = []) ?next_token () =
  { egress_only_internet_gateways; next_token }

let parse xml =
  Some
    { egress_only_internet_gateways =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "egressOnlyInternetGatewaySet" xml)
             EgressOnlyInternetGatewayList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "EgressOnlyInternetGatewaySet"
              , EgressOnlyInternetGatewayList.to_query v.egress_only_internet_gateways ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "egressOnlyInternetGatewaySet"
           , EgressOnlyInternetGatewayList.to_json v.egress_only_internet_gateways )
       ])

let of_json j =
  { egress_only_internet_gateways =
      EgressOnlyInternetGatewayList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "egressOnlyInternetGatewaySet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
