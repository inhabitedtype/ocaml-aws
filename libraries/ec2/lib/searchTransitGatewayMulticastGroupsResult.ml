open Aws.BaseTypes

type t =
  { multicast_groups : TransitGatewayMulticastGroupList.t
  ; next_token : String.t option
  }

let make ?(multicast_groups = []) ?next_token () = { multicast_groups; next_token }

let parse xml =
  Some
    { multicast_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "multicastGroups" xml)
             TransitGatewayMulticastGroupList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "MulticastGroups"
              , TransitGatewayMulticastGroupList.to_query v.multicast_groups ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ("multicastGroups", TransitGatewayMulticastGroupList.to_json v.multicast_groups)
       ])

let of_json j =
  { multicast_groups =
      TransitGatewayMulticastGroupList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "multicastGroups"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
