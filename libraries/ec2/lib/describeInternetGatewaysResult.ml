open Aws.BaseTypes

type t =
  { internet_gateways : InternetGatewayList.t
  ; next_token : String.t option
  }

let make ?(internet_gateways = []) ?next_token () = { internet_gateways; next_token }

let parse xml =
  Some
    { internet_gateways =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "internetGatewaySet" xml)
             InternetGatewayList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("InternetGatewaySet", InternetGatewayList.to_query v.internet_gateways))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("internetGatewaySet", InternetGatewayList.to_json v.internet_gateways)
       ])

let of_json j =
  { internet_gateways =
      InternetGatewayList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "internetGatewaySet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
