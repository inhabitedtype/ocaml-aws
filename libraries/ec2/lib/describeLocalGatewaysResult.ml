open Aws.BaseTypes

type t =
  { local_gateways : LocalGatewaySet.t
  ; next_token : String.t option
  }

let make ?(local_gateways = []) ?next_token () = { local_gateways; next_token }

let parse xml =
  Some
    { local_gateways =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "localGatewaySet" xml)
             LocalGatewaySet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair ("LocalGatewaySet", LocalGatewaySet.to_query v.local_gateways))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("localGatewaySet", LocalGatewaySet.to_json v.local_gateways)
       ])

let of_json j =
  { local_gateways =
      LocalGatewaySet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "localGatewaySet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
