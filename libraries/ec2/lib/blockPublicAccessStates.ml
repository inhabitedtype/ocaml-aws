type t = { internet_gateway_block_mode : BlockPublicAccessMode.t option }

let make ?internet_gateway_block_mode () = { internet_gateway_block_mode }

let parse xml =
  Some
    { internet_gateway_block_mode =
        Aws.Util.option_bind
          (Aws.Xml.member "internetGatewayBlockMode" xml)
          BlockPublicAccessMode.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.internet_gateway_block_mode (fun f ->
             Aws.Query.Pair ("InternetGatewayBlockMode", BlockPublicAccessMode.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.internet_gateway_block_mode (fun f ->
             "internetGatewayBlockMode", BlockPublicAccessMode.to_json f)
       ])

let of_json j =
  { internet_gateway_block_mode =
      Aws.Util.option_map
        (Aws.Json.lookup j "internetGatewayBlockMode")
        BlockPublicAccessMode.of_json
  }
