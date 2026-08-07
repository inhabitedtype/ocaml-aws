open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; internet_gateway_block_mode : InternetGatewayBlockMode.t
  }

let make ?dry_run ~internet_gateway_block_mode () =
  { dry_run; internet_gateway_block_mode }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; internet_gateway_block_mode =
        Aws.Xml.required
          "InternetGatewayBlockMode"
          (Aws.Util.option_bind
             (Aws.Xml.member "InternetGatewayBlockMode" xml)
             InternetGatewayBlockMode.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "InternetGatewayBlockMode"
              , InternetGatewayBlockMode.to_query v.internet_gateway_block_mode ))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "InternetGatewayBlockMode"
           , InternetGatewayBlockMode.to_json v.internet_gateway_block_mode )
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; internet_gateway_block_mode =
      InternetGatewayBlockMode.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InternetGatewayBlockMode"))
  }
