open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; egress_only_internet_gateway_id : String.t
  }

let make ?dry_run ~egress_only_internet_gateway_id () =
  { dry_run; egress_only_internet_gateway_id }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; egress_only_internet_gateway_id =
        Aws.Xml.required
          "EgressOnlyInternetGatewayId"
          (Aws.Util.option_bind
             (Aws.Xml.member "EgressOnlyInternetGatewayId" xml)
             String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "EgressOnlyInternetGatewayId"
              , String.to_query v.egress_only_internet_gateway_id ))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "EgressOnlyInternetGatewayId"
           , String.to_json v.egress_only_internet_gateway_id )
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; egress_only_internet_gateway_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "EgressOnlyInternetGatewayId"))
  }
