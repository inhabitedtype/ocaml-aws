open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; internet_gateway_id : String.t
  ; vpc_id : String.t
  }

let make ?dry_run ~internet_gateway_id ~vpc_id () =
  { dry_run; internet_gateway_id; vpc_id }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; internet_gateway_id =
        Aws.Xml.required
          "internetGatewayId"
          (Aws.Util.option_bind (Aws.Xml.member "internetGatewayId" xml) String.parse)
    ; vpc_id =
        Aws.Xml.required
          "vpcId"
          (Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("VpcId", String.to_query v.vpc_id))
       ; Some
           (Aws.Query.Pair ("InternetGatewayId", String.to_query v.internet_gateway_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("vpcId", String.to_json v.vpc_id)
       ; Some ("internetGatewayId", String.to_json v.internet_gateway_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; internet_gateway_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "internetGatewayId"))
  ; vpc_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "vpcId"))
  }
