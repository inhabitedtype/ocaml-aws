open Aws.BaseTypes

type t =
  { vpc_endpoint : VpcEndpoint.t option
  ; client_token : String.t option
  }

let make ?vpc_endpoint ?client_token () = { vpc_endpoint; client_token }

let parse xml =
  Some
    { vpc_endpoint =
        Aws.Util.option_bind (Aws.Xml.member "vpcEndpoint" xml) VpcEndpoint.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "clientToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.vpc_endpoint (fun f ->
             Aws.Query.Pair ("VpcEndpoint", VpcEndpoint.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f -> "clientToken", String.to_json f)
       ; Aws.Util.option_map v.vpc_endpoint (fun f ->
             "vpcEndpoint", VpcEndpoint.to_json f)
       ])

let of_json j =
  { vpc_endpoint =
      Aws.Util.option_map (Aws.Json.lookup j "vpcEndpoint") VpcEndpoint.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "clientToken") String.of_json
  }
