open Aws.BaseTypes

type t =
  { secondary_subnet : SecondarySubnet.t option
  ; client_token : String.t option
  }

let make ?secondary_subnet ?client_token () = { secondary_subnet; client_token }

let parse xml =
  Some
    { secondary_subnet =
        Aws.Util.option_bind (Aws.Xml.member "secondarySubnet" xml) SecondarySubnet.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "clientToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.secondary_subnet (fun f ->
             Aws.Query.Pair ("SecondarySubnet", SecondarySubnet.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f -> "clientToken", String.to_json f)
       ; Aws.Util.option_map v.secondary_subnet (fun f ->
             "secondarySubnet", SecondarySubnet.to_json f)
       ])

let of_json j =
  { secondary_subnet =
      Aws.Util.option_map (Aws.Json.lookup j "secondarySubnet") SecondarySubnet.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "clientToken") String.of_json
  }
