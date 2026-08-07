open Aws.BaseTypes

type t =
  { client_token : String.t option
  ; egress_only_internet_gateway : EgressOnlyInternetGateway.t option
  }

let make ?client_token ?egress_only_internet_gateway () =
  { client_token; egress_only_internet_gateway }

let parse xml =
  Some
    { client_token = Aws.Util.option_bind (Aws.Xml.member "clientToken" xml) String.parse
    ; egress_only_internet_gateway =
        Aws.Util.option_bind
          (Aws.Xml.member "egressOnlyInternetGateway" xml)
          EgressOnlyInternetGateway.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.egress_only_internet_gateway (fun f ->
             Aws.Query.Pair
               ("EgressOnlyInternetGateway", EgressOnlyInternetGateway.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.egress_only_internet_gateway (fun f ->
             "egressOnlyInternetGateway", EgressOnlyInternetGateway.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "clientToken", String.to_json f)
       ])

let of_json j =
  { client_token = Aws.Util.option_map (Aws.Json.lookup j "clientToken") String.of_json
  ; egress_only_internet_gateway =
      Aws.Util.option_map
        (Aws.Json.lookup j "egressOnlyInternetGateway")
        EgressOnlyInternetGateway.of_json
  }
