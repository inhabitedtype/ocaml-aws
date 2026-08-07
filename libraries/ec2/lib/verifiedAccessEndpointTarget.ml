open Aws.BaseTypes

type t =
  { verified_access_endpoint_id : String.t option
  ; verified_access_endpoint_target_ip_address : String.t option
  ; verified_access_endpoint_target_dns : String.t option
  }

let make
    ?verified_access_endpoint_id
    ?verified_access_endpoint_target_ip_address
    ?verified_access_endpoint_target_dns
    () =
  { verified_access_endpoint_id
  ; verified_access_endpoint_target_ip_address
  ; verified_access_endpoint_target_dns
  }

let parse xml =
  Some
    { verified_access_endpoint_id =
        Aws.Util.option_bind (Aws.Xml.member "verifiedAccessEndpointId" xml) String.parse
    ; verified_access_endpoint_target_ip_address =
        Aws.Util.option_bind
          (Aws.Xml.member "verifiedAccessEndpointTargetIpAddress" xml)
          String.parse
    ; verified_access_endpoint_target_dns =
        Aws.Util.option_bind
          (Aws.Xml.member "verifiedAccessEndpointTargetDns" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.verified_access_endpoint_target_dns (fun f ->
             Aws.Query.Pair ("VerifiedAccessEndpointTargetDns", String.to_query f))
       ; Aws.Util.option_map v.verified_access_endpoint_target_ip_address (fun f ->
             Aws.Query.Pair ("VerifiedAccessEndpointTargetIpAddress", String.to_query f))
       ; Aws.Util.option_map v.verified_access_endpoint_id (fun f ->
             Aws.Query.Pair ("VerifiedAccessEndpointId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.verified_access_endpoint_target_dns (fun f ->
             "verifiedAccessEndpointTargetDns", String.to_json f)
       ; Aws.Util.option_map v.verified_access_endpoint_target_ip_address (fun f ->
             "verifiedAccessEndpointTargetIpAddress", String.to_json f)
       ; Aws.Util.option_map v.verified_access_endpoint_id (fun f ->
             "verifiedAccessEndpointId", String.to_json f)
       ])

let of_json j =
  { verified_access_endpoint_id =
      Aws.Util.option_map (Aws.Json.lookup j "verifiedAccessEndpointId") String.of_json
  ; verified_access_endpoint_target_ip_address =
      Aws.Util.option_map
        (Aws.Json.lookup j "verifiedAccessEndpointTargetIpAddress")
        String.of_json
  ; verified_access_endpoint_target_dns =
      Aws.Util.option_map
        (Aws.Json.lookup j "verifiedAccessEndpointTargetDns")
        String.of_json
  }
