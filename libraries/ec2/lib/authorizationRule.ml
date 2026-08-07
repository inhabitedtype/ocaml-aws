open Aws.BaseTypes

type t =
  { client_vpn_endpoint_id : String.t option
  ; description : String.t option
  ; group_id : String.t option
  ; access_all : Boolean.t option
  ; destination_cidr : String.t option
  ; status : ClientVpnAuthorizationRuleStatus.t option
  }

let make
    ?client_vpn_endpoint_id
    ?description
    ?group_id
    ?access_all
    ?destination_cidr
    ?status
    () =
  { client_vpn_endpoint_id; description; group_id; access_all; destination_cidr; status }

let parse xml =
  Some
    { client_vpn_endpoint_id =
        Aws.Util.option_bind (Aws.Xml.member "clientVpnEndpointId" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; group_id = Aws.Util.option_bind (Aws.Xml.member "groupId" xml) String.parse
    ; access_all = Aws.Util.option_bind (Aws.Xml.member "accessAll" xml) Boolean.parse
    ; destination_cidr =
        Aws.Util.option_bind (Aws.Xml.member "destinationCidr" xml) String.parse
    ; status =
        Aws.Util.option_bind
          (Aws.Xml.member "status" xml)
          ClientVpnAuthorizationRuleStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", ClientVpnAuthorizationRuleStatus.to_query f))
       ; Aws.Util.option_map v.destination_cidr (fun f ->
             Aws.Query.Pair ("DestinationCidr", String.to_query f))
       ; Aws.Util.option_map v.access_all (fun f ->
             Aws.Query.Pair ("AccessAll", Boolean.to_query f))
       ; Aws.Util.option_map v.group_id (fun f ->
             Aws.Query.Pair ("GroupId", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.client_vpn_endpoint_id (fun f ->
             Aws.Query.Pair ("ClientVpnEndpointId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             "status", ClientVpnAuthorizationRuleStatus.to_json f)
       ; Aws.Util.option_map v.destination_cidr (fun f ->
             "destinationCidr", String.to_json f)
       ; Aws.Util.option_map v.access_all (fun f -> "accessAll", Boolean.to_json f)
       ; Aws.Util.option_map v.group_id (fun f -> "groupId", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.client_vpn_endpoint_id (fun f ->
             "clientVpnEndpointId", String.to_json f)
       ])

let of_json j =
  { client_vpn_endpoint_id =
      Aws.Util.option_map (Aws.Json.lookup j "clientVpnEndpointId") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; group_id = Aws.Util.option_map (Aws.Json.lookup j "groupId") String.of_json
  ; access_all = Aws.Util.option_map (Aws.Json.lookup j "accessAll") Boolean.of_json
  ; destination_cidr =
      Aws.Util.option_map (Aws.Json.lookup j "destinationCidr") String.of_json
  ; status =
      Aws.Util.option_map
        (Aws.Json.lookup j "status")
        ClientVpnAuthorizationRuleStatus.of_json
  }
