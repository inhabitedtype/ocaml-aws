open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; instance_connect_endpoint_id : String.t
  ; ip_address_type : IpAddressType.t option
  ; security_group_ids : SecurityGroupIdStringListRequest.t
  ; preserve_client_ip : Boolean.t option
  }

let make
    ?dry_run
    ~instance_connect_endpoint_id
    ?ip_address_type
    ?(security_group_ids = [])
    ?preserve_client_ip
    () =
  { dry_run
  ; instance_connect_endpoint_id
  ; ip_address_type
  ; security_group_ids
  ; preserve_client_ip
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; instance_connect_endpoint_id =
        Aws.Xml.required
          "InstanceConnectEndpointId"
          (Aws.Util.option_bind
             (Aws.Xml.member "InstanceConnectEndpointId" xml)
             String.parse)
    ; ip_address_type =
        Aws.Util.option_bind (Aws.Xml.member "IpAddressType" xml) IpAddressType.parse
    ; security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroupId" xml)
             SecurityGroupIdStringListRequest.parse)
    ; preserve_client_ip =
        Aws.Util.option_bind (Aws.Xml.member "PreserveClientIp" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.preserve_client_ip (fun f ->
             Aws.Query.Pair ("PreserveClientIp", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SecurityGroupId"
              , SecurityGroupIdStringListRequest.to_query v.security_group_ids ))
       ; Aws.Util.option_map v.ip_address_type (fun f ->
             Aws.Query.Pair ("IpAddressType", IpAddressType.to_query f))
       ; Some
           (Aws.Query.Pair
              ("InstanceConnectEndpointId", String.to_query v.instance_connect_endpoint_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.preserve_client_ip (fun f ->
             "PreserveClientIp", Boolean.to_json f)
       ; Some
           ( "SecurityGroupId"
           , SecurityGroupIdStringListRequest.to_json v.security_group_ids )
       ; Aws.Util.option_map v.ip_address_type (fun f ->
             "IpAddressType", IpAddressType.to_json f)
       ; Some ("InstanceConnectEndpointId", String.to_json v.instance_connect_endpoint_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; instance_connect_endpoint_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceConnectEndpointId"))
  ; ip_address_type =
      Aws.Util.option_map (Aws.Json.lookup j "IpAddressType") IpAddressType.of_json
  ; security_group_ids =
      SecurityGroupIdStringListRequest.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupId"))
  ; preserve_client_ip =
      Aws.Util.option_map (Aws.Json.lookup j "PreserveClientIp") Boolean.of_json
  }
