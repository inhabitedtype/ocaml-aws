open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; subnet_id : String.t
  ; security_group_ids : SecurityGroupIdStringListRequest.t
  ; preserve_client_ip : Boolean.t option
  ; client_token : String.t option
  ; tag_specifications : TagSpecificationList.t
  ; ip_address_type : IpAddressType.t option
  }

let make
    ?dry_run
    ~subnet_id
    ?(security_group_ids = [])
    ?preserve_client_ip
    ?client_token
    ?(tag_specifications = [])
    ?ip_address_type
    () =
  { dry_run
  ; subnet_id
  ; security_group_ids
  ; preserve_client_ip
  ; client_token
  ; tag_specifications
  ; ip_address_type
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; subnet_id =
        Aws.Xml.required
          "SubnetId"
          (Aws.Util.option_bind (Aws.Xml.member "SubnetId" xml) String.parse)
    ; security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroupId" xml)
             SecurityGroupIdStringListRequest.parse)
    ; preserve_client_ip =
        Aws.Util.option_bind (Aws.Xml.member "PreserveClientIp" xml) Boolean.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; ip_address_type =
        Aws.Util.option_bind (Aws.Xml.member "IpAddressType" xml) IpAddressType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ip_address_type (fun f ->
             Aws.Query.Pair ("IpAddressType", IpAddressType.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.preserve_client_ip (fun f ->
             Aws.Query.Pair ("PreserveClientIp", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SecurityGroupId"
              , SecurityGroupIdStringListRequest.to_query v.security_group_ids ))
       ; Some (Aws.Query.Pair ("SubnetId", String.to_query v.subnet_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ip_address_type (fun f ->
             "IpAddressType", IpAddressType.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.preserve_client_ip (fun f ->
             "PreserveClientIp", Boolean.to_json f)
       ; Some
           ( "SecurityGroupId"
           , SecurityGroupIdStringListRequest.to_json v.security_group_ids )
       ; Some ("SubnetId", String.to_json v.subnet_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; subnet_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SubnetId"))
  ; security_group_ids =
      SecurityGroupIdStringListRequest.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupId"))
  ; preserve_client_ip =
      Aws.Util.option_map (Aws.Json.lookup j "PreserveClientIp") Boolean.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; ip_address_type =
      Aws.Util.option_map (Aws.Json.lookup j "IpAddressType") IpAddressType.of_json
  }
