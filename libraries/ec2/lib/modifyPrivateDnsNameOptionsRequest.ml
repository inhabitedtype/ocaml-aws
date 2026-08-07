open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; instance_id : String.t
  ; private_dns_hostname_type : HostnameType.t option
  ; enable_resource_name_dns_a_record : Boolean.t option
  ; enable_resource_name_dns_a_a_a_a_record : Boolean.t option
  }

let make
    ?dry_run
    ~instance_id
    ?private_dns_hostname_type
    ?enable_resource_name_dns_a_record
    ?enable_resource_name_dns_a_a_a_a_record
    () =
  { dry_run
  ; instance_id
  ; private_dns_hostname_type
  ; enable_resource_name_dns_a_record
  ; enable_resource_name_dns_a_a_a_a_record
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; instance_id =
        Aws.Xml.required
          "InstanceId"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
    ; private_dns_hostname_type =
        Aws.Util.option_bind
          (Aws.Xml.member "PrivateDnsHostnameType" xml)
          HostnameType.parse
    ; enable_resource_name_dns_a_record =
        Aws.Util.option_bind
          (Aws.Xml.member "EnableResourceNameDnsARecord" xml)
          Boolean.parse
    ; enable_resource_name_dns_a_a_a_a_record =
        Aws.Util.option_bind
          (Aws.Xml.member "EnableResourceNameDnsAAAARecord" xml)
          Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.enable_resource_name_dns_a_a_a_a_record (fun f ->
             Aws.Query.Pair ("EnableResourceNameDnsAAAARecord", Boolean.to_query f))
       ; Aws.Util.option_map v.enable_resource_name_dns_a_record (fun f ->
             Aws.Query.Pair ("EnableResourceNameDnsARecord", Boolean.to_query f))
       ; Aws.Util.option_map v.private_dns_hostname_type (fun f ->
             Aws.Query.Pair ("PrivateDnsHostnameType", HostnameType.to_query f))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.enable_resource_name_dns_a_a_a_a_record (fun f ->
             "EnableResourceNameDnsAAAARecord", Boolean.to_json f)
       ; Aws.Util.option_map v.enable_resource_name_dns_a_record (fun f ->
             "EnableResourceNameDnsARecord", Boolean.to_json f)
       ; Aws.Util.option_map v.private_dns_hostname_type (fun f ->
             "PrivateDnsHostnameType", HostnameType.to_json f)
       ; Some ("InstanceId", String.to_json v.instance_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  ; private_dns_hostname_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "PrivateDnsHostnameType")
        HostnameType.of_json
  ; enable_resource_name_dns_a_record =
      Aws.Util.option_map
        (Aws.Json.lookup j "EnableResourceNameDnsARecord")
        Boolean.of_json
  ; enable_resource_name_dns_a_a_a_a_record =
      Aws.Util.option_map
        (Aws.Json.lookup j "EnableResourceNameDnsAAAARecord")
        Boolean.of_json
  }
