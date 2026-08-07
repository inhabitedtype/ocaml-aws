open Aws.BaseTypes

type t =
  { hostname_type : HostnameType.t option
  ; enable_resource_name_dns_a_record : Boolean.t option
  ; enable_resource_name_dns_a_a_a_a_record : Boolean.t option
  }

let make
    ?hostname_type
    ?enable_resource_name_dns_a_record
    ?enable_resource_name_dns_a_a_a_a_record
    () =
  { hostname_type
  ; enable_resource_name_dns_a_record
  ; enable_resource_name_dns_a_a_a_a_record
  }

let parse xml =
  Some
    { hostname_type =
        Aws.Util.option_bind (Aws.Xml.member "hostnameType" xml) HostnameType.parse
    ; enable_resource_name_dns_a_record =
        Aws.Util.option_bind
          (Aws.Xml.member "enableResourceNameDnsARecord" xml)
          Boolean.parse
    ; enable_resource_name_dns_a_a_a_a_record =
        Aws.Util.option_bind
          (Aws.Xml.member "enableResourceNameDnsAAAARecord" xml)
          Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.enable_resource_name_dns_a_a_a_a_record (fun f ->
             Aws.Query.Pair ("EnableResourceNameDnsAAAARecord", Boolean.to_query f))
       ; Aws.Util.option_map v.enable_resource_name_dns_a_record (fun f ->
             Aws.Query.Pair ("EnableResourceNameDnsARecord", Boolean.to_query f))
       ; Aws.Util.option_map v.hostname_type (fun f ->
             Aws.Query.Pair ("HostnameType", HostnameType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.enable_resource_name_dns_a_a_a_a_record (fun f ->
             "enableResourceNameDnsAAAARecord", Boolean.to_json f)
       ; Aws.Util.option_map v.enable_resource_name_dns_a_record (fun f ->
             "enableResourceNameDnsARecord", Boolean.to_json f)
       ; Aws.Util.option_map v.hostname_type (fun f ->
             "hostnameType", HostnameType.to_json f)
       ])

let of_json j =
  { hostname_type =
      Aws.Util.option_map (Aws.Json.lookup j "hostnameType") HostnameType.of_json
  ; enable_resource_name_dns_a_record =
      Aws.Util.option_map
        (Aws.Json.lookup j "enableResourceNameDnsARecord")
        Boolean.of_json
  ; enable_resource_name_dns_a_a_a_a_record =
      Aws.Util.option_map
        (Aws.Json.lookup j "enableResourceNameDnsAAAARecord")
        Boolean.of_json
  }
