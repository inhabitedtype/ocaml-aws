open Aws.BaseTypes

type t =
  { network_interface_id : String.t
  ; hostname_type : PublicIpDnsOption.t
  ; dry_run : Boolean.t option
  }

let make ~network_interface_id ~hostname_type ?dry_run () =
  { network_interface_id; hostname_type; dry_run }

let parse xml =
  Some
    { network_interface_id =
        Aws.Xml.required
          "NetworkInterfaceId"
          (Aws.Util.option_bind (Aws.Xml.member "NetworkInterfaceId" xml) String.parse)
    ; hostname_type =
        Aws.Xml.required
          "HostnameType"
          (Aws.Util.option_bind
             (Aws.Xml.member "HostnameType" xml)
             PublicIpDnsOption.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair ("HostnameType", PublicIpDnsOption.to_query v.hostname_type))
       ; Some
           (Aws.Query.Pair ("NetworkInterfaceId", String.to_query v.network_interface_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("HostnameType", PublicIpDnsOption.to_json v.hostname_type)
       ; Some ("NetworkInterfaceId", String.to_json v.network_interface_id)
       ])

let of_json j =
  { network_interface_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NetworkInterfaceId"))
  ; hostname_type =
      PublicIpDnsOption.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "HostnameType"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
