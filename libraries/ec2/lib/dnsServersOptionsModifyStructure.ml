open Aws.BaseTypes

type t =
  { custom_dns_servers : ValueStringList.t
  ; enabled : Boolean.t option
  }

let make ?(custom_dns_servers = []) ?enabled () = { custom_dns_servers; enabled }

let parse xml =
  Some
    { custom_dns_servers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "CustomDnsServers" xml)
             ValueStringList.parse)
    ; enabled = Aws.Util.option_bind (Aws.Xml.member "Enabled" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.enabled (fun f ->
             Aws.Query.Pair ("Enabled", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("CustomDnsServers", ValueStringList.to_query v.custom_dns_servers))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.enabled (fun f -> "Enabled", Boolean.to_json f)
       ; Some ("CustomDnsServers", ValueStringList.to_json v.custom_dns_servers)
       ])

let of_json j =
  { custom_dns_servers =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CustomDnsServers"))
  ; enabled = Aws.Util.option_map (Aws.Json.lookup j "Enabled") Boolean.of_json
  }
