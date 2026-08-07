type t =
  { dns_support : DnsSupportValue.t option
  ; security_group_referencing_support : SecurityGroupReferencingSupportValue.t option
  ; ipv6_support : Ipv6SupportValue.t option
  ; appliance_mode_support : ApplianceModeSupportValue.t option
  }

let make
    ?dns_support
    ?security_group_referencing_support
    ?ipv6_support
    ?appliance_mode_support
    () =
  { dns_support
  ; security_group_referencing_support
  ; ipv6_support
  ; appliance_mode_support
  }

let parse xml =
  Some
    { dns_support =
        Aws.Util.option_bind (Aws.Xml.member "dnsSupport" xml) DnsSupportValue.parse
    ; security_group_referencing_support =
        Aws.Util.option_bind
          (Aws.Xml.member "securityGroupReferencingSupport" xml)
          SecurityGroupReferencingSupportValue.parse
    ; ipv6_support =
        Aws.Util.option_bind (Aws.Xml.member "ipv6Support" xml) Ipv6SupportValue.parse
    ; appliance_mode_support =
        Aws.Util.option_bind
          (Aws.Xml.member "applianceModeSupport" xml)
          ApplianceModeSupportValue.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.appliance_mode_support (fun f ->
             Aws.Query.Pair ("ApplianceModeSupport", ApplianceModeSupportValue.to_query f))
       ; Aws.Util.option_map v.ipv6_support (fun f ->
             Aws.Query.Pair ("Ipv6Support", Ipv6SupportValue.to_query f))
       ; Aws.Util.option_map v.security_group_referencing_support (fun f ->
             Aws.Query.Pair
               ( "SecurityGroupReferencingSupport"
               , SecurityGroupReferencingSupportValue.to_query f ))
       ; Aws.Util.option_map v.dns_support (fun f ->
             Aws.Query.Pair ("DnsSupport", DnsSupportValue.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.appliance_mode_support (fun f ->
             "applianceModeSupport", ApplianceModeSupportValue.to_json f)
       ; Aws.Util.option_map v.ipv6_support (fun f ->
             "ipv6Support", Ipv6SupportValue.to_json f)
       ; Aws.Util.option_map v.security_group_referencing_support (fun f ->
             ( "securityGroupReferencingSupport"
             , SecurityGroupReferencingSupportValue.to_json f ))
       ; Aws.Util.option_map v.dns_support (fun f ->
             "dnsSupport", DnsSupportValue.to_json f)
       ])

let of_json j =
  { dns_support =
      Aws.Util.option_map (Aws.Json.lookup j "dnsSupport") DnsSupportValue.of_json
  ; security_group_referencing_support =
      Aws.Util.option_map
        (Aws.Json.lookup j "securityGroupReferencingSupport")
        SecurityGroupReferencingSupportValue.of_json
  ; ipv6_support =
      Aws.Util.option_map (Aws.Json.lookup j "ipv6Support") Ipv6SupportValue.of_json
  ; appliance_mode_support =
      Aws.Util.option_map
        (Aws.Json.lookup j "applianceModeSupport")
        ApplianceModeSupportValue.of_json
  }
