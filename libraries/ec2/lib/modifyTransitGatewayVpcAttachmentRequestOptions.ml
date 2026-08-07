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
        Aws.Util.option_bind (Aws.Xml.member "DnsSupport" xml) DnsSupportValue.parse
    ; security_group_referencing_support =
        Aws.Util.option_bind
          (Aws.Xml.member "SecurityGroupReferencingSupport" xml)
          SecurityGroupReferencingSupportValue.parse
    ; ipv6_support =
        Aws.Util.option_bind (Aws.Xml.member "Ipv6Support" xml) Ipv6SupportValue.parse
    ; appliance_mode_support =
        Aws.Util.option_bind
          (Aws.Xml.member "ApplianceModeSupport" xml)
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
             "ApplianceModeSupport", ApplianceModeSupportValue.to_json f)
       ; Aws.Util.option_map v.ipv6_support (fun f ->
             "Ipv6Support", Ipv6SupportValue.to_json f)
       ; Aws.Util.option_map v.security_group_referencing_support (fun f ->
             ( "SecurityGroupReferencingSupport"
             , SecurityGroupReferencingSupportValue.to_json f ))
       ; Aws.Util.option_map v.dns_support (fun f ->
             "DnsSupport", DnsSupportValue.to_json f)
       ])

let of_json j =
  { dns_support =
      Aws.Util.option_map (Aws.Json.lookup j "DnsSupport") DnsSupportValue.of_json
  ; security_group_referencing_support =
      Aws.Util.option_map
        (Aws.Json.lookup j "SecurityGroupReferencingSupport")
        SecurityGroupReferencingSupportValue.of_json
  ; ipv6_support =
      Aws.Util.option_map (Aws.Json.lookup j "Ipv6Support") Ipv6SupportValue.of_json
  ; appliance_mode_support =
      Aws.Util.option_map
        (Aws.Json.lookup j "ApplianceModeSupport")
        ApplianceModeSupportValue.of_json
  }
