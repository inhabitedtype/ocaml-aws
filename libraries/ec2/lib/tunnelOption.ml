open Aws.BaseTypes

type t =
  { outside_ip_address : String.t option
  ; tunnel_inside_cidr : String.t option
  ; tunnel_inside_ipv6_cidr : String.t option
  ; pre_shared_key : String.t option
  ; phase1_lifetime_seconds : Integer.t option
  ; phase2_lifetime_seconds : Integer.t option
  ; rekey_margin_time_seconds : Integer.t option
  ; rekey_fuzz_percentage : Integer.t option
  ; replay_window_size : Integer.t option
  ; dpd_timeout_seconds : Integer.t option
  ; dpd_timeout_action : String.t option
  ; phase1_encryption_algorithms : Phase1EncryptionAlgorithmsList.t
  ; phase2_encryption_algorithms : Phase2EncryptionAlgorithmsList.t
  ; phase1_integrity_algorithms : Phase1IntegrityAlgorithmsList.t
  ; phase2_integrity_algorithms : Phase2IntegrityAlgorithmsList.t
  ; phase1_d_h_group_numbers : Phase1DHGroupNumbersList.t
  ; phase2_d_h_group_numbers : Phase2DHGroupNumbersList.t
  ; ike_versions : IKEVersionsList.t
  ; startup_action : String.t option
  ; log_options : VpnTunnelLogOptions.t option
  ; enable_tunnel_lifecycle_control : Boolean.t option
  }

let make
    ?outside_ip_address
    ?tunnel_inside_cidr
    ?tunnel_inside_ipv6_cidr
    ?pre_shared_key
    ?phase1_lifetime_seconds
    ?phase2_lifetime_seconds
    ?rekey_margin_time_seconds
    ?rekey_fuzz_percentage
    ?replay_window_size
    ?dpd_timeout_seconds
    ?dpd_timeout_action
    ?(phase1_encryption_algorithms = [])
    ?(phase2_encryption_algorithms = [])
    ?(phase1_integrity_algorithms = [])
    ?(phase2_integrity_algorithms = [])
    ?(phase1_d_h_group_numbers = [])
    ?(phase2_d_h_group_numbers = [])
    ?(ike_versions = [])
    ?startup_action
    ?log_options
    ?enable_tunnel_lifecycle_control
    () =
  { outside_ip_address
  ; tunnel_inside_cidr
  ; tunnel_inside_ipv6_cidr
  ; pre_shared_key
  ; phase1_lifetime_seconds
  ; phase2_lifetime_seconds
  ; rekey_margin_time_seconds
  ; rekey_fuzz_percentage
  ; replay_window_size
  ; dpd_timeout_seconds
  ; dpd_timeout_action
  ; phase1_encryption_algorithms
  ; phase2_encryption_algorithms
  ; phase1_integrity_algorithms
  ; phase2_integrity_algorithms
  ; phase1_d_h_group_numbers
  ; phase2_d_h_group_numbers
  ; ike_versions
  ; startup_action
  ; log_options
  ; enable_tunnel_lifecycle_control
  }

let parse xml =
  Some
    { outside_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "outsideIpAddress" xml) String.parse
    ; tunnel_inside_cidr =
        Aws.Util.option_bind (Aws.Xml.member "tunnelInsideCidr" xml) String.parse
    ; tunnel_inside_ipv6_cidr =
        Aws.Util.option_bind (Aws.Xml.member "tunnelInsideIpv6Cidr" xml) String.parse
    ; pre_shared_key =
        Aws.Util.option_bind (Aws.Xml.member "preSharedKey" xml) String.parse
    ; phase1_lifetime_seconds =
        Aws.Util.option_bind (Aws.Xml.member "phase1LifetimeSeconds" xml) Integer.parse
    ; phase2_lifetime_seconds =
        Aws.Util.option_bind (Aws.Xml.member "phase2LifetimeSeconds" xml) Integer.parse
    ; rekey_margin_time_seconds =
        Aws.Util.option_bind (Aws.Xml.member "rekeyMarginTimeSeconds" xml) Integer.parse
    ; rekey_fuzz_percentage =
        Aws.Util.option_bind (Aws.Xml.member "rekeyFuzzPercentage" xml) Integer.parse
    ; replay_window_size =
        Aws.Util.option_bind (Aws.Xml.member "replayWindowSize" xml) Integer.parse
    ; dpd_timeout_seconds =
        Aws.Util.option_bind (Aws.Xml.member "dpdTimeoutSeconds" xml) Integer.parse
    ; dpd_timeout_action =
        Aws.Util.option_bind (Aws.Xml.member "dpdTimeoutAction" xml) String.parse
    ; phase1_encryption_algorithms =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "phase1EncryptionAlgorithmSet" xml)
             Phase1EncryptionAlgorithmsList.parse)
    ; phase2_encryption_algorithms =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "phase2EncryptionAlgorithmSet" xml)
             Phase2EncryptionAlgorithmsList.parse)
    ; phase1_integrity_algorithms =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "phase1IntegrityAlgorithmSet" xml)
             Phase1IntegrityAlgorithmsList.parse)
    ; phase2_integrity_algorithms =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "phase2IntegrityAlgorithmSet" xml)
             Phase2IntegrityAlgorithmsList.parse)
    ; phase1_d_h_group_numbers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "phase1DHGroupNumberSet" xml)
             Phase1DHGroupNumbersList.parse)
    ; phase2_d_h_group_numbers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "phase2DHGroupNumberSet" xml)
             Phase2DHGroupNumbersList.parse)
    ; ike_versions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ikeVersionSet" xml)
             IKEVersionsList.parse)
    ; startup_action =
        Aws.Util.option_bind (Aws.Xml.member "startupAction" xml) String.parse
    ; log_options =
        Aws.Util.option_bind (Aws.Xml.member "logOptions" xml) VpnTunnelLogOptions.parse
    ; enable_tunnel_lifecycle_control =
        Aws.Util.option_bind
          (Aws.Xml.member "enableTunnelLifecycleControl" xml)
          Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.enable_tunnel_lifecycle_control (fun f ->
             Aws.Query.Pair ("EnableTunnelLifecycleControl", Boolean.to_query f))
       ; Aws.Util.option_map v.log_options (fun f ->
             Aws.Query.Pair ("LogOptions", VpnTunnelLogOptions.to_query f))
       ; Aws.Util.option_map v.startup_action (fun f ->
             Aws.Query.Pair ("StartupAction", String.to_query f))
       ; Some (Aws.Query.Pair ("IkeVersionSet", IKEVersionsList.to_query v.ike_versions))
       ; Some
           (Aws.Query.Pair
              ( "Phase2DHGroupNumberSet"
              , Phase2DHGroupNumbersList.to_query v.phase2_d_h_group_numbers ))
       ; Some
           (Aws.Query.Pair
              ( "Phase1DHGroupNumberSet"
              , Phase1DHGroupNumbersList.to_query v.phase1_d_h_group_numbers ))
       ; Some
           (Aws.Query.Pair
              ( "Phase2IntegrityAlgorithmSet"
              , Phase2IntegrityAlgorithmsList.to_query v.phase2_integrity_algorithms ))
       ; Some
           (Aws.Query.Pair
              ( "Phase1IntegrityAlgorithmSet"
              , Phase1IntegrityAlgorithmsList.to_query v.phase1_integrity_algorithms ))
       ; Some
           (Aws.Query.Pair
              ( "Phase2EncryptionAlgorithmSet"
              , Phase2EncryptionAlgorithmsList.to_query v.phase2_encryption_algorithms ))
       ; Some
           (Aws.Query.Pair
              ( "Phase1EncryptionAlgorithmSet"
              , Phase1EncryptionAlgorithmsList.to_query v.phase1_encryption_algorithms ))
       ; Aws.Util.option_map v.dpd_timeout_action (fun f ->
             Aws.Query.Pair ("DpdTimeoutAction", String.to_query f))
       ; Aws.Util.option_map v.dpd_timeout_seconds (fun f ->
             Aws.Query.Pair ("DpdTimeoutSeconds", Integer.to_query f))
       ; Aws.Util.option_map v.replay_window_size (fun f ->
             Aws.Query.Pair ("ReplayWindowSize", Integer.to_query f))
       ; Aws.Util.option_map v.rekey_fuzz_percentage (fun f ->
             Aws.Query.Pair ("RekeyFuzzPercentage", Integer.to_query f))
       ; Aws.Util.option_map v.rekey_margin_time_seconds (fun f ->
             Aws.Query.Pair ("RekeyMarginTimeSeconds", Integer.to_query f))
       ; Aws.Util.option_map v.phase2_lifetime_seconds (fun f ->
             Aws.Query.Pair ("Phase2LifetimeSeconds", Integer.to_query f))
       ; Aws.Util.option_map v.phase1_lifetime_seconds (fun f ->
             Aws.Query.Pair ("Phase1LifetimeSeconds", Integer.to_query f))
       ; Aws.Util.option_map v.pre_shared_key (fun f ->
             Aws.Query.Pair ("PreSharedKey", String.to_query f))
       ; Aws.Util.option_map v.tunnel_inside_ipv6_cidr (fun f ->
             Aws.Query.Pair ("TunnelInsideIpv6Cidr", String.to_query f))
       ; Aws.Util.option_map v.tunnel_inside_cidr (fun f ->
             Aws.Query.Pair ("TunnelInsideCidr", String.to_query f))
       ; Aws.Util.option_map v.outside_ip_address (fun f ->
             Aws.Query.Pair ("OutsideIpAddress", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.enable_tunnel_lifecycle_control (fun f ->
             "enableTunnelLifecycleControl", Boolean.to_json f)
       ; Aws.Util.option_map v.log_options (fun f ->
             "logOptions", VpnTunnelLogOptions.to_json f)
       ; Aws.Util.option_map v.startup_action (fun f -> "startupAction", String.to_json f)
       ; Some ("ikeVersionSet", IKEVersionsList.to_json v.ike_versions)
       ; Some
           ( "phase2DHGroupNumberSet"
           , Phase2DHGroupNumbersList.to_json v.phase2_d_h_group_numbers )
       ; Some
           ( "phase1DHGroupNumberSet"
           , Phase1DHGroupNumbersList.to_json v.phase1_d_h_group_numbers )
       ; Some
           ( "phase2IntegrityAlgorithmSet"
           , Phase2IntegrityAlgorithmsList.to_json v.phase2_integrity_algorithms )
       ; Some
           ( "phase1IntegrityAlgorithmSet"
           , Phase1IntegrityAlgorithmsList.to_json v.phase1_integrity_algorithms )
       ; Some
           ( "phase2EncryptionAlgorithmSet"
           , Phase2EncryptionAlgorithmsList.to_json v.phase2_encryption_algorithms )
       ; Some
           ( "phase1EncryptionAlgorithmSet"
           , Phase1EncryptionAlgorithmsList.to_json v.phase1_encryption_algorithms )
       ; Aws.Util.option_map v.dpd_timeout_action (fun f ->
             "dpdTimeoutAction", String.to_json f)
       ; Aws.Util.option_map v.dpd_timeout_seconds (fun f ->
             "dpdTimeoutSeconds", Integer.to_json f)
       ; Aws.Util.option_map v.replay_window_size (fun f ->
             "replayWindowSize", Integer.to_json f)
       ; Aws.Util.option_map v.rekey_fuzz_percentage (fun f ->
             "rekeyFuzzPercentage", Integer.to_json f)
       ; Aws.Util.option_map v.rekey_margin_time_seconds (fun f ->
             "rekeyMarginTimeSeconds", Integer.to_json f)
       ; Aws.Util.option_map v.phase2_lifetime_seconds (fun f ->
             "phase2LifetimeSeconds", Integer.to_json f)
       ; Aws.Util.option_map v.phase1_lifetime_seconds (fun f ->
             "phase1LifetimeSeconds", Integer.to_json f)
       ; Aws.Util.option_map v.pre_shared_key (fun f -> "preSharedKey", String.to_json f)
       ; Aws.Util.option_map v.tunnel_inside_ipv6_cidr (fun f ->
             "tunnelInsideIpv6Cidr", String.to_json f)
       ; Aws.Util.option_map v.tunnel_inside_cidr (fun f ->
             "tunnelInsideCidr", String.to_json f)
       ; Aws.Util.option_map v.outside_ip_address (fun f ->
             "outsideIpAddress", String.to_json f)
       ])

let of_json j =
  { outside_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "outsideIpAddress") String.of_json
  ; tunnel_inside_cidr =
      Aws.Util.option_map (Aws.Json.lookup j "tunnelInsideCidr") String.of_json
  ; tunnel_inside_ipv6_cidr =
      Aws.Util.option_map (Aws.Json.lookup j "tunnelInsideIpv6Cidr") String.of_json
  ; pre_shared_key = Aws.Util.option_map (Aws.Json.lookup j "preSharedKey") String.of_json
  ; phase1_lifetime_seconds =
      Aws.Util.option_map (Aws.Json.lookup j "phase1LifetimeSeconds") Integer.of_json
  ; phase2_lifetime_seconds =
      Aws.Util.option_map (Aws.Json.lookup j "phase2LifetimeSeconds") Integer.of_json
  ; rekey_margin_time_seconds =
      Aws.Util.option_map (Aws.Json.lookup j "rekeyMarginTimeSeconds") Integer.of_json
  ; rekey_fuzz_percentage =
      Aws.Util.option_map (Aws.Json.lookup j "rekeyFuzzPercentage") Integer.of_json
  ; replay_window_size =
      Aws.Util.option_map (Aws.Json.lookup j "replayWindowSize") Integer.of_json
  ; dpd_timeout_seconds =
      Aws.Util.option_map (Aws.Json.lookup j "dpdTimeoutSeconds") Integer.of_json
  ; dpd_timeout_action =
      Aws.Util.option_map (Aws.Json.lookup j "dpdTimeoutAction") String.of_json
  ; phase1_encryption_algorithms =
      Phase1EncryptionAlgorithmsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "phase1EncryptionAlgorithmSet"))
  ; phase2_encryption_algorithms =
      Phase2EncryptionAlgorithmsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "phase2EncryptionAlgorithmSet"))
  ; phase1_integrity_algorithms =
      Phase1IntegrityAlgorithmsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "phase1IntegrityAlgorithmSet"))
  ; phase2_integrity_algorithms =
      Phase2IntegrityAlgorithmsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "phase2IntegrityAlgorithmSet"))
  ; phase1_d_h_group_numbers =
      Phase1DHGroupNumbersList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "phase1DHGroupNumberSet"))
  ; phase2_d_h_group_numbers =
      Phase2DHGroupNumbersList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "phase2DHGroupNumberSet"))
  ; ike_versions =
      IKEVersionsList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ikeVersionSet"))
  ; startup_action =
      Aws.Util.option_map (Aws.Json.lookup j "startupAction") String.of_json
  ; log_options =
      Aws.Util.option_map (Aws.Json.lookup j "logOptions") VpnTunnelLogOptions.of_json
  ; enable_tunnel_lifecycle_control =
      Aws.Util.option_map
        (Aws.Json.lookup j "enableTunnelLifecycleControl")
        Boolean.of_json
  }
