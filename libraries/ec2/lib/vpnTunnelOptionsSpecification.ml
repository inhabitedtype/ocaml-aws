open Aws.BaseTypes

type t =
  { tunnel_inside_cidr : String.t option
  ; tunnel_inside_ipv6_cidr : String.t option
  ; pre_shared_key : String.t option
  ; phase1_lifetime_seconds : Integer.t option
  ; phase2_lifetime_seconds : Integer.t option
  ; rekey_margin_time_seconds : Integer.t option
  ; rekey_fuzz_percentage : Integer.t option
  ; replay_window_size : Integer.t option
  ; d_p_d_timeout_seconds : Integer.t option
  ; d_p_d_timeout_action : String.t option
  ; phase1_encryption_algorithms : Phase1EncryptionAlgorithmsRequestList.t
  ; phase2_encryption_algorithms : Phase2EncryptionAlgorithmsRequestList.t
  ; phase1_integrity_algorithms : Phase1IntegrityAlgorithmsRequestList.t
  ; phase2_integrity_algorithms : Phase2IntegrityAlgorithmsRequestList.t
  ; phase1_d_h_group_numbers : Phase1DHGroupNumbersRequestList.t
  ; phase2_d_h_group_numbers : Phase2DHGroupNumbersRequestList.t
  ; i_k_e_versions : IKEVersionsRequestList.t
  ; startup_action : String.t option
  ; log_options : VpnTunnelLogOptionsSpecification.t option
  ; enable_tunnel_lifecycle_control : Boolean.t option
  }

let make
    ?tunnel_inside_cidr
    ?tunnel_inside_ipv6_cidr
    ?pre_shared_key
    ?phase1_lifetime_seconds
    ?phase2_lifetime_seconds
    ?rekey_margin_time_seconds
    ?rekey_fuzz_percentage
    ?replay_window_size
    ?d_p_d_timeout_seconds
    ?d_p_d_timeout_action
    ?(phase1_encryption_algorithms = [])
    ?(phase2_encryption_algorithms = [])
    ?(phase1_integrity_algorithms = [])
    ?(phase2_integrity_algorithms = [])
    ?(phase1_d_h_group_numbers = [])
    ?(phase2_d_h_group_numbers = [])
    ?(i_k_e_versions = [])
    ?startup_action
    ?log_options
    ?enable_tunnel_lifecycle_control
    () =
  { tunnel_inside_cidr
  ; tunnel_inside_ipv6_cidr
  ; pre_shared_key
  ; phase1_lifetime_seconds
  ; phase2_lifetime_seconds
  ; rekey_margin_time_seconds
  ; rekey_fuzz_percentage
  ; replay_window_size
  ; d_p_d_timeout_seconds
  ; d_p_d_timeout_action
  ; phase1_encryption_algorithms
  ; phase2_encryption_algorithms
  ; phase1_integrity_algorithms
  ; phase2_integrity_algorithms
  ; phase1_d_h_group_numbers
  ; phase2_d_h_group_numbers
  ; i_k_e_versions
  ; startup_action
  ; log_options
  ; enable_tunnel_lifecycle_control
  }

let parse xml =
  Some
    { tunnel_inside_cidr =
        Aws.Util.option_bind (Aws.Xml.member "TunnelInsideCidr" xml) String.parse
    ; tunnel_inside_ipv6_cidr =
        Aws.Util.option_bind (Aws.Xml.member "TunnelInsideIpv6Cidr" xml) String.parse
    ; pre_shared_key =
        Aws.Util.option_bind (Aws.Xml.member "PreSharedKey" xml) String.parse
    ; phase1_lifetime_seconds =
        Aws.Util.option_bind (Aws.Xml.member "Phase1LifetimeSeconds" xml) Integer.parse
    ; phase2_lifetime_seconds =
        Aws.Util.option_bind (Aws.Xml.member "Phase2LifetimeSeconds" xml) Integer.parse
    ; rekey_margin_time_seconds =
        Aws.Util.option_bind (Aws.Xml.member "RekeyMarginTimeSeconds" xml) Integer.parse
    ; rekey_fuzz_percentage =
        Aws.Util.option_bind (Aws.Xml.member "RekeyFuzzPercentage" xml) Integer.parse
    ; replay_window_size =
        Aws.Util.option_bind (Aws.Xml.member "ReplayWindowSize" xml) Integer.parse
    ; d_p_d_timeout_seconds =
        Aws.Util.option_bind (Aws.Xml.member "DPDTimeoutSeconds" xml) Integer.parse
    ; d_p_d_timeout_action =
        Aws.Util.option_bind (Aws.Xml.member "DPDTimeoutAction" xml) String.parse
    ; phase1_encryption_algorithms =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Phase1EncryptionAlgorithm" xml)
             Phase1EncryptionAlgorithmsRequestList.parse)
    ; phase2_encryption_algorithms =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Phase2EncryptionAlgorithm" xml)
             Phase2EncryptionAlgorithmsRequestList.parse)
    ; phase1_integrity_algorithms =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Phase1IntegrityAlgorithm" xml)
             Phase1IntegrityAlgorithmsRequestList.parse)
    ; phase2_integrity_algorithms =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Phase2IntegrityAlgorithm" xml)
             Phase2IntegrityAlgorithmsRequestList.parse)
    ; phase1_d_h_group_numbers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Phase1DHGroupNumber" xml)
             Phase1DHGroupNumbersRequestList.parse)
    ; phase2_d_h_group_numbers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Phase2DHGroupNumber" xml)
             Phase2DHGroupNumbersRequestList.parse)
    ; i_k_e_versions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "IKEVersion" xml)
             IKEVersionsRequestList.parse)
    ; startup_action =
        Aws.Util.option_bind (Aws.Xml.member "StartupAction" xml) String.parse
    ; log_options =
        Aws.Util.option_bind
          (Aws.Xml.member "LogOptions" xml)
          VpnTunnelLogOptionsSpecification.parse
    ; enable_tunnel_lifecycle_control =
        Aws.Util.option_bind
          (Aws.Xml.member "EnableTunnelLifecycleControl" xml)
          Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.enable_tunnel_lifecycle_control (fun f ->
             Aws.Query.Pair ("EnableTunnelLifecycleControl", Boolean.to_query f))
       ; Aws.Util.option_map v.log_options (fun f ->
             Aws.Query.Pair ("LogOptions", VpnTunnelLogOptionsSpecification.to_query f))
       ; Aws.Util.option_map v.startup_action (fun f ->
             Aws.Query.Pair ("StartupAction", String.to_query f))
       ; Some
           (Aws.Query.Pair ("IKEVersion", IKEVersionsRequestList.to_query v.i_k_e_versions))
       ; Some
           (Aws.Query.Pair
              ( "Phase2DHGroupNumber"
              , Phase2DHGroupNumbersRequestList.to_query v.phase2_d_h_group_numbers ))
       ; Some
           (Aws.Query.Pair
              ( "Phase1DHGroupNumber"
              , Phase1DHGroupNumbersRequestList.to_query v.phase1_d_h_group_numbers ))
       ; Some
           (Aws.Query.Pair
              ( "Phase2IntegrityAlgorithm"
              , Phase2IntegrityAlgorithmsRequestList.to_query
                  v.phase2_integrity_algorithms ))
       ; Some
           (Aws.Query.Pair
              ( "Phase1IntegrityAlgorithm"
              , Phase1IntegrityAlgorithmsRequestList.to_query
                  v.phase1_integrity_algorithms ))
       ; Some
           (Aws.Query.Pair
              ( "Phase2EncryptionAlgorithm"
              , Phase2EncryptionAlgorithmsRequestList.to_query
                  v.phase2_encryption_algorithms ))
       ; Some
           (Aws.Query.Pair
              ( "Phase1EncryptionAlgorithm"
              , Phase1EncryptionAlgorithmsRequestList.to_query
                  v.phase1_encryption_algorithms ))
       ; Aws.Util.option_map v.d_p_d_timeout_action (fun f ->
             Aws.Query.Pair ("DPDTimeoutAction", String.to_query f))
       ; Aws.Util.option_map v.d_p_d_timeout_seconds (fun f ->
             Aws.Query.Pair ("DPDTimeoutSeconds", Integer.to_query f))
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
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.enable_tunnel_lifecycle_control (fun f ->
             "EnableTunnelLifecycleControl", Boolean.to_json f)
       ; Aws.Util.option_map v.log_options (fun f ->
             "LogOptions", VpnTunnelLogOptionsSpecification.to_json f)
       ; Aws.Util.option_map v.startup_action (fun f -> "StartupAction", String.to_json f)
       ; Some ("IKEVersion", IKEVersionsRequestList.to_json v.i_k_e_versions)
       ; Some
           ( "Phase2DHGroupNumber"
           , Phase2DHGroupNumbersRequestList.to_json v.phase2_d_h_group_numbers )
       ; Some
           ( "Phase1DHGroupNumber"
           , Phase1DHGroupNumbersRequestList.to_json v.phase1_d_h_group_numbers )
       ; Some
           ( "Phase2IntegrityAlgorithm"
           , Phase2IntegrityAlgorithmsRequestList.to_json v.phase2_integrity_algorithms )
       ; Some
           ( "Phase1IntegrityAlgorithm"
           , Phase1IntegrityAlgorithmsRequestList.to_json v.phase1_integrity_algorithms )
       ; Some
           ( "Phase2EncryptionAlgorithm"
           , Phase2EncryptionAlgorithmsRequestList.to_json v.phase2_encryption_algorithms
           )
       ; Some
           ( "Phase1EncryptionAlgorithm"
           , Phase1EncryptionAlgorithmsRequestList.to_json v.phase1_encryption_algorithms
           )
       ; Aws.Util.option_map v.d_p_d_timeout_action (fun f ->
             "DPDTimeoutAction", String.to_json f)
       ; Aws.Util.option_map v.d_p_d_timeout_seconds (fun f ->
             "DPDTimeoutSeconds", Integer.to_json f)
       ; Aws.Util.option_map v.replay_window_size (fun f ->
             "ReplayWindowSize", Integer.to_json f)
       ; Aws.Util.option_map v.rekey_fuzz_percentage (fun f ->
             "RekeyFuzzPercentage", Integer.to_json f)
       ; Aws.Util.option_map v.rekey_margin_time_seconds (fun f ->
             "RekeyMarginTimeSeconds", Integer.to_json f)
       ; Aws.Util.option_map v.phase2_lifetime_seconds (fun f ->
             "Phase2LifetimeSeconds", Integer.to_json f)
       ; Aws.Util.option_map v.phase1_lifetime_seconds (fun f ->
             "Phase1LifetimeSeconds", Integer.to_json f)
       ; Aws.Util.option_map v.pre_shared_key (fun f -> "PreSharedKey", String.to_json f)
       ; Aws.Util.option_map v.tunnel_inside_ipv6_cidr (fun f ->
             "TunnelInsideIpv6Cidr", String.to_json f)
       ; Aws.Util.option_map v.tunnel_inside_cidr (fun f ->
             "TunnelInsideCidr", String.to_json f)
       ])

let of_json j =
  { tunnel_inside_cidr =
      Aws.Util.option_map (Aws.Json.lookup j "TunnelInsideCidr") String.of_json
  ; tunnel_inside_ipv6_cidr =
      Aws.Util.option_map (Aws.Json.lookup j "TunnelInsideIpv6Cidr") String.of_json
  ; pre_shared_key = Aws.Util.option_map (Aws.Json.lookup j "PreSharedKey") String.of_json
  ; phase1_lifetime_seconds =
      Aws.Util.option_map (Aws.Json.lookup j "Phase1LifetimeSeconds") Integer.of_json
  ; phase2_lifetime_seconds =
      Aws.Util.option_map (Aws.Json.lookup j "Phase2LifetimeSeconds") Integer.of_json
  ; rekey_margin_time_seconds =
      Aws.Util.option_map (Aws.Json.lookup j "RekeyMarginTimeSeconds") Integer.of_json
  ; rekey_fuzz_percentage =
      Aws.Util.option_map (Aws.Json.lookup j "RekeyFuzzPercentage") Integer.of_json
  ; replay_window_size =
      Aws.Util.option_map (Aws.Json.lookup j "ReplayWindowSize") Integer.of_json
  ; d_p_d_timeout_seconds =
      Aws.Util.option_map (Aws.Json.lookup j "DPDTimeoutSeconds") Integer.of_json
  ; d_p_d_timeout_action =
      Aws.Util.option_map (Aws.Json.lookup j "DPDTimeoutAction") String.of_json
  ; phase1_encryption_algorithms =
      Phase1EncryptionAlgorithmsRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Phase1EncryptionAlgorithm"))
  ; phase2_encryption_algorithms =
      Phase2EncryptionAlgorithmsRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Phase2EncryptionAlgorithm"))
  ; phase1_integrity_algorithms =
      Phase1IntegrityAlgorithmsRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Phase1IntegrityAlgorithm"))
  ; phase2_integrity_algorithms =
      Phase2IntegrityAlgorithmsRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Phase2IntegrityAlgorithm"))
  ; phase1_d_h_group_numbers =
      Phase1DHGroupNumbersRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Phase1DHGroupNumber"))
  ; phase2_d_h_group_numbers =
      Phase2DHGroupNumbersRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Phase2DHGroupNumber"))
  ; i_k_e_versions =
      IKEVersionsRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "IKEVersion"))
  ; startup_action =
      Aws.Util.option_map (Aws.Json.lookup j "StartupAction") String.of_json
  ; log_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "LogOptions")
        VpnTunnelLogOptionsSpecification.of_json
  ; enable_tunnel_lifecycle_control =
      Aws.Util.option_map
        (Aws.Json.lookup j "EnableTunnelLifecycleControl")
        Boolean.of_json
  }
