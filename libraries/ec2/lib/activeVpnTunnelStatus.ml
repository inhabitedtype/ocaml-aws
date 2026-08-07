open Aws.BaseTypes

type t =
  { phase1_encryption_algorithm : String.t option
  ; phase2_encryption_algorithm : String.t option
  ; phase1_integrity_algorithm : String.t option
  ; phase2_integrity_algorithm : String.t option
  ; phase1_d_h_group : Integer.t option
  ; phase2_d_h_group : Integer.t option
  ; ike_version : String.t option
  ; provisioning_status : VpnTunnelProvisioningStatus.t option
  ; provisioning_status_reason : String.t option
  }

let make
    ?phase1_encryption_algorithm
    ?phase2_encryption_algorithm
    ?phase1_integrity_algorithm
    ?phase2_integrity_algorithm
    ?phase1_d_h_group
    ?phase2_d_h_group
    ?ike_version
    ?provisioning_status
    ?provisioning_status_reason
    () =
  { phase1_encryption_algorithm
  ; phase2_encryption_algorithm
  ; phase1_integrity_algorithm
  ; phase2_integrity_algorithm
  ; phase1_d_h_group
  ; phase2_d_h_group
  ; ike_version
  ; provisioning_status
  ; provisioning_status_reason
  }

let parse xml =
  Some
    { phase1_encryption_algorithm =
        Aws.Util.option_bind (Aws.Xml.member "phase1EncryptionAlgorithm" xml) String.parse
    ; phase2_encryption_algorithm =
        Aws.Util.option_bind (Aws.Xml.member "phase2EncryptionAlgorithm" xml) String.parse
    ; phase1_integrity_algorithm =
        Aws.Util.option_bind (Aws.Xml.member "phase1IntegrityAlgorithm" xml) String.parse
    ; phase2_integrity_algorithm =
        Aws.Util.option_bind (Aws.Xml.member "phase2IntegrityAlgorithm" xml) String.parse
    ; phase1_d_h_group =
        Aws.Util.option_bind (Aws.Xml.member "phase1DHGroup" xml) Integer.parse
    ; phase2_d_h_group =
        Aws.Util.option_bind (Aws.Xml.member "phase2DHGroup" xml) Integer.parse
    ; ike_version = Aws.Util.option_bind (Aws.Xml.member "ikeVersion" xml) String.parse
    ; provisioning_status =
        Aws.Util.option_bind
          (Aws.Xml.member "provisioningStatus" xml)
          VpnTunnelProvisioningStatus.parse
    ; provisioning_status_reason =
        Aws.Util.option_bind (Aws.Xml.member "provisioningStatusReason" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.provisioning_status_reason (fun f ->
             Aws.Query.Pair ("ProvisioningStatusReason", String.to_query f))
       ; Aws.Util.option_map v.provisioning_status (fun f ->
             Aws.Query.Pair ("ProvisioningStatus", VpnTunnelProvisioningStatus.to_query f))
       ; Aws.Util.option_map v.ike_version (fun f ->
             Aws.Query.Pair ("IkeVersion", String.to_query f))
       ; Aws.Util.option_map v.phase2_d_h_group (fun f ->
             Aws.Query.Pair ("Phase2DHGroup", Integer.to_query f))
       ; Aws.Util.option_map v.phase1_d_h_group (fun f ->
             Aws.Query.Pair ("Phase1DHGroup", Integer.to_query f))
       ; Aws.Util.option_map v.phase2_integrity_algorithm (fun f ->
             Aws.Query.Pair ("Phase2IntegrityAlgorithm", String.to_query f))
       ; Aws.Util.option_map v.phase1_integrity_algorithm (fun f ->
             Aws.Query.Pair ("Phase1IntegrityAlgorithm", String.to_query f))
       ; Aws.Util.option_map v.phase2_encryption_algorithm (fun f ->
             Aws.Query.Pair ("Phase2EncryptionAlgorithm", String.to_query f))
       ; Aws.Util.option_map v.phase1_encryption_algorithm (fun f ->
             Aws.Query.Pair ("Phase1EncryptionAlgorithm", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.provisioning_status_reason (fun f ->
             "provisioningStatusReason", String.to_json f)
       ; Aws.Util.option_map v.provisioning_status (fun f ->
             "provisioningStatus", VpnTunnelProvisioningStatus.to_json f)
       ; Aws.Util.option_map v.ike_version (fun f -> "ikeVersion", String.to_json f)
       ; Aws.Util.option_map v.phase2_d_h_group (fun f ->
             "phase2DHGroup", Integer.to_json f)
       ; Aws.Util.option_map v.phase1_d_h_group (fun f ->
             "phase1DHGroup", Integer.to_json f)
       ; Aws.Util.option_map v.phase2_integrity_algorithm (fun f ->
             "phase2IntegrityAlgorithm", String.to_json f)
       ; Aws.Util.option_map v.phase1_integrity_algorithm (fun f ->
             "phase1IntegrityAlgorithm", String.to_json f)
       ; Aws.Util.option_map v.phase2_encryption_algorithm (fun f ->
             "phase2EncryptionAlgorithm", String.to_json f)
       ; Aws.Util.option_map v.phase1_encryption_algorithm (fun f ->
             "phase1EncryptionAlgorithm", String.to_json f)
       ])

let of_json j =
  { phase1_encryption_algorithm =
      Aws.Util.option_map (Aws.Json.lookup j "phase1EncryptionAlgorithm") String.of_json
  ; phase2_encryption_algorithm =
      Aws.Util.option_map (Aws.Json.lookup j "phase2EncryptionAlgorithm") String.of_json
  ; phase1_integrity_algorithm =
      Aws.Util.option_map (Aws.Json.lookup j "phase1IntegrityAlgorithm") String.of_json
  ; phase2_integrity_algorithm =
      Aws.Util.option_map (Aws.Json.lookup j "phase2IntegrityAlgorithm") String.of_json
  ; phase1_d_h_group =
      Aws.Util.option_map (Aws.Json.lookup j "phase1DHGroup") Integer.of_json
  ; phase2_d_h_group =
      Aws.Util.option_map (Aws.Json.lookup j "phase2DHGroup") Integer.of_json
  ; ike_version = Aws.Util.option_map (Aws.Json.lookup j "ikeVersion") String.of_json
  ; provisioning_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "provisioningStatus")
        VpnTunnelProvisioningStatus.of_json
  ; provisioning_status_reason =
      Aws.Util.option_map (Aws.Json.lookup j "provisioningStatusReason") String.of_json
  }
