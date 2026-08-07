open Aws.BaseTypes

type t =
  { account_id : String.t option
  ; discovery_region : String.t option
  ; failure_reason : IpamDiscoveryFailureReason.t option
  ; last_attempted_discovery_time : DateTime.t option
  ; last_successful_discovery_time : DateTime.t option
  ; organizational_unit_id : String.t option
  }

let make
    ?account_id
    ?discovery_region
    ?failure_reason
    ?last_attempted_discovery_time
    ?last_successful_discovery_time
    ?organizational_unit_id
    () =
  { account_id
  ; discovery_region
  ; failure_reason
  ; last_attempted_discovery_time
  ; last_successful_discovery_time
  ; organizational_unit_id
  }

let parse xml =
  Some
    { account_id = Aws.Util.option_bind (Aws.Xml.member "accountId" xml) String.parse
    ; discovery_region =
        Aws.Util.option_bind (Aws.Xml.member "discoveryRegion" xml) String.parse
    ; failure_reason =
        Aws.Util.option_bind
          (Aws.Xml.member "failureReason" xml)
          IpamDiscoveryFailureReason.parse
    ; last_attempted_discovery_time =
        Aws.Util.option_bind
          (Aws.Xml.member "lastAttemptedDiscoveryTime" xml)
          DateTime.parse
    ; last_successful_discovery_time =
        Aws.Util.option_bind
          (Aws.Xml.member "lastSuccessfulDiscoveryTime" xml)
          DateTime.parse
    ; organizational_unit_id =
        Aws.Util.option_bind (Aws.Xml.member "organizationalUnitId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.organizational_unit_id (fun f ->
             Aws.Query.Pair ("OrganizationalUnitId", String.to_query f))
       ; Aws.Util.option_map v.last_successful_discovery_time (fun f ->
             Aws.Query.Pair ("LastSuccessfulDiscoveryTime", DateTime.to_query f))
       ; Aws.Util.option_map v.last_attempted_discovery_time (fun f ->
             Aws.Query.Pair ("LastAttemptedDiscoveryTime", DateTime.to_query f))
       ; Aws.Util.option_map v.failure_reason (fun f ->
             Aws.Query.Pair ("FailureReason", IpamDiscoveryFailureReason.to_query f))
       ; Aws.Util.option_map v.discovery_region (fun f ->
             Aws.Query.Pair ("DiscoveryRegion", String.to_query f))
       ; Aws.Util.option_map v.account_id (fun f ->
             Aws.Query.Pair ("AccountId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.organizational_unit_id (fun f ->
             "organizationalUnitId", String.to_json f)
       ; Aws.Util.option_map v.last_successful_discovery_time (fun f ->
             "lastSuccessfulDiscoveryTime", DateTime.to_json f)
       ; Aws.Util.option_map v.last_attempted_discovery_time (fun f ->
             "lastAttemptedDiscoveryTime", DateTime.to_json f)
       ; Aws.Util.option_map v.failure_reason (fun f ->
             "failureReason", IpamDiscoveryFailureReason.to_json f)
       ; Aws.Util.option_map v.discovery_region (fun f ->
             "discoveryRegion", String.to_json f)
       ; Aws.Util.option_map v.account_id (fun f -> "accountId", String.to_json f)
       ])

let of_json j =
  { account_id = Aws.Util.option_map (Aws.Json.lookup j "accountId") String.of_json
  ; discovery_region =
      Aws.Util.option_map (Aws.Json.lookup j "discoveryRegion") String.of_json
  ; failure_reason =
      Aws.Util.option_map
        (Aws.Json.lookup j "failureReason")
        IpamDiscoveryFailureReason.of_json
  ; last_attempted_discovery_time =
      Aws.Util.option_map
        (Aws.Json.lookup j "lastAttemptedDiscoveryTime")
        DateTime.of_json
  ; last_successful_discovery_time =
      Aws.Util.option_map
        (Aws.Json.lookup j "lastSuccessfulDiscoveryTime")
        DateTime.of_json
  ; organizational_unit_id =
      Aws.Util.option_map (Aws.Json.lookup j "organizationalUnitId") String.of_json
  }
