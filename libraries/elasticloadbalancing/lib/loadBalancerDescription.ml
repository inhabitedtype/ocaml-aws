open Aws.BaseTypes

type t =
  { load_balancer_name : String.t option
  ; d_n_s_name : String.t option
  ; canonical_hosted_zone_name : String.t option
  ; canonical_hosted_zone_name_i_d : String.t option
  ; listener_descriptions : ListenerDescriptions.t
  ; policies : Policies.t option
  ; backend_server_descriptions : BackendServerDescriptions.t
  ; availability_zones : AvailabilityZones.t
  ; subnets : Subnets.t
  ; v_p_c_id : String.t option
  ; instances : Instances.t
  ; health_check : HealthCheck.t option
  ; source_security_group : SourceSecurityGroup.t option
  ; security_groups : SecurityGroups.t
  ; created_time : DateTime.t option
  ; scheme : String.t option
  }

let make
    ?load_balancer_name
    ?d_n_s_name
    ?canonical_hosted_zone_name
    ?canonical_hosted_zone_name_i_d
    ?(listener_descriptions = [])
    ?policies
    ?(backend_server_descriptions = [])
    ?(availability_zones = [])
    ?(subnets = [])
    ?v_p_c_id
    ?(instances = [])
    ?health_check
    ?source_security_group
    ?(security_groups = [])
    ?created_time
    ?scheme
    () =
  { load_balancer_name
  ; d_n_s_name
  ; canonical_hosted_zone_name
  ; canonical_hosted_zone_name_i_d
  ; listener_descriptions
  ; policies
  ; backend_server_descriptions
  ; availability_zones
  ; subnets
  ; v_p_c_id
  ; instances
  ; health_check
  ; source_security_group
  ; security_groups
  ; created_time
  ; scheme
  }

let parse xml =
  Some
    { load_balancer_name =
        Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse
    ; d_n_s_name = Aws.Util.option_bind (Aws.Xml.member "DNSName" xml) String.parse
    ; canonical_hosted_zone_name =
        Aws.Util.option_bind (Aws.Xml.member "CanonicalHostedZoneName" xml) String.parse
    ; canonical_hosted_zone_name_i_d =
        Aws.Util.option_bind (Aws.Xml.member "CanonicalHostedZoneNameID" xml) String.parse
    ; listener_descriptions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ListenerDescriptions" xml)
             ListenerDescriptions.parse)
    ; policies = Aws.Util.option_bind (Aws.Xml.member "Policies" xml) Policies.parse
    ; backend_server_descriptions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "BackendServerDescriptions" xml)
             BackendServerDescriptions.parse)
    ; availability_zones =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AvailabilityZones" xml)
             AvailabilityZones.parse)
    ; subnets =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Subnets" xml) Subnets.parse)
    ; v_p_c_id = Aws.Util.option_bind (Aws.Xml.member "VPCId" xml) String.parse
    ; instances =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Instances" xml) Instances.parse)
    ; health_check =
        Aws.Util.option_bind (Aws.Xml.member "HealthCheck" xml) HealthCheck.parse
    ; source_security_group =
        Aws.Util.option_bind
          (Aws.Xml.member "SourceSecurityGroup" xml)
          SourceSecurityGroup.parse
    ; security_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroups" xml)
             SecurityGroups.parse)
    ; created_time =
        Aws.Util.option_bind (Aws.Xml.member "CreatedTime" xml) DateTime.parse
    ; scheme = Aws.Util.option_bind (Aws.Xml.member "Scheme" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.scheme (fun f ->
             Aws.Query.Pair ("Scheme", String.to_query f))
       ; Aws.Util.option_map v.created_time (fun f ->
             Aws.Query.Pair ("CreatedTime", DateTime.to_query f))
       ; Some
           (Aws.Query.Pair
              ("SecurityGroups.member", SecurityGroups.to_query v.security_groups))
       ; Aws.Util.option_map v.source_security_group (fun f ->
             Aws.Query.Pair ("SourceSecurityGroup", SourceSecurityGroup.to_query f))
       ; Aws.Util.option_map v.health_check (fun f ->
             Aws.Query.Pair ("HealthCheck", HealthCheck.to_query f))
       ; Some (Aws.Query.Pair ("Instances.member", Instances.to_query v.instances))
       ; Aws.Util.option_map v.v_p_c_id (fun f ->
             Aws.Query.Pair ("VPCId", String.to_query f))
       ; Some (Aws.Query.Pair ("Subnets.member", Subnets.to_query v.subnets))
       ; Some
           (Aws.Query.Pair
              ("AvailabilityZones.member", AvailabilityZones.to_query v.availability_zones))
       ; Some
           (Aws.Query.Pair
              ( "BackendServerDescriptions.member"
              , BackendServerDescriptions.to_query v.backend_server_descriptions ))
       ; Aws.Util.option_map v.policies (fun f ->
             Aws.Query.Pair ("Policies", Policies.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ListenerDescriptions.member"
              , ListenerDescriptions.to_query v.listener_descriptions ))
       ; Aws.Util.option_map v.canonical_hosted_zone_name_i_d (fun f ->
             Aws.Query.Pair ("CanonicalHostedZoneNameID", String.to_query f))
       ; Aws.Util.option_map v.canonical_hosted_zone_name (fun f ->
             Aws.Query.Pair ("CanonicalHostedZoneName", String.to_query f))
       ; Aws.Util.option_map v.d_n_s_name (fun f ->
             Aws.Query.Pair ("DNSName", String.to_query f))
       ; Aws.Util.option_map v.load_balancer_name (fun f ->
             Aws.Query.Pair ("LoadBalancerName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.scheme (fun f -> "Scheme", String.to_json f)
       ; Aws.Util.option_map v.created_time (fun f -> "CreatedTime", DateTime.to_json f)
       ; Some ("SecurityGroups", SecurityGroups.to_json v.security_groups)
       ; Aws.Util.option_map v.source_security_group (fun f ->
             "SourceSecurityGroup", SourceSecurityGroup.to_json f)
       ; Aws.Util.option_map v.health_check (fun f ->
             "HealthCheck", HealthCheck.to_json f)
       ; Some ("Instances", Instances.to_json v.instances)
       ; Aws.Util.option_map v.v_p_c_id (fun f -> "VPCId", String.to_json f)
       ; Some ("Subnets", Subnets.to_json v.subnets)
       ; Some ("AvailabilityZones", AvailabilityZones.to_json v.availability_zones)
       ; Some
           ( "BackendServerDescriptions"
           , BackendServerDescriptions.to_json v.backend_server_descriptions )
       ; Aws.Util.option_map v.policies (fun f -> "Policies", Policies.to_json f)
       ; Some
           ("ListenerDescriptions", ListenerDescriptions.to_json v.listener_descriptions)
       ; Aws.Util.option_map v.canonical_hosted_zone_name_i_d (fun f ->
             "CanonicalHostedZoneNameID", String.to_json f)
       ; Aws.Util.option_map v.canonical_hosted_zone_name (fun f ->
             "CanonicalHostedZoneName", String.to_json f)
       ; Aws.Util.option_map v.d_n_s_name (fun f -> "DNSName", String.to_json f)
       ; Aws.Util.option_map v.load_balancer_name (fun f ->
             "LoadBalancerName", String.to_json f)
       ])

let of_json j =
  { load_balancer_name =
      Aws.Util.option_map (Aws.Json.lookup j "LoadBalancerName") String.of_json
  ; d_n_s_name = Aws.Util.option_map (Aws.Json.lookup j "DNSName") String.of_json
  ; canonical_hosted_zone_name =
      Aws.Util.option_map (Aws.Json.lookup j "CanonicalHostedZoneName") String.of_json
  ; canonical_hosted_zone_name_i_d =
      Aws.Util.option_map (Aws.Json.lookup j "CanonicalHostedZoneNameID") String.of_json
  ; listener_descriptions =
      ListenerDescriptions.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ListenerDescriptions"))
  ; policies = Aws.Util.option_map (Aws.Json.lookup j "Policies") Policies.of_json
  ; backend_server_descriptions =
      BackendServerDescriptions.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "BackendServerDescriptions"))
  ; availability_zones =
      AvailabilityZones.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZones"))
  ; subnets = Subnets.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Subnets"))
  ; v_p_c_id = Aws.Util.option_map (Aws.Json.lookup j "VPCId") String.of_json
  ; instances = Instances.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Instances"))
  ; health_check =
      Aws.Util.option_map (Aws.Json.lookup j "HealthCheck") HealthCheck.of_json
  ; source_security_group =
      Aws.Util.option_map
        (Aws.Json.lookup j "SourceSecurityGroup")
        SourceSecurityGroup.of_json
  ; security_groups =
      SecurityGroups.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroups"))
  ; created_time = Aws.Util.option_map (Aws.Json.lookup j "CreatedTime") DateTime.of_json
  ; scheme = Aws.Util.option_map (Aws.Json.lookup j "Scheme") String.of_json
  }
