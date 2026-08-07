open Aws.BaseTypes

type t =
  { health_check_id : String.t
  ; health_check_version : Long.t option
  ; i_p_address : String.t option
  ; port : Integer.t option
  ; resource_path : String.t option
  ; fully_qualified_domain_name : String.t option
  ; search_string : String.t option
  ; failure_threshold : Integer.t option
  ; inverted : Boolean.t option
  ; disabled : Boolean.t option
  ; health_threshold : Integer.t option
  ; child_health_checks : ChildHealthCheckList.t
  ; enable_s_n_i : Boolean.t option
  ; regions : HealthCheckRegionList.t
  ; alarm_identifier : AlarmIdentifier.t option
  ; insufficient_data_health_status : InsufficientDataHealthStatus.t option
  ; reset_elements : ResettableElementNameList.t
  }

let make
    ~health_check_id
    ?health_check_version
    ?i_p_address
    ?port
    ?resource_path
    ?fully_qualified_domain_name
    ?search_string
    ?failure_threshold
    ?inverted
    ?disabled
    ?health_threshold
    ?(child_health_checks = [])
    ?enable_s_n_i
    ?(regions = [])
    ?alarm_identifier
    ?insufficient_data_health_status
    ?(reset_elements = [])
    () =
  { health_check_id
  ; health_check_version
  ; i_p_address
  ; port
  ; resource_path
  ; fully_qualified_domain_name
  ; search_string
  ; failure_threshold
  ; inverted
  ; disabled
  ; health_threshold
  ; child_health_checks
  ; enable_s_n_i
  ; regions
  ; alarm_identifier
  ; insufficient_data_health_status
  ; reset_elements
  }

let parse xml =
  Some
    { health_check_id =
        Aws.Xml.required
          "HealthCheckId"
          (Aws.Util.option_bind (Aws.Xml.member "HealthCheckId" xml) String.parse)
    ; health_check_version =
        Aws.Util.option_bind (Aws.Xml.member "HealthCheckVersion" xml) Long.parse
    ; i_p_address = Aws.Util.option_bind (Aws.Xml.member "IPAddress" xml) String.parse
    ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
    ; resource_path =
        Aws.Util.option_bind (Aws.Xml.member "ResourcePath" xml) String.parse
    ; fully_qualified_domain_name =
        Aws.Util.option_bind (Aws.Xml.member "FullyQualifiedDomainName" xml) String.parse
    ; search_string =
        Aws.Util.option_bind (Aws.Xml.member "SearchString" xml) String.parse
    ; failure_threshold =
        Aws.Util.option_bind (Aws.Xml.member "FailureThreshold" xml) Integer.parse
    ; inverted = Aws.Util.option_bind (Aws.Xml.member "Inverted" xml) Boolean.parse
    ; disabled = Aws.Util.option_bind (Aws.Xml.member "Disabled" xml) Boolean.parse
    ; health_threshold =
        Aws.Util.option_bind (Aws.Xml.member "HealthThreshold" xml) Integer.parse
    ; child_health_checks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ChildHealthChecks" xml)
             ChildHealthCheckList.parse)
    ; enable_s_n_i = Aws.Util.option_bind (Aws.Xml.member "EnableSNI" xml) Boolean.parse
    ; regions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Regions" xml)
             HealthCheckRegionList.parse)
    ; alarm_identifier =
        Aws.Util.option_bind (Aws.Xml.member "AlarmIdentifier" xml) AlarmIdentifier.parse
    ; insufficient_data_health_status =
        Aws.Util.option_bind
          (Aws.Xml.member "InsufficientDataHealthStatus" xml)
          InsufficientDataHealthStatus.parse
    ; reset_elements =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ResetElements" xml)
             ResettableElementNameList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("ResetElements.member", ResettableElementNameList.to_query v.reset_elements))
       ; Aws.Util.option_map v.insufficient_data_health_status (fun f ->
             Aws.Query.Pair
               ("InsufficientDataHealthStatus", InsufficientDataHealthStatus.to_query f))
       ; Aws.Util.option_map v.alarm_identifier (fun f ->
             Aws.Query.Pair ("AlarmIdentifier", AlarmIdentifier.to_query f))
       ; Some
           (Aws.Query.Pair ("Regions.member", HealthCheckRegionList.to_query v.regions))
       ; Aws.Util.option_map v.enable_s_n_i (fun f ->
             Aws.Query.Pair ("EnableSNI", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ChildHealthChecks.member"
              , ChildHealthCheckList.to_query v.child_health_checks ))
       ; Aws.Util.option_map v.health_threshold (fun f ->
             Aws.Query.Pair ("HealthThreshold", Integer.to_query f))
       ; Aws.Util.option_map v.disabled (fun f ->
             Aws.Query.Pair ("Disabled", Boolean.to_query f))
       ; Aws.Util.option_map v.inverted (fun f ->
             Aws.Query.Pair ("Inverted", Boolean.to_query f))
       ; Aws.Util.option_map v.failure_threshold (fun f ->
             Aws.Query.Pair ("FailureThreshold", Integer.to_query f))
       ; Aws.Util.option_map v.search_string (fun f ->
             Aws.Query.Pair ("SearchString", String.to_query f))
       ; Aws.Util.option_map v.fully_qualified_domain_name (fun f ->
             Aws.Query.Pair ("FullyQualifiedDomainName", String.to_query f))
       ; Aws.Util.option_map v.resource_path (fun f ->
             Aws.Query.Pair ("ResourcePath", String.to_query f))
       ; Aws.Util.option_map v.port (fun f -> Aws.Query.Pair ("Port", Integer.to_query f))
       ; Aws.Util.option_map v.i_p_address (fun f ->
             Aws.Query.Pair ("IPAddress", String.to_query f))
       ; Aws.Util.option_map v.health_check_version (fun f ->
             Aws.Query.Pair ("HealthCheckVersion", Long.to_query f))
       ; Some (Aws.Query.Pair ("HealthCheckId", String.to_query v.health_check_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ResetElements", ResettableElementNameList.to_json v.reset_elements)
       ; Aws.Util.option_map v.insufficient_data_health_status (fun f ->
             "InsufficientDataHealthStatus", InsufficientDataHealthStatus.to_json f)
       ; Aws.Util.option_map v.alarm_identifier (fun f ->
             "AlarmIdentifier", AlarmIdentifier.to_json f)
       ; Some ("Regions", HealthCheckRegionList.to_json v.regions)
       ; Aws.Util.option_map v.enable_s_n_i (fun f -> "EnableSNI", Boolean.to_json f)
       ; Some ("ChildHealthChecks", ChildHealthCheckList.to_json v.child_health_checks)
       ; Aws.Util.option_map v.health_threshold (fun f ->
             "HealthThreshold", Integer.to_json f)
       ; Aws.Util.option_map v.disabled (fun f -> "Disabled", Boolean.to_json f)
       ; Aws.Util.option_map v.inverted (fun f -> "Inverted", Boolean.to_json f)
       ; Aws.Util.option_map v.failure_threshold (fun f ->
             "FailureThreshold", Integer.to_json f)
       ; Aws.Util.option_map v.search_string (fun f -> "SearchString", String.to_json f)
       ; Aws.Util.option_map v.fully_qualified_domain_name (fun f ->
             "FullyQualifiedDomainName", String.to_json f)
       ; Aws.Util.option_map v.resource_path (fun f -> "ResourcePath", String.to_json f)
       ; Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
       ; Aws.Util.option_map v.i_p_address (fun f -> "IPAddress", String.to_json f)
       ; Aws.Util.option_map v.health_check_version (fun f ->
             "HealthCheckVersion", Long.to_json f)
       ; Some ("HealthCheckId", String.to_json v.health_check_id)
       ])

let of_json j =
  { health_check_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheckId"))
  ; health_check_version =
      Aws.Util.option_map (Aws.Json.lookup j "HealthCheckVersion") Long.of_json
  ; i_p_address = Aws.Util.option_map (Aws.Json.lookup j "IPAddress") String.of_json
  ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
  ; resource_path = Aws.Util.option_map (Aws.Json.lookup j "ResourcePath") String.of_json
  ; fully_qualified_domain_name =
      Aws.Util.option_map (Aws.Json.lookup j "FullyQualifiedDomainName") String.of_json
  ; search_string = Aws.Util.option_map (Aws.Json.lookup j "SearchString") String.of_json
  ; failure_threshold =
      Aws.Util.option_map (Aws.Json.lookup j "FailureThreshold") Integer.of_json
  ; inverted = Aws.Util.option_map (Aws.Json.lookup j "Inverted") Boolean.of_json
  ; disabled = Aws.Util.option_map (Aws.Json.lookup j "Disabled") Boolean.of_json
  ; health_threshold =
      Aws.Util.option_map (Aws.Json.lookup j "HealthThreshold") Integer.of_json
  ; child_health_checks =
      ChildHealthCheckList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ChildHealthChecks"))
  ; enable_s_n_i = Aws.Util.option_map (Aws.Json.lookup j "EnableSNI") Boolean.of_json
  ; regions =
      HealthCheckRegionList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Regions"))
  ; alarm_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "AlarmIdentifier") AlarmIdentifier.of_json
  ; insufficient_data_health_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "InsufficientDataHealthStatus")
        InsufficientDataHealthStatus.of_json
  ; reset_elements =
      ResettableElementNameList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ResetElements"))
  }
