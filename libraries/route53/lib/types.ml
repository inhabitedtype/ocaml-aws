open Aws
open Aws.BaseTypes
open CalendarLib

type calendar = Calendar.t

module ResourceRecord = struct
  type t = { value : String.t }

  let make ~value () = { value }

  let parse xml =
    Some
      { value =
          Xml.required "Value" (Util.option_bind (Xml.member "Value" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt [ Some (Query.Pair ("Value", String.to_query v.value)) ])

  let to_json v = `Assoc (Util.list_filter_opt [ Some ("value", String.to_json v.value) ])

  let of_json j = { value = String.of_json (Util.of_option_exn (Json.lookup j "value")) }
end

module AliasTarget = struct
  type t =
    { hosted_zone_id : String.t
    ; d_n_s_name : String.t
    ; evaluate_target_health : Boolean.t
    }

  let make ~hosted_zone_id ~d_n_s_name ~evaluate_target_health () =
    { hosted_zone_id; d_n_s_name; evaluate_target_health }

  let parse xml =
    Some
      { hosted_zone_id =
          Xml.required
            "HostedZoneId"
            (Util.option_bind (Xml.member "HostedZoneId" xml) String.parse)
      ; d_n_s_name =
          Xml.required
            "DNSName"
            (Util.option_bind (Xml.member "DNSName" xml) String.parse)
      ; evaluate_target_health =
          Xml.required
            "EvaluateTargetHealth"
            (Util.option_bind (Xml.member "EvaluateTargetHealth" xml) Boolean.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("EvaluateTargetHealth", Boolean.to_query v.evaluate_target_health))
         ; Some (Query.Pair ("DNSName", String.to_query v.d_n_s_name))
         ; Some (Query.Pair ("HostedZoneId", String.to_query v.hosted_zone_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("evaluate_target_health", Boolean.to_json v.evaluate_target_health)
         ; Some ("d_n_s_name", String.to_json v.d_n_s_name)
         ; Some ("hosted_zone_id", String.to_json v.hosted_zone_id)
         ])

  let of_json j =
    { hosted_zone_id =
        String.of_json (Util.of_option_exn (Json.lookup j "hosted_zone_id"))
    ; d_n_s_name = String.of_json (Util.of_option_exn (Json.lookup j "d_n_s_name"))
    ; evaluate_target_health =
        Boolean.of_json (Util.of_option_exn (Json.lookup j "evaluate_target_health"))
    }
end

module GeoLocation = struct
  type t =
    { continent_code : String.t option
    ; country_code : String.t option
    ; subdivision_code : String.t option
    }

  let make ?continent_code ?country_code ?subdivision_code () =
    { continent_code; country_code; subdivision_code }

  let parse xml =
    Some
      { continent_code = Util.option_bind (Xml.member "ContinentCode" xml) String.parse
      ; country_code = Util.option_bind (Xml.member "CountryCode" xml) String.parse
      ; subdivision_code =
          Util.option_bind (Xml.member "SubdivisionCode" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.subdivision_code (fun f ->
               Query.Pair ("SubdivisionCode", String.to_query f))
         ; Util.option_map v.country_code (fun f ->
               Query.Pair ("CountryCode", String.to_query f))
         ; Util.option_map v.continent_code (fun f ->
               Query.Pair ("ContinentCode", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.subdivision_code (fun f ->
               "subdivision_code", String.to_json f)
         ; Util.option_map v.country_code (fun f -> "country_code", String.to_json f)
         ; Util.option_map v.continent_code (fun f -> "continent_code", String.to_json f)
         ])

  let of_json j =
    { continent_code = Util.option_map (Json.lookup j "continent_code") String.of_json
    ; country_code = Util.option_map (Json.lookup j "country_code") String.of_json
    ; subdivision_code = Util.option_map (Json.lookup j "subdivision_code") String.of_json
    }
end

module RRType = struct
  type t =
    | SOA
    | A
    | TXT
    | NS
    | CNAME
    | MX
    | PTR
    | SRV
    | SPF
    | AAAA

  let str_to_t =
    [ "AAAA", AAAA
    ; "SPF", SPF
    ; "SRV", SRV
    ; "PTR", PTR
    ; "MX", MX
    ; "CNAME", CNAME
    ; "NS", NS
    ; "TXT", TXT
    ; "A", A
    ; "SOA", SOA
    ]

  let t_to_str =
    [ AAAA, "AAAA"
    ; SPF, "SPF"
    ; SRV, "SRV"
    ; PTR, "PTR"
    ; MX, "MX"
    ; CNAME, "CNAME"
    ; NS, "NS"
    ; TXT, "TXT"
    ; A, "A"
    ; SOA, "SOA"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module ResourceRecordSetFailover = struct
  type t =
    | PRIMARY
    | SECONDARY

  let str_to_t = [ "SECONDARY", SECONDARY; "PRIMARY", PRIMARY ]

  let t_to_str = [ SECONDARY, "SECONDARY"; PRIMARY, "PRIMARY" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module ResourceRecordSetRegion = struct
  type t =
    | Us_east_1
    | Us_west_1
    | Us_west_2
    | Eu_west_1
    | Eu_central_1
    | Ap_southeast_1
    | Ap_southeast_2
    | Ap_northeast_1
    | Sa_east_1
    | Cn_north_1

  let str_to_t =
    [ "cn-north-1", Cn_north_1
    ; "sa-east-1", Sa_east_1
    ; "ap-northeast-1", Ap_northeast_1
    ; "ap-southeast-2", Ap_southeast_2
    ; "ap-southeast-1", Ap_southeast_1
    ; "eu-central-1", Eu_central_1
    ; "eu-west-1", Eu_west_1
    ; "us-west-2", Us_west_2
    ; "us-west-1", Us_west_1
    ; "us-east-1", Us_east_1
    ]

  let t_to_str =
    [ Cn_north_1, "cn-north-1"
    ; Sa_east_1, "sa-east-1"
    ; Ap_northeast_1, "ap-northeast-1"
    ; Ap_southeast_2, "ap-southeast-2"
    ; Ap_southeast_1, "ap-southeast-1"
    ; Eu_central_1, "eu-central-1"
    ; Eu_west_1, "eu-west-1"
    ; Us_west_2, "us-west-2"
    ; Us_west_1, "us-west-1"
    ; Us_east_1, "us-east-1"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module ResourceRecords = struct
  type t = ResourceRecord.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map ResourceRecord.parse (Xml.members "ResourceRecord" xml))

  let to_query v = Query.to_query_list ResourceRecord.to_query v

  let to_json v = `List (List.map ResourceRecord.to_json v)

  let of_json j = Json.to_list ResourceRecord.of_json j
end

module HealthCheckType = struct
  type t =
    | HTTP
    | HTTPS
    | HTTP_STR_MATCH
    | HTTPS_STR_MATCH
    | TCP

  let str_to_t =
    [ "TCP", TCP
    ; "HTTPS_STR_MATCH", HTTPS_STR_MATCH
    ; "HTTP_STR_MATCH", HTTP_STR_MATCH
    ; "HTTPS", HTTPS
    ; "HTTP", HTTP
    ]

  let t_to_str =
    [ TCP, "TCP"
    ; HTTPS_STR_MATCH, "HTTPS_STR_MATCH"
    ; HTTP_STR_MATCH, "HTTP_STR_MATCH"
    ; HTTPS, "HTTPS"
    ; HTTP, "HTTP"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module Tag = struct
  type t =
    { key : String.t option
    ; value : String.t option
    }

  let make ?key ?value () = { key; value }

  let parse xml =
    Some
      { key = Util.option_bind (Xml.member "Key" xml) String.parse
      ; value = Util.option_bind (Xml.member "Value" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.value (fun f -> Query.Pair ("Value", String.to_query f))
         ; Util.option_map v.key (fun f -> Query.Pair ("Key", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.value (fun f -> "value", String.to_json f)
         ; Util.option_map v.key (fun f -> "key", String.to_json f)
         ])

  let of_json j =
    { key = Util.option_map (Json.lookup j "key") String.of_json
    ; value = Util.option_map (Json.lookup j "value") String.of_json
    }
end

module ChangeAction = struct
  type t =
    | CREATE
    | DELETE
    | UPSERT

  let str_to_t = [ "UPSERT", UPSERT; "DELETE", DELETE; "CREATE", CREATE ]

  let t_to_str = [ UPSERT, "UPSERT"; DELETE, "DELETE"; CREATE, "CREATE" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module ResourceRecordSet = struct
  type t =
    { name : String.t
    ; type_ : RRType.t
    ; set_identifier : String.t option
    ; weight : Long.t option
    ; region : ResourceRecordSetRegion.t option
    ; geo_location : GeoLocation.t option
    ; failover : ResourceRecordSetFailover.t option
    ; t_t_l : Long.t option
    ; resource_records : ResourceRecords.t
    ; alias_target : AliasTarget.t option
    ; health_check_id : String.t option
    }

  let make
      ~name
      ~type_
      ?set_identifier
      ?weight
      ?region
      ?geo_location
      ?failover
      ?t_t_l
      ?(resource_records = [])
      ?alias_target
      ?health_check_id
      () =
    { name
    ; type_
    ; set_identifier
    ; weight
    ; region
    ; geo_location
    ; failover
    ; t_t_l
    ; resource_records
    ; alias_target
    ; health_check_id
    }

  let parse xml =
    Some
      { name = Xml.required "Name" (Util.option_bind (Xml.member "Name" xml) String.parse)
      ; type_ =
          Xml.required "Type" (Util.option_bind (Xml.member "Type" xml) RRType.parse)
      ; set_identifier = Util.option_bind (Xml.member "SetIdentifier" xml) String.parse
      ; weight = Util.option_bind (Xml.member "Weight" xml) Long.parse
      ; region = Util.option_bind (Xml.member "Region" xml) ResourceRecordSetRegion.parse
      ; geo_location = Util.option_bind (Xml.member "GeoLocation" xml) GeoLocation.parse
      ; failover =
          Util.option_bind (Xml.member "Failover" xml) ResourceRecordSetFailover.parse
      ; t_t_l = Util.option_bind (Xml.member "TTL" xml) Long.parse
      ; resource_records =
          Util.of_option
            []
            (Util.option_bind (Xml.member "ResourceRecords" xml) ResourceRecords.parse)
      ; alias_target = Util.option_bind (Xml.member "AliasTarget" xml) AliasTarget.parse
      ; health_check_id = Util.option_bind (Xml.member "HealthCheckId" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.health_check_id (fun f ->
               Query.Pair ("HealthCheckId", String.to_query f))
         ; Util.option_map v.alias_target (fun f ->
               Query.Pair ("AliasTarget", AliasTarget.to_query f))
         ; Some
             (Query.Pair
                ("ResourceRecords.member", ResourceRecords.to_query v.resource_records))
         ; Util.option_map v.t_t_l (fun f -> Query.Pair ("TTL", Long.to_query f))
         ; Util.option_map v.failover (fun f ->
               Query.Pair ("Failover", ResourceRecordSetFailover.to_query f))
         ; Util.option_map v.geo_location (fun f ->
               Query.Pair ("GeoLocation", GeoLocation.to_query f))
         ; Util.option_map v.region (fun f ->
               Query.Pair ("Region", ResourceRecordSetRegion.to_query f))
         ; Util.option_map v.weight (fun f -> Query.Pair ("Weight", Long.to_query f))
         ; Util.option_map v.set_identifier (fun f ->
               Query.Pair ("SetIdentifier", String.to_query f))
         ; Some (Query.Pair ("Type", RRType.to_query v.type_))
         ; Some (Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.health_check_id (fun f ->
               "health_check_id", String.to_json f)
         ; Util.option_map v.alias_target (fun f -> "alias_target", AliasTarget.to_json f)
         ; Some ("resource_records", ResourceRecords.to_json v.resource_records)
         ; Util.option_map v.t_t_l (fun f -> "t_t_l", Long.to_json f)
         ; Util.option_map v.failover (fun f ->
               "failover", ResourceRecordSetFailover.to_json f)
         ; Util.option_map v.geo_location (fun f -> "geo_location", GeoLocation.to_json f)
         ; Util.option_map v.region (fun f -> "region", ResourceRecordSetRegion.to_json f)
         ; Util.option_map v.weight (fun f -> "weight", Long.to_json f)
         ; Util.option_map v.set_identifier (fun f -> "set_identifier", String.to_json f)
         ; Some ("type_", RRType.to_json v.type_)
         ; Some ("name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Util.of_option_exn (Json.lookup j "name"))
    ; type_ = RRType.of_json (Util.of_option_exn (Json.lookup j "type_"))
    ; set_identifier = Util.option_map (Json.lookup j "set_identifier") String.of_json
    ; weight = Util.option_map (Json.lookup j "weight") Long.of_json
    ; region = Util.option_map (Json.lookup j "region") ResourceRecordSetRegion.of_json
    ; geo_location = Util.option_map (Json.lookup j "geo_location") GeoLocation.of_json
    ; failover =
        Util.option_map (Json.lookup j "failover") ResourceRecordSetFailover.of_json
    ; t_t_l = Util.option_map (Json.lookup j "t_t_l") Long.of_json
    ; resource_records =
        ResourceRecords.of_json (Util.of_option_exn (Json.lookup j "resource_records"))
    ; alias_target = Util.option_map (Json.lookup j "alias_target") AliasTarget.of_json
    ; health_check_id = Util.option_map (Json.lookup j "health_check_id") String.of_json
    }
end

module HostedZoneConfig = struct
  type t =
    { comment : String.t option
    ; private_zone : Boolean.t option
    }

  let make ?comment ?private_zone () = { comment; private_zone }

  let parse xml =
    Some
      { comment = Util.option_bind (Xml.member "Comment" xml) String.parse
      ; private_zone = Util.option_bind (Xml.member "PrivateZone" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.private_zone (fun f ->
               Query.Pair ("PrivateZone", Boolean.to_query f))
         ; Util.option_map v.comment (fun f -> Query.Pair ("Comment", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.private_zone (fun f -> "private_zone", Boolean.to_json f)
         ; Util.option_map v.comment (fun f -> "comment", String.to_json f)
         ])

  let of_json j =
    { comment = Util.option_map (Json.lookup j "comment") String.of_json
    ; private_zone = Util.option_map (Json.lookup j "private_zone") Boolean.of_json
    }
end

module StatusReport = struct
  type t =
    { status : String.t option
    ; checked_time : DateTime.t option
    }

  let make ?status ?checked_time () = { status; checked_time }

  let parse xml =
    Some
      { status = Util.option_bind (Xml.member "Status" xml) String.parse
      ; checked_time = Util.option_bind (Xml.member "CheckedTime" xml) DateTime.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.checked_time (fun f ->
               Query.Pair ("CheckedTime", DateTime.to_query f))
         ; Util.option_map v.status (fun f -> Query.Pair ("Status", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.checked_time (fun f -> "checked_time", DateTime.to_json f)
         ; Util.option_map v.status (fun f -> "status", String.to_json f)
         ])

  let of_json j =
    { status = Util.option_map (Json.lookup j "status") String.of_json
    ; checked_time = Util.option_map (Json.lookup j "checked_time") DateTime.of_json
    }
end

module HealthCheckConfig = struct
  type t =
    { i_p_address : String.t option
    ; port : Integer.t option
    ; type_ : HealthCheckType.t
    ; resource_path : String.t option
    ; fully_qualified_domain_name : String.t option
    ; search_string : String.t option
    ; request_interval : Integer.t option
    ; failure_threshold : Integer.t option
    }

  let make
      ?i_p_address
      ?port
      ~type_
      ?resource_path
      ?fully_qualified_domain_name
      ?search_string
      ?request_interval
      ?failure_threshold
      () =
    { i_p_address
    ; port
    ; type_
    ; resource_path
    ; fully_qualified_domain_name
    ; search_string
    ; request_interval
    ; failure_threshold
    }

  let parse xml =
    Some
      { i_p_address = Util.option_bind (Xml.member "IPAddress" xml) String.parse
      ; port = Util.option_bind (Xml.member "Port" xml) Integer.parse
      ; type_ =
          Xml.required
            "Type"
            (Util.option_bind (Xml.member "Type" xml) HealthCheckType.parse)
      ; resource_path = Util.option_bind (Xml.member "ResourcePath" xml) String.parse
      ; fully_qualified_domain_name =
          Util.option_bind (Xml.member "FullyQualifiedDomainName" xml) String.parse
      ; search_string = Util.option_bind (Xml.member "SearchString" xml) String.parse
      ; request_interval =
          Util.option_bind (Xml.member "RequestInterval" xml) Integer.parse
      ; failure_threshold =
          Util.option_bind (Xml.member "FailureThreshold" xml) Integer.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.failure_threshold (fun f ->
               Query.Pair ("FailureThreshold", Integer.to_query f))
         ; Util.option_map v.request_interval (fun f ->
               Query.Pair ("RequestInterval", Integer.to_query f))
         ; Util.option_map v.search_string (fun f ->
               Query.Pair ("SearchString", String.to_query f))
         ; Util.option_map v.fully_qualified_domain_name (fun f ->
               Query.Pair ("FullyQualifiedDomainName", String.to_query f))
         ; Util.option_map v.resource_path (fun f ->
               Query.Pair ("ResourcePath", String.to_query f))
         ; Some (Query.Pair ("Type", HealthCheckType.to_query v.type_))
         ; Util.option_map v.port (fun f -> Query.Pair ("Port", Integer.to_query f))
         ; Util.option_map v.i_p_address (fun f ->
               Query.Pair ("IPAddress", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.failure_threshold (fun f ->
               "failure_threshold", Integer.to_json f)
         ; Util.option_map v.request_interval (fun f ->
               "request_interval", Integer.to_json f)
         ; Util.option_map v.search_string (fun f -> "search_string", String.to_json f)
         ; Util.option_map v.fully_qualified_domain_name (fun f ->
               "fully_qualified_domain_name", String.to_json f)
         ; Util.option_map v.resource_path (fun f -> "resource_path", String.to_json f)
         ; Some ("type_", HealthCheckType.to_json v.type_)
         ; Util.option_map v.port (fun f -> "port", Integer.to_json f)
         ; Util.option_map v.i_p_address (fun f -> "i_p_address", String.to_json f)
         ])

  let of_json j =
    { i_p_address = Util.option_map (Json.lookup j "i_p_address") String.of_json
    ; port = Util.option_map (Json.lookup j "port") Integer.of_json
    ; type_ = HealthCheckType.of_json (Util.of_option_exn (Json.lookup j "type_"))
    ; resource_path = Util.option_map (Json.lookup j "resource_path") String.of_json
    ; fully_qualified_domain_name =
        Util.option_map (Json.lookup j "fully_qualified_domain_name") String.of_json
    ; search_string = Util.option_map (Json.lookup j "search_string") String.of_json
    ; request_interval =
        Util.option_map (Json.lookup j "request_interval") Integer.of_json
    ; failure_threshold =
        Util.option_map (Json.lookup j "failure_threshold") Integer.of_json
    }
end

module TagList = struct
  type t = Tag.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map Tag.parse (Xml.members "Tag" xml))

  let to_query v = Query.to_query_list Tag.to_query v

  let to_json v = `List (List.map Tag.to_json v)

  let of_json j = Json.to_list Tag.of_json j
end

module TagResourceType = struct
  type t =
    | Healthcheck
    | Hostedzone

  let str_to_t = [ "hostedzone", Hostedzone; "healthcheck", Healthcheck ]

  let t_to_str = [ Hostedzone, "hostedzone"; Healthcheck, "healthcheck" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module DelegationSetNameServers = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "NameServer" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module Change = struct
  type t =
    { action : ChangeAction.t
    ; resource_record_set : ResourceRecordSet.t
    }

  let make ~action ~resource_record_set () = { action; resource_record_set }

  let parse xml =
    Some
      { action =
          Xml.required
            "Action"
            (Util.option_bind (Xml.member "Action" xml) ChangeAction.parse)
      ; resource_record_set =
          Xml.required
            "ResourceRecordSet"
            (Util.option_bind
               (Xml.member "ResourceRecordSet" xml)
               ResourceRecordSet.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("ResourceRecordSet", ResourceRecordSet.to_query v.resource_record_set))
         ; Some (Query.Pair ("Action", ChangeAction.to_query v.action))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("resource_record_set", ResourceRecordSet.to_json v.resource_record_set)
         ; Some ("action", ChangeAction.to_json v.action)
         ])

  let of_json j =
    { action = ChangeAction.of_json (Util.of_option_exn (Json.lookup j "action"))
    ; resource_record_set =
        ResourceRecordSet.of_json
          (Util.of_option_exn (Json.lookup j "resource_record_set"))
    }
end

module VPCRegion = struct
  type t =
    | Us_east_1
    | Us_west_1
    | Us_west_2
    | Eu_west_1
    | Eu_central_1
    | Ap_southeast_1
    | Ap_southeast_2
    | Ap_northeast_1
    | Sa_east_1
    | Cn_north_1

  let str_to_t =
    [ "cn-north-1", Cn_north_1
    ; "sa-east-1", Sa_east_1
    ; "ap-northeast-1", Ap_northeast_1
    ; "ap-southeast-2", Ap_southeast_2
    ; "ap-southeast-1", Ap_southeast_1
    ; "eu-central-1", Eu_central_1
    ; "eu-west-1", Eu_west_1
    ; "us-west-2", Us_west_2
    ; "us-west-1", Us_west_1
    ; "us-east-1", Us_east_1
    ]

  let t_to_str =
    [ Cn_north_1, "cn-north-1"
    ; Sa_east_1, "sa-east-1"
    ; Ap_northeast_1, "ap-northeast-1"
    ; Ap_southeast_2, "ap-southeast-2"
    ; Ap_southeast_1, "ap-southeast-1"
    ; Eu_central_1, "eu-central-1"
    ; Eu_west_1, "eu-west-1"
    ; Us_west_2, "us-west-2"
    ; Us_west_1, "us-west-1"
    ; Us_east_1, "us-east-1"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module HostedZone = struct
  type t =
    { id : String.t
    ; name : String.t
    ; caller_reference : String.t
    ; config : HostedZoneConfig.t option
    ; resource_record_set_count : Long.t option
    }

  let make ~id ~name ~caller_reference ?config ?resource_record_set_count () =
    { id; name; caller_reference; config; resource_record_set_count }

  let parse xml =
    Some
      { id = Xml.required "Id" (Util.option_bind (Xml.member "Id" xml) String.parse)
      ; name = Xml.required "Name" (Util.option_bind (Xml.member "Name" xml) String.parse)
      ; caller_reference =
          Xml.required
            "CallerReference"
            (Util.option_bind (Xml.member "CallerReference" xml) String.parse)
      ; config = Util.option_bind (Xml.member "Config" xml) HostedZoneConfig.parse
      ; resource_record_set_count =
          Util.option_bind (Xml.member "ResourceRecordSetCount" xml) Long.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.resource_record_set_count (fun f ->
               Query.Pair ("ResourceRecordSetCount", Long.to_query f))
         ; Util.option_map v.config (fun f ->
               Query.Pair ("Config", HostedZoneConfig.to_query f))
         ; Some (Query.Pair ("CallerReference", String.to_query v.caller_reference))
         ; Some (Query.Pair ("Name", String.to_query v.name))
         ; Some (Query.Pair ("Id", String.to_query v.id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.resource_record_set_count (fun f ->
               "resource_record_set_count", Long.to_json f)
         ; Util.option_map v.config (fun f -> "config", HostedZoneConfig.to_json f)
         ; Some ("caller_reference", String.to_json v.caller_reference)
         ; Some ("name", String.to_json v.name)
         ; Some ("id", String.to_json v.id)
         ])

  let of_json j =
    { id = String.of_json (Util.of_option_exn (Json.lookup j "id"))
    ; name = String.of_json (Util.of_option_exn (Json.lookup j "name"))
    ; caller_reference =
        String.of_json (Util.of_option_exn (Json.lookup j "caller_reference"))
    ; config = Util.option_map (Json.lookup j "config") HostedZoneConfig.of_json
    ; resource_record_set_count =
        Util.option_map (Json.lookup j "resource_record_set_count") Long.of_json
    }
end

module ChangeStatus = struct
  type t =
    | PENDING
    | INSYNC

  let str_to_t = [ "INSYNC", INSYNC; "PENDING", PENDING ]

  let t_to_str = [ INSYNC, "INSYNC"; PENDING, "PENDING" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module HealthCheckObservation = struct
  type t =
    { i_p_address : String.t option
    ; status_report : StatusReport.t option
    }

  let make ?i_p_address ?status_report () = { i_p_address; status_report }

  let parse xml =
    Some
      { i_p_address = Util.option_bind (Xml.member "IPAddress" xml) String.parse
      ; status_report =
          Util.option_bind (Xml.member "StatusReport" xml) StatusReport.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.status_report (fun f ->
               Query.Pair ("StatusReport", StatusReport.to_query f))
         ; Util.option_map v.i_p_address (fun f ->
               Query.Pair ("IPAddress", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.status_report (fun f ->
               "status_report", StatusReport.to_json f)
         ; Util.option_map v.i_p_address (fun f -> "i_p_address", String.to_json f)
         ])

  let of_json j =
    { i_p_address = Util.option_map (Json.lookup j "i_p_address") String.of_json
    ; status_report = Util.option_map (Json.lookup j "status_report") StatusReport.of_json
    }
end

module GeoLocationDetails = struct
  type t =
    { continent_code : String.t option
    ; continent_name : String.t option
    ; country_code : String.t option
    ; country_name : String.t option
    ; subdivision_code : String.t option
    ; subdivision_name : String.t option
    }

  let make
      ?continent_code
      ?continent_name
      ?country_code
      ?country_name
      ?subdivision_code
      ?subdivision_name
      () =
    { continent_code
    ; continent_name
    ; country_code
    ; country_name
    ; subdivision_code
    ; subdivision_name
    }

  let parse xml =
    Some
      { continent_code = Util.option_bind (Xml.member "ContinentCode" xml) String.parse
      ; continent_name = Util.option_bind (Xml.member "ContinentName" xml) String.parse
      ; country_code = Util.option_bind (Xml.member "CountryCode" xml) String.parse
      ; country_name = Util.option_bind (Xml.member "CountryName" xml) String.parse
      ; subdivision_code =
          Util.option_bind (Xml.member "SubdivisionCode" xml) String.parse
      ; subdivision_name =
          Util.option_bind (Xml.member "SubdivisionName" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.subdivision_name (fun f ->
               Query.Pair ("SubdivisionName", String.to_query f))
         ; Util.option_map v.subdivision_code (fun f ->
               Query.Pair ("SubdivisionCode", String.to_query f))
         ; Util.option_map v.country_name (fun f ->
               Query.Pair ("CountryName", String.to_query f))
         ; Util.option_map v.country_code (fun f ->
               Query.Pair ("CountryCode", String.to_query f))
         ; Util.option_map v.continent_name (fun f ->
               Query.Pair ("ContinentName", String.to_query f))
         ; Util.option_map v.continent_code (fun f ->
               Query.Pair ("ContinentCode", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.subdivision_name (fun f ->
               "subdivision_name", String.to_json f)
         ; Util.option_map v.subdivision_code (fun f ->
               "subdivision_code", String.to_json f)
         ; Util.option_map v.country_name (fun f -> "country_name", String.to_json f)
         ; Util.option_map v.country_code (fun f -> "country_code", String.to_json f)
         ; Util.option_map v.continent_name (fun f -> "continent_name", String.to_json f)
         ; Util.option_map v.continent_code (fun f -> "continent_code", String.to_json f)
         ])

  let of_json j =
    { continent_code = Util.option_map (Json.lookup j "continent_code") String.of_json
    ; continent_name = Util.option_map (Json.lookup j "continent_name") String.of_json
    ; country_code = Util.option_map (Json.lookup j "country_code") String.of_json
    ; country_name = Util.option_map (Json.lookup j "country_name") String.of_json
    ; subdivision_code = Util.option_map (Json.lookup j "subdivision_code") String.of_json
    ; subdivision_name = Util.option_map (Json.lookup j "subdivision_name") String.of_json
    }
end

module HealthCheck = struct
  type t =
    { id : String.t
    ; caller_reference : String.t
    ; health_check_config : HealthCheckConfig.t
    ; health_check_version : Long.t
    }

  let make ~id ~caller_reference ~health_check_config ~health_check_version () =
    { id; caller_reference; health_check_config; health_check_version }

  let parse xml =
    Some
      { id = Xml.required "Id" (Util.option_bind (Xml.member "Id" xml) String.parse)
      ; caller_reference =
          Xml.required
            "CallerReference"
            (Util.option_bind (Xml.member "CallerReference" xml) String.parse)
      ; health_check_config =
          Xml.required
            "HealthCheckConfig"
            (Util.option_bind
               (Xml.member "HealthCheckConfig" xml)
               HealthCheckConfig.parse)
      ; health_check_version =
          Xml.required
            "HealthCheckVersion"
            (Util.option_bind (Xml.member "HealthCheckVersion" xml) Long.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("HealthCheckVersion", Long.to_query v.health_check_version))
         ; Some
             (Query.Pair
                ("HealthCheckConfig", HealthCheckConfig.to_query v.health_check_config))
         ; Some (Query.Pair ("CallerReference", String.to_query v.caller_reference))
         ; Some (Query.Pair ("Id", String.to_query v.id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("health_check_version", Long.to_json v.health_check_version)
         ; Some ("health_check_config", HealthCheckConfig.to_json v.health_check_config)
         ; Some ("caller_reference", String.to_json v.caller_reference)
         ; Some ("id", String.to_json v.id)
         ])

  let of_json j =
    { id = String.of_json (Util.of_option_exn (Json.lookup j "id"))
    ; caller_reference =
        String.of_json (Util.of_option_exn (Json.lookup j "caller_reference"))
    ; health_check_config =
        HealthCheckConfig.of_json
          (Util.of_option_exn (Json.lookup j "health_check_config"))
    ; health_check_version =
        Long.of_json (Util.of_option_exn (Json.lookup j "health_check_version"))
    }
end

module ResourceTagSet = struct
  type t =
    { resource_type : TagResourceType.t option
    ; resource_id : String.t option
    ; tags : TagList.t
    }

  let make ?resource_type ?resource_id ?(tags = []) () =
    { resource_type; resource_id; tags }

  let parse xml =
    Some
      { resource_type =
          Util.option_bind (Xml.member "ResourceType" xml) TagResourceType.parse
      ; resource_id = Util.option_bind (Xml.member "ResourceId" xml) String.parse
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Util.option_map v.resource_id (fun f ->
               Query.Pair ("ResourceId", String.to_query f))
         ; Util.option_map v.resource_type (fun f ->
               Query.Pair ("ResourceType", TagResourceType.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tags", TagList.to_json v.tags)
         ; Util.option_map v.resource_id (fun f -> "resource_id", String.to_json f)
         ; Util.option_map v.resource_type (fun f ->
               "resource_type", TagResourceType.to_json f)
         ])

  let of_json j =
    { resource_type =
        Util.option_map (Json.lookup j "resource_type") TagResourceType.of_json
    ; resource_id = Util.option_map (Json.lookup j "resource_id") String.of_json
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    }
end

module DelegationSet = struct
  type t =
    { id : String.t option
    ; caller_reference : String.t option
    ; name_servers : DelegationSetNameServers.t
    }

  let make ?id ?caller_reference ~name_servers () = { id; caller_reference; name_servers }

  let parse xml =
    Some
      { id = Util.option_bind (Xml.member "Id" xml) String.parse
      ; caller_reference =
          Util.option_bind (Xml.member "CallerReference" xml) String.parse
      ; name_servers =
          Xml.required
            "NameServers"
            (Util.option_bind
               (Xml.member "NameServers" xml)
               DelegationSetNameServers.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("NameServers.member", DelegationSetNameServers.to_query v.name_servers))
         ; Util.option_map v.caller_reference (fun f ->
               Query.Pair ("CallerReference", String.to_query f))
         ; Util.option_map v.id (fun f -> Query.Pair ("Id", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("name_servers", DelegationSetNameServers.to_json v.name_servers)
         ; Util.option_map v.caller_reference (fun f ->
               "caller_reference", String.to_json f)
         ; Util.option_map v.id (fun f -> "id", String.to_json f)
         ])

  let of_json j =
    { id = Util.option_map (Json.lookup j "id") String.of_json
    ; caller_reference = Util.option_map (Json.lookup j "caller_reference") String.of_json
    ; name_servers =
        DelegationSetNameServers.of_json
          (Util.of_option_exn (Json.lookup j "name_servers"))
    }
end

module Changes = struct
  type t = Change.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map Change.parse (Xml.members "Change" xml))

  let to_query v = Query.to_query_list Change.to_query v

  let to_json v = `List (List.map Change.to_json v)

  let of_json j = Json.to_list Change.of_json j
end

module VPC = struct
  type t =
    { v_p_c_region : VPCRegion.t option
    ; v_p_c_id : String.t option
    }

  let make ?v_p_c_region ?v_p_c_id () = { v_p_c_region; v_p_c_id }

  let parse xml =
    Some
      { v_p_c_region = Util.option_bind (Xml.member "VPCRegion" xml) VPCRegion.parse
      ; v_p_c_id = Util.option_bind (Xml.member "VPCId" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.v_p_c_id (fun f -> Query.Pair ("VPCId", String.to_query f))
         ; Util.option_map v.v_p_c_region (fun f ->
               Query.Pair ("VPCRegion", VPCRegion.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.v_p_c_id (fun f -> "v_p_c_id", String.to_json f)
         ; Util.option_map v.v_p_c_region (fun f -> "v_p_c_region", VPCRegion.to_json f)
         ])

  let of_json j =
    { v_p_c_region = Util.option_map (Json.lookup j "v_p_c_region") VPCRegion.of_json
    ; v_p_c_id = Util.option_map (Json.lookup j "v_p_c_id") String.of_json
    }
end

module HostedZones = struct
  type t = HostedZone.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map HostedZone.parse (Xml.members "HostedZone" xml))

  let to_query v = Query.to_query_list HostedZone.to_query v

  let to_json v = `List (List.map HostedZone.to_json v)

  let of_json j = Json.to_list HostedZone.of_json j
end

module ChangeInfo = struct
  type t =
    { id : String.t
    ; status : ChangeStatus.t
    ; submitted_at : DateTime.t
    ; comment : String.t option
    }

  let make ~id ~status ~submitted_at ?comment () = { id; status; submitted_at; comment }

  let parse xml =
    Some
      { id = Xml.required "Id" (Util.option_bind (Xml.member "Id" xml) String.parse)
      ; status =
          Xml.required
            "Status"
            (Util.option_bind (Xml.member "Status" xml) ChangeStatus.parse)
      ; submitted_at =
          Xml.required
            "SubmittedAt"
            (Util.option_bind (Xml.member "SubmittedAt" xml) DateTime.parse)
      ; comment = Util.option_bind (Xml.member "Comment" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.comment (fun f -> Query.Pair ("Comment", String.to_query f))
         ; Some (Query.Pair ("SubmittedAt", DateTime.to_query v.submitted_at))
         ; Some (Query.Pair ("Status", ChangeStatus.to_query v.status))
         ; Some (Query.Pair ("Id", String.to_query v.id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.comment (fun f -> "comment", String.to_json f)
         ; Some ("submitted_at", DateTime.to_json v.submitted_at)
         ; Some ("status", ChangeStatus.to_json v.status)
         ; Some ("id", String.to_json v.id)
         ])

  let of_json j =
    { id = String.of_json (Util.of_option_exn (Json.lookup j "id"))
    ; status = ChangeStatus.of_json (Util.of_option_exn (Json.lookup j "status"))
    ; submitted_at = DateTime.of_json (Util.of_option_exn (Json.lookup j "submitted_at"))
    ; comment = Util.option_map (Json.lookup j "comment") String.of_json
    }
end

module HealthCheckObservations = struct
  type t = HealthCheckObservation.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map HealthCheckObservation.parse (Xml.members "HealthCheckObservation" xml))

  let to_query v = Query.to_query_list HealthCheckObservation.to_query v

  let to_json v = `List (List.map HealthCheckObservation.to_json v)

  let of_json j = Json.to_list HealthCheckObservation.of_json j
end

module CheckerIpRanges = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module ErrorMessages = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "Message" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module TagResourceIdList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "ResourceId" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module GeoLocationDetailsList = struct
  type t = GeoLocationDetails.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map GeoLocationDetails.parse (Xml.members "GeoLocationDetails" xml))

  let to_query v = Query.to_query_list GeoLocationDetails.to_query v

  let to_json v = `List (List.map GeoLocationDetails.to_json v)

  let of_json j = Json.to_list GeoLocationDetails.of_json j
end

module HealthChecks = struct
  type t = HealthCheck.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map HealthCheck.parse (Xml.members "HealthCheck" xml))

  let to_query v = Query.to_query_list HealthCheck.to_query v

  let to_json v = `List (List.map HealthCheck.to_json v)

  let of_json j = Json.to_list HealthCheck.of_json j
end

module TagKeyList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "Key" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module ResourceTagSetList = struct
  type t = ResourceTagSet.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map ResourceTagSet.parse (Xml.members "ResourceTagSet" xml))

  let to_query v = Query.to_query_list ResourceTagSet.to_query v

  let to_json v = `List (List.map ResourceTagSet.to_json v)

  let of_json j = Json.to_list ResourceTagSet.of_json j
end

module ResourceRecordSets = struct
  type t = ResourceRecordSet.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map ResourceRecordSet.parse (Xml.members "ResourceRecordSet" xml))

  let to_query v = Query.to_query_list ResourceRecordSet.to_query v

  let to_json v = `List (List.map ResourceRecordSet.to_json v)

  let of_json j = Json.to_list ResourceRecordSet.of_json j
end

module DelegationSets = struct
  type t = DelegationSet.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map DelegationSet.parse (Xml.members "DelegationSet" xml))

  let to_query v = Query.to_query_list DelegationSet.to_query v

  let to_json v = `List (List.map DelegationSet.to_json v)

  let of_json j = Json.to_list DelegationSet.of_json j
end

module ChangeBatch = struct
  type t =
    { comment : String.t option
    ; changes : Changes.t
    }

  let make ?comment ~changes () = { comment; changes }

  let parse xml =
    Some
      { comment = Util.option_bind (Xml.member "Comment" xml) String.parse
      ; changes =
          Xml.required
            "Changes"
            (Util.option_bind (Xml.member "Changes" xml) Changes.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Changes.member", Changes.to_query v.changes))
         ; Util.option_map v.comment (fun f -> Query.Pair ("Comment", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("changes", Changes.to_json v.changes)
         ; Util.option_map v.comment (fun f -> "comment", String.to_json f)
         ])

  let of_json j =
    { comment = Util.option_map (Json.lookup j "comment") String.of_json
    ; changes = Changes.of_json (Util.of_option_exn (Json.lookup j "changes"))
    }
end

module VPCs = struct
  type t = VPC.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map VPC.parse (Xml.members "VPC" xml))

  let to_query v = Query.to_query_list VPC.to_query v

  let to_json v = `List (List.map VPC.to_json v)

  let of_json j = Json.to_list VPC.of_json j
end

module ListHostedZonesByNameResponse = struct
  type t =
    { hosted_zones : HostedZones.t
    ; d_n_s_name : String.t option
    ; hosted_zone_id : String.t option
    ; is_truncated : Boolean.t
    ; next_d_n_s_name : String.t option
    ; next_hosted_zone_id : String.t option
    ; max_items : String.t
    }

  let make
      ~hosted_zones
      ?d_n_s_name
      ?hosted_zone_id
      ~is_truncated
      ?next_d_n_s_name
      ?next_hosted_zone_id
      ~max_items
      () =
    { hosted_zones
    ; d_n_s_name
    ; hosted_zone_id
    ; is_truncated
    ; next_d_n_s_name
    ; next_hosted_zone_id
    ; max_items
    }

  let parse xml =
    Some
      { hosted_zones =
          Xml.required
            "HostedZones"
            (Util.option_bind (Xml.member "HostedZones" xml) HostedZones.parse)
      ; d_n_s_name = Util.option_bind (Xml.member "DNSName" xml) String.parse
      ; hosted_zone_id = Util.option_bind (Xml.member "HostedZoneId" xml) String.parse
      ; is_truncated =
          Xml.required
            "IsTruncated"
            (Util.option_bind (Xml.member "IsTruncated" xml) Boolean.parse)
      ; next_d_n_s_name = Util.option_bind (Xml.member "NextDNSName" xml) String.parse
      ; next_hosted_zone_id =
          Util.option_bind (Xml.member "NextHostedZoneId" xml) String.parse
      ; max_items =
          Xml.required
            "MaxItems"
            (Util.option_bind (Xml.member "MaxItems" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("MaxItems", String.to_query v.max_items))
         ; Util.option_map v.next_hosted_zone_id (fun f ->
               Query.Pair ("NextHostedZoneId", String.to_query f))
         ; Util.option_map v.next_d_n_s_name (fun f ->
               Query.Pair ("NextDNSName", String.to_query f))
         ; Some (Query.Pair ("IsTruncated", Boolean.to_query v.is_truncated))
         ; Util.option_map v.hosted_zone_id (fun f ->
               Query.Pair ("HostedZoneId", String.to_query f))
         ; Util.option_map v.d_n_s_name (fun f ->
               Query.Pair ("DNSName", String.to_query f))
         ; Some (Query.Pair ("HostedZones.member", HostedZones.to_query v.hosted_zones))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("max_items", String.to_json v.max_items)
         ; Util.option_map v.next_hosted_zone_id (fun f ->
               "next_hosted_zone_id", String.to_json f)
         ; Util.option_map v.next_d_n_s_name (fun f ->
               "next_d_n_s_name", String.to_json f)
         ; Some ("is_truncated", Boolean.to_json v.is_truncated)
         ; Util.option_map v.hosted_zone_id (fun f -> "hosted_zone_id", String.to_json f)
         ; Util.option_map v.d_n_s_name (fun f -> "d_n_s_name", String.to_json f)
         ; Some ("hosted_zones", HostedZones.to_json v.hosted_zones)
         ])

  let of_json j =
    { hosted_zones =
        HostedZones.of_json (Util.of_option_exn (Json.lookup j "hosted_zones"))
    ; d_n_s_name = Util.option_map (Json.lookup j "d_n_s_name") String.of_json
    ; hosted_zone_id = Util.option_map (Json.lookup j "hosted_zone_id") String.of_json
    ; is_truncated = Boolean.of_json (Util.of_option_exn (Json.lookup j "is_truncated"))
    ; next_d_n_s_name = Util.option_map (Json.lookup j "next_d_n_s_name") String.of_json
    ; next_hosted_zone_id =
        Util.option_map (Json.lookup j "next_hosted_zone_id") String.of_json
    ; max_items = String.of_json (Util.of_option_exn (Json.lookup j "max_items"))
    }
end

module DeleteReusableDelegationSetResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module HostedZoneNotFound = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module ListHealthChecksRequest = struct
  type t =
    { marker : String.t option
    ; max_items : String.t option
    }

  let make ?marker ?max_items () = { marker; max_items }

  let parse xml =
    Some
      { marker = Util.option_bind (Xml.member "marker" xml) String.parse
      ; max_items = Util.option_bind (Xml.member "maxitems" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.max_items (fun f ->
               Query.Pair ("maxitems", String.to_query f))
         ; Util.option_map v.marker (fun f -> Query.Pair ("marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.max_items (fun f -> "max_items", String.to_json f)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; max_items = Util.option_map (Json.lookup j "max_items") String.of_json
    }
end

module AssociateVPCWithHostedZoneResponse = struct
  type t = { change_info : ChangeInfo.t }

  let make ~change_info () = { change_info }

  let parse xml =
    Some
      { change_info =
          Xml.required
            "ChangeInfo"
            (Util.option_bind (Xml.member "ChangeInfo" xml) ChangeInfo.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("ChangeInfo", ChangeInfo.to_query v.change_info)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt [ Some ("change_info", ChangeInfo.to_json v.change_info) ])

  let of_json j =
    { change_info = ChangeInfo.of_json (Util.of_option_exn (Json.lookup j "change_info"))
    }
end

module DelegationSetAlreadyReusable = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module GetChangeRequest = struct
  type t = { id : String.t }

  let make ~id () = { id }

  let parse xml =
    Some { id = Xml.required "Id" (Util.option_bind (Xml.member "Id" xml) String.parse) }

  let to_query v =
    Query.List (Util.list_filter_opt [ Some (Query.Pair ("Id", String.to_query v.id)) ])

  let to_json v = `Assoc (Util.list_filter_opt [ Some ("id", String.to_json v.id) ])

  let of_json j = { id = String.of_json (Util.of_option_exn (Json.lookup j "id")) }
end

module GetGeoLocationResponse = struct
  type t = { geo_location_details : GeoLocationDetails.t }

  let make ~geo_location_details () = { geo_location_details }

  let parse xml =
    Some
      { geo_location_details =
          Xml.required
            "GeoLocationDetails"
            (Util.option_bind
               (Xml.member "GeoLocationDetails" xml)
               GeoLocationDetails.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("GeoLocationDetails", GeoLocationDetails.to_query v.geo_location_details))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("geo_location_details", GeoLocationDetails.to_json v.geo_location_details)
         ])

  let of_json j =
    { geo_location_details =
        GeoLocationDetails.of_json
          (Util.of_option_exn (Json.lookup j "geo_location_details"))
    }
end

module GetHealthCheckLastFailureReasonResponse = struct
  type t = { health_check_observations : HealthCheckObservations.t }

  let make ~health_check_observations () = { health_check_observations }

  let parse xml =
    Some
      { health_check_observations =
          Xml.required
            "HealthCheckObservations"
            (Util.option_bind
               (Xml.member "HealthCheckObservations" xml)
               HealthCheckObservations.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "HealthCheckObservations.member"
                , HealthCheckObservations.to_query v.health_check_observations ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "health_check_observations"
             , HealthCheckObservations.to_json v.health_check_observations )
         ])

  let of_json j =
    { health_check_observations =
        HealthCheckObservations.of_json
          (Util.of_option_exn (Json.lookup j "health_check_observations"))
    }
end

module GetHostedZoneRequest = struct
  type t = { id : String.t }

  let make ~id () = { id }

  let parse xml =
    Some { id = Xml.required "Id" (Util.option_bind (Xml.member "Id" xml) String.parse) }

  let to_query v =
    Query.List (Util.list_filter_opt [ Some (Query.Pair ("Id", String.to_query v.id)) ])

  let to_json v = `Assoc (Util.list_filter_opt [ Some ("id", String.to_json v.id) ])

  let of_json j = { id = String.of_json (Util.of_option_exn (Json.lookup j "id")) }
end

module DelegationSetAlreadyCreated = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module ListReusableDelegationSetsRequest = struct
  type t =
    { marker : String.t option
    ; max_items : String.t option
    }

  let make ?marker ?max_items () = { marker; max_items }

  let parse xml =
    Some
      { marker = Util.option_bind (Xml.member "marker" xml) String.parse
      ; max_items = Util.option_bind (Xml.member "maxitems" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.max_items (fun f ->
               Query.Pair ("maxitems", String.to_query f))
         ; Util.option_map v.marker (fun f -> Query.Pair ("marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.max_items (fun f -> "max_items", String.to_json f)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; max_items = Util.option_map (Json.lookup j "max_items") String.of_json
    }
end

module InvalidArgument = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module CreateHostedZoneResponse = struct
  type t =
    { hosted_zone : HostedZone.t
    ; change_info : ChangeInfo.t
    ; delegation_set : DelegationSet.t
    ; v_p_c : VPC.t option
    ; location : String.t
    }

  let make ~hosted_zone ~change_info ~delegation_set ?v_p_c ~location () =
    { hosted_zone; change_info; delegation_set; v_p_c; location }

  let parse xml =
    Some
      { hosted_zone =
          Xml.required
            "HostedZone"
            (Util.option_bind (Xml.member "HostedZone" xml) HostedZone.parse)
      ; change_info =
          Xml.required
            "ChangeInfo"
            (Util.option_bind (Xml.member "ChangeInfo" xml) ChangeInfo.parse)
      ; delegation_set =
          Xml.required
            "DelegationSet"
            (Util.option_bind (Xml.member "DelegationSet" xml) DelegationSet.parse)
      ; v_p_c = Util.option_bind (Xml.member "VPC" xml) VPC.parse
      ; location =
          Xml.required
            "Location"
            (Util.option_bind (Xml.member "Location" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Location", String.to_query v.location))
         ; Util.option_map v.v_p_c (fun f -> Query.Pair ("VPC", VPC.to_query f))
         ; Some (Query.Pair ("DelegationSet", DelegationSet.to_query v.delegation_set))
         ; Some (Query.Pair ("ChangeInfo", ChangeInfo.to_query v.change_info))
         ; Some (Query.Pair ("HostedZone", HostedZone.to_query v.hosted_zone))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("location", String.to_json v.location)
         ; Util.option_map v.v_p_c (fun f -> "v_p_c", VPC.to_json f)
         ; Some ("delegation_set", DelegationSet.to_json v.delegation_set)
         ; Some ("change_info", ChangeInfo.to_json v.change_info)
         ; Some ("hosted_zone", HostedZone.to_json v.hosted_zone)
         ])

  let of_json j =
    { hosted_zone = HostedZone.of_json (Util.of_option_exn (Json.lookup j "hosted_zone"))
    ; change_info = ChangeInfo.of_json (Util.of_option_exn (Json.lookup j "change_info"))
    ; delegation_set =
        DelegationSet.of_json (Util.of_option_exn (Json.lookup j "delegation_set"))
    ; v_p_c = Util.option_map (Json.lookup j "v_p_c") VPC.of_json
    ; location = String.of_json (Util.of_option_exn (Json.lookup j "location"))
    }
end

module GetHealthCheckStatusResponse = struct
  type t = { health_check_observations : HealthCheckObservations.t }

  let make ~health_check_observations () = { health_check_observations }

  let parse xml =
    Some
      { health_check_observations =
          Xml.required
            "HealthCheckObservations"
            (Util.option_bind
               (Xml.member "HealthCheckObservations" xml)
               HealthCheckObservations.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "HealthCheckObservations.member"
                , HealthCheckObservations.to_query v.health_check_observations ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "health_check_observations"
             , HealthCheckObservations.to_json v.health_check_observations )
         ])

  let of_json j =
    { health_check_observations =
        HealthCheckObservations.of_json
          (Util.of_option_exn (Json.lookup j "health_check_observations"))
    }
end

module GetCheckerIpRangesResponse = struct
  type t = { checker_ip_ranges : CheckerIpRanges.t }

  let make ~checker_ip_ranges () = { checker_ip_ranges }

  let parse xml =
    Some
      { checker_ip_ranges =
          Xml.required
            "CheckerIpRanges"
            (Util.option_bind (Xml.member "CheckerIpRanges" xml) CheckerIpRanges.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("CheckerIpRanges.member", CheckerIpRanges.to_query v.checker_ip_ranges))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("checker_ip_ranges", CheckerIpRanges.to_json v.checker_ip_ranges) ])

  let of_json j =
    { checker_ip_ranges =
        CheckerIpRanges.of_json (Util.of_option_exn (Json.lookup j "checker_ip_ranges"))
    }
end

module ThrottlingException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module InvalidInput = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module NoSuchDelegationSet = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module GetHealthCheckResponse = struct
  type t = { health_check : HealthCheck.t }

  let make ~health_check () = { health_check }

  let parse xml =
    Some
      { health_check =
          Xml.required
            "HealthCheck"
            (Util.option_bind (Xml.member "HealthCheck" xml) HealthCheck.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("HealthCheck", HealthCheck.to_query v.health_check)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt [ Some ("health_check", HealthCheck.to_json v.health_check) ])

  let of_json j =
    { health_check =
        HealthCheck.of_json (Util.of_option_exn (Json.lookup j "health_check"))
    }
end

module GetHealthCheckStatusRequest = struct
  type t = { health_check_id : String.t }

  let make ~health_check_id () = { health_check_id }

  let parse xml =
    Some
      { health_check_id =
          Xml.required
            "HealthCheckId"
            (Util.option_bind (Xml.member "HealthCheckId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("HealthCheckId", String.to_query v.health_check_id)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("health_check_id", String.to_json v.health_check_id) ])

  let of_json j =
    { health_check_id =
        String.of_json (Util.of_option_exn (Json.lookup j "health_check_id"))
    }
end

module ListTagsForResourceRequest = struct
  type t =
    { resource_type : TagResourceType.t
    ; resource_id : String.t
    }

  let make ~resource_type ~resource_id () = { resource_type; resource_id }

  let parse xml =
    Some
      { resource_type =
          Xml.required
            "ResourceType"
            (Util.option_bind (Xml.member "ResourceType" xml) TagResourceType.parse)
      ; resource_id =
          Xml.required
            "ResourceId"
            (Util.option_bind (Xml.member "ResourceId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("ResourceId", String.to_query v.resource_id))
         ; Some (Query.Pair ("ResourceType", TagResourceType.to_query v.resource_type))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("resource_id", String.to_json v.resource_id)
         ; Some ("resource_type", TagResourceType.to_json v.resource_type)
         ])

  let of_json j =
    { resource_type =
        TagResourceType.of_json (Util.of_option_exn (Json.lookup j "resource_type"))
    ; resource_id = String.of_json (Util.of_option_exn (Json.lookup j "resource_id"))
    }
end

module HostedZoneNotEmpty = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module GetCheckerIpRangesRequest = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module GetHealthCheckLastFailureReasonRequest = struct
  type t = { health_check_id : String.t }

  let make ~health_check_id () = { health_check_id }

  let parse xml =
    Some
      { health_check_id =
          Xml.required
            "HealthCheckId"
            (Util.option_bind (Xml.member "HealthCheckId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("HealthCheckId", String.to_query v.health_check_id)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("health_check_id", String.to_json v.health_check_id) ])

  let of_json j =
    { health_check_id =
        String.of_json (Util.of_option_exn (Json.lookup j "health_check_id"))
    }
end

module LimitsExceeded = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module PublicZoneVPCAssociation = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module GetHealthCheckRequest = struct
  type t = { health_check_id : String.t }

  let make ~health_check_id () = { health_check_id }

  let parse xml =
    Some
      { health_check_id =
          Xml.required
            "HealthCheckId"
            (Util.option_bind (Xml.member "HealthCheckId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("HealthCheckId", String.to_query v.health_check_id)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("health_check_id", String.to_json v.health_check_id) ])

  let of_json j =
    { health_check_id =
        String.of_json (Util.of_option_exn (Json.lookup j "health_check_id"))
    }
end

module ListResourceRecordSetsRequest = struct
  type t =
    { hosted_zone_id : String.t
    ; start_record_name : String.t option
    ; start_record_type : RRType.t option
    ; start_record_identifier : String.t option
    ; max_items : String.t option
    }

  let make
      ~hosted_zone_id
      ?start_record_name
      ?start_record_type
      ?start_record_identifier
      ?max_items
      () =
    { hosted_zone_id
    ; start_record_name
    ; start_record_type
    ; start_record_identifier
    ; max_items
    }

  let parse xml =
    Some
      { hosted_zone_id =
          Xml.required "Id" (Util.option_bind (Xml.member "Id" xml) String.parse)
      ; start_record_name = Util.option_bind (Xml.member "name" xml) String.parse
      ; start_record_type = Util.option_bind (Xml.member "type" xml) RRType.parse
      ; start_record_identifier =
          Util.option_bind (Xml.member "identifier" xml) String.parse
      ; max_items = Util.option_bind (Xml.member "maxitems" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.max_items (fun f ->
               Query.Pair ("maxitems", String.to_query f))
         ; Util.option_map v.start_record_identifier (fun f ->
               Query.Pair ("identifier", String.to_query f))
         ; Util.option_map v.start_record_type (fun f ->
               Query.Pair ("type", RRType.to_query f))
         ; Util.option_map v.start_record_name (fun f ->
               Query.Pair ("name", String.to_query f))
         ; Some (Query.Pair ("Id", String.to_query v.hosted_zone_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.max_items (fun f -> "max_items", String.to_json f)
         ; Util.option_map v.start_record_identifier (fun f ->
               "start_record_identifier", String.to_json f)
         ; Util.option_map v.start_record_type (fun f ->
               "start_record_type", RRType.to_json f)
         ; Util.option_map v.start_record_name (fun f ->
               "start_record_name", String.to_json f)
         ; Some ("hosted_zone_id", String.to_json v.hosted_zone_id)
         ])

  let of_json j =
    { hosted_zone_id =
        String.of_json (Util.of_option_exn (Json.lookup j "hosted_zone_id"))
    ; start_record_name =
        Util.option_map (Json.lookup j "start_record_name") String.of_json
    ; start_record_type =
        Util.option_map (Json.lookup j "start_record_type") RRType.of_json
    ; start_record_identifier =
        Util.option_map (Json.lookup j "start_record_identifier") String.of_json
    ; max_items = Util.option_map (Json.lookup j "max_items") String.of_json
    }
end

module NoSuchChange = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module GetReusableDelegationSetResponse = struct
  type t = { delegation_set : DelegationSet.t }

  let make ~delegation_set () = { delegation_set }

  let parse xml =
    Some
      { delegation_set =
          Xml.required
            "DelegationSet"
            (Util.option_bind (Xml.member "DelegationSet" xml) DelegationSet.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("DelegationSet", DelegationSet.to_query v.delegation_set)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("delegation_set", DelegationSet.to_json v.delegation_set) ])

  let of_json j =
    { delegation_set =
        DelegationSet.of_json (Util.of_option_exn (Json.lookup j "delegation_set"))
    }
end

module ChangeResourceRecordSetsResponse = struct
  type t = { change_info : ChangeInfo.t }

  let make ~change_info () = { change_info }

  let parse xml =
    Some
      { change_info =
          Xml.required
            "ChangeInfo"
            (Util.option_bind (Xml.member "ChangeInfo" xml) ChangeInfo.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("ChangeInfo", ChangeInfo.to_query v.change_info)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt [ Some ("change_info", ChangeInfo.to_json v.change_info) ])

  let of_json j =
    { change_info = ChangeInfo.of_json (Util.of_option_exn (Json.lookup j "change_info"))
    }
end

module ChangeTagsForResourceResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidChangeBatch = struct
  type t = { messages : ErrorMessages.t }

  let make ?(messages = []) () = { messages }

  let parse xml =
    Some
      { messages =
          Util.of_option
            []
            (Util.option_bind (Xml.member "messages" xml) ErrorMessages.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("messages.member", ErrorMessages.to_query v.messages)) ])

  let to_json v =
    `Assoc (Util.list_filter_opt [ Some ("messages", ErrorMessages.to_json v.messages) ])

  let of_json j =
    { messages = ErrorMessages.of_json (Util.of_option_exn (Json.lookup j "messages")) }
end

module GetGeoLocationRequest = struct
  type t =
    { continent_code : String.t option
    ; country_code : String.t option
    ; subdivision_code : String.t option
    }

  let make ?continent_code ?country_code ?subdivision_code () =
    { continent_code; country_code; subdivision_code }

  let parse xml =
    Some
      { continent_code = Util.option_bind (Xml.member "continentcode" xml) String.parse
      ; country_code = Util.option_bind (Xml.member "countrycode" xml) String.parse
      ; subdivision_code =
          Util.option_bind (Xml.member "subdivisioncode" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.subdivision_code (fun f ->
               Query.Pair ("subdivisioncode", String.to_query f))
         ; Util.option_map v.country_code (fun f ->
               Query.Pair ("countrycode", String.to_query f))
         ; Util.option_map v.continent_code (fun f ->
               Query.Pair ("continentcode", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.subdivision_code (fun f ->
               "subdivision_code", String.to_json f)
         ; Util.option_map v.country_code (fun f -> "country_code", String.to_json f)
         ; Util.option_map v.continent_code (fun f -> "continent_code", String.to_json f)
         ])

  let of_json j =
    { continent_code = Util.option_map (Json.lookup j "continent_code") String.of_json
    ; country_code = Util.option_map (Json.lookup j "country_code") String.of_json
    ; subdivision_code = Util.option_map (Json.lookup j "subdivision_code") String.of_json
    }
end

module GetHostedZoneCountRequest = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module UpdateHealthCheckRequest = struct
  type t =
    { health_check_id : String.t
    ; health_check_version : Long.t option
    ; i_p_address : String.t option
    ; port : Integer.t option
    ; resource_path : String.t option
    ; fully_qualified_domain_name : String.t option
    ; search_string : String.t option
    ; failure_threshold : Integer.t option
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
      () =
    { health_check_id
    ; health_check_version
    ; i_p_address
    ; port
    ; resource_path
    ; fully_qualified_domain_name
    ; search_string
    ; failure_threshold
    }

  let parse xml =
    Some
      { health_check_id =
          Xml.required
            "HealthCheckId"
            (Util.option_bind (Xml.member "HealthCheckId" xml) String.parse)
      ; health_check_version =
          Util.option_bind (Xml.member "HealthCheckVersion" xml) Long.parse
      ; i_p_address = Util.option_bind (Xml.member "IPAddress" xml) String.parse
      ; port = Util.option_bind (Xml.member "Port" xml) Integer.parse
      ; resource_path = Util.option_bind (Xml.member "ResourcePath" xml) String.parse
      ; fully_qualified_domain_name =
          Util.option_bind (Xml.member "FullyQualifiedDomainName" xml) String.parse
      ; search_string = Util.option_bind (Xml.member "SearchString" xml) String.parse
      ; failure_threshold =
          Util.option_bind (Xml.member "FailureThreshold" xml) Integer.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.failure_threshold (fun f ->
               Query.Pair ("FailureThreshold", Integer.to_query f))
         ; Util.option_map v.search_string (fun f ->
               Query.Pair ("SearchString", String.to_query f))
         ; Util.option_map v.fully_qualified_domain_name (fun f ->
               Query.Pair ("FullyQualifiedDomainName", String.to_query f))
         ; Util.option_map v.resource_path (fun f ->
               Query.Pair ("ResourcePath", String.to_query f))
         ; Util.option_map v.port (fun f -> Query.Pair ("Port", Integer.to_query f))
         ; Util.option_map v.i_p_address (fun f ->
               Query.Pair ("IPAddress", String.to_query f))
         ; Util.option_map v.health_check_version (fun f ->
               Query.Pair ("HealthCheckVersion", Long.to_query f))
         ; Some (Query.Pair ("HealthCheckId", String.to_query v.health_check_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.failure_threshold (fun f ->
               "failure_threshold", Integer.to_json f)
         ; Util.option_map v.search_string (fun f -> "search_string", String.to_json f)
         ; Util.option_map v.fully_qualified_domain_name (fun f ->
               "fully_qualified_domain_name", String.to_json f)
         ; Util.option_map v.resource_path (fun f -> "resource_path", String.to_json f)
         ; Util.option_map v.port (fun f -> "port", Integer.to_json f)
         ; Util.option_map v.i_p_address (fun f -> "i_p_address", String.to_json f)
         ; Util.option_map v.health_check_version (fun f ->
               "health_check_version", Long.to_json f)
         ; Some ("health_check_id", String.to_json v.health_check_id)
         ])

  let of_json j =
    { health_check_id =
        String.of_json (Util.of_option_exn (Json.lookup j "health_check_id"))
    ; health_check_version =
        Util.option_map (Json.lookup j "health_check_version") Long.of_json
    ; i_p_address = Util.option_map (Json.lookup j "i_p_address") String.of_json
    ; port = Util.option_map (Json.lookup j "port") Integer.of_json
    ; resource_path = Util.option_map (Json.lookup j "resource_path") String.of_json
    ; fully_qualified_domain_name =
        Util.option_map (Json.lookup j "fully_qualified_domain_name") String.of_json
    ; search_string = Util.option_map (Json.lookup j "search_string") String.of_json
    ; failure_threshold =
        Util.option_map (Json.lookup j "failure_threshold") Integer.of_json
    }
end

module GetHealthCheckCountResponse = struct
  type t = { health_check_count : Long.t }

  let make ~health_check_count () = { health_check_count }

  let parse xml =
    Some
      { health_check_count =
          Xml.required
            "HealthCheckCount"
            (Util.option_bind (Xml.member "HealthCheckCount" xml) Long.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("HealthCheckCount", Long.to_query v.health_check_count)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("health_check_count", Long.to_json v.health_check_count) ])

  let of_json j =
    { health_check_count =
        Long.of_json (Util.of_option_exn (Json.lookup j "health_check_count"))
    }
end

module NoSuchHostedZone = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module DeleteReusableDelegationSetRequest = struct
  type t = { id : String.t }

  let make ~id () = { id }

  let parse xml =
    Some { id = Xml.required "Id" (Util.option_bind (Xml.member "Id" xml) String.parse) }

  let to_query v =
    Query.List (Util.list_filter_opt [ Some (Query.Pair ("Id", String.to_query v.id)) ])

  let to_json v = `Assoc (Util.list_filter_opt [ Some ("id", String.to_json v.id) ])

  let of_json j = { id = String.of_json (Util.of_option_exn (Json.lookup j "id")) }
end

module VPCAssociationNotFound = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module ConflictingDomainExists = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module ListTagsForResourcesRequest = struct
  type t =
    { resource_type : TagResourceType.t
    ; resource_ids : TagResourceIdList.t
    }

  let make ~resource_type ~resource_ids () = { resource_type; resource_ids }

  let parse xml =
    Some
      { resource_type =
          Xml.required
            "ResourceType"
            (Util.option_bind (Xml.member "ResourceType" xml) TagResourceType.parse)
      ; resource_ids =
          Xml.required
            "ResourceIds"
            (Util.option_bind (Xml.member "ResourceIds" xml) TagResourceIdList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("ResourceIds.member", TagResourceIdList.to_query v.resource_ids))
         ; Some (Query.Pair ("ResourceType", TagResourceType.to_query v.resource_type))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("resource_ids", TagResourceIdList.to_json v.resource_ids)
         ; Some ("resource_type", TagResourceType.to_json v.resource_type)
         ])

  let of_json j =
    { resource_type =
        TagResourceType.of_json (Util.of_option_exn (Json.lookup j "resource_type"))
    ; resource_ids =
        TagResourceIdList.of_json (Util.of_option_exn (Json.lookup j "resource_ids"))
    }
end

module ListHostedZonesRequest = struct
  type t =
    { marker : String.t option
    ; max_items : String.t option
    ; delegation_set_id : String.t option
    }

  let make ?marker ?max_items ?delegation_set_id () =
    { marker; max_items; delegation_set_id }

  let parse xml =
    Some
      { marker = Util.option_bind (Xml.member "marker" xml) String.parse
      ; max_items = Util.option_bind (Xml.member "maxitems" xml) String.parse
      ; delegation_set_id =
          Util.option_bind (Xml.member "delegationsetid" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.delegation_set_id (fun f ->
               Query.Pair ("delegationsetid", String.to_query f))
         ; Util.option_map v.max_items (fun f ->
               Query.Pair ("maxitems", String.to_query f))
         ; Util.option_map v.marker (fun f -> Query.Pair ("marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.delegation_set_id (fun f ->
               "delegation_set_id", String.to_json f)
         ; Util.option_map v.max_items (fun f -> "max_items", String.to_json f)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; max_items = Util.option_map (Json.lookup j "max_items") String.of_json
    ; delegation_set_id =
        Util.option_map (Json.lookup j "delegation_set_id") String.of_json
    }
end

module ListHostedZonesResponse = struct
  type t =
    { hosted_zones : HostedZones.t
    ; marker : String.t option
    ; is_truncated : Boolean.t
    ; next_marker : String.t option
    ; max_items : String.t
    }

  let make ~hosted_zones ?marker ~is_truncated ?next_marker ~max_items () =
    { hosted_zones; marker; is_truncated; next_marker; max_items }

  let parse xml =
    Some
      { hosted_zones =
          Xml.required
            "HostedZones"
            (Util.option_bind (Xml.member "HostedZones" xml) HostedZones.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; is_truncated =
          Xml.required
            "IsTruncated"
            (Util.option_bind (Xml.member "IsTruncated" xml) Boolean.parse)
      ; next_marker = Util.option_bind (Xml.member "NextMarker" xml) String.parse
      ; max_items =
          Xml.required
            "MaxItems"
            (Util.option_bind (Xml.member "MaxItems" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("MaxItems", String.to_query v.max_items))
         ; Util.option_map v.next_marker (fun f ->
               Query.Pair ("NextMarker", String.to_query f))
         ; Some (Query.Pair ("IsTruncated", Boolean.to_query v.is_truncated))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some (Query.Pair ("HostedZones.member", HostedZones.to_query v.hosted_zones))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("max_items", String.to_json v.max_items)
         ; Util.option_map v.next_marker (fun f -> "next_marker", String.to_json f)
         ; Some ("is_truncated", Boolean.to_json v.is_truncated)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("hosted_zones", HostedZones.to_json v.hosted_zones)
         ])

  let of_json j =
    { hosted_zones =
        HostedZones.of_json (Util.of_option_exn (Json.lookup j "hosted_zones"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; is_truncated = Boolean.of_json (Util.of_option_exn (Json.lookup j "is_truncated"))
    ; next_marker = Util.option_map (Json.lookup j "next_marker") String.of_json
    ; max_items = String.of_json (Util.of_option_exn (Json.lookup j "max_items"))
    }
end

module CreateReusableDelegationSetRequest = struct
  type t =
    { caller_reference : String.t
    ; hosted_zone_id : String.t option
    }

  let make ~caller_reference ?hosted_zone_id () = { caller_reference; hosted_zone_id }

  let parse xml =
    Some
      { caller_reference =
          Xml.required
            "CallerReference"
            (Util.option_bind (Xml.member "CallerReference" xml) String.parse)
      ; hosted_zone_id = Util.option_bind (Xml.member "HostedZoneId" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.hosted_zone_id (fun f ->
               Query.Pair ("HostedZoneId", String.to_query f))
         ; Some (Query.Pair ("CallerReference", String.to_query v.caller_reference))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.hosted_zone_id (fun f -> "hosted_zone_id", String.to_json f)
         ; Some ("caller_reference", String.to_json v.caller_reference)
         ])

  let of_json j =
    { caller_reference =
        String.of_json (Util.of_option_exn (Json.lookup j "caller_reference"))
    ; hosted_zone_id = Util.option_map (Json.lookup j "hosted_zone_id") String.of_json
    }
end

module ListTagsForResourceResponse = struct
  type t = { resource_tag_set : ResourceTagSet.t }

  let make ~resource_tag_set () = { resource_tag_set }

  let parse xml =
    Some
      { resource_tag_set =
          Xml.required
            "ResourceTagSet"
            (Util.option_bind (Xml.member "ResourceTagSet" xml) ResourceTagSet.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("ResourceTagSet", ResourceTagSet.to_query v.resource_tag_set))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("resource_tag_set", ResourceTagSet.to_json v.resource_tag_set) ])

  let of_json j =
    { resource_tag_set =
        ResourceTagSet.of_json (Util.of_option_exn (Json.lookup j "resource_tag_set"))
    }
end

module CreateHealthCheckResponse = struct
  type t =
    { health_check : HealthCheck.t
    ; location : String.t
    }

  let make ~health_check ~location () = { health_check; location }

  let parse xml =
    Some
      { health_check =
          Xml.required
            "HealthCheck"
            (Util.option_bind (Xml.member "HealthCheck" xml) HealthCheck.parse)
      ; location =
          Xml.required
            "Location"
            (Util.option_bind (Xml.member "Location" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Location", String.to_query v.location))
         ; Some (Query.Pair ("HealthCheck", HealthCheck.to_query v.health_check))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("location", String.to_json v.location)
         ; Some ("health_check", HealthCheck.to_json v.health_check)
         ])

  let of_json j =
    { health_check =
        HealthCheck.of_json (Util.of_option_exn (Json.lookup j "health_check"))
    ; location = String.of_json (Util.of_option_exn (Json.lookup j "location"))
    }
end

module GetChangeResponse = struct
  type t = { change_info : ChangeInfo.t }

  let make ~change_info () = { change_info }

  let parse xml =
    Some
      { change_info =
          Xml.required
            "ChangeInfo"
            (Util.option_bind (Xml.member "ChangeInfo" xml) ChangeInfo.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("ChangeInfo", ChangeInfo.to_query v.change_info)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt [ Some ("change_info", ChangeInfo.to_json v.change_info) ])

  let of_json j =
    { change_info = ChangeInfo.of_json (Util.of_option_exn (Json.lookup j "change_info"))
    }
end

module InvalidVPCId = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module ListGeoLocationsResponse = struct
  type t =
    { geo_location_details_list : GeoLocationDetailsList.t
    ; is_truncated : Boolean.t
    ; next_continent_code : String.t option
    ; next_country_code : String.t option
    ; next_subdivision_code : String.t option
    ; max_items : String.t
    }

  let make
      ~geo_location_details_list
      ~is_truncated
      ?next_continent_code
      ?next_country_code
      ?next_subdivision_code
      ~max_items
      () =
    { geo_location_details_list
    ; is_truncated
    ; next_continent_code
    ; next_country_code
    ; next_subdivision_code
    ; max_items
    }

  let parse xml =
    Some
      { geo_location_details_list =
          Xml.required
            "GeoLocationDetailsList"
            (Util.option_bind
               (Xml.member "GeoLocationDetailsList" xml)
               GeoLocationDetailsList.parse)
      ; is_truncated =
          Xml.required
            "IsTruncated"
            (Util.option_bind (Xml.member "IsTruncated" xml) Boolean.parse)
      ; next_continent_code =
          Util.option_bind (Xml.member "NextContinentCode" xml) String.parse
      ; next_country_code =
          Util.option_bind (Xml.member "NextCountryCode" xml) String.parse
      ; next_subdivision_code =
          Util.option_bind (Xml.member "NextSubdivisionCode" xml) String.parse
      ; max_items =
          Xml.required
            "MaxItems"
            (Util.option_bind (Xml.member "MaxItems" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("MaxItems", String.to_query v.max_items))
         ; Util.option_map v.next_subdivision_code (fun f ->
               Query.Pair ("NextSubdivisionCode", String.to_query f))
         ; Util.option_map v.next_country_code (fun f ->
               Query.Pair ("NextCountryCode", String.to_query f))
         ; Util.option_map v.next_continent_code (fun f ->
               Query.Pair ("NextContinentCode", String.to_query f))
         ; Some (Query.Pair ("IsTruncated", Boolean.to_query v.is_truncated))
         ; Some
             (Query.Pair
                ( "GeoLocationDetailsList.member"
                , GeoLocationDetailsList.to_query v.geo_location_details_list ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("max_items", String.to_json v.max_items)
         ; Util.option_map v.next_subdivision_code (fun f ->
               "next_subdivision_code", String.to_json f)
         ; Util.option_map v.next_country_code (fun f ->
               "next_country_code", String.to_json f)
         ; Util.option_map v.next_continent_code (fun f ->
               "next_continent_code", String.to_json f)
         ; Some ("is_truncated", Boolean.to_json v.is_truncated)
         ; Some
             ( "geo_location_details_list"
             , GeoLocationDetailsList.to_json v.geo_location_details_list )
         ])

  let of_json j =
    { geo_location_details_list =
        GeoLocationDetailsList.of_json
          (Util.of_option_exn (Json.lookup j "geo_location_details_list"))
    ; is_truncated = Boolean.of_json (Util.of_option_exn (Json.lookup j "is_truncated"))
    ; next_continent_code =
        Util.option_map (Json.lookup j "next_continent_code") String.of_json
    ; next_country_code =
        Util.option_map (Json.lookup j "next_country_code") String.of_json
    ; next_subdivision_code =
        Util.option_map (Json.lookup j "next_subdivision_code") String.of_json
    ; max_items = String.of_json (Util.of_option_exn (Json.lookup j "max_items"))
    }
end

module ListHealthChecksResponse = struct
  type t =
    { health_checks : HealthChecks.t
    ; marker : String.t
    ; is_truncated : Boolean.t
    ; next_marker : String.t option
    ; max_items : String.t
    }

  let make ~health_checks ~marker ~is_truncated ?next_marker ~max_items () =
    { health_checks; marker; is_truncated; next_marker; max_items }

  let parse xml =
    Some
      { health_checks =
          Xml.required
            "HealthChecks"
            (Util.option_bind (Xml.member "HealthChecks" xml) HealthChecks.parse)
      ; marker =
          Xml.required "Marker" (Util.option_bind (Xml.member "Marker" xml) String.parse)
      ; is_truncated =
          Xml.required
            "IsTruncated"
            (Util.option_bind (Xml.member "IsTruncated" xml) Boolean.parse)
      ; next_marker = Util.option_bind (Xml.member "NextMarker" xml) String.parse
      ; max_items =
          Xml.required
            "MaxItems"
            (Util.option_bind (Xml.member "MaxItems" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("MaxItems", String.to_query v.max_items))
         ; Util.option_map v.next_marker (fun f ->
               Query.Pair ("NextMarker", String.to_query f))
         ; Some (Query.Pair ("IsTruncated", Boolean.to_query v.is_truncated))
         ; Some (Query.Pair ("Marker", String.to_query v.marker))
         ; Some
             (Query.Pair ("HealthChecks.member", HealthChecks.to_query v.health_checks))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("max_items", String.to_json v.max_items)
         ; Util.option_map v.next_marker (fun f -> "next_marker", String.to_json f)
         ; Some ("is_truncated", Boolean.to_json v.is_truncated)
         ; Some ("marker", String.to_json v.marker)
         ; Some ("health_checks", HealthChecks.to_json v.health_checks)
         ])

  let of_json j =
    { health_checks =
        HealthChecks.of_json (Util.of_option_exn (Json.lookup j "health_checks"))
    ; marker = String.of_json (Util.of_option_exn (Json.lookup j "marker"))
    ; is_truncated = Boolean.of_json (Util.of_option_exn (Json.lookup j "is_truncated"))
    ; next_marker = Util.option_map (Json.lookup j "next_marker") String.of_json
    ; max_items = String.of_json (Util.of_option_exn (Json.lookup j "max_items"))
    }
end

module IncompatibleVersion = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module HealthCheckInUse = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module TooManyHealthChecks = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module DeleteHostedZoneRequest = struct
  type t = { id : String.t }

  let make ~id () = { id }

  let parse xml =
    Some { id = Xml.required "Id" (Util.option_bind (Xml.member "Id" xml) String.parse) }

  let to_query v =
    Query.List (Util.list_filter_opt [ Some (Query.Pair ("Id", String.to_query v.id)) ])

  let to_json v = `Assoc (Util.list_filter_opt [ Some ("id", String.to_json v.id) ])

  let of_json j = { id = String.of_json (Util.of_option_exn (Json.lookup j "id")) }
end

module UpdateHealthCheckResponse = struct
  type t = { health_check : HealthCheck.t }

  let make ~health_check () = { health_check }

  let parse xml =
    Some
      { health_check =
          Xml.required
            "HealthCheck"
            (Util.option_bind (Xml.member "HealthCheck" xml) HealthCheck.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("HealthCheck", HealthCheck.to_query v.health_check)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt [ Some ("health_check", HealthCheck.to_json v.health_check) ])

  let of_json j =
    { health_check =
        HealthCheck.of_json (Util.of_option_exn (Json.lookup j "health_check"))
    }
end

module ChangeTagsForResourceRequest = struct
  type t =
    { resource_type : TagResourceType.t
    ; resource_id : String.t
    ; add_tags : TagList.t
    ; remove_tag_keys : TagKeyList.t
    }

  let make ~resource_type ~resource_id ?(add_tags = []) ?(remove_tag_keys = []) () =
    { resource_type; resource_id; add_tags; remove_tag_keys }

  let parse xml =
    Some
      { resource_type =
          Xml.required
            "ResourceType"
            (Util.option_bind (Xml.member "ResourceType" xml) TagResourceType.parse)
      ; resource_id =
          Xml.required
            "ResourceId"
            (Util.option_bind (Xml.member "ResourceId" xml) String.parse)
      ; add_tags =
          Util.of_option [] (Util.option_bind (Xml.member "AddTags" xml) TagList.parse)
      ; remove_tag_keys =
          Util.of_option
            []
            (Util.option_bind (Xml.member "RemoveTagKeys" xml) TagKeyList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("RemoveTagKeys.member", TagKeyList.to_query v.remove_tag_keys))
         ; Some (Query.Pair ("AddTags.member", TagList.to_query v.add_tags))
         ; Some (Query.Pair ("ResourceId", String.to_query v.resource_id))
         ; Some (Query.Pair ("ResourceType", TagResourceType.to_query v.resource_type))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("remove_tag_keys", TagKeyList.to_json v.remove_tag_keys)
         ; Some ("add_tags", TagList.to_json v.add_tags)
         ; Some ("resource_id", String.to_json v.resource_id)
         ; Some ("resource_type", TagResourceType.to_json v.resource_type)
         ])

  let of_json j =
    { resource_type =
        TagResourceType.of_json (Util.of_option_exn (Json.lookup j "resource_type"))
    ; resource_id = String.of_json (Util.of_option_exn (Json.lookup j "resource_id"))
    ; add_tags = TagList.of_json (Util.of_option_exn (Json.lookup j "add_tags"))
    ; remove_tag_keys =
        TagKeyList.of_json (Util.of_option_exn (Json.lookup j "remove_tag_keys"))
    }
end

module DelegationSetNotAvailable = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module ListTagsForResourcesResponse = struct
  type t = { resource_tag_sets : ResourceTagSetList.t }

  let make ~resource_tag_sets () = { resource_tag_sets }

  let parse xml =
    Some
      { resource_tag_sets =
          Xml.required
            "ResourceTagSets"
            (Util.option_bind (Xml.member "ResourceTagSets" xml) ResourceTagSetList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("ResourceTagSets.member", ResourceTagSetList.to_query v.resource_tag_sets))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("resource_tag_sets", ResourceTagSetList.to_json v.resource_tag_sets) ])

  let of_json j =
    { resource_tag_sets =
        ResourceTagSetList.of_json
          (Util.of_option_exn (Json.lookup j "resource_tag_sets"))
    }
end

module AssociateVPCWithHostedZoneRequest = struct
  type t =
    { hosted_zone_id : String.t
    ; v_p_c : VPC.t
    ; comment : String.t option
    }

  let make ~hosted_zone_id ~v_p_c ?comment () = { hosted_zone_id; v_p_c; comment }

  let parse xml =
    Some
      { hosted_zone_id =
          Xml.required "Id" (Util.option_bind (Xml.member "Id" xml) String.parse)
      ; v_p_c = Xml.required "VPC" (Util.option_bind (Xml.member "VPC" xml) VPC.parse)
      ; comment = Util.option_bind (Xml.member "Comment" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.comment (fun f -> Query.Pair ("Comment", String.to_query f))
         ; Some (Query.Pair ("VPC", VPC.to_query v.v_p_c))
         ; Some (Query.Pair ("Id", String.to_query v.hosted_zone_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.comment (fun f -> "comment", String.to_json f)
         ; Some ("v_p_c", VPC.to_json v.v_p_c)
         ; Some ("hosted_zone_id", String.to_json v.hosted_zone_id)
         ])

  let of_json j =
    { hosted_zone_id =
        String.of_json (Util.of_option_exn (Json.lookup j "hosted_zone_id"))
    ; v_p_c = VPC.of_json (Util.of_option_exn (Json.lookup j "v_p_c"))
    ; comment = Util.option_map (Json.lookup j "comment") String.of_json
    }
end

module GetHostedZoneCountResponse = struct
  type t = { hosted_zone_count : Long.t }

  let make ~hosted_zone_count () = { hosted_zone_count }

  let parse xml =
    Some
      { hosted_zone_count =
          Xml.required
            "HostedZoneCount"
            (Util.option_bind (Xml.member "HostedZoneCount" xml) Long.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("HostedZoneCount", Long.to_query v.hosted_zone_count)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("hosted_zone_count", Long.to_json v.hosted_zone_count) ])

  let of_json j =
    { hosted_zone_count =
        Long.of_json (Util.of_option_exn (Json.lookup j "hosted_zone_count"))
    }
end

module TooManyHostedZones = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module DisassociateVPCFromHostedZoneRequest = struct
  type t =
    { hosted_zone_id : String.t
    ; v_p_c : VPC.t
    ; comment : String.t option
    }

  let make ~hosted_zone_id ~v_p_c ?comment () = { hosted_zone_id; v_p_c; comment }

  let parse xml =
    Some
      { hosted_zone_id =
          Xml.required "Id" (Util.option_bind (Xml.member "Id" xml) String.parse)
      ; v_p_c = Xml.required "VPC" (Util.option_bind (Xml.member "VPC" xml) VPC.parse)
      ; comment = Util.option_bind (Xml.member "Comment" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.comment (fun f -> Query.Pair ("Comment", String.to_query f))
         ; Some (Query.Pair ("VPC", VPC.to_query v.v_p_c))
         ; Some (Query.Pair ("Id", String.to_query v.hosted_zone_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.comment (fun f -> "comment", String.to_json f)
         ; Some ("v_p_c", VPC.to_json v.v_p_c)
         ; Some ("hosted_zone_id", String.to_json v.hosted_zone_id)
         ])

  let of_json j =
    { hosted_zone_id =
        String.of_json (Util.of_option_exn (Json.lookup j "hosted_zone_id"))
    ; v_p_c = VPC.of_json (Util.of_option_exn (Json.lookup j "v_p_c"))
    ; comment = Util.option_map (Json.lookup j "comment") String.of_json
    }
end

module ListGeoLocationsRequest = struct
  type t =
    { start_continent_code : String.t option
    ; start_country_code : String.t option
    ; start_subdivision_code : String.t option
    ; max_items : String.t option
    }

  let make ?start_continent_code ?start_country_code ?start_subdivision_code ?max_items ()
      =
    { start_continent_code; start_country_code; start_subdivision_code; max_items }

  let parse xml =
    Some
      { start_continent_code =
          Util.option_bind (Xml.member "startcontinentcode" xml) String.parse
      ; start_country_code =
          Util.option_bind (Xml.member "startcountrycode" xml) String.parse
      ; start_subdivision_code =
          Util.option_bind (Xml.member "startsubdivisioncode" xml) String.parse
      ; max_items = Util.option_bind (Xml.member "maxitems" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.max_items (fun f ->
               Query.Pair ("maxitems", String.to_query f))
         ; Util.option_map v.start_subdivision_code (fun f ->
               Query.Pair ("startsubdivisioncode", String.to_query f))
         ; Util.option_map v.start_country_code (fun f ->
               Query.Pair ("startcountrycode", String.to_query f))
         ; Util.option_map v.start_continent_code (fun f ->
               Query.Pair ("startcontinentcode", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.max_items (fun f -> "max_items", String.to_json f)
         ; Util.option_map v.start_subdivision_code (fun f ->
               "start_subdivision_code", String.to_json f)
         ; Util.option_map v.start_country_code (fun f ->
               "start_country_code", String.to_json f)
         ; Util.option_map v.start_continent_code (fun f ->
               "start_continent_code", String.to_json f)
         ])

  let of_json j =
    { start_continent_code =
        Util.option_map (Json.lookup j "start_continent_code") String.of_json
    ; start_country_code =
        Util.option_map (Json.lookup j "start_country_code") String.of_json
    ; start_subdivision_code =
        Util.option_map (Json.lookup j "start_subdivision_code") String.of_json
    ; max_items = Util.option_map (Json.lookup j "max_items") String.of_json
    }
end

module CreateReusableDelegationSetResponse = struct
  type t =
    { delegation_set : DelegationSet.t
    ; location : String.t
    }

  let make ~delegation_set ~location () = { delegation_set; location }

  let parse xml =
    Some
      { delegation_set =
          Xml.required
            "DelegationSet"
            (Util.option_bind (Xml.member "DelegationSet" xml) DelegationSet.parse)
      ; location =
          Xml.required
            "Location"
            (Util.option_bind (Xml.member "Location" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Location", String.to_query v.location))
         ; Some (Query.Pair ("DelegationSet", DelegationSet.to_query v.delegation_set))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("location", String.to_json v.location)
         ; Some ("delegation_set", DelegationSet.to_json v.delegation_set)
         ])

  let of_json j =
    { delegation_set =
        DelegationSet.of_json (Util.of_option_exn (Json.lookup j "delegation_set"))
    ; location = String.of_json (Util.of_option_exn (Json.lookup j "location"))
    }
end

module DisassociateVPCFromHostedZoneResponse = struct
  type t = { change_info : ChangeInfo.t }

  let make ~change_info () = { change_info }

  let parse xml =
    Some
      { change_info =
          Xml.required
            "ChangeInfo"
            (Util.option_bind (Xml.member "ChangeInfo" xml) ChangeInfo.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("ChangeInfo", ChangeInfo.to_query v.change_info)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt [ Some ("change_info", ChangeInfo.to_json v.change_info) ])

  let of_json j =
    { change_info = ChangeInfo.of_json (Util.of_option_exn (Json.lookup j "change_info"))
    }
end

module ListResourceRecordSetsResponse = struct
  type t =
    { resource_record_sets : ResourceRecordSets.t
    ; is_truncated : Boolean.t
    ; next_record_name : String.t option
    ; next_record_type : RRType.t option
    ; next_record_identifier : String.t option
    ; max_items : String.t
    }

  let make
      ~resource_record_sets
      ~is_truncated
      ?next_record_name
      ?next_record_type
      ?next_record_identifier
      ~max_items
      () =
    { resource_record_sets
    ; is_truncated
    ; next_record_name
    ; next_record_type
    ; next_record_identifier
    ; max_items
    }

  let parse xml =
    Some
      { resource_record_sets =
          Xml.required
            "ResourceRecordSets"
            (Util.option_bind
               (Xml.member "ResourceRecordSets" xml)
               ResourceRecordSets.parse)
      ; is_truncated =
          Xml.required
            "IsTruncated"
            (Util.option_bind (Xml.member "IsTruncated" xml) Boolean.parse)
      ; next_record_name = Util.option_bind (Xml.member "NextRecordName" xml) String.parse
      ; next_record_type = Util.option_bind (Xml.member "NextRecordType" xml) RRType.parse
      ; next_record_identifier =
          Util.option_bind (Xml.member "NextRecordIdentifier" xml) String.parse
      ; max_items =
          Xml.required
            "MaxItems"
            (Util.option_bind (Xml.member "MaxItems" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("MaxItems", String.to_query v.max_items))
         ; Util.option_map v.next_record_identifier (fun f ->
               Query.Pair ("NextRecordIdentifier", String.to_query f))
         ; Util.option_map v.next_record_type (fun f ->
               Query.Pair ("NextRecordType", RRType.to_query f))
         ; Util.option_map v.next_record_name (fun f ->
               Query.Pair ("NextRecordName", String.to_query f))
         ; Some (Query.Pair ("IsTruncated", Boolean.to_query v.is_truncated))
         ; Some
             (Query.Pair
                ( "ResourceRecordSets.member"
                , ResourceRecordSets.to_query v.resource_record_sets ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("max_items", String.to_json v.max_items)
         ; Util.option_map v.next_record_identifier (fun f ->
               "next_record_identifier", String.to_json f)
         ; Util.option_map v.next_record_type (fun f ->
               "next_record_type", RRType.to_json f)
         ; Util.option_map v.next_record_name (fun f ->
               "next_record_name", String.to_json f)
         ; Some ("is_truncated", Boolean.to_json v.is_truncated)
         ; Some ("resource_record_sets", ResourceRecordSets.to_json v.resource_record_sets)
         ])

  let of_json j =
    { resource_record_sets =
        ResourceRecordSets.of_json
          (Util.of_option_exn (Json.lookup j "resource_record_sets"))
    ; is_truncated = Boolean.of_json (Util.of_option_exn (Json.lookup j "is_truncated"))
    ; next_record_name = Util.option_map (Json.lookup j "next_record_name") String.of_json
    ; next_record_type = Util.option_map (Json.lookup j "next_record_type") RRType.of_json
    ; next_record_identifier =
        Util.option_map (Json.lookup j "next_record_identifier") String.of_json
    ; max_items = String.of_json (Util.of_option_exn (Json.lookup j "max_items"))
    }
end

module NoSuchGeoLocation = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module GetHealthCheckCountRequest = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DelegationSetInUse = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module GetReusableDelegationSetRequest = struct
  type t = { id : String.t }

  let make ~id () = { id }

  let parse xml =
    Some { id = Xml.required "Id" (Util.option_bind (Xml.member "Id" xml) String.parse) }

  let to_query v =
    Query.List (Util.list_filter_opt [ Some (Query.Pair ("Id", String.to_query v.id)) ])

  let to_json v = `Assoc (Util.list_filter_opt [ Some ("id", String.to_json v.id) ])

  let of_json j = { id = String.of_json (Util.of_option_exn (Json.lookup j "id")) }
end

module DeleteHostedZoneResponse = struct
  type t = { change_info : ChangeInfo.t }

  let make ~change_info () = { change_info }

  let parse xml =
    Some
      { change_info =
          Xml.required
            "ChangeInfo"
            (Util.option_bind (Xml.member "ChangeInfo" xml) ChangeInfo.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("ChangeInfo", ChangeInfo.to_query v.change_info)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt [ Some ("change_info", ChangeInfo.to_json v.change_info) ])

  let of_json j =
    { change_info = ChangeInfo.of_json (Util.of_option_exn (Json.lookup j "change_info"))
    }
end

module NoSuchHealthCheck = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module DelegationSetNotReusable = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module HealthCheckVersionMismatch = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module InvalidDomainName = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module ListReusableDelegationSetsResponse = struct
  type t =
    { delegation_sets : DelegationSets.t
    ; marker : String.t
    ; is_truncated : Boolean.t
    ; next_marker : String.t option
    ; max_items : String.t
    }

  let make ~delegation_sets ~marker ~is_truncated ?next_marker ~max_items () =
    { delegation_sets; marker; is_truncated; next_marker; max_items }

  let parse xml =
    Some
      { delegation_sets =
          Xml.required
            "DelegationSets"
            (Util.option_bind (Xml.member "DelegationSets" xml) DelegationSets.parse)
      ; marker =
          Xml.required "Marker" (Util.option_bind (Xml.member "Marker" xml) String.parse)
      ; is_truncated =
          Xml.required
            "IsTruncated"
            (Util.option_bind (Xml.member "IsTruncated" xml) Boolean.parse)
      ; next_marker = Util.option_bind (Xml.member "NextMarker" xml) String.parse
      ; max_items =
          Xml.required
            "MaxItems"
            (Util.option_bind (Xml.member "MaxItems" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("MaxItems", String.to_query v.max_items))
         ; Util.option_map v.next_marker (fun f ->
               Query.Pair ("NextMarker", String.to_query f))
         ; Some (Query.Pair ("IsTruncated", Boolean.to_query v.is_truncated))
         ; Some (Query.Pair ("Marker", String.to_query v.marker))
         ; Some
             (Query.Pair
                ("DelegationSets.member", DelegationSets.to_query v.delegation_sets))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("max_items", String.to_json v.max_items)
         ; Util.option_map v.next_marker (fun f -> "next_marker", String.to_json f)
         ; Some ("is_truncated", Boolean.to_json v.is_truncated)
         ; Some ("marker", String.to_json v.marker)
         ; Some ("delegation_sets", DelegationSets.to_json v.delegation_sets)
         ])

  let of_json j =
    { delegation_sets =
        DelegationSets.of_json (Util.of_option_exn (Json.lookup j "delegation_sets"))
    ; marker = String.of_json (Util.of_option_exn (Json.lookup j "marker"))
    ; is_truncated = Boolean.of_json (Util.of_option_exn (Json.lookup j "is_truncated"))
    ; next_marker = Util.option_map (Json.lookup j "next_marker") String.of_json
    ; max_items = String.of_json (Util.of_option_exn (Json.lookup j "max_items"))
    }
end

module DeleteHealthCheckResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module UpdateHostedZoneCommentResponse = struct
  type t = { hosted_zone : HostedZone.t }

  let make ~hosted_zone () = { hosted_zone }

  let parse xml =
    Some
      { hosted_zone =
          Xml.required
            "HostedZone"
            (Util.option_bind (Xml.member "HostedZone" xml) HostedZone.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("HostedZone", HostedZone.to_query v.hosted_zone)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt [ Some ("hosted_zone", HostedZone.to_json v.hosted_zone) ])

  let of_json j =
    { hosted_zone = HostedZone.of_json (Util.of_option_exn (Json.lookup j "hosted_zone"))
    }
end

module CreateHealthCheckRequest = struct
  type t =
    { caller_reference : String.t
    ; health_check_config : HealthCheckConfig.t
    }

  let make ~caller_reference ~health_check_config () =
    { caller_reference; health_check_config }

  let parse xml =
    Some
      { caller_reference =
          Xml.required
            "CallerReference"
            (Util.option_bind (Xml.member "CallerReference" xml) String.parse)
      ; health_check_config =
          Xml.required
            "HealthCheckConfig"
            (Util.option_bind
               (Xml.member "HealthCheckConfig" xml)
               HealthCheckConfig.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("HealthCheckConfig", HealthCheckConfig.to_query v.health_check_config))
         ; Some (Query.Pair ("CallerReference", String.to_query v.caller_reference))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("health_check_config", HealthCheckConfig.to_json v.health_check_config)
         ; Some ("caller_reference", String.to_json v.caller_reference)
         ])

  let of_json j =
    { caller_reference =
        String.of_json (Util.of_option_exn (Json.lookup j "caller_reference"))
    ; health_check_config =
        HealthCheckConfig.of_json
          (Util.of_option_exn (Json.lookup j "health_check_config"))
    }
end

module CreateHostedZoneRequest = struct
  type t =
    { name : String.t
    ; v_p_c : VPC.t option
    ; caller_reference : String.t
    ; hosted_zone_config : HostedZoneConfig.t option
    ; delegation_set_id : String.t option
    }

  let make ~name ?v_p_c ~caller_reference ?hosted_zone_config ?delegation_set_id () =
    { name; v_p_c; caller_reference; hosted_zone_config; delegation_set_id }

  let parse xml =
    Some
      { name = Xml.required "Name" (Util.option_bind (Xml.member "Name" xml) String.parse)
      ; v_p_c = Util.option_bind (Xml.member "VPC" xml) VPC.parse
      ; caller_reference =
          Xml.required
            "CallerReference"
            (Util.option_bind (Xml.member "CallerReference" xml) String.parse)
      ; hosted_zone_config =
          Util.option_bind (Xml.member "HostedZoneConfig" xml) HostedZoneConfig.parse
      ; delegation_set_id =
          Util.option_bind (Xml.member "DelegationSetId" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.delegation_set_id (fun f ->
               Query.Pair ("DelegationSetId", String.to_query f))
         ; Util.option_map v.hosted_zone_config (fun f ->
               Query.Pair ("HostedZoneConfig", HostedZoneConfig.to_query f))
         ; Some (Query.Pair ("CallerReference", String.to_query v.caller_reference))
         ; Util.option_map v.v_p_c (fun f -> Query.Pair ("VPC", VPC.to_query f))
         ; Some (Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.delegation_set_id (fun f ->
               "delegation_set_id", String.to_json f)
         ; Util.option_map v.hosted_zone_config (fun f ->
               "hosted_zone_config", HostedZoneConfig.to_json f)
         ; Some ("caller_reference", String.to_json v.caller_reference)
         ; Util.option_map v.v_p_c (fun f -> "v_p_c", VPC.to_json f)
         ; Some ("name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Util.of_option_exn (Json.lookup j "name"))
    ; v_p_c = Util.option_map (Json.lookup j "v_p_c") VPC.of_json
    ; caller_reference =
        String.of_json (Util.of_option_exn (Json.lookup j "caller_reference"))
    ; hosted_zone_config =
        Util.option_map (Json.lookup j "hosted_zone_config") HostedZoneConfig.of_json
    ; delegation_set_id =
        Util.option_map (Json.lookup j "delegation_set_id") String.of_json
    }
end

module DeleteHealthCheckRequest = struct
  type t = { health_check_id : String.t }

  let make ~health_check_id () = { health_check_id }

  let parse xml =
    Some
      { health_check_id =
          Xml.required
            "HealthCheckId"
            (Util.option_bind (Xml.member "HealthCheckId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("HealthCheckId", String.to_query v.health_check_id)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("health_check_id", String.to_json v.health_check_id) ])

  let of_json j =
    { health_check_id =
        String.of_json (Util.of_option_exn (Json.lookup j "health_check_id"))
    }
end

module LastVPCAssociation = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module PriorRequestNotComplete = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module ChangeResourceRecordSetsRequest = struct
  type t =
    { hosted_zone_id : String.t
    ; change_batch : ChangeBatch.t
    }

  let make ~hosted_zone_id ~change_batch () = { hosted_zone_id; change_batch }

  let parse xml =
    Some
      { hosted_zone_id =
          Xml.required "Id" (Util.option_bind (Xml.member "Id" xml) String.parse)
      ; change_batch =
          Xml.required
            "ChangeBatch"
            (Util.option_bind (Xml.member "ChangeBatch" xml) ChangeBatch.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("ChangeBatch", ChangeBatch.to_query v.change_batch))
         ; Some (Query.Pair ("Id", String.to_query v.hosted_zone_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("change_batch", ChangeBatch.to_json v.change_batch)
         ; Some ("hosted_zone_id", String.to_json v.hosted_zone_id)
         ])

  let of_json j =
    { hosted_zone_id =
        String.of_json (Util.of_option_exn (Json.lookup j "hosted_zone_id"))
    ; change_batch =
        ChangeBatch.of_json (Util.of_option_exn (Json.lookup j "change_batch"))
    }
end

module GetHostedZoneResponse = struct
  type t =
    { hosted_zone : HostedZone.t
    ; delegation_set : DelegationSet.t option
    ; v_p_cs : VPCs.t
    }

  let make ~hosted_zone ?delegation_set ?(v_p_cs = []) () =
    { hosted_zone; delegation_set; v_p_cs }

  let parse xml =
    Some
      { hosted_zone =
          Xml.required
            "HostedZone"
            (Util.option_bind (Xml.member "HostedZone" xml) HostedZone.parse)
      ; delegation_set =
          Util.option_bind (Xml.member "DelegationSet" xml) DelegationSet.parse
      ; v_p_cs = Util.of_option [] (Util.option_bind (Xml.member "VPCs" xml) VPCs.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("VPCs.member", VPCs.to_query v.v_p_cs))
         ; Util.option_map v.delegation_set (fun f ->
               Query.Pair ("DelegationSet", DelegationSet.to_query f))
         ; Some (Query.Pair ("HostedZone", HostedZone.to_query v.hosted_zone))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("v_p_cs", VPCs.to_json v.v_p_cs)
         ; Util.option_map v.delegation_set (fun f ->
               "delegation_set", DelegationSet.to_json f)
         ; Some ("hosted_zone", HostedZone.to_json v.hosted_zone)
         ])

  let of_json j =
    { hosted_zone = HostedZone.of_json (Util.of_option_exn (Json.lookup j "hosted_zone"))
    ; delegation_set =
        Util.option_map (Json.lookup j "delegation_set") DelegationSet.of_json
    ; v_p_cs = VPCs.of_json (Util.of_option_exn (Json.lookup j "v_p_cs"))
    }
end

module HostedZoneAlreadyExists = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module ListHostedZonesByNameRequest = struct
  type t =
    { d_n_s_name : String.t option
    ; hosted_zone_id : String.t option
    ; max_items : String.t option
    }

  let make ?d_n_s_name ?hosted_zone_id ?max_items () =
    { d_n_s_name; hosted_zone_id; max_items }

  let parse xml =
    Some
      { d_n_s_name = Util.option_bind (Xml.member "dnsname" xml) String.parse
      ; hosted_zone_id = Util.option_bind (Xml.member "hostedzoneid" xml) String.parse
      ; max_items = Util.option_bind (Xml.member "maxitems" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.max_items (fun f ->
               Query.Pair ("maxitems", String.to_query f))
         ; Util.option_map v.hosted_zone_id (fun f ->
               Query.Pair ("hostedzoneid", String.to_query f))
         ; Util.option_map v.d_n_s_name (fun f ->
               Query.Pair ("dnsname", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.max_items (fun f -> "max_items", String.to_json f)
         ; Util.option_map v.hosted_zone_id (fun f -> "hosted_zone_id", String.to_json f)
         ; Util.option_map v.d_n_s_name (fun f -> "d_n_s_name", String.to_json f)
         ])

  let of_json j =
    { d_n_s_name = Util.option_map (Json.lookup j "d_n_s_name") String.of_json
    ; hosted_zone_id = Util.option_map (Json.lookup j "hosted_zone_id") String.of_json
    ; max_items = Util.option_map (Json.lookup j "max_items") String.of_json
    }
end

module UpdateHostedZoneCommentRequest = struct
  type t =
    { id : String.t
    ; comment : String.t option
    }

  let make ~id ?comment () = { id; comment }

  let parse xml =
    Some
      { id = Xml.required "Id" (Util.option_bind (Xml.member "Id" xml) String.parse)
      ; comment = Util.option_bind (Xml.member "Comment" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.comment (fun f -> Query.Pair ("Comment", String.to_query f))
         ; Some (Query.Pair ("Id", String.to_query v.id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.comment (fun f -> "comment", String.to_json f)
         ; Some ("id", String.to_json v.id)
         ])

  let of_json j =
    { id = String.of_json (Util.of_option_exn (Json.lookup j "id"))
    ; comment = Util.option_map (Json.lookup j "comment") String.of_json
    }
end

module HealthCheckAlreadyExists = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end
