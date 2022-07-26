open Aws.BaseTypes

type calendar = CalendarLib.Calendar.t

module RecordData = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "RecordDataEntry" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module RRType = struct
  type t =
    | SOA
    | A
    | TXT
    | NS
    | CNAME
    | MX
    | NAPTR
    | PTR
    | SRV
    | SPF
    | AAAA
    | CAA

  let str_to_t =
    [ "CAA", CAA
    ; "AAAA", AAAA
    ; "SPF", SPF
    ; "SRV", SRV
    ; "PTR", PTR
    ; "NAPTR", NAPTR
    ; "MX", MX
    ; "CNAME", CNAME
    ; "NS", NS
    ; "TXT", TXT
    ; "A", A
    ; "SOA", SOA
    ]

  let t_to_str =
    [ CAA, "CAA"
    ; AAAA, "AAAA"
    ; SPF, "SPF"
    ; SRV, "SRV"
    ; PTR, "PTR"
    ; NAPTR, "NAPTR"
    ; MX, "MX"
    ; CNAME, "CNAME"
    ; NS, "NS"
    ; TXT, "TXT"
    ; A, "A"
    ; SOA, "SOA"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module TestDNSAnswerResponse = struct
  type t =
    { nameserver : String.t
    ; record_name : String.t
    ; record_type : RRType.t
    ; record_data : RecordData.t
    ; response_code : String.t
    ; protocol : String.t
    }

  let make ~nameserver ~record_name ~record_type ~record_data ~response_code ~protocol ()
      =
    { nameserver; record_name; record_type; record_data; response_code; protocol }

  let parse xml =
    Some
      { nameserver =
          Aws.Xml.required
            "Nameserver"
            (Aws.Util.option_bind (Aws.Xml.member "Nameserver" xml) String.parse)
      ; record_name =
          Aws.Xml.required
            "RecordName"
            (Aws.Util.option_bind (Aws.Xml.member "RecordName" xml) String.parse)
      ; record_type =
          Aws.Xml.required
            "RecordType"
            (Aws.Util.option_bind (Aws.Xml.member "RecordType" xml) RRType.parse)
      ; record_data =
          Aws.Xml.required
            "RecordData"
            (Aws.Util.option_bind (Aws.Xml.member "RecordData" xml) RecordData.parse)
      ; response_code =
          Aws.Xml.required
            "ResponseCode"
            (Aws.Util.option_bind (Aws.Xml.member "ResponseCode" xml) String.parse)
      ; protocol =
          Aws.Xml.required
            "Protocol"
            (Aws.Util.option_bind (Aws.Xml.member "Protocol" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Protocol", String.to_query v.protocol))
         ; Some (Aws.Query.Pair ("ResponseCode", String.to_query v.response_code))
         ; Some (Aws.Query.Pair ("RecordData.member", RecordData.to_query v.record_data))
         ; Some (Aws.Query.Pair ("RecordType", RRType.to_query v.record_type))
         ; Some (Aws.Query.Pair ("RecordName", String.to_query v.record_name))
         ; Some (Aws.Query.Pair ("Nameserver", String.to_query v.nameserver))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Protocol", String.to_json v.protocol)
         ; Some ("ResponseCode", String.to_json v.response_code)
         ; Some ("RecordData", RecordData.to_json v.record_data)
         ; Some ("RecordType", RRType.to_json v.record_type)
         ; Some ("RecordName", String.to_json v.record_name)
         ; Some ("Nameserver", String.to_json v.nameserver)
         ])

  let of_json j =
    { nameserver =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Nameserver"))
    ; record_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RecordName"))
    ; record_type =
        RRType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RecordType"))
    ; record_data =
        RecordData.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RecordData"))
    ; response_code =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResponseCode"))
    ; protocol = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Protocol"))
    }
end

module HealthCheckAlreadyExists = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module ConcurrentModification = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module HostedZoneOwner = struct
  type t =
    { owning_account : String.t option
    ; owning_service : String.t option
    }

  let make ?owning_account ?owning_service () = { owning_account; owning_service }

  let parse xml =
    Some
      { owning_account =
          Aws.Util.option_bind (Aws.Xml.member "OwningAccount" xml) String.parse
      ; owning_service =
          Aws.Util.option_bind (Aws.Xml.member "OwningService" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.owning_service (fun f ->
               Aws.Query.Pair ("OwningService", String.to_query f))
         ; Aws.Util.option_map v.owning_account (fun f ->
               Aws.Query.Pair ("OwningAccount", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.owning_service (fun f ->
               "OwningService", String.to_json f)
         ; Aws.Util.option_map v.owning_account (fun f ->
               "OwningAccount", String.to_json f)
         ])

  let of_json j =
    { owning_account =
        Aws.Util.option_map (Aws.Json.lookup j "OwningAccount") String.of_json
    ; owning_service =
        Aws.Util.option_map (Aws.Json.lookup j "OwningService") String.of_json
    }
end

module HostedZoneSummary = struct
  type t =
    { hosted_zone_id : String.t
    ; name : String.t
    ; owner : HostedZoneOwner.t
    }

  let make ~hosted_zone_id ~name ~owner () = { hosted_zone_id; name; owner }

  let parse xml =
    Some
      { hosted_zone_id =
          Aws.Xml.required
            "HostedZoneId"
            (Aws.Util.option_bind (Aws.Xml.member "HostedZoneId" xml) String.parse)
      ; name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      ; owner =
          Aws.Xml.required
            "Owner"
            (Aws.Util.option_bind (Aws.Xml.member "Owner" xml) HostedZoneOwner.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Owner", HostedZoneOwner.to_query v.owner))
         ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
         ; Some (Aws.Query.Pair ("HostedZoneId", String.to_query v.hosted_zone_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Owner", HostedZoneOwner.to_json v.owner)
         ; Some ("Name", String.to_json v.name)
         ; Some ("HostedZoneId", String.to_json v.hosted_zone_id)
         ])

  let of_json j =
    { hosted_zone_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZoneId"))
    ; name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    ; owner = HostedZoneOwner.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Owner"))
    }
end

module VPCRegion = struct
  type t =
    | Us_east_1
    | Us_east_2
    | Us_west_1
    | Us_west_2
    | Eu_west_1
    | Eu_west_2
    | Eu_west_3
    | Eu_central_1
    | Ap_east_1
    | Me_south_1
    | Us_gov_west_1
    | Us_gov_east_1
    | Us_iso_east_1
    | Us_isob_east_1
    | Ap_southeast_1
    | Ap_southeast_2
    | Ap_south_1
    | Ap_northeast_1
    | Ap_northeast_2
    | Ap_northeast_3
    | Eu_north_1
    | Sa_east_1
    | Ca_central_1
    | Cn_north_1
    | Af_south_1
    | Eu_south_1

  let str_to_t =
    [ "eu-south-1", Eu_south_1
    ; "af-south-1", Af_south_1
    ; "cn-north-1", Cn_north_1
    ; "ca-central-1", Ca_central_1
    ; "sa-east-1", Sa_east_1
    ; "eu-north-1", Eu_north_1
    ; "ap-northeast-3", Ap_northeast_3
    ; "ap-northeast-2", Ap_northeast_2
    ; "ap-northeast-1", Ap_northeast_1
    ; "ap-south-1", Ap_south_1
    ; "ap-southeast-2", Ap_southeast_2
    ; "ap-southeast-1", Ap_southeast_1
    ; "us-isob-east-1", Us_isob_east_1
    ; "us-iso-east-1", Us_iso_east_1
    ; "us-gov-east-1", Us_gov_east_1
    ; "us-gov-west-1", Us_gov_west_1
    ; "me-south-1", Me_south_1
    ; "ap-east-1", Ap_east_1
    ; "eu-central-1", Eu_central_1
    ; "eu-west-3", Eu_west_3
    ; "eu-west-2", Eu_west_2
    ; "eu-west-1", Eu_west_1
    ; "us-west-2", Us_west_2
    ; "us-west-1", Us_west_1
    ; "us-east-2", Us_east_2
    ; "us-east-1", Us_east_1
    ]

  let t_to_str =
    [ Eu_south_1, "eu-south-1"
    ; Af_south_1, "af-south-1"
    ; Cn_north_1, "cn-north-1"
    ; Ca_central_1, "ca-central-1"
    ; Sa_east_1, "sa-east-1"
    ; Eu_north_1, "eu-north-1"
    ; Ap_northeast_3, "ap-northeast-3"
    ; Ap_northeast_2, "ap-northeast-2"
    ; Ap_northeast_1, "ap-northeast-1"
    ; Ap_south_1, "ap-south-1"
    ; Ap_southeast_2, "ap-southeast-2"
    ; Ap_southeast_1, "ap-southeast-1"
    ; Us_isob_east_1, "us-isob-east-1"
    ; Us_iso_east_1, "us-iso-east-1"
    ; Us_gov_east_1, "us-gov-east-1"
    ; Us_gov_west_1, "us-gov-west-1"
    ; Me_south_1, "me-south-1"
    ; Ap_east_1, "ap-east-1"
    ; Eu_central_1, "eu-central-1"
    ; Eu_west_3, "eu-west-3"
    ; Eu_west_2, "eu-west-2"
    ; Eu_west_1, "eu-west-1"
    ; Us_west_2, "us-west-2"
    ; Us_west_1, "us-west-1"
    ; Us_east_2, "us-east-2"
    ; Us_east_1, "us-east-1"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module VPC = struct
  type t =
    { v_p_c_region : VPCRegion.t option
    ; v_p_c_id : String.t option
    }

  let make ?v_p_c_region ?v_p_c_id () = { v_p_c_region; v_p_c_id }

  let parse xml =
    Some
      { v_p_c_region =
          Aws.Util.option_bind (Aws.Xml.member "VPCRegion" xml) VPCRegion.parse
      ; v_p_c_id = Aws.Util.option_bind (Aws.Xml.member "VPCId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.v_p_c_id (fun f ->
               Aws.Query.Pair ("VPCId", String.to_query f))
         ; Aws.Util.option_map v.v_p_c_region (fun f ->
               Aws.Query.Pair ("VPCRegion", VPCRegion.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.v_p_c_id (fun f -> "VPCId", String.to_json f)
         ; Aws.Util.option_map v.v_p_c_region (fun f -> "VPCRegion", VPCRegion.to_json f)
         ])

  let of_json j =
    { v_p_c_region = Aws.Util.option_map (Aws.Json.lookup j "VPCRegion") VPCRegion.of_json
    ; v_p_c_id = Aws.Util.option_map (Aws.Json.lookup j "VPCId") String.of_json
    }
end

module CreateVPCAssociationAuthorizationRequest = struct
  type t =
    { hosted_zone_id : String.t
    ; v_p_c : VPC.t
    }

  let make ~hosted_zone_id ~v_p_c () = { hosted_zone_id; v_p_c }

  let parse xml =
    Some
      { hosted_zone_id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; v_p_c =
          Aws.Xml.required
            "VPC"
            (Aws.Util.option_bind (Aws.Xml.member "VPC" xml) VPC.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("VPC", VPC.to_query v.v_p_c))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.hosted_zone_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("VPC", VPC.to_json v.v_p_c)
         ; Some ("Id", String.to_json v.hosted_zone_id)
         ])

  let of_json j =
    { hosted_zone_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; v_p_c = VPC.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VPC"))
    }
end

module ConflictingTypes = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module UpdateHostedZoneCommentRequest = struct
  type t =
    { id : String.t
    ; comment : String.t option
    }

  let make ~id ?comment () = { id; comment }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; comment = Aws.Util.option_bind (Aws.Xml.member "Comment" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.comment (fun f ->
               Aws.Query.Pair ("Comment", String.to_query f))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.comment (fun f -> "Comment", String.to_json f)
         ; Some ("Id", String.to_json v.id)
         ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; comment = Aws.Util.option_map (Aws.Json.lookup j "Comment") String.of_json
    }
end

module QueryLoggingConfig = struct
  type t =
    { id : String.t
    ; hosted_zone_id : String.t
    ; cloud_watch_logs_log_group_arn : String.t
    }

  let make ~id ~hosted_zone_id ~cloud_watch_logs_log_group_arn () =
    { id; hosted_zone_id; cloud_watch_logs_log_group_arn }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; hosted_zone_id =
          Aws.Xml.required
            "HostedZoneId"
            (Aws.Util.option_bind (Aws.Xml.member "HostedZoneId" xml) String.parse)
      ; cloud_watch_logs_log_group_arn =
          Aws.Xml.required
            "CloudWatchLogsLogGroupArn"
            (Aws.Util.option_bind
               (Aws.Xml.member "CloudWatchLogsLogGroupArn" xml)
               String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "CloudWatchLogsLogGroupArn"
                , String.to_query v.cloud_watch_logs_log_group_arn ))
         ; Some (Aws.Query.Pair ("HostedZoneId", String.to_query v.hosted_zone_id))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ("CloudWatchLogsLogGroupArn", String.to_json v.cloud_watch_logs_log_group_arn)
         ; Some ("HostedZoneId", String.to_json v.hosted_zone_id)
         ; Some ("Id", String.to_json v.id)
         ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; hosted_zone_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZoneId"))
    ; cloud_watch_logs_log_group_arn =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CloudWatchLogsLogGroupArn"))
    }
end

module QueryLoggingConfigs = struct
  type t = QueryLoggingConfig.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map QueryLoggingConfig.parse (Aws.Xml.members "QueryLoggingConfig" xml))

  let to_query v = Aws.Query.to_query_list QueryLoggingConfig.to_query v

  let to_json v = `List (List.map QueryLoggingConfig.to_json v)

  let of_json j = Aws.Json.to_list QueryLoggingConfig.of_json j
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
      { d_n_s_name = Aws.Util.option_bind (Aws.Xml.member "dnsname" xml) String.parse
      ; hosted_zone_id =
          Aws.Util.option_bind (Aws.Xml.member "hostedzoneid" xml) String.parse
      ; max_items = Aws.Util.option_bind (Aws.Xml.member "maxitems" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_items (fun f ->
               Aws.Query.Pair ("maxitems", String.to_query f))
         ; Aws.Util.option_map v.hosted_zone_id (fun f ->
               Aws.Query.Pair ("hostedzoneid", String.to_query f))
         ; Aws.Util.option_map v.d_n_s_name (fun f ->
               Aws.Query.Pair ("dnsname", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_items (fun f -> "maxitems", String.to_json f)
         ; Aws.Util.option_map v.hosted_zone_id (fun f ->
               "hostedzoneid", String.to_json f)
         ; Aws.Util.option_map v.d_n_s_name (fun f -> "dnsname", String.to_json f)
         ])

  let of_json j =
    { d_n_s_name = Aws.Util.option_map (Aws.Json.lookup j "dnsname") String.of_json
    ; hosted_zone_id =
        Aws.Util.option_map (Aws.Json.lookup j "hostedzoneid") String.of_json
    ; max_items = Aws.Util.option_map (Aws.Json.lookup j "maxitems") String.of_json
    }
end

module TrafficPolicyInstance = struct
  type t =
    { id : String.t
    ; hosted_zone_id : String.t
    ; name : String.t
    ; t_t_l : Long.t
    ; state : String.t
    ; message : String.t
    ; traffic_policy_id : String.t
    ; traffic_policy_version : Integer.t
    ; traffic_policy_type : RRType.t
    }

  let make
      ~id
      ~hosted_zone_id
      ~name
      ~t_t_l
      ~state
      ~message
      ~traffic_policy_id
      ~traffic_policy_version
      ~traffic_policy_type
      () =
    { id
    ; hosted_zone_id
    ; name
    ; t_t_l
    ; state
    ; message
    ; traffic_policy_id
    ; traffic_policy_version
    ; traffic_policy_type
    }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; hosted_zone_id =
          Aws.Xml.required
            "HostedZoneId"
            (Aws.Util.option_bind (Aws.Xml.member "HostedZoneId" xml) String.parse)
      ; name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      ; t_t_l =
          Aws.Xml.required
            "TTL"
            (Aws.Util.option_bind (Aws.Xml.member "TTL" xml) Long.parse)
      ; state =
          Aws.Xml.required
            "State"
            (Aws.Util.option_bind (Aws.Xml.member "State" xml) String.parse)
      ; message =
          Aws.Xml.required
            "Message"
            (Aws.Util.option_bind (Aws.Xml.member "Message" xml) String.parse)
      ; traffic_policy_id =
          Aws.Xml.required
            "TrafficPolicyId"
            (Aws.Util.option_bind (Aws.Xml.member "TrafficPolicyId" xml) String.parse)
      ; traffic_policy_version =
          Aws.Xml.required
            "TrafficPolicyVersion"
            (Aws.Util.option_bind
               (Aws.Xml.member "TrafficPolicyVersion" xml)
               Integer.parse)
      ; traffic_policy_type =
          Aws.Xml.required
            "TrafficPolicyType"
            (Aws.Util.option_bind (Aws.Xml.member "TrafficPolicyType" xml) RRType.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("TrafficPolicyType", RRType.to_query v.traffic_policy_type))
         ; Some
             (Aws.Query.Pair
                ("TrafficPolicyVersion", Integer.to_query v.traffic_policy_version))
         ; Some (Aws.Query.Pair ("TrafficPolicyId", String.to_query v.traffic_policy_id))
         ; Some (Aws.Query.Pair ("Message", String.to_query v.message))
         ; Some (Aws.Query.Pair ("State", String.to_query v.state))
         ; Some (Aws.Query.Pair ("TTL", Long.to_query v.t_t_l))
         ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
         ; Some (Aws.Query.Pair ("HostedZoneId", String.to_query v.hosted_zone_id))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("TrafficPolicyType", RRType.to_json v.traffic_policy_type)
         ; Some ("TrafficPolicyVersion", Integer.to_json v.traffic_policy_version)
         ; Some ("TrafficPolicyId", String.to_json v.traffic_policy_id)
         ; Some ("Message", String.to_json v.message)
         ; Some ("State", String.to_json v.state)
         ; Some ("TTL", Long.to_json v.t_t_l)
         ; Some ("Name", String.to_json v.name)
         ; Some ("HostedZoneId", String.to_json v.hosted_zone_id)
         ; Some ("Id", String.to_json v.id)
         ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; hosted_zone_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZoneId"))
    ; name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    ; t_t_l = Long.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TTL"))
    ; state = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "State"))
    ; message = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Message"))
    ; traffic_policy_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyId"))
    ; traffic_policy_version =
        Integer.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyVersion"))
    ; traffic_policy_type =
        RRType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyType"))
    }
end

module TrafficPolicyInstances = struct
  type t = TrafficPolicyInstance.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map TrafficPolicyInstance.parse (Aws.Xml.members "TrafficPolicyInstance" xml))

  let to_query v = Aws.Query.to_query_list TrafficPolicyInstance.to_query v

  let to_json v = `List (List.map TrafficPolicyInstance.to_json v)

  let of_json j = Aws.Json.to_list TrafficPolicyInstance.of_json j
end

module ListTrafficPolicyInstancesByPolicyResponse = struct
  type t =
    { traffic_policy_instances : TrafficPolicyInstances.t
    ; hosted_zone_id_marker : String.t option
    ; traffic_policy_instance_name_marker : String.t option
    ; traffic_policy_instance_type_marker : RRType.t option
    ; is_truncated : Boolean.t
    ; max_items : String.t
    }

  let make
      ~traffic_policy_instances
      ?hosted_zone_id_marker
      ?traffic_policy_instance_name_marker
      ?traffic_policy_instance_type_marker
      ~is_truncated
      ~max_items
      () =
    { traffic_policy_instances
    ; hosted_zone_id_marker
    ; traffic_policy_instance_name_marker
    ; traffic_policy_instance_type_marker
    ; is_truncated
    ; max_items
    }

  let parse xml =
    Some
      { traffic_policy_instances =
          Aws.Xml.required
            "TrafficPolicyInstances"
            (Aws.Util.option_bind
               (Aws.Xml.member "TrafficPolicyInstances" xml)
               TrafficPolicyInstances.parse)
      ; hosted_zone_id_marker =
          Aws.Util.option_bind (Aws.Xml.member "HostedZoneIdMarker" xml) String.parse
      ; traffic_policy_instance_name_marker =
          Aws.Util.option_bind
            (Aws.Xml.member "TrafficPolicyInstanceNameMarker" xml)
            String.parse
      ; traffic_policy_instance_type_marker =
          Aws.Util.option_bind
            (Aws.Xml.member "TrafficPolicyInstanceTypeMarker" xml)
            RRType.parse
      ; is_truncated =
          Aws.Xml.required
            "IsTruncated"
            (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml) Boolean.parse)
      ; max_items =
          Aws.Xml.required
            "MaxItems"
            (Aws.Util.option_bind (Aws.Xml.member "MaxItems" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("MaxItems", String.to_query v.max_items))
         ; Some (Aws.Query.Pair ("IsTruncated", Boolean.to_query v.is_truncated))
         ; Aws.Util.option_map v.traffic_policy_instance_type_marker (fun f ->
               Aws.Query.Pair ("TrafficPolicyInstanceTypeMarker", RRType.to_query f))
         ; Aws.Util.option_map v.traffic_policy_instance_name_marker (fun f ->
               Aws.Query.Pair ("TrafficPolicyInstanceNameMarker", String.to_query f))
         ; Aws.Util.option_map v.hosted_zone_id_marker (fun f ->
               Aws.Query.Pair ("HostedZoneIdMarker", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "TrafficPolicyInstances.member"
                , TrafficPolicyInstances.to_query v.traffic_policy_instances ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("MaxItems", String.to_json v.max_items)
         ; Some ("IsTruncated", Boolean.to_json v.is_truncated)
         ; Aws.Util.option_map v.traffic_policy_instance_type_marker (fun f ->
               "TrafficPolicyInstanceTypeMarker", RRType.to_json f)
         ; Aws.Util.option_map v.traffic_policy_instance_name_marker (fun f ->
               "TrafficPolicyInstanceNameMarker", String.to_json f)
         ; Aws.Util.option_map v.hosted_zone_id_marker (fun f ->
               "HostedZoneIdMarker", String.to_json f)
         ; Some
             ( "TrafficPolicyInstances"
             , TrafficPolicyInstances.to_json v.traffic_policy_instances )
         ])

  let of_json j =
    { traffic_policy_instances =
        TrafficPolicyInstances.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyInstances"))
    ; hosted_zone_id_marker =
        Aws.Util.option_map (Aws.Json.lookup j "HostedZoneIdMarker") String.of_json
    ; traffic_policy_instance_name_marker =
        Aws.Util.option_map
          (Aws.Json.lookup j "TrafficPolicyInstanceNameMarker")
          String.of_json
    ; traffic_policy_instance_type_marker =
        Aws.Util.option_map
          (Aws.Json.lookup j "TrafficPolicyInstanceTypeMarker")
          RRType.of_json
    ; is_truncated =
        Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IsTruncated"))
    ; max_items = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxItems"))
    }
end

module ResourceRecord = struct
  type t = { value : String.t }

  let make ~value () = { value }

  let parse xml =
    Some
      { value =
          Aws.Xml.required
            "Value"
            (Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Value", String.to_query v.value)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("Value", String.to_json v.value) ])

  let of_json j =
    { value = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Value")) }
end

module ResourceRecords = struct
  type t = ResourceRecord.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map ResourceRecord.parse (Aws.Xml.members "ResourceRecord" xml))

  let to_query v = Aws.Query.to_query_list ResourceRecord.to_query v

  let to_json v = `List (List.map ResourceRecord.to_json v)

  let of_json j = Aws.Json.to_list ResourceRecord.of_json j
end

module ResourceRecordSetRegion = struct
  type t =
    | Us_east_1
    | Us_east_2
    | Us_west_1
    | Us_west_2
    | Ca_central_1
    | Eu_west_1
    | Eu_west_2
    | Eu_west_3
    | Eu_central_1
    | Ap_southeast_1
    | Ap_southeast_2
    | Ap_northeast_1
    | Ap_northeast_2
    | Ap_northeast_3
    | Eu_north_1
    | Sa_east_1
    | Cn_north_1
    | Cn_northwest_1
    | Ap_east_1
    | Me_south_1
    | Ap_south_1
    | Af_south_1
    | Eu_south_1

  let str_to_t =
    [ "eu-south-1", Eu_south_1
    ; "af-south-1", Af_south_1
    ; "ap-south-1", Ap_south_1
    ; "me-south-1", Me_south_1
    ; "ap-east-1", Ap_east_1
    ; "cn-northwest-1", Cn_northwest_1
    ; "cn-north-1", Cn_north_1
    ; "sa-east-1", Sa_east_1
    ; "eu-north-1", Eu_north_1
    ; "ap-northeast-3", Ap_northeast_3
    ; "ap-northeast-2", Ap_northeast_2
    ; "ap-northeast-1", Ap_northeast_1
    ; "ap-southeast-2", Ap_southeast_2
    ; "ap-southeast-1", Ap_southeast_1
    ; "eu-central-1", Eu_central_1
    ; "eu-west-3", Eu_west_3
    ; "eu-west-2", Eu_west_2
    ; "eu-west-1", Eu_west_1
    ; "ca-central-1", Ca_central_1
    ; "us-west-2", Us_west_2
    ; "us-west-1", Us_west_1
    ; "us-east-2", Us_east_2
    ; "us-east-1", Us_east_1
    ]

  let t_to_str =
    [ Eu_south_1, "eu-south-1"
    ; Af_south_1, "af-south-1"
    ; Ap_south_1, "ap-south-1"
    ; Me_south_1, "me-south-1"
    ; Ap_east_1, "ap-east-1"
    ; Cn_northwest_1, "cn-northwest-1"
    ; Cn_north_1, "cn-north-1"
    ; Sa_east_1, "sa-east-1"
    ; Eu_north_1, "eu-north-1"
    ; Ap_northeast_3, "ap-northeast-3"
    ; Ap_northeast_2, "ap-northeast-2"
    ; Ap_northeast_1, "ap-northeast-1"
    ; Ap_southeast_2, "ap-southeast-2"
    ; Ap_southeast_1, "ap-southeast-1"
    ; Eu_central_1, "eu-central-1"
    ; Eu_west_3, "eu-west-3"
    ; Eu_west_2, "eu-west-2"
    ; Eu_west_1, "eu-west-1"
    ; Ca_central_1, "ca-central-1"
    ; Us_west_2, "us-west-2"
    ; Us_west_1, "us-west-1"
    ; Us_east_2, "us-east-2"
    ; Us_east_1, "us-east-1"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module ResourceRecordSetFailover = struct
  type t =
    | PRIMARY
    | SECONDARY

  let str_to_t = [ "SECONDARY", SECONDARY; "PRIMARY", PRIMARY ]

  let t_to_str = [ SECONDARY, "SECONDARY"; PRIMARY, "PRIMARY" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
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
      { continent_code =
          Aws.Util.option_bind (Aws.Xml.member "ContinentCode" xml) String.parse
      ; country_code =
          Aws.Util.option_bind (Aws.Xml.member "CountryCode" xml) String.parse
      ; subdivision_code =
          Aws.Util.option_bind (Aws.Xml.member "SubdivisionCode" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.subdivision_code (fun f ->
               Aws.Query.Pair ("SubdivisionCode", String.to_query f))
         ; Aws.Util.option_map v.country_code (fun f ->
               Aws.Query.Pair ("CountryCode", String.to_query f))
         ; Aws.Util.option_map v.continent_code (fun f ->
               Aws.Query.Pair ("ContinentCode", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.subdivision_code (fun f ->
               "SubdivisionCode", String.to_json f)
         ; Aws.Util.option_map v.country_code (fun f -> "CountryCode", String.to_json f)
         ; Aws.Util.option_map v.continent_code (fun f ->
               "ContinentCode", String.to_json f)
         ])

  let of_json j =
    { continent_code =
        Aws.Util.option_map (Aws.Json.lookup j "ContinentCode") String.of_json
    ; country_code = Aws.Util.option_map (Aws.Json.lookup j "CountryCode") String.of_json
    ; subdivision_code =
        Aws.Util.option_map (Aws.Json.lookup j "SubdivisionCode") String.of_json
    }
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
          Aws.Xml.required
            "HostedZoneId"
            (Aws.Util.option_bind (Aws.Xml.member "HostedZoneId" xml) String.parse)
      ; d_n_s_name =
          Aws.Xml.required
            "DNSName"
            (Aws.Util.option_bind (Aws.Xml.member "DNSName" xml) String.parse)
      ; evaluate_target_health =
          Aws.Xml.required
            "EvaluateTargetHealth"
            (Aws.Util.option_bind
               (Aws.Xml.member "EvaluateTargetHealth" xml)
               Boolean.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("EvaluateTargetHealth", Boolean.to_query v.evaluate_target_health))
         ; Some (Aws.Query.Pair ("DNSName", String.to_query v.d_n_s_name))
         ; Some (Aws.Query.Pair ("HostedZoneId", String.to_query v.hosted_zone_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("EvaluateTargetHealth", Boolean.to_json v.evaluate_target_health)
         ; Some ("DNSName", String.to_json v.d_n_s_name)
         ; Some ("HostedZoneId", String.to_json v.hosted_zone_id)
         ])

  let of_json j =
    { hosted_zone_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZoneId"))
    ; d_n_s_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DNSName"))
    ; evaluate_target_health =
        Boolean.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "EvaluateTargetHealth"))
    }
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
    ; multi_value_answer : Boolean.t option
    ; t_t_l : Long.t option
    ; resource_records : ResourceRecords.t
    ; alias_target : AliasTarget.t option
    ; health_check_id : String.t option
    ; traffic_policy_instance_id : String.t option
    }

  let make
      ~name
      ~type_
      ?set_identifier
      ?weight
      ?region
      ?geo_location
      ?failover
      ?multi_value_answer
      ?t_t_l
      ?(resource_records = [])
      ?alias_target
      ?health_check_id
      ?traffic_policy_instance_id
      () =
    { name
    ; type_
    ; set_identifier
    ; weight
    ; region
    ; geo_location
    ; failover
    ; multi_value_answer
    ; t_t_l
    ; resource_records
    ; alias_target
    ; health_check_id
    ; traffic_policy_instance_id
    }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      ; type_ =
          Aws.Xml.required
            "Type"
            (Aws.Util.option_bind (Aws.Xml.member "Type" xml) RRType.parse)
      ; set_identifier =
          Aws.Util.option_bind (Aws.Xml.member "SetIdentifier" xml) String.parse
      ; weight = Aws.Util.option_bind (Aws.Xml.member "Weight" xml) Long.parse
      ; region =
          Aws.Util.option_bind (Aws.Xml.member "Region" xml) ResourceRecordSetRegion.parse
      ; geo_location =
          Aws.Util.option_bind (Aws.Xml.member "GeoLocation" xml) GeoLocation.parse
      ; failover =
          Aws.Util.option_bind
            (Aws.Xml.member "Failover" xml)
            ResourceRecordSetFailover.parse
      ; multi_value_answer =
          Aws.Util.option_bind (Aws.Xml.member "MultiValueAnswer" xml) Boolean.parse
      ; t_t_l = Aws.Util.option_bind (Aws.Xml.member "TTL" xml) Long.parse
      ; resource_records =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ResourceRecords" xml)
               ResourceRecords.parse)
      ; alias_target =
          Aws.Util.option_bind (Aws.Xml.member "AliasTarget" xml) AliasTarget.parse
      ; health_check_id =
          Aws.Util.option_bind (Aws.Xml.member "HealthCheckId" xml) String.parse
      ; traffic_policy_instance_id =
          Aws.Util.option_bind (Aws.Xml.member "TrafficPolicyInstanceId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.traffic_policy_instance_id (fun f ->
               Aws.Query.Pair ("TrafficPolicyInstanceId", String.to_query f))
         ; Aws.Util.option_map v.health_check_id (fun f ->
               Aws.Query.Pair ("HealthCheckId", String.to_query f))
         ; Aws.Util.option_map v.alias_target (fun f ->
               Aws.Query.Pair ("AliasTarget", AliasTarget.to_query f))
         ; Some
             (Aws.Query.Pair
                ("ResourceRecords.member", ResourceRecords.to_query v.resource_records))
         ; Aws.Util.option_map v.t_t_l (fun f -> Aws.Query.Pair ("TTL", Long.to_query f))
         ; Aws.Util.option_map v.multi_value_answer (fun f ->
               Aws.Query.Pair ("MultiValueAnswer", Boolean.to_query f))
         ; Aws.Util.option_map v.failover (fun f ->
               Aws.Query.Pair ("Failover", ResourceRecordSetFailover.to_query f))
         ; Aws.Util.option_map v.geo_location (fun f ->
               Aws.Query.Pair ("GeoLocation", GeoLocation.to_query f))
         ; Aws.Util.option_map v.region (fun f ->
               Aws.Query.Pair ("Region", ResourceRecordSetRegion.to_query f))
         ; Aws.Util.option_map v.weight (fun f ->
               Aws.Query.Pair ("Weight", Long.to_query f))
         ; Aws.Util.option_map v.set_identifier (fun f ->
               Aws.Query.Pair ("SetIdentifier", String.to_query f))
         ; Some (Aws.Query.Pair ("Type", RRType.to_query v.type_))
         ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.traffic_policy_instance_id (fun f ->
               "TrafficPolicyInstanceId", String.to_json f)
         ; Aws.Util.option_map v.health_check_id (fun f ->
               "HealthCheckId", String.to_json f)
         ; Aws.Util.option_map v.alias_target (fun f ->
               "AliasTarget", AliasTarget.to_json f)
         ; Some ("ResourceRecords", ResourceRecords.to_json v.resource_records)
         ; Aws.Util.option_map v.t_t_l (fun f -> "TTL", Long.to_json f)
         ; Aws.Util.option_map v.multi_value_answer (fun f ->
               "MultiValueAnswer", Boolean.to_json f)
         ; Aws.Util.option_map v.failover (fun f ->
               "Failover", ResourceRecordSetFailover.to_json f)
         ; Aws.Util.option_map v.geo_location (fun f ->
               "GeoLocation", GeoLocation.to_json f)
         ; Aws.Util.option_map v.region (fun f ->
               "Region", ResourceRecordSetRegion.to_json f)
         ; Aws.Util.option_map v.weight (fun f -> "Weight", Long.to_json f)
         ; Aws.Util.option_map v.set_identifier (fun f ->
               "SetIdentifier", String.to_json f)
         ; Some ("Type", RRType.to_json v.type_)
         ; Some ("Name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    ; type_ = RRType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Type"))
    ; set_identifier =
        Aws.Util.option_map (Aws.Json.lookup j "SetIdentifier") String.of_json
    ; weight = Aws.Util.option_map (Aws.Json.lookup j "Weight") Long.of_json
    ; region =
        Aws.Util.option_map (Aws.Json.lookup j "Region") ResourceRecordSetRegion.of_json
    ; geo_location =
        Aws.Util.option_map (Aws.Json.lookup j "GeoLocation") GeoLocation.of_json
    ; failover =
        Aws.Util.option_map
          (Aws.Json.lookup j "Failover")
          ResourceRecordSetFailover.of_json
    ; multi_value_answer =
        Aws.Util.option_map (Aws.Json.lookup j "MultiValueAnswer") Boolean.of_json
    ; t_t_l = Aws.Util.option_map (Aws.Json.lookup j "TTL") Long.of_json
    ; resource_records =
        ResourceRecords.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceRecords"))
    ; alias_target =
        Aws.Util.option_map (Aws.Json.lookup j "AliasTarget") AliasTarget.of_json
    ; health_check_id =
        Aws.Util.option_map (Aws.Json.lookup j "HealthCheckId") String.of_json
    ; traffic_policy_instance_id =
        Aws.Util.option_map (Aws.Json.lookup j "TrafficPolicyInstanceId") String.of_json
    }
end

module ResourceRecordSets = struct
  type t = ResourceRecordSet.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map ResourceRecordSet.parse (Aws.Xml.members "ResourceRecordSet" xml))

  let to_query v = Aws.Query.to_query_list ResourceRecordSet.to_query v

  let to_json v = `List (List.map ResourceRecordSet.to_json v)

  let of_json j = Aws.Json.to_list ResourceRecordSet.of_json j
end

module HostedZoneAlreadyExists = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module DelegationSetNameServers = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "NameServer" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
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
      { id = Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse
      ; caller_reference =
          Aws.Util.option_bind (Aws.Xml.member "CallerReference" xml) String.parse
      ; name_servers =
          Aws.Xml.required
            "NameServers"
            (Aws.Util.option_bind
               (Aws.Xml.member "NameServers" xml)
               DelegationSetNameServers.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("NameServers.member", DelegationSetNameServers.to_query v.name_servers))
         ; Aws.Util.option_map v.caller_reference (fun f ->
               Aws.Query.Pair ("CallerReference", String.to_query f))
         ; Aws.Util.option_map v.id (fun f -> Aws.Query.Pair ("Id", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("NameServers", DelegationSetNameServers.to_json v.name_servers)
         ; Aws.Util.option_map v.caller_reference (fun f ->
               "CallerReference", String.to_json f)
         ; Aws.Util.option_map v.id (fun f -> "Id", String.to_json f)
         ])

  let of_json j =
    { id = Aws.Util.option_map (Aws.Json.lookup j "Id") String.of_json
    ; caller_reference =
        Aws.Util.option_map (Aws.Json.lookup j "CallerReference") String.of_json
    ; name_servers =
        DelegationSetNameServers.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "NameServers"))
    }
end

module DelegationSets = struct
  type t = DelegationSet.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map DelegationSet.parse (Aws.Xml.members "DelegationSet" xml))

  let to_query v = Aws.Query.to_query_list DelegationSet.to_query v

  let to_json v = `List (List.map DelegationSet.to_json v)

  let of_json j = Aws.Json.to_list DelegationSet.of_json j
end

module TrafficPolicy = struct
  type t =
    { id : String.t
    ; version : Integer.t
    ; name : String.t
    ; type_ : RRType.t
    ; document : String.t
    ; comment : String.t option
    }

  let make ~id ~version ~name ~type_ ~document ?comment () =
    { id; version; name; type_; document; comment }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; version =
          Aws.Xml.required
            "Version"
            (Aws.Util.option_bind (Aws.Xml.member "Version" xml) Integer.parse)
      ; name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      ; type_ =
          Aws.Xml.required
            "Type"
            (Aws.Util.option_bind (Aws.Xml.member "Type" xml) RRType.parse)
      ; document =
          Aws.Xml.required
            "Document"
            (Aws.Util.option_bind (Aws.Xml.member "Document" xml) String.parse)
      ; comment = Aws.Util.option_bind (Aws.Xml.member "Comment" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.comment (fun f ->
               Aws.Query.Pair ("Comment", String.to_query f))
         ; Some (Aws.Query.Pair ("Document", String.to_query v.document))
         ; Some (Aws.Query.Pair ("Type", RRType.to_query v.type_))
         ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
         ; Some (Aws.Query.Pair ("Version", Integer.to_query v.version))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.comment (fun f -> "Comment", String.to_json f)
         ; Some ("Document", String.to_json v.document)
         ; Some ("Type", RRType.to_json v.type_)
         ; Some ("Name", String.to_json v.name)
         ; Some ("Version", Integer.to_json v.version)
         ; Some ("Id", String.to_json v.id)
         ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; version = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Version"))
    ; name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    ; type_ = RRType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Type"))
    ; document = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Document"))
    ; comment = Aws.Util.option_map (Aws.Json.lookup j "Comment") String.of_json
    }
end

module CreateTrafficPolicyResponse = struct
  type t =
    { traffic_policy : TrafficPolicy.t
    ; location : String.t
    }

  let make ~traffic_policy ~location () = { traffic_policy; location }

  let parse xml =
    Some
      { traffic_policy =
          Aws.Xml.required
            "TrafficPolicy"
            (Aws.Util.option_bind
               (Aws.Xml.member "TrafficPolicy" xml)
               TrafficPolicy.parse)
      ; location =
          Aws.Xml.required
            "Location"
            (Aws.Util.option_bind (Aws.Xml.member "Location" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Location", String.to_query v.location))
         ; Some
             (Aws.Query.Pair ("TrafficPolicy", TrafficPolicy.to_query v.traffic_policy))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Location", String.to_json v.location)
         ; Some ("TrafficPolicy", TrafficPolicy.to_json v.traffic_policy)
         ])

  let of_json j =
    { traffic_policy =
        TrafficPolicy.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicy"))
    ; location = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Location"))
    }
end

module VPCs = struct
  type t = VPC.t list

  let make elems () = elems

  let parse xml = Aws.Util.option_all (List.map VPC.parse (Aws.Xml.members "VPC" xml))

  let to_query v = Aws.Query.to_query_list VPC.to_query v

  let to_json v = `List (List.map VPC.to_json v)

  let of_json j = Aws.Json.to_list VPC.of_json j
end

module LinkedService = struct
  type t =
    { service_principal : String.t option
    ; description : String.t option
    }

  let make ?service_principal ?description () = { service_principal; description }

  let parse xml =
    Some
      { service_principal =
          Aws.Util.option_bind (Aws.Xml.member "ServicePrincipal" xml) String.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.service_principal (fun f ->
               Aws.Query.Pair ("ServicePrincipal", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.service_principal (fun f ->
               "ServicePrincipal", String.to_json f)
         ])

  let of_json j =
    { service_principal =
        Aws.Util.option_map (Aws.Json.lookup j "ServicePrincipal") String.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
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
      { comment = Aws.Util.option_bind (Aws.Xml.member "Comment" xml) String.parse
      ; private_zone =
          Aws.Util.option_bind (Aws.Xml.member "PrivateZone" xml) Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.private_zone (fun f ->
               Aws.Query.Pair ("PrivateZone", Boolean.to_query f))
         ; Aws.Util.option_map v.comment (fun f ->
               Aws.Query.Pair ("Comment", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.private_zone (fun f -> "PrivateZone", Boolean.to_json f)
         ; Aws.Util.option_map v.comment (fun f -> "Comment", String.to_json f)
         ])

  let of_json j =
    { comment = Aws.Util.option_map (Aws.Json.lookup j "Comment") String.of_json
    ; private_zone = Aws.Util.option_map (Aws.Json.lookup j "PrivateZone") Boolean.of_json
    }
end

module HostedZone = struct
  type t =
    { id : String.t
    ; name : String.t
    ; caller_reference : String.t
    ; config : HostedZoneConfig.t option
    ; resource_record_set_count : Long.t option
    ; linked_service : LinkedService.t option
    }

  let make
      ~id
      ~name
      ~caller_reference
      ?config
      ?resource_record_set_count
      ?linked_service
      () =
    { id; name; caller_reference; config; resource_record_set_count; linked_service }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      ; caller_reference =
          Aws.Xml.required
            "CallerReference"
            (Aws.Util.option_bind (Aws.Xml.member "CallerReference" xml) String.parse)
      ; config = Aws.Util.option_bind (Aws.Xml.member "Config" xml) HostedZoneConfig.parse
      ; resource_record_set_count =
          Aws.Util.option_bind (Aws.Xml.member "ResourceRecordSetCount" xml) Long.parse
      ; linked_service =
          Aws.Util.option_bind (Aws.Xml.member "LinkedService" xml) LinkedService.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.linked_service (fun f ->
               Aws.Query.Pair ("LinkedService", LinkedService.to_query f))
         ; Aws.Util.option_map v.resource_record_set_count (fun f ->
               Aws.Query.Pair ("ResourceRecordSetCount", Long.to_query f))
         ; Aws.Util.option_map v.config (fun f ->
               Aws.Query.Pair ("Config", HostedZoneConfig.to_query f))
         ; Some (Aws.Query.Pair ("CallerReference", String.to_query v.caller_reference))
         ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.linked_service (fun f ->
               "LinkedService", LinkedService.to_json f)
         ; Aws.Util.option_map v.resource_record_set_count (fun f ->
               "ResourceRecordSetCount", Long.to_json f)
         ; Aws.Util.option_map v.config (fun f -> "Config", HostedZoneConfig.to_json f)
         ; Some ("CallerReference", String.to_json v.caller_reference)
         ; Some ("Name", String.to_json v.name)
         ; Some ("Id", String.to_json v.id)
         ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    ; caller_reference =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CallerReference"))
    ; config = Aws.Util.option_map (Aws.Json.lookup j "Config") HostedZoneConfig.of_json
    ; resource_record_set_count =
        Aws.Util.option_map (Aws.Json.lookup j "ResourceRecordSetCount") Long.of_json
    ; linked_service =
        Aws.Util.option_map (Aws.Json.lookup j "LinkedService") LinkedService.of_json
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
          Aws.Xml.required
            "HostedZone"
            (Aws.Util.option_bind (Aws.Xml.member "HostedZone" xml) HostedZone.parse)
      ; delegation_set =
          Aws.Util.option_bind (Aws.Xml.member "DelegationSet" xml) DelegationSet.parse
      ; v_p_cs =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "VPCs" xml) VPCs.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("VPCs.member", VPCs.to_query v.v_p_cs))
         ; Aws.Util.option_map v.delegation_set (fun f ->
               Aws.Query.Pair ("DelegationSet", DelegationSet.to_query f))
         ; Some (Aws.Query.Pair ("HostedZone", HostedZone.to_query v.hosted_zone))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("VPCs", VPCs.to_json v.v_p_cs)
         ; Aws.Util.option_map v.delegation_set (fun f ->
               "DelegationSet", DelegationSet.to_json f)
         ; Some ("HostedZone", HostedZone.to_json v.hosted_zone)
         ])

  let of_json j =
    { hosted_zone =
        HostedZone.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZone"))
    ; delegation_set =
        Aws.Util.option_map (Aws.Json.lookup j "DelegationSet") DelegationSet.of_json
    ; v_p_cs = VPCs.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VPCs"))
    }
end

module ChangeAction = struct
  type t =
    | CREATE
    | DELETE
    | UPSERT

  let str_to_t = [ "UPSERT", UPSERT; "DELETE", DELETE; "CREATE", CREATE ]

  let t_to_str = [ UPSERT, "UPSERT"; DELETE, "DELETE"; CREATE, "CREATE" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
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
          Aws.Xml.required
            "Action"
            (Aws.Util.option_bind (Aws.Xml.member "Action" xml) ChangeAction.parse)
      ; resource_record_set =
          Aws.Xml.required
            "ResourceRecordSet"
            (Aws.Util.option_bind
               (Aws.Xml.member "ResourceRecordSet" xml)
               ResourceRecordSet.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("ResourceRecordSet", ResourceRecordSet.to_query v.resource_record_set))
         ; Some (Aws.Query.Pair ("Action", ChangeAction.to_query v.action))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ResourceRecordSet", ResourceRecordSet.to_json v.resource_record_set)
         ; Some ("Action", ChangeAction.to_json v.action)
         ])

  let of_json j =
    { action = ChangeAction.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Action"))
    ; resource_record_set =
        ResourceRecordSet.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceRecordSet"))
    }
end

module Changes = struct
  type t = Change.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Change.parse (Aws.Xml.members "Change" xml))

  let to_query v = Aws.Query.to_query_list Change.to_query v

  let to_json v = `List (List.map Change.to_json v)

  let of_json j = Aws.Json.to_list Change.of_json j
end

module ChangeBatch = struct
  type t =
    { comment : String.t option
    ; changes : Changes.t
    }

  let make ?comment ~changes () = { comment; changes }

  let parse xml =
    Some
      { comment = Aws.Util.option_bind (Aws.Xml.member "Comment" xml) String.parse
      ; changes =
          Aws.Xml.required
            "Changes"
            (Aws.Util.option_bind (Aws.Xml.member "Changes" xml) Changes.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Changes.member", Changes.to_query v.changes))
         ; Aws.Util.option_map v.comment (fun f ->
               Aws.Query.Pair ("Comment", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Changes", Changes.to_json v.changes)
         ; Aws.Util.option_map v.comment (fun f -> "Comment", String.to_json f)
         ])

  let of_json j =
    { comment = Aws.Util.option_map (Aws.Json.lookup j "Comment") String.of_json
    ; changes = Changes.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Changes"))
    }
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
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; change_batch =
          Aws.Xml.required
            "ChangeBatch"
            (Aws.Util.option_bind (Aws.Xml.member "ChangeBatch" xml) ChangeBatch.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("ChangeBatch", ChangeBatch.to_query v.change_batch))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.hosted_zone_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ChangeBatch", ChangeBatch.to_json v.change_batch)
         ; Some ("Id", String.to_json v.hosted_zone_id)
         ])

  let of_json j =
    { hosted_zone_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; change_batch =
        ChangeBatch.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ChangeBatch"))
    }
end

module HostedZoneNotPrivate = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module PriorRequestNotComplete = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module TrafficPolicySummary = struct
  type t =
    { id : String.t
    ; name : String.t
    ; type_ : RRType.t
    ; latest_version : Integer.t
    ; traffic_policy_count : Integer.t
    }

  let make ~id ~name ~type_ ~latest_version ~traffic_policy_count () =
    { id; name; type_; latest_version; traffic_policy_count }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      ; type_ =
          Aws.Xml.required
            "Type"
            (Aws.Util.option_bind (Aws.Xml.member "Type" xml) RRType.parse)
      ; latest_version =
          Aws.Xml.required
            "LatestVersion"
            (Aws.Util.option_bind (Aws.Xml.member "LatestVersion" xml) Integer.parse)
      ; traffic_policy_count =
          Aws.Xml.required
            "TrafficPolicyCount"
            (Aws.Util.option_bind (Aws.Xml.member "TrafficPolicyCount" xml) Integer.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("TrafficPolicyCount", Integer.to_query v.traffic_policy_count))
         ; Some (Aws.Query.Pair ("LatestVersion", Integer.to_query v.latest_version))
         ; Some (Aws.Query.Pair ("Type", RRType.to_query v.type_))
         ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("TrafficPolicyCount", Integer.to_json v.traffic_policy_count)
         ; Some ("LatestVersion", Integer.to_json v.latest_version)
         ; Some ("Type", RRType.to_json v.type_)
         ; Some ("Name", String.to_json v.name)
         ; Some ("Id", String.to_json v.id)
         ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    ; type_ = RRType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Type"))
    ; latest_version =
        Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LatestVersion"))
    ; traffic_policy_count =
        Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyCount"))
    }
end

module TrafficPolicySummaries = struct
  type t = TrafficPolicySummary.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map TrafficPolicySummary.parse (Aws.Xml.members "TrafficPolicySummary" xml))

  let to_query v = Aws.Query.to_query_list TrafficPolicySummary.to_query v

  let to_json v = `List (List.map TrafficPolicySummary.to_json v)

  let of_json j = Aws.Json.to_list TrafficPolicySummary.of_json j
end

module ListTrafficPoliciesResponse = struct
  type t =
    { traffic_policy_summaries : TrafficPolicySummaries.t
    ; is_truncated : Boolean.t
    ; traffic_policy_id_marker : String.t
    ; max_items : String.t
    }

  let make ~traffic_policy_summaries ~is_truncated ~traffic_policy_id_marker ~max_items ()
      =
    { traffic_policy_summaries; is_truncated; traffic_policy_id_marker; max_items }

  let parse xml =
    Some
      { traffic_policy_summaries =
          Aws.Xml.required
            "TrafficPolicySummaries"
            (Aws.Util.option_bind
               (Aws.Xml.member "TrafficPolicySummaries" xml)
               TrafficPolicySummaries.parse)
      ; is_truncated =
          Aws.Xml.required
            "IsTruncated"
            (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml) Boolean.parse)
      ; traffic_policy_id_marker =
          Aws.Xml.required
            "TrafficPolicyIdMarker"
            (Aws.Util.option_bind
               (Aws.Xml.member "TrafficPolicyIdMarker" xml)
               String.parse)
      ; max_items =
          Aws.Xml.required
            "MaxItems"
            (Aws.Util.option_bind (Aws.Xml.member "MaxItems" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("MaxItems", String.to_query v.max_items))
         ; Some
             (Aws.Query.Pair
                ("TrafficPolicyIdMarker", String.to_query v.traffic_policy_id_marker))
         ; Some (Aws.Query.Pair ("IsTruncated", Boolean.to_query v.is_truncated))
         ; Some
             (Aws.Query.Pair
                ( "TrafficPolicySummaries.member"
                , TrafficPolicySummaries.to_query v.traffic_policy_summaries ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("MaxItems", String.to_json v.max_items)
         ; Some ("TrafficPolicyIdMarker", String.to_json v.traffic_policy_id_marker)
         ; Some ("IsTruncated", Boolean.to_json v.is_truncated)
         ; Some
             ( "TrafficPolicySummaries"
             , TrafficPolicySummaries.to_json v.traffic_policy_summaries )
         ])

  let of_json j =
    { traffic_policy_summaries =
        TrafficPolicySummaries.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicySummaries"))
    ; is_truncated =
        Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IsTruncated"))
    ; traffic_policy_id_marker =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyIdMarker"))
    ; max_items = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxItems"))
    }
end

module LastVPCAssociation = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module DeleteHealthCheckRequest = struct
  type t = { health_check_id : String.t }

  let make ~health_check_id () = { health_check_id }

  let parse xml =
    Some
      { health_check_id =
          Aws.Xml.required
            "HealthCheckId"
            (Aws.Util.option_bind (Aws.Xml.member "HealthCheckId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("HealthCheckId", String.to_query v.health_check_id)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("HealthCheckId", String.to_json v.health_check_id) ])

  let of_json j =
    { health_check_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheckId"))
    }
end

module CreateTrafficPolicyRequest = struct
  type t =
    { name : String.t
    ; document : String.t
    ; comment : String.t option
    }

  let make ~name ~document ?comment () = { name; document; comment }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      ; document =
          Aws.Xml.required
            "Document"
            (Aws.Util.option_bind (Aws.Xml.member "Document" xml) String.parse)
      ; comment = Aws.Util.option_bind (Aws.Xml.member "Comment" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.comment (fun f ->
               Aws.Query.Pair ("Comment", String.to_query f))
         ; Some (Aws.Query.Pair ("Document", String.to_query v.document))
         ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.comment (fun f -> "Comment", String.to_json f)
         ; Some ("Document", String.to_json v.document)
         ; Some ("Name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    ; document = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Document"))
    ; comment = Aws.Util.option_map (Aws.Json.lookup j "Comment") String.of_json
    }
end

module CreateTrafficPolicyVersionResponse = struct
  type t =
    { traffic_policy : TrafficPolicy.t
    ; location : String.t
    }

  let make ~traffic_policy ~location () = { traffic_policy; location }

  let parse xml =
    Some
      { traffic_policy =
          Aws.Xml.required
            "TrafficPolicy"
            (Aws.Util.option_bind
               (Aws.Xml.member "TrafficPolicy" xml)
               TrafficPolicy.parse)
      ; location =
          Aws.Xml.required
            "Location"
            (Aws.Util.option_bind (Aws.Xml.member "Location" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Location", String.to_query v.location))
         ; Some
             (Aws.Query.Pair ("TrafficPolicy", TrafficPolicy.to_query v.traffic_policy))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Location", String.to_json v.location)
         ; Some ("TrafficPolicy", TrafficPolicy.to_json v.traffic_policy)
         ])

  let of_json j =
    { traffic_policy =
        TrafficPolicy.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicy"))
    ; location = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Location"))
    }
end

module ResettableElementName = struct
  type t =
    | FullyQualifiedDomainName
    | Regions
    | ResourcePath
    | ChildHealthChecks

  let str_to_t =
    [ "ChildHealthChecks", ChildHealthChecks
    ; "ResourcePath", ResourcePath
    ; "Regions", Regions
    ; "FullyQualifiedDomainName", FullyQualifiedDomainName
    ]

  let t_to_str =
    [ ChildHealthChecks, "ChildHealthChecks"
    ; ResourcePath, "ResourcePath"
    ; Regions, "Regions"
    ; FullyQualifiedDomainName, "FullyQualifiedDomainName"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module ReusableDelegationSetLimitType = struct
  type t = MAX_ZONES_BY_REUSABLE_DELEGATION_SET

  let str_to_t =
    [ "MAX_ZONES_BY_REUSABLE_DELEGATION_SET", MAX_ZONES_BY_REUSABLE_DELEGATION_SET ]

  let t_to_str =
    [ MAX_ZONES_BY_REUSABLE_DELEGATION_SET, "MAX_ZONES_BY_REUSABLE_DELEGATION_SET" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module ReusableDelegationSetLimit = struct
  type t =
    { type_ : ReusableDelegationSetLimitType.t
    ; value : Long.t
    }

  let make ~type_ ~value () = { type_; value }

  let parse xml =
    Some
      { type_ =
          Aws.Xml.required
            "Type"
            (Aws.Util.option_bind
               (Aws.Xml.member "Type" xml)
               ReusableDelegationSetLimitType.parse)
      ; value =
          Aws.Xml.required
            "Value"
            (Aws.Util.option_bind (Aws.Xml.member "Value" xml) Long.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Value", Long.to_query v.value))
         ; Some (Aws.Query.Pair ("Type", ReusableDelegationSetLimitType.to_query v.type_))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Value", Long.to_json v.value)
         ; Some ("Type", ReusableDelegationSetLimitType.to_json v.type_)
         ])

  let of_json j =
    { type_ =
        ReusableDelegationSetLimitType.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Type"))
    ; value = Long.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Value"))
    }
end

module GetTrafficPolicyRequest = struct
  type t =
    { id : String.t
    ; version : Integer.t
    }

  let make ~id ~version () = { id; version }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; version =
          Aws.Xml.required
            "Version"
            (Aws.Util.option_bind (Aws.Xml.member "Version" xml) Integer.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Version", Integer.to_query v.version))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Version", Integer.to_json v.version); Some ("Id", String.to_json v.id) ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; version = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Version"))
    }
end

module DeleteTrafficPolicyInstanceRequest = struct
  type t = { id : String.t }

  let make ~id () = { id }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt [ Some (Aws.Query.Pair ("Id", String.to_query v.id)) ])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [ Some ("Id", String.to_json v.id) ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id")) }
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
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      ; v_p_c = Aws.Util.option_bind (Aws.Xml.member "VPC" xml) VPC.parse
      ; caller_reference =
          Aws.Xml.required
            "CallerReference"
            (Aws.Util.option_bind (Aws.Xml.member "CallerReference" xml) String.parse)
      ; hosted_zone_config =
          Aws.Util.option_bind
            (Aws.Xml.member "HostedZoneConfig" xml)
            HostedZoneConfig.parse
      ; delegation_set_id =
          Aws.Util.option_bind (Aws.Xml.member "DelegationSetId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.delegation_set_id (fun f ->
               Aws.Query.Pair ("DelegationSetId", String.to_query f))
         ; Aws.Util.option_map v.hosted_zone_config (fun f ->
               Aws.Query.Pair ("HostedZoneConfig", HostedZoneConfig.to_query f))
         ; Some (Aws.Query.Pair ("CallerReference", String.to_query v.caller_reference))
         ; Aws.Util.option_map v.v_p_c (fun f -> Aws.Query.Pair ("VPC", VPC.to_query f))
         ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.delegation_set_id (fun f ->
               "DelegationSetId", String.to_json f)
         ; Aws.Util.option_map v.hosted_zone_config (fun f ->
               "HostedZoneConfig", HostedZoneConfig.to_json f)
         ; Some ("CallerReference", String.to_json v.caller_reference)
         ; Aws.Util.option_map v.v_p_c (fun f -> "VPC", VPC.to_json f)
         ; Some ("Name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    ; v_p_c = Aws.Util.option_map (Aws.Json.lookup j "VPC") VPC.of_json
    ; caller_reference =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CallerReference"))
    ; hosted_zone_config =
        Aws.Util.option_map
          (Aws.Json.lookup j "HostedZoneConfig")
          HostedZoneConfig.of_json
    ; delegation_set_id =
        Aws.Util.option_map (Aws.Json.lookup j "DelegationSetId") String.of_json
    }
end

module InsufficientDataHealthStatus = struct
  type t =
    | Healthy
    | Unhealthy
    | LastKnownStatus

  let str_to_t =
    [ "LastKnownStatus", LastKnownStatus; "Unhealthy", Unhealthy; "Healthy", Healthy ]

  let t_to_str =
    [ LastKnownStatus, "LastKnownStatus"; Unhealthy, "Unhealthy"; Healthy, "Healthy" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module HealthCheckType = struct
  type t =
    | HTTP
    | HTTPS
    | HTTP_STR_MATCH
    | HTTPS_STR_MATCH
    | TCP
    | CALCULATED
    | CLOUDWATCH_METRIC

  let str_to_t =
    [ "CLOUDWATCH_METRIC", CLOUDWATCH_METRIC
    ; "CALCULATED", CALCULATED
    ; "TCP", TCP
    ; "HTTPS_STR_MATCH", HTTPS_STR_MATCH
    ; "HTTP_STR_MATCH", HTTP_STR_MATCH
    ; "HTTPS", HTTPS
    ; "HTTP", HTTP
    ]

  let t_to_str =
    [ CLOUDWATCH_METRIC, "CLOUDWATCH_METRIC"
    ; CALCULATED, "CALCULATED"
    ; TCP, "TCP"
    ; HTTPS_STR_MATCH, "HTTPS_STR_MATCH"
    ; HTTP_STR_MATCH, "HTTP_STR_MATCH"
    ; HTTPS, "HTTPS"
    ; HTTP, "HTTP"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module HealthCheckRegion = struct
  type t =
    | Us_east_1
    | Us_west_1
    | Us_west_2
    | Eu_west_1
    | Ap_southeast_1
    | Ap_southeast_2
    | Ap_northeast_1
    | Sa_east_1

  let str_to_t =
    [ "sa-east-1", Sa_east_1
    ; "ap-northeast-1", Ap_northeast_1
    ; "ap-southeast-2", Ap_southeast_2
    ; "ap-southeast-1", Ap_southeast_1
    ; "eu-west-1", Eu_west_1
    ; "us-west-2", Us_west_2
    ; "us-west-1", Us_west_1
    ; "us-east-1", Us_east_1
    ]

  let t_to_str =
    [ Sa_east_1, "sa-east-1"
    ; Ap_northeast_1, "ap-northeast-1"
    ; Ap_southeast_2, "ap-southeast-2"
    ; Ap_southeast_1, "ap-southeast-1"
    ; Eu_west_1, "eu-west-1"
    ; Us_west_2, "us-west-2"
    ; Us_west_1, "us-west-1"
    ; Us_east_1, "us-east-1"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module HealthCheckRegionList = struct
  type t = HealthCheckRegion.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map HealthCheckRegion.parse (Aws.Xml.members "Region" xml))

  let to_query v = Aws.Query.to_query_list HealthCheckRegion.to_query v

  let to_json v = `List (List.map HealthCheckRegion.to_json v)

  let of_json j = Aws.Json.to_list HealthCheckRegion.of_json j
end

module ChildHealthCheckList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "ChildHealthCheck" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module CloudWatchRegion = struct
  type t =
    | Us_east_1
    | Us_east_2
    | Us_west_1
    | Us_west_2
    | Ca_central_1
    | Eu_central_1
    | Eu_west_1
    | Eu_west_2
    | Eu_west_3
    | Ap_east_1
    | Me_south_1
    | Ap_south_1
    | Ap_southeast_1
    | Ap_southeast_2
    | Ap_northeast_1
    | Ap_northeast_2
    | Ap_northeast_3
    | Eu_north_1
    | Sa_east_1
    | Cn_northwest_1
    | Cn_north_1
    | Af_south_1
    | Eu_south_1
    | Us_gov_west_1
    | Us_gov_east_1
    | Us_iso_east_1
    | Us_isob_east_1

  let str_to_t =
    [ "us-isob-east-1", Us_isob_east_1
    ; "us-iso-east-1", Us_iso_east_1
    ; "us-gov-east-1", Us_gov_east_1
    ; "us-gov-west-1", Us_gov_west_1
    ; "eu-south-1", Eu_south_1
    ; "af-south-1", Af_south_1
    ; "cn-north-1", Cn_north_1
    ; "cn-northwest-1", Cn_northwest_1
    ; "sa-east-1", Sa_east_1
    ; "eu-north-1", Eu_north_1
    ; "ap-northeast-3", Ap_northeast_3
    ; "ap-northeast-2", Ap_northeast_2
    ; "ap-northeast-1", Ap_northeast_1
    ; "ap-southeast-2", Ap_southeast_2
    ; "ap-southeast-1", Ap_southeast_1
    ; "ap-south-1", Ap_south_1
    ; "me-south-1", Me_south_1
    ; "ap-east-1", Ap_east_1
    ; "eu-west-3", Eu_west_3
    ; "eu-west-2", Eu_west_2
    ; "eu-west-1", Eu_west_1
    ; "eu-central-1", Eu_central_1
    ; "ca-central-1", Ca_central_1
    ; "us-west-2", Us_west_2
    ; "us-west-1", Us_west_1
    ; "us-east-2", Us_east_2
    ; "us-east-1", Us_east_1
    ]

  let t_to_str =
    [ Us_isob_east_1, "us-isob-east-1"
    ; Us_iso_east_1, "us-iso-east-1"
    ; Us_gov_east_1, "us-gov-east-1"
    ; Us_gov_west_1, "us-gov-west-1"
    ; Eu_south_1, "eu-south-1"
    ; Af_south_1, "af-south-1"
    ; Cn_north_1, "cn-north-1"
    ; Cn_northwest_1, "cn-northwest-1"
    ; Sa_east_1, "sa-east-1"
    ; Eu_north_1, "eu-north-1"
    ; Ap_northeast_3, "ap-northeast-3"
    ; Ap_northeast_2, "ap-northeast-2"
    ; Ap_northeast_1, "ap-northeast-1"
    ; Ap_southeast_2, "ap-southeast-2"
    ; Ap_southeast_1, "ap-southeast-1"
    ; Ap_south_1, "ap-south-1"
    ; Me_south_1, "me-south-1"
    ; Ap_east_1, "ap-east-1"
    ; Eu_west_3, "eu-west-3"
    ; Eu_west_2, "eu-west-2"
    ; Eu_west_1, "eu-west-1"
    ; Eu_central_1, "eu-central-1"
    ; Ca_central_1, "ca-central-1"
    ; Us_west_2, "us-west-2"
    ; Us_west_1, "us-west-1"
    ; Us_east_2, "us-east-2"
    ; Us_east_1, "us-east-1"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module AlarmIdentifier = struct
  type t =
    { region : CloudWatchRegion.t
    ; name : String.t
    }

  let make ~region ~name () = { region; name }

  let parse xml =
    Some
      { region =
          Aws.Xml.required
            "Region"
            (Aws.Util.option_bind (Aws.Xml.member "Region" xml) CloudWatchRegion.parse)
      ; name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Name", String.to_query v.name))
         ; Some (Aws.Query.Pair ("Region", CloudWatchRegion.to_query v.region))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Name", String.to_json v.name)
         ; Some ("Region", CloudWatchRegion.to_json v.region)
         ])

  let of_json j =
    { region =
        CloudWatchRegion.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Region"))
    ; name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
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
    ; measure_latency : Boolean.t option
    ; inverted : Boolean.t option
    ; disabled : Boolean.t option
    ; health_threshold : Integer.t option
    ; child_health_checks : ChildHealthCheckList.t
    ; enable_s_n_i : Boolean.t option
    ; regions : HealthCheckRegionList.t
    ; alarm_identifier : AlarmIdentifier.t option
    ; insufficient_data_health_status : InsufficientDataHealthStatus.t option
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
      ?measure_latency
      ?inverted
      ?disabled
      ?health_threshold
      ?(child_health_checks = [])
      ?enable_s_n_i
      ?(regions = [])
      ?alarm_identifier
      ?insufficient_data_health_status
      () =
    { i_p_address
    ; port
    ; type_
    ; resource_path
    ; fully_qualified_domain_name
    ; search_string
    ; request_interval
    ; failure_threshold
    ; measure_latency
    ; inverted
    ; disabled
    ; health_threshold
    ; child_health_checks
    ; enable_s_n_i
    ; regions
    ; alarm_identifier
    ; insufficient_data_health_status
    }

  let parse xml =
    Some
      { i_p_address = Aws.Util.option_bind (Aws.Xml.member "IPAddress" xml) String.parse
      ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
      ; type_ =
          Aws.Xml.required
            "Type"
            (Aws.Util.option_bind (Aws.Xml.member "Type" xml) HealthCheckType.parse)
      ; resource_path =
          Aws.Util.option_bind (Aws.Xml.member "ResourcePath" xml) String.parse
      ; fully_qualified_domain_name =
          Aws.Util.option_bind
            (Aws.Xml.member "FullyQualifiedDomainName" xml)
            String.parse
      ; search_string =
          Aws.Util.option_bind (Aws.Xml.member "SearchString" xml) String.parse
      ; request_interval =
          Aws.Util.option_bind (Aws.Xml.member "RequestInterval" xml) Integer.parse
      ; failure_threshold =
          Aws.Util.option_bind (Aws.Xml.member "FailureThreshold" xml) Integer.parse
      ; measure_latency =
          Aws.Util.option_bind (Aws.Xml.member "MeasureLatency" xml) Boolean.parse
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
          Aws.Util.option_bind
            (Aws.Xml.member "AlarmIdentifier" xml)
            AlarmIdentifier.parse
      ; insufficient_data_health_status =
          Aws.Util.option_bind
            (Aws.Xml.member "InsufficientDataHealthStatus" xml)
            InsufficientDataHealthStatus.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.insufficient_data_health_status (fun f ->
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
         ; Aws.Util.option_map v.measure_latency (fun f ->
               Aws.Query.Pair ("MeasureLatency", Boolean.to_query f))
         ; Aws.Util.option_map v.failure_threshold (fun f ->
               Aws.Query.Pair ("FailureThreshold", Integer.to_query f))
         ; Aws.Util.option_map v.request_interval (fun f ->
               Aws.Query.Pair ("RequestInterval", Integer.to_query f))
         ; Aws.Util.option_map v.search_string (fun f ->
               Aws.Query.Pair ("SearchString", String.to_query f))
         ; Aws.Util.option_map v.fully_qualified_domain_name (fun f ->
               Aws.Query.Pair ("FullyQualifiedDomainName", String.to_query f))
         ; Aws.Util.option_map v.resource_path (fun f ->
               Aws.Query.Pair ("ResourcePath", String.to_query f))
         ; Some (Aws.Query.Pair ("Type", HealthCheckType.to_query v.type_))
         ; Aws.Util.option_map v.port (fun f ->
               Aws.Query.Pair ("Port", Integer.to_query f))
         ; Aws.Util.option_map v.i_p_address (fun f ->
               Aws.Query.Pair ("IPAddress", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.insufficient_data_health_status (fun f ->
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
         ; Aws.Util.option_map v.measure_latency (fun f ->
               "MeasureLatency", Boolean.to_json f)
         ; Aws.Util.option_map v.failure_threshold (fun f ->
               "FailureThreshold", Integer.to_json f)
         ; Aws.Util.option_map v.request_interval (fun f ->
               "RequestInterval", Integer.to_json f)
         ; Aws.Util.option_map v.search_string (fun f -> "SearchString", String.to_json f)
         ; Aws.Util.option_map v.fully_qualified_domain_name (fun f ->
               "FullyQualifiedDomainName", String.to_json f)
         ; Aws.Util.option_map v.resource_path (fun f -> "ResourcePath", String.to_json f)
         ; Some ("Type", HealthCheckType.to_json v.type_)
         ; Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
         ; Aws.Util.option_map v.i_p_address (fun f -> "IPAddress", String.to_json f)
         ])

  let of_json j =
    { i_p_address = Aws.Util.option_map (Aws.Json.lookup j "IPAddress") String.of_json
    ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
    ; type_ = HealthCheckType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Type"))
    ; resource_path =
        Aws.Util.option_map (Aws.Json.lookup j "ResourcePath") String.of_json
    ; fully_qualified_domain_name =
        Aws.Util.option_map (Aws.Json.lookup j "FullyQualifiedDomainName") String.of_json
    ; search_string =
        Aws.Util.option_map (Aws.Json.lookup j "SearchString") String.of_json
    ; request_interval =
        Aws.Util.option_map (Aws.Json.lookup j "RequestInterval") Integer.of_json
    ; failure_threshold =
        Aws.Util.option_map (Aws.Json.lookup j "FailureThreshold") Integer.of_json
    ; measure_latency =
        Aws.Util.option_map (Aws.Json.lookup j "MeasureLatency") Boolean.of_json
    ; inverted = Aws.Util.option_map (Aws.Json.lookup j "Inverted") Boolean.of_json
    ; disabled = Aws.Util.option_map (Aws.Json.lookup j "Disabled") Boolean.of_json
    ; health_threshold =
        Aws.Util.option_map (Aws.Json.lookup j "HealthThreshold") Integer.of_json
    ; child_health_checks =
        ChildHealthCheckList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ChildHealthChecks"))
    ; enable_s_n_i = Aws.Util.option_map (Aws.Json.lookup j "EnableSNI") Boolean.of_json
    ; regions =
        HealthCheckRegionList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Regions"))
    ; alarm_identifier =
        Aws.Util.option_map (Aws.Json.lookup j "AlarmIdentifier") AlarmIdentifier.of_json
    ; insufficient_data_health_status =
        Aws.Util.option_map
          (Aws.Json.lookup j "InsufficientDataHealthStatus")
          InsufficientDataHealthStatus.of_json
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
          Aws.Xml.required
            "CallerReference"
            (Aws.Util.option_bind (Aws.Xml.member "CallerReference" xml) String.parse)
      ; health_check_config =
          Aws.Xml.required
            "HealthCheckConfig"
            (Aws.Util.option_bind
               (Aws.Xml.member "HealthCheckConfig" xml)
               HealthCheckConfig.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("HealthCheckConfig", HealthCheckConfig.to_query v.health_check_config))
         ; Some (Aws.Query.Pair ("CallerReference", String.to_query v.caller_reference))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("HealthCheckConfig", HealthCheckConfig.to_json v.health_check_config)
         ; Some ("CallerReference", String.to_json v.caller_reference)
         ])

  let of_json j =
    { caller_reference =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CallerReference"))
    ; health_check_config =
        HealthCheckConfig.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheckConfig"))
    }
end

module UpdateTrafficPolicyCommentResponse = struct
  type t = { traffic_policy : TrafficPolicy.t }

  let make ~traffic_policy () = { traffic_policy }

  let parse xml =
    Some
      { traffic_policy =
          Aws.Xml.required
            "TrafficPolicy"
            (Aws.Util.option_bind
               (Aws.Xml.member "TrafficPolicy" xml)
               TrafficPolicy.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("TrafficPolicy", TrafficPolicy.to_query v.traffic_policy))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("TrafficPolicy", TrafficPolicy.to_json v.traffic_policy) ])

  let of_json j =
    { traffic_policy =
        TrafficPolicy.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicy"))
    }
end

module TrafficPolicyInUse = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module NoSuchQueryLoggingConfig = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module Statistic = struct
  type t =
    | Average
    | Sum
    | SampleCount
    | Maximum
    | Minimum

  let str_to_t =
    [ "Minimum", Minimum
    ; "Maximum", Maximum
    ; "SampleCount", SampleCount
    ; "Sum", Sum
    ; "Average", Average
    ]

  let t_to_str =
    [ Minimum, "Minimum"
    ; Maximum, "Maximum"
    ; SampleCount, "SampleCount"
    ; Sum, "Sum"
    ; Average, "Average"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module Dimension = struct
  type t =
    { name : String.t
    ; value : String.t
    }

  let make ~name ~value () = { name; value }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      ; value =
          Aws.Xml.required
            "Value"
            (Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Value", String.to_query v.value))
         ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Value", String.to_json v.value); Some ("Name", String.to_json v.name) ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    ; value = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Value"))
    }
end

module DimensionList = struct
  type t = Dimension.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Dimension.parse (Aws.Xml.members "Dimension" xml))

  let to_query v = Aws.Query.to_query_list Dimension.to_query v

  let to_json v = `List (List.map Dimension.to_json v)

  let of_json j = Aws.Json.to_list Dimension.of_json j
end

module ComparisonOperator = struct
  type t =
    | GreaterThanOrEqualToThreshold
    | GreaterThanThreshold
    | LessThanThreshold
    | LessThanOrEqualToThreshold

  let str_to_t =
    [ "LessThanOrEqualToThreshold", LessThanOrEqualToThreshold
    ; "LessThanThreshold", LessThanThreshold
    ; "GreaterThanThreshold", GreaterThanThreshold
    ; "GreaterThanOrEqualToThreshold", GreaterThanOrEqualToThreshold
    ]

  let t_to_str =
    [ LessThanOrEqualToThreshold, "LessThanOrEqualToThreshold"
    ; LessThanThreshold, "LessThanThreshold"
    ; GreaterThanThreshold, "GreaterThanThreshold"
    ; GreaterThanOrEqualToThreshold, "GreaterThanOrEqualToThreshold"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module CloudWatchAlarmConfiguration = struct
  type t =
    { evaluation_periods : Integer.t
    ; threshold : Double.t
    ; comparison_operator : ComparisonOperator.t
    ; period : Integer.t
    ; metric_name : String.t
    ; namespace : String.t
    ; statistic : Statistic.t
    ; dimensions : DimensionList.t
    }

  let make
      ~evaluation_periods
      ~threshold
      ~comparison_operator
      ~period
      ~metric_name
      ~namespace
      ~statistic
      ?(dimensions = [])
      () =
    { evaluation_periods
    ; threshold
    ; comparison_operator
    ; period
    ; metric_name
    ; namespace
    ; statistic
    ; dimensions
    }

  let parse xml =
    Some
      { evaluation_periods =
          Aws.Xml.required
            "EvaluationPeriods"
            (Aws.Util.option_bind (Aws.Xml.member "EvaluationPeriods" xml) Integer.parse)
      ; threshold =
          Aws.Xml.required
            "Threshold"
            (Aws.Util.option_bind (Aws.Xml.member "Threshold" xml) Double.parse)
      ; comparison_operator =
          Aws.Xml.required
            "ComparisonOperator"
            (Aws.Util.option_bind
               (Aws.Xml.member "ComparisonOperator" xml)
               ComparisonOperator.parse)
      ; period =
          Aws.Xml.required
            "Period"
            (Aws.Util.option_bind (Aws.Xml.member "Period" xml) Integer.parse)
      ; metric_name =
          Aws.Xml.required
            "MetricName"
            (Aws.Util.option_bind (Aws.Xml.member "MetricName" xml) String.parse)
      ; namespace =
          Aws.Xml.required
            "Namespace"
            (Aws.Util.option_bind (Aws.Xml.member "Namespace" xml) String.parse)
      ; statistic =
          Aws.Xml.required
            "Statistic"
            (Aws.Util.option_bind (Aws.Xml.member "Statistic" xml) Statistic.parse)
      ; dimensions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Dimensions" xml) DimensionList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("Dimensions.member", DimensionList.to_query v.dimensions))
         ; Some (Aws.Query.Pair ("Statistic", Statistic.to_query v.statistic))
         ; Some (Aws.Query.Pair ("Namespace", String.to_query v.namespace))
         ; Some (Aws.Query.Pair ("MetricName", String.to_query v.metric_name))
         ; Some (Aws.Query.Pair ("Period", Integer.to_query v.period))
         ; Some
             (Aws.Query.Pair
                ("ComparisonOperator", ComparisonOperator.to_query v.comparison_operator))
         ; Some (Aws.Query.Pair ("Threshold", Double.to_query v.threshold))
         ; Some
             (Aws.Query.Pair ("EvaluationPeriods", Integer.to_query v.evaluation_periods))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Dimensions", DimensionList.to_json v.dimensions)
         ; Some ("Statistic", Statistic.to_json v.statistic)
         ; Some ("Namespace", String.to_json v.namespace)
         ; Some ("MetricName", String.to_json v.metric_name)
         ; Some ("Period", Integer.to_json v.period)
         ; Some ("ComparisonOperator", ComparisonOperator.to_json v.comparison_operator)
         ; Some ("Threshold", Double.to_json v.threshold)
         ; Some ("EvaluationPeriods", Integer.to_json v.evaluation_periods)
         ])

  let of_json j =
    { evaluation_periods =
        Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EvaluationPeriods"))
    ; threshold = Double.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Threshold"))
    ; comparison_operator =
        ComparisonOperator.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ComparisonOperator"))
    ; period = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Period"))
    ; metric_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricName"))
    ; namespace = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Namespace"))
    ; statistic =
        Statistic.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Statistic"))
    ; dimensions =
        DimensionList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Dimensions"))
    }
end

module HealthCheck = struct
  type t =
    { id : String.t
    ; caller_reference : String.t
    ; linked_service : LinkedService.t option
    ; health_check_config : HealthCheckConfig.t
    ; health_check_version : Long.t
    ; cloud_watch_alarm_configuration : CloudWatchAlarmConfiguration.t option
    }

  let make
      ~id
      ~caller_reference
      ?linked_service
      ~health_check_config
      ~health_check_version
      ?cloud_watch_alarm_configuration
      () =
    { id
    ; caller_reference
    ; linked_service
    ; health_check_config
    ; health_check_version
    ; cloud_watch_alarm_configuration
    }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; caller_reference =
          Aws.Xml.required
            "CallerReference"
            (Aws.Util.option_bind (Aws.Xml.member "CallerReference" xml) String.parse)
      ; linked_service =
          Aws.Util.option_bind (Aws.Xml.member "LinkedService" xml) LinkedService.parse
      ; health_check_config =
          Aws.Xml.required
            "HealthCheckConfig"
            (Aws.Util.option_bind
               (Aws.Xml.member "HealthCheckConfig" xml)
               HealthCheckConfig.parse)
      ; health_check_version =
          Aws.Xml.required
            "HealthCheckVersion"
            (Aws.Util.option_bind (Aws.Xml.member "HealthCheckVersion" xml) Long.parse)
      ; cloud_watch_alarm_configuration =
          Aws.Util.option_bind
            (Aws.Xml.member "CloudWatchAlarmConfiguration" xml)
            CloudWatchAlarmConfiguration.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cloud_watch_alarm_configuration (fun f ->
               Aws.Query.Pair
                 ("CloudWatchAlarmConfiguration", CloudWatchAlarmConfiguration.to_query f))
         ; Some
             (Aws.Query.Pair ("HealthCheckVersion", Long.to_query v.health_check_version))
         ; Some
             (Aws.Query.Pair
                ("HealthCheckConfig", HealthCheckConfig.to_query v.health_check_config))
         ; Aws.Util.option_map v.linked_service (fun f ->
               Aws.Query.Pair ("LinkedService", LinkedService.to_query f))
         ; Some (Aws.Query.Pair ("CallerReference", String.to_query v.caller_reference))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cloud_watch_alarm_configuration (fun f ->
               "CloudWatchAlarmConfiguration", CloudWatchAlarmConfiguration.to_json f)
         ; Some ("HealthCheckVersion", Long.to_json v.health_check_version)
         ; Some ("HealthCheckConfig", HealthCheckConfig.to_json v.health_check_config)
         ; Aws.Util.option_map v.linked_service (fun f ->
               "LinkedService", LinkedService.to_json f)
         ; Some ("CallerReference", String.to_json v.caller_reference)
         ; Some ("Id", String.to_json v.id)
         ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; caller_reference =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CallerReference"))
    ; linked_service =
        Aws.Util.option_map (Aws.Json.lookup j "LinkedService") LinkedService.of_json
    ; health_check_config =
        HealthCheckConfig.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheckConfig"))
    ; health_check_version =
        Long.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheckVersion"))
    ; cloud_watch_alarm_configuration =
        Aws.Util.option_map
          (Aws.Json.lookup j "CloudWatchAlarmConfiguration")
          CloudWatchAlarmConfiguration.of_json
    }
end

module UpdateHostedZoneCommentResponse = struct
  type t = { hosted_zone : HostedZone.t }

  let make ~hosted_zone () = { hosted_zone }

  let parse xml =
    Some
      { hosted_zone =
          Aws.Xml.required
            "HostedZone"
            (Aws.Util.option_bind (Aws.Xml.member "HostedZone" xml) HostedZone.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("HostedZone", HostedZone.to_query v.hosted_zone)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("HostedZone", HostedZone.to_json v.hosted_zone) ])

  let of_json j =
    { hosted_zone =
        HostedZone.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZone"))
    }
end

module HostedZoneLimitType = struct
  type t =
    | MAX_RRSETS_BY_ZONE
    | MAX_VPCS_ASSOCIATED_BY_ZONE

  let str_to_t =
    [ "MAX_VPCS_ASSOCIATED_BY_ZONE", MAX_VPCS_ASSOCIATED_BY_ZONE
    ; "MAX_RRSETS_BY_ZONE", MAX_RRSETS_BY_ZONE
    ]

  let t_to_str =
    [ MAX_VPCS_ASSOCIATED_BY_ZONE, "MAX_VPCS_ASSOCIATED_BY_ZONE"
    ; MAX_RRSETS_BY_ZONE, "MAX_RRSETS_BY_ZONE"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module DeleteHealthCheckResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
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
          Aws.Xml.required
            "DelegationSets"
            (Aws.Util.option_bind
               (Aws.Xml.member "DelegationSets" xml)
               DelegationSets.parse)
      ; marker =
          Aws.Xml.required
            "Marker"
            (Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse)
      ; is_truncated =
          Aws.Xml.required
            "IsTruncated"
            (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml) Boolean.parse)
      ; next_marker = Aws.Util.option_bind (Aws.Xml.member "NextMarker" xml) String.parse
      ; max_items =
          Aws.Xml.required
            "MaxItems"
            (Aws.Util.option_bind (Aws.Xml.member "MaxItems" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("MaxItems", String.to_query v.max_items))
         ; Aws.Util.option_map v.next_marker (fun f ->
               Aws.Query.Pair ("NextMarker", String.to_query f))
         ; Some (Aws.Query.Pair ("IsTruncated", Boolean.to_query v.is_truncated))
         ; Some (Aws.Query.Pair ("Marker", String.to_query v.marker))
         ; Some
             (Aws.Query.Pair
                ("DelegationSets.member", DelegationSets.to_query v.delegation_sets))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("MaxItems", String.to_json v.max_items)
         ; Aws.Util.option_map v.next_marker (fun f -> "NextMarker", String.to_json f)
         ; Some ("IsTruncated", Boolean.to_json v.is_truncated)
         ; Some ("Marker", String.to_json v.marker)
         ; Some ("DelegationSets", DelegationSets.to_json v.delegation_sets)
         ])

  let of_json j =
    { delegation_sets =
        DelegationSets.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "DelegationSets"))
    ; marker = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Marker"))
    ; is_truncated =
        Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IsTruncated"))
    ; next_marker = Aws.Util.option_map (Aws.Json.lookup j "NextMarker") String.of_json
    ; max_items = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxItems"))
    }
end

module InvalidDomainName = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module HealthCheckVersionMismatch = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module GetReusableDelegationSetLimitResponse = struct
  type t =
    { limit : ReusableDelegationSetLimit.t
    ; count : Long.t
    }

  let make ~limit ~count () = { limit; count }

  let parse xml =
    Some
      { limit =
          Aws.Xml.required
            "Limit"
            (Aws.Util.option_bind
               (Aws.Xml.member "Limit" xml)
               ReusableDelegationSetLimit.parse)
      ; count =
          Aws.Xml.required
            "Count"
            (Aws.Util.option_bind (Aws.Xml.member "Count" xml) Long.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Count", Long.to_query v.count))
         ; Some (Aws.Query.Pair ("Limit", ReusableDelegationSetLimit.to_query v.limit))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Count", Long.to_json v.count)
         ; Some ("Limit", ReusableDelegationSetLimit.to_json v.limit)
         ])

  let of_json j =
    { limit =
        ReusableDelegationSetLimit.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Limit"))
    ; count = Long.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Count"))
    }
end

module DelegationSetNotReusable = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module NoSuchTrafficPolicy = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module NoSuchHealthCheck = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module ListTrafficPolicyInstancesByHostedZoneResponse = struct
  type t =
    { traffic_policy_instances : TrafficPolicyInstances.t
    ; traffic_policy_instance_name_marker : String.t option
    ; traffic_policy_instance_type_marker : RRType.t option
    ; is_truncated : Boolean.t
    ; max_items : String.t
    }

  let make
      ~traffic_policy_instances
      ?traffic_policy_instance_name_marker
      ?traffic_policy_instance_type_marker
      ~is_truncated
      ~max_items
      () =
    { traffic_policy_instances
    ; traffic_policy_instance_name_marker
    ; traffic_policy_instance_type_marker
    ; is_truncated
    ; max_items
    }

  let parse xml =
    Some
      { traffic_policy_instances =
          Aws.Xml.required
            "TrafficPolicyInstances"
            (Aws.Util.option_bind
               (Aws.Xml.member "TrafficPolicyInstances" xml)
               TrafficPolicyInstances.parse)
      ; traffic_policy_instance_name_marker =
          Aws.Util.option_bind
            (Aws.Xml.member "TrafficPolicyInstanceNameMarker" xml)
            String.parse
      ; traffic_policy_instance_type_marker =
          Aws.Util.option_bind
            (Aws.Xml.member "TrafficPolicyInstanceTypeMarker" xml)
            RRType.parse
      ; is_truncated =
          Aws.Xml.required
            "IsTruncated"
            (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml) Boolean.parse)
      ; max_items =
          Aws.Xml.required
            "MaxItems"
            (Aws.Util.option_bind (Aws.Xml.member "MaxItems" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("MaxItems", String.to_query v.max_items))
         ; Some (Aws.Query.Pair ("IsTruncated", Boolean.to_query v.is_truncated))
         ; Aws.Util.option_map v.traffic_policy_instance_type_marker (fun f ->
               Aws.Query.Pair ("TrafficPolicyInstanceTypeMarker", RRType.to_query f))
         ; Aws.Util.option_map v.traffic_policy_instance_name_marker (fun f ->
               Aws.Query.Pair ("TrafficPolicyInstanceNameMarker", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "TrafficPolicyInstances.member"
                , TrafficPolicyInstances.to_query v.traffic_policy_instances ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("MaxItems", String.to_json v.max_items)
         ; Some ("IsTruncated", Boolean.to_json v.is_truncated)
         ; Aws.Util.option_map v.traffic_policy_instance_type_marker (fun f ->
               "TrafficPolicyInstanceTypeMarker", RRType.to_json f)
         ; Aws.Util.option_map v.traffic_policy_instance_name_marker (fun f ->
               "TrafficPolicyInstanceNameMarker", String.to_json f)
         ; Some
             ( "TrafficPolicyInstances"
             , TrafficPolicyInstances.to_json v.traffic_policy_instances )
         ])

  let of_json j =
    { traffic_policy_instances =
        TrafficPolicyInstances.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyInstances"))
    ; traffic_policy_instance_name_marker =
        Aws.Util.option_map
          (Aws.Json.lookup j "TrafficPolicyInstanceNameMarker")
          String.of_json
    ; traffic_policy_instance_type_marker =
        Aws.Util.option_map
          (Aws.Json.lookup j "TrafficPolicyInstanceTypeMarker")
          RRType.of_json
    ; is_truncated =
        Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IsTruncated"))
    ; max_items = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxItems"))
    }
end

module DeleteTrafficPolicyResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ChangeStatus = struct
  type t =
    | PENDING
    | INSYNC

  let str_to_t = [ "INSYNC", INSYNC; "PENDING", PENDING ]

  let t_to_str = [ INSYNC, "INSYNC"; PENDING, "PENDING" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
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
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; status =
          Aws.Xml.required
            "Status"
            (Aws.Util.option_bind (Aws.Xml.member "Status" xml) ChangeStatus.parse)
      ; submitted_at =
          Aws.Xml.required
            "SubmittedAt"
            (Aws.Util.option_bind (Aws.Xml.member "SubmittedAt" xml) DateTime.parse)
      ; comment = Aws.Util.option_bind (Aws.Xml.member "Comment" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.comment (fun f ->
               Aws.Query.Pair ("Comment", String.to_query f))
         ; Some (Aws.Query.Pair ("SubmittedAt", DateTime.to_query v.submitted_at))
         ; Some (Aws.Query.Pair ("Status", ChangeStatus.to_query v.status))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.comment (fun f -> "Comment", String.to_json f)
         ; Some ("SubmittedAt", DateTime.to_json v.submitted_at)
         ; Some ("Status", ChangeStatus.to_json v.status)
         ; Some ("Id", String.to_json v.id)
         ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; status = ChangeStatus.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Status"))
    ; submitted_at =
        DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SubmittedAt"))
    ; comment = Aws.Util.option_map (Aws.Json.lookup j "Comment") String.of_json
    }
end

module DeleteHostedZoneResponse = struct
  type t = { change_info : ChangeInfo.t }

  let make ~change_info () = { change_info }

  let parse xml =
    Some
      { change_info =
          Aws.Xml.required
            "ChangeInfo"
            (Aws.Util.option_bind (Aws.Xml.member "ChangeInfo" xml) ChangeInfo.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("ChangeInfo", ChangeInfo.to_query v.change_info)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("ChangeInfo", ChangeInfo.to_json v.change_info) ])

  let of_json j =
    { change_info =
        ChangeInfo.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ChangeInfo"))
    }
end

module TrafficPolicyInstanceAlreadyExists = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module GetReusableDelegationSetRequest = struct
  type t = { id : String.t }

  let make ~id () = { id }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt [ Some (Aws.Query.Pair ("Id", String.to_query v.id)) ])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [ Some ("Id", String.to_json v.id) ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id")) }
end

module ListTrafficPolicyInstancesRequest = struct
  type t =
    { hosted_zone_id_marker : String.t option
    ; traffic_policy_instance_name_marker : String.t option
    ; traffic_policy_instance_type_marker : RRType.t option
    ; max_items : String.t option
    }

  let make
      ?hosted_zone_id_marker
      ?traffic_policy_instance_name_marker
      ?traffic_policy_instance_type_marker
      ?max_items
      () =
    { hosted_zone_id_marker
    ; traffic_policy_instance_name_marker
    ; traffic_policy_instance_type_marker
    ; max_items
    }

  let parse xml =
    Some
      { hosted_zone_id_marker =
          Aws.Util.option_bind (Aws.Xml.member "hostedzoneid" xml) String.parse
      ; traffic_policy_instance_name_marker =
          Aws.Util.option_bind
            (Aws.Xml.member "trafficpolicyinstancename" xml)
            String.parse
      ; traffic_policy_instance_type_marker =
          Aws.Util.option_bind
            (Aws.Xml.member "trafficpolicyinstancetype" xml)
            RRType.parse
      ; max_items = Aws.Util.option_bind (Aws.Xml.member "maxitems" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_items (fun f ->
               Aws.Query.Pair ("maxitems", String.to_query f))
         ; Aws.Util.option_map v.traffic_policy_instance_type_marker (fun f ->
               Aws.Query.Pair ("trafficpolicyinstancetype", RRType.to_query f))
         ; Aws.Util.option_map v.traffic_policy_instance_name_marker (fun f ->
               Aws.Query.Pair ("trafficpolicyinstancename", String.to_query f))
         ; Aws.Util.option_map v.hosted_zone_id_marker (fun f ->
               Aws.Query.Pair ("hostedzoneid", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_items (fun f -> "maxitems", String.to_json f)
         ; Aws.Util.option_map v.traffic_policy_instance_type_marker (fun f ->
               "trafficpolicyinstancetype", RRType.to_json f)
         ; Aws.Util.option_map v.traffic_policy_instance_name_marker (fun f ->
               "trafficpolicyinstancename", String.to_json f)
         ; Aws.Util.option_map v.hosted_zone_id_marker (fun f ->
               "hostedzoneid", String.to_json f)
         ])

  let of_json j =
    { hosted_zone_id_marker =
        Aws.Util.option_map (Aws.Json.lookup j "hostedzoneid") String.of_json
    ; traffic_policy_instance_name_marker =
        Aws.Util.option_map (Aws.Json.lookup j "trafficpolicyinstancename") String.of_json
    ; traffic_policy_instance_type_marker =
        Aws.Util.option_map (Aws.Json.lookup j "trafficpolicyinstancetype") RRType.of_json
    ; max_items = Aws.Util.option_map (Aws.Json.lookup j "maxitems") String.of_json
    }
end

module DelegationSetInUse = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module CreateTrafficPolicyVersionRequest = struct
  type t =
    { id : String.t
    ; document : String.t
    ; comment : String.t option
    }

  let make ~id ~document ?comment () = { id; document; comment }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; document =
          Aws.Xml.required
            "Document"
            (Aws.Util.option_bind (Aws.Xml.member "Document" xml) String.parse)
      ; comment = Aws.Util.option_bind (Aws.Xml.member "Comment" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.comment (fun f ->
               Aws.Query.Pair ("Comment", String.to_query f))
         ; Some (Aws.Query.Pair ("Document", String.to_query v.document))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.comment (fun f -> "Comment", String.to_json f)
         ; Some ("Document", String.to_json v.document)
         ; Some ("Id", String.to_json v.id)
         ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; document = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Document"))
    ; comment = Aws.Util.option_map (Aws.Json.lookup j "Comment") String.of_json
    }
end

module InsufficientCloudWatchLogsResourcePolicy = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module GetHealthCheckCountRequest = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteVPCAssociationAuthorizationResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module UpdateTrafficPolicyInstanceResponse = struct
  type t = { traffic_policy_instance : TrafficPolicyInstance.t }

  let make ~traffic_policy_instance () = { traffic_policy_instance }

  let parse xml =
    Some
      { traffic_policy_instance =
          Aws.Xml.required
            "TrafficPolicyInstance"
            (Aws.Util.option_bind
               (Aws.Xml.member "TrafficPolicyInstance" xml)
               TrafficPolicyInstance.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "TrafficPolicyInstance"
                , TrafficPolicyInstance.to_query v.traffic_policy_instance ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "TrafficPolicyInstance"
             , TrafficPolicyInstance.to_json v.traffic_policy_instance )
         ])

  let of_json j =
    { traffic_policy_instance =
        TrafficPolicyInstance.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyInstance"))
    }
end

module UpdateTrafficPolicyCommentRequest = struct
  type t =
    { id : String.t
    ; version : Integer.t
    ; comment : String.t
    }

  let make ~id ~version ~comment () = { id; version; comment }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; version =
          Aws.Xml.required
            "Version"
            (Aws.Util.option_bind (Aws.Xml.member "Version" xml) Integer.parse)
      ; comment =
          Aws.Xml.required
            "Comment"
            (Aws.Util.option_bind (Aws.Xml.member "Comment" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Comment", String.to_query v.comment))
         ; Some (Aws.Query.Pair ("Version", Integer.to_query v.version))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Comment", String.to_json v.comment)
         ; Some ("Version", Integer.to_json v.version)
         ; Some ("Id", String.to_json v.id)
         ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; version = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Version"))
    ; comment = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Comment"))
    }
end

module NoSuchGeoLocation = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module ListTrafficPoliciesRequest = struct
  type t =
    { traffic_policy_id_marker : String.t option
    ; max_items : String.t option
    }

  let make ?traffic_policy_id_marker ?max_items () =
    { traffic_policy_id_marker; max_items }

  let parse xml =
    Some
      { traffic_policy_id_marker =
          Aws.Util.option_bind (Aws.Xml.member "trafficpolicyid" xml) String.parse
      ; max_items = Aws.Util.option_bind (Aws.Xml.member "maxitems" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_items (fun f ->
               Aws.Query.Pair ("maxitems", String.to_query f))
         ; Aws.Util.option_map v.traffic_policy_id_marker (fun f ->
               Aws.Query.Pair ("trafficpolicyid", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_items (fun f -> "maxitems", String.to_json f)
         ; Aws.Util.option_map v.traffic_policy_id_marker (fun f ->
               "trafficpolicyid", String.to_json f)
         ])

  let of_json j =
    { traffic_policy_id_marker =
        Aws.Util.option_map (Aws.Json.lookup j "trafficpolicyid") String.of_json
    ; max_items = Aws.Util.option_map (Aws.Json.lookup j "maxitems") String.of_json
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
          Aws.Xml.required
            "ResourceRecordSets"
            (Aws.Util.option_bind
               (Aws.Xml.member "ResourceRecordSets" xml)
               ResourceRecordSets.parse)
      ; is_truncated =
          Aws.Xml.required
            "IsTruncated"
            (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml) Boolean.parse)
      ; next_record_name =
          Aws.Util.option_bind (Aws.Xml.member "NextRecordName" xml) String.parse
      ; next_record_type =
          Aws.Util.option_bind (Aws.Xml.member "NextRecordType" xml) RRType.parse
      ; next_record_identifier =
          Aws.Util.option_bind (Aws.Xml.member "NextRecordIdentifier" xml) String.parse
      ; max_items =
          Aws.Xml.required
            "MaxItems"
            (Aws.Util.option_bind (Aws.Xml.member "MaxItems" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("MaxItems", String.to_query v.max_items))
         ; Aws.Util.option_map v.next_record_identifier (fun f ->
               Aws.Query.Pair ("NextRecordIdentifier", String.to_query f))
         ; Aws.Util.option_map v.next_record_type (fun f ->
               Aws.Query.Pair ("NextRecordType", RRType.to_query f))
         ; Aws.Util.option_map v.next_record_name (fun f ->
               Aws.Query.Pair ("NextRecordName", String.to_query f))
         ; Some (Aws.Query.Pair ("IsTruncated", Boolean.to_query v.is_truncated))
         ; Some
             (Aws.Query.Pair
                ( "ResourceRecordSets.member"
                , ResourceRecordSets.to_query v.resource_record_sets ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("MaxItems", String.to_json v.max_items)
         ; Aws.Util.option_map v.next_record_identifier (fun f ->
               "NextRecordIdentifier", String.to_json f)
         ; Aws.Util.option_map v.next_record_type (fun f ->
               "NextRecordType", RRType.to_json f)
         ; Aws.Util.option_map v.next_record_name (fun f ->
               "NextRecordName", String.to_json f)
         ; Some ("IsTruncated", Boolean.to_json v.is_truncated)
         ; Some ("ResourceRecordSets", ResourceRecordSets.to_json v.resource_record_sets)
         ])

  let of_json j =
    { resource_record_sets =
        ResourceRecordSets.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceRecordSets"))
    ; is_truncated =
        Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IsTruncated"))
    ; next_record_name =
        Aws.Util.option_map (Aws.Json.lookup j "NextRecordName") String.of_json
    ; next_record_type =
        Aws.Util.option_map (Aws.Json.lookup j "NextRecordType") RRType.of_json
    ; next_record_identifier =
        Aws.Util.option_map (Aws.Json.lookup j "NextRecordIdentifier") String.of_json
    ; max_items = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxItems"))
    }
end

module DisassociateVPCFromHostedZoneResponse = struct
  type t = { change_info : ChangeInfo.t }

  let make ~change_info () = { change_info }

  let parse xml =
    Some
      { change_info =
          Aws.Xml.required
            "ChangeInfo"
            (Aws.Util.option_bind (Aws.Xml.member "ChangeInfo" xml) ChangeInfo.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("ChangeInfo", ChangeInfo.to_query v.change_info)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("ChangeInfo", ChangeInfo.to_json v.change_info) ])

  let of_json j =
    { change_info =
        ChangeInfo.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ChangeInfo"))
    }
end

module AccountLimitType = struct
  type t =
    | MAX_HEALTH_CHECKS_BY_OWNER
    | MAX_HOSTED_ZONES_BY_OWNER
    | MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER
    | MAX_REUSABLE_DELEGATION_SETS_BY_OWNER
    | MAX_TRAFFIC_POLICIES_BY_OWNER

  let str_to_t =
    [ "MAX_TRAFFIC_POLICIES_BY_OWNER", MAX_TRAFFIC_POLICIES_BY_OWNER
    ; "MAX_REUSABLE_DELEGATION_SETS_BY_OWNER", MAX_REUSABLE_DELEGATION_SETS_BY_OWNER
    ; "MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER", MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER
    ; "MAX_HOSTED_ZONES_BY_OWNER", MAX_HOSTED_ZONES_BY_OWNER
    ; "MAX_HEALTH_CHECKS_BY_OWNER", MAX_HEALTH_CHECKS_BY_OWNER
    ]

  let t_to_str =
    [ MAX_TRAFFIC_POLICIES_BY_OWNER, "MAX_TRAFFIC_POLICIES_BY_OWNER"
    ; MAX_REUSABLE_DELEGATION_SETS_BY_OWNER, "MAX_REUSABLE_DELEGATION_SETS_BY_OWNER"
    ; MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER, "MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER"
    ; MAX_HOSTED_ZONES_BY_OWNER, "MAX_HOSTED_ZONES_BY_OWNER"
    ; MAX_HEALTH_CHECKS_BY_OWNER, "MAX_HEALTH_CHECKS_BY_OWNER"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module AccountLimit = struct
  type t =
    { type_ : AccountLimitType.t
    ; value : Long.t
    }

  let make ~type_ ~value () = { type_; value }

  let parse xml =
    Some
      { type_ =
          Aws.Xml.required
            "Type"
            (Aws.Util.option_bind (Aws.Xml.member "Type" xml) AccountLimitType.parse)
      ; value =
          Aws.Xml.required
            "Value"
            (Aws.Util.option_bind (Aws.Xml.member "Value" xml) Long.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Value", Long.to_query v.value))
         ; Some (Aws.Query.Pair ("Type", AccountLimitType.to_query v.type_))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Value", Long.to_json v.value)
         ; Some ("Type", AccountLimitType.to_json v.type_)
         ])

  let of_json j =
    { type_ = AccountLimitType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Type"))
    ; value = Long.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Value"))
    }
end

module GetAccountLimitResponse = struct
  type t =
    { limit : AccountLimit.t
    ; count : Long.t
    }

  let make ~limit ~count () = { limit; count }

  let parse xml =
    Some
      { limit =
          Aws.Xml.required
            "Limit"
            (Aws.Util.option_bind (Aws.Xml.member "Limit" xml) AccountLimit.parse)
      ; count =
          Aws.Xml.required
            "Count"
            (Aws.Util.option_bind (Aws.Xml.member "Count" xml) Long.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Count", Long.to_query v.count))
         ; Some (Aws.Query.Pair ("Limit", AccountLimit.to_query v.limit))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Count", Long.to_json v.count)
         ; Some ("Limit", AccountLimit.to_json v.limit)
         ])

  let of_json j =
    { limit = AccountLimit.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Limit"))
    ; count = Long.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Count"))
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
          Aws.Xml.required
            "DelegationSet"
            (Aws.Util.option_bind
               (Aws.Xml.member "DelegationSet" xml)
               DelegationSet.parse)
      ; location =
          Aws.Xml.required
            "Location"
            (Aws.Util.option_bind (Aws.Xml.member "Location" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Location", String.to_query v.location))
         ; Some
             (Aws.Query.Pair ("DelegationSet", DelegationSet.to_query v.delegation_set))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Location", String.to_json v.location)
         ; Some ("DelegationSet", DelegationSet.to_json v.delegation_set)
         ])

  let of_json j =
    { delegation_set =
        DelegationSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DelegationSet"))
    ; location = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Location"))
    }
end

module ListTrafficPolicyInstancesByHostedZoneRequest = struct
  type t =
    { hosted_zone_id : String.t
    ; traffic_policy_instance_name_marker : String.t option
    ; traffic_policy_instance_type_marker : RRType.t option
    ; max_items : String.t option
    }

  let make
      ~hosted_zone_id
      ?traffic_policy_instance_name_marker
      ?traffic_policy_instance_type_marker
      ?max_items
      () =
    { hosted_zone_id
    ; traffic_policy_instance_name_marker
    ; traffic_policy_instance_type_marker
    ; max_items
    }

  let parse xml =
    Some
      { hosted_zone_id =
          Aws.Xml.required
            "id"
            (Aws.Util.option_bind (Aws.Xml.member "id" xml) String.parse)
      ; traffic_policy_instance_name_marker =
          Aws.Util.option_bind
            (Aws.Xml.member "trafficpolicyinstancename" xml)
            String.parse
      ; traffic_policy_instance_type_marker =
          Aws.Util.option_bind
            (Aws.Xml.member "trafficpolicyinstancetype" xml)
            RRType.parse
      ; max_items = Aws.Util.option_bind (Aws.Xml.member "maxitems" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_items (fun f ->
               Aws.Query.Pair ("maxitems", String.to_query f))
         ; Aws.Util.option_map v.traffic_policy_instance_type_marker (fun f ->
               Aws.Query.Pair ("trafficpolicyinstancetype", RRType.to_query f))
         ; Aws.Util.option_map v.traffic_policy_instance_name_marker (fun f ->
               Aws.Query.Pair ("trafficpolicyinstancename", String.to_query f))
         ; Some (Aws.Query.Pair ("id", String.to_query v.hosted_zone_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_items (fun f -> "maxitems", String.to_json f)
         ; Aws.Util.option_map v.traffic_policy_instance_type_marker (fun f ->
               "trafficpolicyinstancetype", RRType.to_json f)
         ; Aws.Util.option_map v.traffic_policy_instance_name_marker (fun f ->
               "trafficpolicyinstancename", String.to_json f)
         ; Some ("id", String.to_json v.hosted_zone_id)
         ])

  let of_json j =
    { hosted_zone_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "id"))
    ; traffic_policy_instance_name_marker =
        Aws.Util.option_map (Aws.Json.lookup j "trafficpolicyinstancename") String.of_json
    ; traffic_policy_instance_type_marker =
        Aws.Util.option_map (Aws.Json.lookup j "trafficpolicyinstancetype") RRType.of_json
    ; max_items = Aws.Util.option_map (Aws.Json.lookup j "maxitems") String.of_json
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
          Aws.Util.option_bind (Aws.Xml.member "startcontinentcode" xml) String.parse
      ; start_country_code =
          Aws.Util.option_bind (Aws.Xml.member "startcountrycode" xml) String.parse
      ; start_subdivision_code =
          Aws.Util.option_bind (Aws.Xml.member "startsubdivisioncode" xml) String.parse
      ; max_items = Aws.Util.option_bind (Aws.Xml.member "maxitems" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_items (fun f ->
               Aws.Query.Pair ("maxitems", String.to_query f))
         ; Aws.Util.option_map v.start_subdivision_code (fun f ->
               Aws.Query.Pair ("startsubdivisioncode", String.to_query f))
         ; Aws.Util.option_map v.start_country_code (fun f ->
               Aws.Query.Pair ("startcountrycode", String.to_query f))
         ; Aws.Util.option_map v.start_continent_code (fun f ->
               Aws.Query.Pair ("startcontinentcode", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_items (fun f -> "maxitems", String.to_json f)
         ; Aws.Util.option_map v.start_subdivision_code (fun f ->
               "startsubdivisioncode", String.to_json f)
         ; Aws.Util.option_map v.start_country_code (fun f ->
               "startcountrycode", String.to_json f)
         ; Aws.Util.option_map v.start_continent_code (fun f ->
               "startcontinentcode", String.to_json f)
         ])

  let of_json j =
    { start_continent_code =
        Aws.Util.option_map (Aws.Json.lookup j "startcontinentcode") String.of_json
    ; start_country_code =
        Aws.Util.option_map (Aws.Json.lookup j "startcountrycode") String.of_json
    ; start_subdivision_code =
        Aws.Util.option_map (Aws.Json.lookup j "startsubdivisioncode") String.of_json
    ; max_items = Aws.Util.option_map (Aws.Json.lookup j "maxitems") String.of_json
    }
end

module GetTrafficPolicyInstanceCountRequest = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
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
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; v_p_c =
          Aws.Xml.required
            "VPC"
            (Aws.Util.option_bind (Aws.Xml.member "VPC" xml) VPC.parse)
      ; comment = Aws.Util.option_bind (Aws.Xml.member "Comment" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.comment (fun f ->
               Aws.Query.Pair ("Comment", String.to_query f))
         ; Some (Aws.Query.Pair ("VPC", VPC.to_query v.v_p_c))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.hosted_zone_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.comment (fun f -> "Comment", String.to_json f)
         ; Some ("VPC", VPC.to_json v.v_p_c)
         ; Some ("Id", String.to_json v.hosted_zone_id)
         ])

  let of_json j =
    { hosted_zone_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; v_p_c = VPC.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VPC"))
    ; comment = Aws.Util.option_map (Aws.Json.lookup j "Comment") String.of_json
    }
end

module TooManyHostedZones = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module GetHostedZoneCountResponse = struct
  type t = { hosted_zone_count : Long.t }

  let make ~hosted_zone_count () = { hosted_zone_count }

  let parse xml =
    Some
      { hosted_zone_count =
          Aws.Xml.required
            "HostedZoneCount"
            (Aws.Util.option_bind (Aws.Xml.member "HostedZoneCount" xml) Long.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("HostedZoneCount", Long.to_query v.hosted_zone_count)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("HostedZoneCount", Long.to_json v.hosted_zone_count) ])

  let of_json j =
    { hosted_zone_count =
        Long.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZoneCount"))
    }
end

module HostedZoneSummaries = struct
  type t = HostedZoneSummary.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map HostedZoneSummary.parse (Aws.Xml.members "HostedZoneSummary" xml))

  let to_query v = Aws.Query.to_query_list HostedZoneSummary.to_query v

  let to_json v = `List (List.map HostedZoneSummary.to_json v)

  let of_json j = Aws.Json.to_list HostedZoneSummary.of_json j
end

module Tag = struct
  type t =
    { key : String.t option
    ; value : String.t option
    }

  let make ?key ?value () = { key; value }

  let parse xml =
    Some
      { key = Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse
      ; value = Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.value (fun f ->
               Aws.Query.Pair ("Value", String.to_query f))
         ; Aws.Util.option_map v.key (fun f -> Aws.Query.Pair ("Key", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.value (fun f -> "Value", String.to_json f)
         ; Aws.Util.option_map v.key (fun f -> "Key", String.to_json f)
         ])

  let of_json j =
    { key = Aws.Util.option_map (Aws.Json.lookup j "Key") String.of_json
    ; value = Aws.Util.option_map (Aws.Json.lookup j "Value") String.of_json
    }
end

module TagList = struct
  type t = Tag.t list

  let make elems () = elems

  let parse xml = Aws.Util.option_all (List.map Tag.parse (Aws.Xml.members "Tag" xml))

  let to_query v = Aws.Query.to_query_list Tag.to_query v

  let to_json v = `List (List.map Tag.to_json v)

  let of_json j = Aws.Json.to_list Tag.of_json j
end

module HostedZones = struct
  type t = HostedZone.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map HostedZone.parse (Aws.Xml.members "HostedZone" xml))

  let to_query v = Aws.Query.to_query_list HostedZone.to_query v

  let to_json v = `List (List.map HostedZone.to_json v)

  let of_json j = Aws.Json.to_list HostedZone.of_json j
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
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; v_p_c =
          Aws.Xml.required
            "VPC"
            (Aws.Util.option_bind (Aws.Xml.member "VPC" xml) VPC.parse)
      ; comment = Aws.Util.option_bind (Aws.Xml.member "Comment" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.comment (fun f ->
               Aws.Query.Pair ("Comment", String.to_query f))
         ; Some (Aws.Query.Pair ("VPC", VPC.to_query v.v_p_c))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.hosted_zone_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.comment (fun f -> "Comment", String.to_json f)
         ; Some ("VPC", VPC.to_json v.v_p_c)
         ; Some ("Id", String.to_json v.hosted_zone_id)
         ])

  let of_json j =
    { hosted_zone_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; v_p_c = VPC.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VPC"))
    ; comment = Aws.Util.option_map (Aws.Json.lookup j "Comment") String.of_json
    }
end

module TagResourceType = struct
  type t =
    | Healthcheck
    | Hostedzone

  let str_to_t = [ "hostedzone", Hostedzone; "healthcheck", Healthcheck ]

  let t_to_str = [ Hostedzone, "hostedzone"; Healthcheck, "healthcheck" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
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
          Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) TagResourceType.parse
      ; resource_id = Aws.Util.option_bind (Aws.Xml.member "ResourceId" xml) String.parse
      ; tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Aws.Util.option_map v.resource_id (fun f ->
               Aws.Query.Pair ("ResourceId", String.to_query f))
         ; Aws.Util.option_map v.resource_type (fun f ->
               Aws.Query.Pair ("ResourceType", TagResourceType.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Tags", TagList.to_json v.tags)
         ; Aws.Util.option_map v.resource_id (fun f -> "ResourceId", String.to_json f)
         ; Aws.Util.option_map v.resource_type (fun f ->
               "ResourceType", TagResourceType.to_json f)
         ])

  let of_json j =
    { resource_type =
        Aws.Util.option_map (Aws.Json.lookup j "ResourceType") TagResourceType.of_json
    ; resource_id = Aws.Util.option_map (Aws.Json.lookup j "ResourceId") String.of_json
    ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    }
end

module ResourceTagSetList = struct
  type t = ResourceTagSet.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map ResourceTagSet.parse (Aws.Xml.members "ResourceTagSet" xml))

  let to_query v = Aws.Query.to_query_list ResourceTagSet.to_query v

  let to_json v = `List (List.map ResourceTagSet.to_json v)

  let of_json j = Aws.Json.to_list ResourceTagSet.of_json j
end

module ListTagsForResourcesResponse = struct
  type t = { resource_tag_sets : ResourceTagSetList.t }

  let make ~resource_tag_sets () = { resource_tag_sets }

  let parse xml =
    Some
      { resource_tag_sets =
          Aws.Xml.required
            "ResourceTagSets"
            (Aws.Util.option_bind
               (Aws.Xml.member "ResourceTagSets" xml)
               ResourceTagSetList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("ResourceTagSets.member", ResourceTagSetList.to_query v.resource_tag_sets))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ResourceTagSets", ResourceTagSetList.to_json v.resource_tag_sets) ])

  let of_json j =
    { resource_tag_sets =
        ResourceTagSetList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceTagSets"))
    }
end

module ListHostedZonesByVPCRequest = struct
  type t =
    { v_p_c_id : String.t
    ; v_p_c_region : VPCRegion.t
    ; max_items : String.t option
    ; next_token : String.t option
    }

  let make ~v_p_c_id ~v_p_c_region ?max_items ?next_token () =
    { v_p_c_id; v_p_c_region; max_items; next_token }

  let parse xml =
    Some
      { v_p_c_id =
          Aws.Xml.required
            "vpcid"
            (Aws.Util.option_bind (Aws.Xml.member "vpcid" xml) String.parse)
      ; v_p_c_region =
          Aws.Xml.required
            "vpcregion"
            (Aws.Util.option_bind (Aws.Xml.member "vpcregion" xml) VPCRegion.parse)
      ; max_items = Aws.Util.option_bind (Aws.Xml.member "maxitems" xml) String.parse
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "nexttoken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("nexttoken", String.to_query f))
         ; Aws.Util.option_map v.max_items (fun f ->
               Aws.Query.Pair ("maxitems", String.to_query f))
         ; Some (Aws.Query.Pair ("vpcregion", VPCRegion.to_query v.v_p_c_region))
         ; Some (Aws.Query.Pair ("vpcid", String.to_query v.v_p_c_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "nexttoken", String.to_json f)
         ; Aws.Util.option_map v.max_items (fun f -> "maxitems", String.to_json f)
         ; Some ("vpcregion", VPCRegion.to_json v.v_p_c_region)
         ; Some ("vpcid", String.to_json v.v_p_c_id)
         ])

  let of_json j =
    { v_p_c_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "vpcid"))
    ; v_p_c_region =
        VPCRegion.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "vpcregion"))
    ; max_items = Aws.Util.option_map (Aws.Json.lookup j "maxitems") String.of_json
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nexttoken") String.of_json
    }
end

module GetReusableDelegationSetLimitRequest = struct
  type t =
    { type_ : ReusableDelegationSetLimitType.t
    ; delegation_set_id : String.t
    }

  let make ~type_ ~delegation_set_id () = { type_; delegation_set_id }

  let parse xml =
    Some
      { type_ =
          Aws.Xml.required
            "Type"
            (Aws.Util.option_bind
               (Aws.Xml.member "Type" xml)
               ReusableDelegationSetLimitType.parse)
      ; delegation_set_id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Id", String.to_query v.delegation_set_id))
         ; Some (Aws.Query.Pair ("Type", ReusableDelegationSetLimitType.to_query v.type_))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Id", String.to_json v.delegation_set_id)
         ; Some ("Type", ReusableDelegationSetLimitType.to_json v.type_)
         ])

  let of_json j =
    { type_ =
        ReusableDelegationSetLimitType.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Type"))
    ; delegation_set_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    }
end

module DeleteQueryLoggingConfigRequest = struct
  type t = { id : String.t }

  let make ~id () = { id }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt [ Some (Aws.Query.Pair ("Id", String.to_query v.id)) ])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [ Some ("Id", String.to_json v.id) ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id")) }
end

module DelegationSetNotAvailable = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module ListQueryLoggingConfigsRequest = struct
  type t =
    { hosted_zone_id : String.t option
    ; next_token : String.t option
    ; max_results : String.t option
    }

  let make ?hosted_zone_id ?next_token ?max_results () =
    { hosted_zone_id; next_token; max_results }

  let parse xml =
    Some
      { hosted_zone_id =
          Aws.Util.option_bind (Aws.Xml.member "hostedzoneid" xml) String.parse
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "nexttoken" xml) String.parse
      ; max_results = Aws.Util.option_bind (Aws.Xml.member "maxresults" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_results (fun f ->
               Aws.Query.Pair ("maxresults", String.to_query f))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("nexttoken", String.to_query f))
         ; Aws.Util.option_map v.hosted_zone_id (fun f ->
               Aws.Query.Pair ("hostedzoneid", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_results (fun f -> "maxresults", String.to_json f)
         ; Aws.Util.option_map v.next_token (fun f -> "nexttoken", String.to_json f)
         ; Aws.Util.option_map v.hosted_zone_id (fun f ->
               "hostedzoneid", String.to_json f)
         ])

  let of_json j =
    { hosted_zone_id =
        Aws.Util.option_map (Aws.Json.lookup j "hostedzoneid") String.of_json
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nexttoken") String.of_json
    ; max_results = Aws.Util.option_map (Aws.Json.lookup j "maxresults") String.of_json
    }
end

module TooManyTrafficPolicies = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module CreateQueryLoggingConfigResponse = struct
  type t =
    { query_logging_config : QueryLoggingConfig.t
    ; location : String.t
    }

  let make ~query_logging_config ~location () = { query_logging_config; location }

  let parse xml =
    Some
      { query_logging_config =
          Aws.Xml.required
            "QueryLoggingConfig"
            (Aws.Util.option_bind
               (Aws.Xml.member "QueryLoggingConfig" xml)
               QueryLoggingConfig.parse)
      ; location =
          Aws.Xml.required
            "Location"
            (Aws.Util.option_bind (Aws.Xml.member "Location" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Location", String.to_query v.location))
         ; Some
             (Aws.Query.Pair
                ("QueryLoggingConfig", QueryLoggingConfig.to_query v.query_logging_config))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Location", String.to_json v.location)
         ; Some ("QueryLoggingConfig", QueryLoggingConfig.to_json v.query_logging_config)
         ])

  let of_json j =
    { query_logging_config =
        QueryLoggingConfig.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "QueryLoggingConfig"))
    ; location = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Location"))
    }
end

module TagKeyList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Aws.Util.option_all (List.map String.parse (Aws.Xml.members "Key" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
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
          Aws.Xml.required
            "ResourceType"
            (Aws.Util.option_bind
               (Aws.Xml.member "ResourceType" xml)
               TagResourceType.parse)
      ; resource_id =
          Aws.Xml.required
            "ResourceId"
            (Aws.Util.option_bind (Aws.Xml.member "ResourceId" xml) String.parse)
      ; add_tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "AddTags" xml) TagList.parse)
      ; remove_tag_keys =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "RemoveTagKeys" xml) TagKeyList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("RemoveTagKeys.member", TagKeyList.to_query v.remove_tag_keys))
         ; Some (Aws.Query.Pair ("AddTags.member", TagList.to_query v.add_tags))
         ; Some (Aws.Query.Pair ("ResourceId", String.to_query v.resource_id))
         ; Some
             (Aws.Query.Pair ("ResourceType", TagResourceType.to_query v.resource_type))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("RemoveTagKeys", TagKeyList.to_json v.remove_tag_keys)
         ; Some ("AddTags", TagList.to_json v.add_tags)
         ; Some ("ResourceId", String.to_json v.resource_id)
         ; Some ("ResourceType", TagResourceType.to_json v.resource_type)
         ])

  let of_json j =
    { resource_type =
        TagResourceType.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceType"))
    ; resource_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceId"))
    ; add_tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AddTags"))
    ; remove_tag_keys =
        TagKeyList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RemoveTagKeys"))
    }
end

module UpdateHealthCheckResponse = struct
  type t = { health_check : HealthCheck.t }

  let make ~health_check () = { health_check }

  let parse xml =
    Some
      { health_check =
          Aws.Xml.required
            "HealthCheck"
            (Aws.Util.option_bind (Aws.Xml.member "HealthCheck" xml) HealthCheck.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("HealthCheck", HealthCheck.to_query v.health_check)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("HealthCheck", HealthCheck.to_json v.health_check) ])

  let of_json j =
    { health_check =
        HealthCheck.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheck"))
    }
end

module DeleteHostedZoneRequest = struct
  type t = { id : String.t }

  let make ~id () = { id }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt [ Some (Aws.Query.Pair ("Id", String.to_query v.id)) ])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [ Some ("Id", String.to_json v.id) ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id")) }
end

module GetTrafficPolicyInstanceCountResponse = struct
  type t = { traffic_policy_instance_count : Integer.t }

  let make ~traffic_policy_instance_count () = { traffic_policy_instance_count }

  let parse xml =
    Some
      { traffic_policy_instance_count =
          Aws.Xml.required
            "TrafficPolicyInstanceCount"
            (Aws.Util.option_bind
               (Aws.Xml.member "TrafficPolicyInstanceCount" xml)
               Integer.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "TrafficPolicyInstanceCount"
                , Integer.to_query v.traffic_policy_instance_count ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "TrafficPolicyInstanceCount"
             , Integer.to_json v.traffic_policy_instance_count )
         ])

  let of_json j =
    { traffic_policy_instance_count =
        Integer.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyInstanceCount"))
    }
end

module TooManyHealthChecks = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module HealthCheckInUse = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module CreateTrafficPolicyInstanceResponse = struct
  type t =
    { traffic_policy_instance : TrafficPolicyInstance.t
    ; location : String.t
    }

  let make ~traffic_policy_instance ~location () = { traffic_policy_instance; location }

  let parse xml =
    Some
      { traffic_policy_instance =
          Aws.Xml.required
            "TrafficPolicyInstance"
            (Aws.Util.option_bind
               (Aws.Xml.member "TrafficPolicyInstance" xml)
               TrafficPolicyInstance.parse)
      ; location =
          Aws.Xml.required
            "Location"
            (Aws.Util.option_bind (Aws.Xml.member "Location" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Location", String.to_query v.location))
         ; Some
             (Aws.Query.Pair
                ( "TrafficPolicyInstance"
                , TrafficPolicyInstance.to_query v.traffic_policy_instance ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Location", String.to_json v.location)
         ; Some
             ( "TrafficPolicyInstance"
             , TrafficPolicyInstance.to_json v.traffic_policy_instance )
         ])

  let of_json j =
    { traffic_policy_instance =
        TrafficPolicyInstance.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyInstance"))
    ; location = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Location"))
    }
end

module ListQueryLoggingConfigsResponse = struct
  type t =
    { query_logging_configs : QueryLoggingConfigs.t
    ; next_token : String.t option
    }

  let make ~query_logging_configs ?next_token () = { query_logging_configs; next_token }

  let parse xml =
    Some
      { query_logging_configs =
          Aws.Xml.required
            "QueryLoggingConfigs"
            (Aws.Util.option_bind
               (Aws.Xml.member "QueryLoggingConfigs" xml)
               QueryLoggingConfigs.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "QueryLoggingConfigs.member"
                , QueryLoggingConfigs.to_query v.query_logging_configs ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some
             ("QueryLoggingConfigs", QueryLoggingConfigs.to_json v.query_logging_configs)
         ])

  let of_json j =
    { query_logging_configs =
        QueryLoggingConfigs.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "QueryLoggingConfigs"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module IncompatibleVersion = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module HealthChecks = struct
  type t = HealthCheck.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map HealthCheck.parse (Aws.Xml.members "HealthCheck" xml))

  let to_query v = Aws.Query.to_query_list HealthCheck.to_query v

  let to_json v = `List (List.map HealthCheck.to_json v)

  let of_json j = Aws.Json.to_list HealthCheck.of_json j
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
          Aws.Xml.required
            "HealthChecks"
            (Aws.Util.option_bind (Aws.Xml.member "HealthChecks" xml) HealthChecks.parse)
      ; marker =
          Aws.Xml.required
            "Marker"
            (Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse)
      ; is_truncated =
          Aws.Xml.required
            "IsTruncated"
            (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml) Boolean.parse)
      ; next_marker = Aws.Util.option_bind (Aws.Xml.member "NextMarker" xml) String.parse
      ; max_items =
          Aws.Xml.required
            "MaxItems"
            (Aws.Util.option_bind (Aws.Xml.member "MaxItems" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("MaxItems", String.to_query v.max_items))
         ; Aws.Util.option_map v.next_marker (fun f ->
               Aws.Query.Pair ("NextMarker", String.to_query f))
         ; Some (Aws.Query.Pair ("IsTruncated", Boolean.to_query v.is_truncated))
         ; Some (Aws.Query.Pair ("Marker", String.to_query v.marker))
         ; Some
             (Aws.Query.Pair ("HealthChecks.member", HealthChecks.to_query v.health_checks))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("MaxItems", String.to_json v.max_items)
         ; Aws.Util.option_map v.next_marker (fun f -> "NextMarker", String.to_json f)
         ; Some ("IsTruncated", Boolean.to_json v.is_truncated)
         ; Some ("Marker", String.to_json v.marker)
         ; Some ("HealthChecks", HealthChecks.to_json v.health_checks)
         ])

  let of_json j =
    { health_checks =
        HealthChecks.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthChecks"))
    ; marker = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Marker"))
    ; is_truncated =
        Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IsTruncated"))
    ; next_marker = Aws.Util.option_map (Aws.Json.lookup j "NextMarker") String.of_json
    ; max_items = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxItems"))
    }
end

module HostedZoneLimit = struct
  type t =
    { type_ : HostedZoneLimitType.t
    ; value : Long.t
    }

  let make ~type_ ~value () = { type_; value }

  let parse xml =
    Some
      { type_ =
          Aws.Xml.required
            "Type"
            (Aws.Util.option_bind (Aws.Xml.member "Type" xml) HostedZoneLimitType.parse)
      ; value =
          Aws.Xml.required
            "Value"
            (Aws.Util.option_bind (Aws.Xml.member "Value" xml) Long.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Value", Long.to_query v.value))
         ; Some (Aws.Query.Pair ("Type", HostedZoneLimitType.to_query v.type_))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Value", Long.to_json v.value)
         ; Some ("Type", HostedZoneLimitType.to_json v.type_)
         ])

  let of_json j =
    { type_ =
        HostedZoneLimitType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Type"))
    ; value = Long.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Value"))
    }
end

module GetHostedZoneLimitResponse = struct
  type t =
    { limit : HostedZoneLimit.t
    ; count : Long.t
    }

  let make ~limit ~count () = { limit; count }

  let parse xml =
    Some
      { limit =
          Aws.Xml.required
            "Limit"
            (Aws.Util.option_bind (Aws.Xml.member "Limit" xml) HostedZoneLimit.parse)
      ; count =
          Aws.Xml.required
            "Count"
            (Aws.Util.option_bind (Aws.Xml.member "Count" xml) Long.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Count", Long.to_query v.count))
         ; Some (Aws.Query.Pair ("Limit", HostedZoneLimit.to_query v.limit))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Count", Long.to_json v.count)
         ; Some ("Limit", HostedZoneLimit.to_json v.limit)
         ])

  let of_json j =
    { limit = HostedZoneLimit.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Limit"))
    ; count = Long.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Count"))
    }
end

module UpdateTrafficPolicyInstanceRequest = struct
  type t =
    { id : String.t
    ; t_t_l : Long.t
    ; traffic_policy_id : String.t
    ; traffic_policy_version : Integer.t
    }

  let make ~id ~t_t_l ~traffic_policy_id ~traffic_policy_version () =
    { id; t_t_l; traffic_policy_id; traffic_policy_version }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; t_t_l =
          Aws.Xml.required
            "TTL"
            (Aws.Util.option_bind (Aws.Xml.member "TTL" xml) Long.parse)
      ; traffic_policy_id =
          Aws.Xml.required
            "TrafficPolicyId"
            (Aws.Util.option_bind (Aws.Xml.member "TrafficPolicyId" xml) String.parse)
      ; traffic_policy_version =
          Aws.Xml.required
            "TrafficPolicyVersion"
            (Aws.Util.option_bind
               (Aws.Xml.member "TrafficPolicyVersion" xml)
               Integer.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("TrafficPolicyVersion", Integer.to_query v.traffic_policy_version))
         ; Some (Aws.Query.Pair ("TrafficPolicyId", String.to_query v.traffic_policy_id))
         ; Some (Aws.Query.Pair ("TTL", Long.to_query v.t_t_l))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("TrafficPolicyVersion", Integer.to_json v.traffic_policy_version)
         ; Some ("TrafficPolicyId", String.to_json v.traffic_policy_id)
         ; Some ("TTL", Long.to_json v.t_t_l)
         ; Some ("Id", String.to_json v.id)
         ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; t_t_l = Long.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TTL"))
    ; traffic_policy_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyId"))
    ; traffic_policy_version =
        Integer.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyVersion"))
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
      { continent_code =
          Aws.Util.option_bind (Aws.Xml.member "ContinentCode" xml) String.parse
      ; continent_name =
          Aws.Util.option_bind (Aws.Xml.member "ContinentName" xml) String.parse
      ; country_code =
          Aws.Util.option_bind (Aws.Xml.member "CountryCode" xml) String.parse
      ; country_name =
          Aws.Util.option_bind (Aws.Xml.member "CountryName" xml) String.parse
      ; subdivision_code =
          Aws.Util.option_bind (Aws.Xml.member "SubdivisionCode" xml) String.parse
      ; subdivision_name =
          Aws.Util.option_bind (Aws.Xml.member "SubdivisionName" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.subdivision_name (fun f ->
               Aws.Query.Pair ("SubdivisionName", String.to_query f))
         ; Aws.Util.option_map v.subdivision_code (fun f ->
               Aws.Query.Pair ("SubdivisionCode", String.to_query f))
         ; Aws.Util.option_map v.country_name (fun f ->
               Aws.Query.Pair ("CountryName", String.to_query f))
         ; Aws.Util.option_map v.country_code (fun f ->
               Aws.Query.Pair ("CountryCode", String.to_query f))
         ; Aws.Util.option_map v.continent_name (fun f ->
               Aws.Query.Pair ("ContinentName", String.to_query f))
         ; Aws.Util.option_map v.continent_code (fun f ->
               Aws.Query.Pair ("ContinentCode", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.subdivision_name (fun f ->
               "SubdivisionName", String.to_json f)
         ; Aws.Util.option_map v.subdivision_code (fun f ->
               "SubdivisionCode", String.to_json f)
         ; Aws.Util.option_map v.country_name (fun f -> "CountryName", String.to_json f)
         ; Aws.Util.option_map v.country_code (fun f -> "CountryCode", String.to_json f)
         ; Aws.Util.option_map v.continent_name (fun f ->
               "ContinentName", String.to_json f)
         ; Aws.Util.option_map v.continent_code (fun f ->
               "ContinentCode", String.to_json f)
         ])

  let of_json j =
    { continent_code =
        Aws.Util.option_map (Aws.Json.lookup j "ContinentCode") String.of_json
    ; continent_name =
        Aws.Util.option_map (Aws.Json.lookup j "ContinentName") String.of_json
    ; country_code = Aws.Util.option_map (Aws.Json.lookup j "CountryCode") String.of_json
    ; country_name = Aws.Util.option_map (Aws.Json.lookup j "CountryName") String.of_json
    ; subdivision_code =
        Aws.Util.option_map (Aws.Json.lookup j "SubdivisionCode") String.of_json
    ; subdivision_name =
        Aws.Util.option_map (Aws.Json.lookup j "SubdivisionName") String.of_json
    }
end

module GeoLocationDetailsList = struct
  type t = GeoLocationDetails.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map GeoLocationDetails.parse (Aws.Xml.members "GeoLocationDetails" xml))

  let to_query v = Aws.Query.to_query_list GeoLocationDetails.to_query v

  let to_json v = `List (List.map GeoLocationDetails.to_json v)

  let of_json j = Aws.Json.to_list GeoLocationDetails.of_json j
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
          Aws.Xml.required
            "GeoLocationDetailsList"
            (Aws.Util.option_bind
               (Aws.Xml.member "GeoLocationDetailsList" xml)
               GeoLocationDetailsList.parse)
      ; is_truncated =
          Aws.Xml.required
            "IsTruncated"
            (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml) Boolean.parse)
      ; next_continent_code =
          Aws.Util.option_bind (Aws.Xml.member "NextContinentCode" xml) String.parse
      ; next_country_code =
          Aws.Util.option_bind (Aws.Xml.member "NextCountryCode" xml) String.parse
      ; next_subdivision_code =
          Aws.Util.option_bind (Aws.Xml.member "NextSubdivisionCode" xml) String.parse
      ; max_items =
          Aws.Xml.required
            "MaxItems"
            (Aws.Util.option_bind (Aws.Xml.member "MaxItems" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("MaxItems", String.to_query v.max_items))
         ; Aws.Util.option_map v.next_subdivision_code (fun f ->
               Aws.Query.Pair ("NextSubdivisionCode", String.to_query f))
         ; Aws.Util.option_map v.next_country_code (fun f ->
               Aws.Query.Pair ("NextCountryCode", String.to_query f))
         ; Aws.Util.option_map v.next_continent_code (fun f ->
               Aws.Query.Pair ("NextContinentCode", String.to_query f))
         ; Some (Aws.Query.Pair ("IsTruncated", Boolean.to_query v.is_truncated))
         ; Some
             (Aws.Query.Pair
                ( "GeoLocationDetailsList.member"
                , GeoLocationDetailsList.to_query v.geo_location_details_list ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("MaxItems", String.to_json v.max_items)
         ; Aws.Util.option_map v.next_subdivision_code (fun f ->
               "NextSubdivisionCode", String.to_json f)
         ; Aws.Util.option_map v.next_country_code (fun f ->
               "NextCountryCode", String.to_json f)
         ; Aws.Util.option_map v.next_continent_code (fun f ->
               "NextContinentCode", String.to_json f)
         ; Some ("IsTruncated", Boolean.to_json v.is_truncated)
         ; Some
             ( "GeoLocationDetailsList"
             , GeoLocationDetailsList.to_json v.geo_location_details_list )
         ])

  let of_json j =
    { geo_location_details_list =
        GeoLocationDetailsList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "GeoLocationDetailsList"))
    ; is_truncated =
        Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IsTruncated"))
    ; next_continent_code =
        Aws.Util.option_map (Aws.Json.lookup j "NextContinentCode") String.of_json
    ; next_country_code =
        Aws.Util.option_map (Aws.Json.lookup j "NextCountryCode") String.of_json
    ; next_subdivision_code =
        Aws.Util.option_map (Aws.Json.lookup j "NextSubdivisionCode") String.of_json
    ; max_items = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxItems"))
    }
end

module InvalidVPCId = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module GetChangeResponse = struct
  type t = { change_info : ChangeInfo.t }

  let make ~change_info () = { change_info }

  let parse xml =
    Some
      { change_info =
          Aws.Xml.required
            "ChangeInfo"
            (Aws.Util.option_bind (Aws.Xml.member "ChangeInfo" xml) ChangeInfo.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("ChangeInfo", ChangeInfo.to_query v.change_info)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("ChangeInfo", ChangeInfo.to_json v.change_info) ])

  let of_json j =
    { change_info =
        ChangeInfo.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ChangeInfo"))
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
          Aws.Xml.required
            "HealthCheck"
            (Aws.Util.option_bind (Aws.Xml.member "HealthCheck" xml) HealthCheck.parse)
      ; location =
          Aws.Xml.required
            "Location"
            (Aws.Util.option_bind (Aws.Xml.member "Location" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Location", String.to_query v.location))
         ; Some (Aws.Query.Pair ("HealthCheck", HealthCheck.to_query v.health_check))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Location", String.to_json v.location)
         ; Some ("HealthCheck", HealthCheck.to_json v.health_check)
         ])

  let of_json j =
    { health_check =
        HealthCheck.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheck"))
    ; location = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Location"))
    }
end

module ListTagsForResourceResponse = struct
  type t = { resource_tag_set : ResourceTagSet.t }

  let make ~resource_tag_set () = { resource_tag_set }

  let parse xml =
    Some
      { resource_tag_set =
          Aws.Xml.required
            "ResourceTagSet"
            (Aws.Util.option_bind
               (Aws.Xml.member "ResourceTagSet" xml)
               ResourceTagSet.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("ResourceTagSet", ResourceTagSet.to_query v.resource_tag_set))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ResourceTagSet", ResourceTagSet.to_json v.resource_tag_set) ])

  let of_json j =
    { resource_tag_set =
        ResourceTagSet.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceTagSet"))
    }
end

module ErrorMessages = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "Message" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
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
          Aws.Xml.required
            "CallerReference"
            (Aws.Util.option_bind (Aws.Xml.member "CallerReference" xml) String.parse)
      ; hosted_zone_id =
          Aws.Util.option_bind (Aws.Xml.member "HostedZoneId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.hosted_zone_id (fun f ->
               Aws.Query.Pair ("HostedZoneId", String.to_query f))
         ; Some (Aws.Query.Pair ("CallerReference", String.to_query v.caller_reference))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.hosted_zone_id (fun f ->
               "HostedZoneId", String.to_json f)
         ; Some ("CallerReference", String.to_json v.caller_reference)
         ])

  let of_json j =
    { caller_reference =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CallerReference"))
    ; hosted_zone_id =
        Aws.Util.option_map (Aws.Json.lookup j "HostedZoneId") String.of_json
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
          Aws.Xml.required
            "HostedZones"
            (Aws.Util.option_bind (Aws.Xml.member "HostedZones" xml) HostedZones.parse)
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      ; is_truncated =
          Aws.Xml.required
            "IsTruncated"
            (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml) Boolean.parse)
      ; next_marker = Aws.Util.option_bind (Aws.Xml.member "NextMarker" xml) String.parse
      ; max_items =
          Aws.Xml.required
            "MaxItems"
            (Aws.Util.option_bind (Aws.Xml.member "MaxItems" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("MaxItems", String.to_query v.max_items))
         ; Aws.Util.option_map v.next_marker (fun f ->
               Aws.Query.Pair ("NextMarker", String.to_query f))
         ; Some (Aws.Query.Pair ("IsTruncated", Boolean.to_query v.is_truncated))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Some
             (Aws.Query.Pair ("HostedZones.member", HostedZones.to_query v.hosted_zones))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("MaxItems", String.to_json v.max_items)
         ; Aws.Util.option_map v.next_marker (fun f -> "NextMarker", String.to_json f)
         ; Some ("IsTruncated", Boolean.to_json v.is_truncated)
         ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Some ("HostedZones", HostedZones.to_json v.hosted_zones)
         ])

  let of_json j =
    { hosted_zones =
        HostedZones.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZones"))
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    ; is_truncated =
        Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IsTruncated"))
    ; next_marker = Aws.Util.option_map (Aws.Json.lookup j "NextMarker") String.of_json
    ; max_items = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxItems"))
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
      { marker = Aws.Util.option_bind (Aws.Xml.member "marker" xml) String.parse
      ; max_items = Aws.Util.option_bind (Aws.Xml.member "maxitems" xml) String.parse
      ; delegation_set_id =
          Aws.Util.option_bind (Aws.Xml.member "delegationsetid" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.delegation_set_id (fun f ->
               Aws.Query.Pair ("delegationsetid", String.to_query f))
         ; Aws.Util.option_map v.max_items (fun f ->
               Aws.Query.Pair ("maxitems", String.to_query f))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.delegation_set_id (fun f ->
               "delegationsetid", String.to_json f)
         ; Aws.Util.option_map v.max_items (fun f -> "maxitems", String.to_json f)
         ; Aws.Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Aws.Util.option_map (Aws.Json.lookup j "marker") String.of_json
    ; max_items = Aws.Util.option_map (Aws.Json.lookup j "maxitems") String.of_json
    ; delegation_set_id =
        Aws.Util.option_map (Aws.Json.lookup j "delegationsetid") String.of_json
    }
end

module TagResourceIdList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "ResourceId" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
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
          Aws.Xml.required
            "ResourceType"
            (Aws.Util.option_bind
               (Aws.Xml.member "ResourceType" xml)
               TagResourceType.parse)
      ; resource_ids =
          Aws.Xml.required
            "ResourceIds"
            (Aws.Util.option_bind
               (Aws.Xml.member "ResourceIds" xml)
               TagResourceIdList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("ResourceIds.member", TagResourceIdList.to_query v.resource_ids))
         ; Some
             (Aws.Query.Pair ("ResourceType", TagResourceType.to_query v.resource_type))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ResourceIds", TagResourceIdList.to_json v.resource_ids)
         ; Some ("ResourceType", TagResourceType.to_json v.resource_type)
         ])

  let of_json j =
    { resource_type =
        TagResourceType.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceType"))
    ; resource_ids =
        TagResourceIdList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceIds"))
    }
end

module NoSuchCloudWatchLogsLogGroup = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module ConflictingDomainExists = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module VPCAssociationNotFound = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module InvalidTrafficPolicyDocument = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module StatusReport = struct
  type t =
    { status : String.t option
    ; checked_time : DateTime.t option
    }

  let make ?status ?checked_time () = { status; checked_time }

  let parse xml =
    Some
      { status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
      ; checked_time =
          Aws.Util.option_bind (Aws.Xml.member "CheckedTime" xml) DateTime.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.checked_time (fun f ->
               Aws.Query.Pair ("CheckedTime", DateTime.to_query f))
         ; Aws.Util.option_map v.status (fun f ->
               Aws.Query.Pair ("Status", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.checked_time (fun f -> "CheckedTime", DateTime.to_json f)
         ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
         ])

  let of_json j =
    { status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
    ; checked_time =
        Aws.Util.option_map (Aws.Json.lookup j "CheckedTime") DateTime.of_json
    }
end

module HealthCheckObservation = struct
  type t =
    { region : HealthCheckRegion.t option
    ; i_p_address : String.t option
    ; status_report : StatusReport.t option
    }

  let make ?region ?i_p_address ?status_report () = { region; i_p_address; status_report }

  let parse xml =
    Some
      { region =
          Aws.Util.option_bind (Aws.Xml.member "Region" xml) HealthCheckRegion.parse
      ; i_p_address = Aws.Util.option_bind (Aws.Xml.member "IPAddress" xml) String.parse
      ; status_report =
          Aws.Util.option_bind (Aws.Xml.member "StatusReport" xml) StatusReport.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.status_report (fun f ->
               Aws.Query.Pair ("StatusReport", StatusReport.to_query f))
         ; Aws.Util.option_map v.i_p_address (fun f ->
               Aws.Query.Pair ("IPAddress", String.to_query f))
         ; Aws.Util.option_map v.region (fun f ->
               Aws.Query.Pair ("Region", HealthCheckRegion.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.status_report (fun f ->
               "StatusReport", StatusReport.to_json f)
         ; Aws.Util.option_map v.i_p_address (fun f -> "IPAddress", String.to_json f)
         ; Aws.Util.option_map v.region (fun f -> "Region", HealthCheckRegion.to_json f)
         ])

  let of_json j =
    { region = Aws.Util.option_map (Aws.Json.lookup j "Region") HealthCheckRegion.of_json
    ; i_p_address = Aws.Util.option_map (Aws.Json.lookup j "IPAddress") String.of_json
    ; status_report =
        Aws.Util.option_map (Aws.Json.lookup j "StatusReport") StatusReport.of_json
    }
end

module DeleteReusableDelegationSetRequest = struct
  type t = { id : String.t }

  let make ~id () = { id }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt [ Some (Aws.Query.Pair ("Id", String.to_query v.id)) ])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [ Some ("Id", String.to_json v.id) ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id")) }
end

module TrafficPolicyAlreadyExists = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module NoSuchHostedZone = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module GetQueryLoggingConfigRequest = struct
  type t = { id : String.t }

  let make ~id () = { id }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt [ Some (Aws.Query.Pair ("Id", String.to_query v.id)) ])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [ Some ("Id", String.to_json v.id) ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id")) }
end

module CreateVPCAssociationAuthorizationResponse = struct
  type t =
    { hosted_zone_id : String.t
    ; v_p_c : VPC.t
    }

  let make ~hosted_zone_id ~v_p_c () = { hosted_zone_id; v_p_c }

  let parse xml =
    Some
      { hosted_zone_id =
          Aws.Xml.required
            "HostedZoneId"
            (Aws.Util.option_bind (Aws.Xml.member "HostedZoneId" xml) String.parse)
      ; v_p_c =
          Aws.Xml.required
            "VPC"
            (Aws.Util.option_bind (Aws.Xml.member "VPC" xml) VPC.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("VPC", VPC.to_query v.v_p_c))
         ; Some (Aws.Query.Pair ("HostedZoneId", String.to_query v.hosted_zone_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("VPC", VPC.to_json v.v_p_c)
         ; Some ("HostedZoneId", String.to_json v.hosted_zone_id)
         ])

  let of_json j =
    { hosted_zone_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZoneId"))
    ; v_p_c = VPC.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VPC"))
    }
end

module ListVPCAssociationAuthorizationsResponse = struct
  type t =
    { hosted_zone_id : String.t
    ; next_token : String.t option
    ; v_p_cs : VPCs.t
    }

  let make ~hosted_zone_id ?next_token ~v_p_cs () = { hosted_zone_id; next_token; v_p_cs }

  let parse xml =
    Some
      { hosted_zone_id =
          Aws.Xml.required
            "HostedZoneId"
            (Aws.Util.option_bind (Aws.Xml.member "HostedZoneId" xml) String.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      ; v_p_cs =
          Aws.Xml.required
            "VPCs"
            (Aws.Util.option_bind (Aws.Xml.member "VPCs" xml) VPCs.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("VPCs.member", VPCs.to_query v.v_p_cs))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some (Aws.Query.Pair ("HostedZoneId", String.to_query v.hosted_zone_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("VPCs", VPCs.to_json v.v_p_cs)
         ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("HostedZoneId", String.to_json v.hosted_zone_id)
         ])

  let of_json j =
    { hosted_zone_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZoneId"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    ; v_p_cs = VPCs.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VPCs"))
    }
end

module GetHealthCheckCountResponse = struct
  type t = { health_check_count : Long.t }

  let make ~health_check_count () = { health_check_count }

  let parse xml =
    Some
      { health_check_count =
          Aws.Xml.required
            "HealthCheckCount"
            (Aws.Util.option_bind (Aws.Xml.member "HealthCheckCount" xml) Long.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("HealthCheckCount", Long.to_query v.health_check_count))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("HealthCheckCount", Long.to_json v.health_check_count) ])

  let of_json j =
    { health_check_count =
        Long.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheckCount"))
    }
end

module ResettableElementNameList = struct
  type t = ResettableElementName.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map ResettableElementName.parse (Aws.Xml.members "ResettableElementName" xml))

  let to_query v = Aws.Query.to_query_list ResettableElementName.to_query v

  let to_json v = `List (List.map ResettableElementName.to_json v)

  let of_json j = Aws.Json.to_list ResettableElementName.of_json j
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
          Aws.Util.option_bind
            (Aws.Xml.member "FullyQualifiedDomainName" xml)
            String.parse
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
          Aws.Util.option_bind
            (Aws.Xml.member "AlarmIdentifier" xml)
            AlarmIdentifier.parse
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
                ( "ResetElements.member"
                , ResettableElementNameList.to_query v.reset_elements ))
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
         ; Aws.Util.option_map v.port (fun f ->
               Aws.Query.Pair ("Port", Integer.to_query f))
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
    ; resource_path =
        Aws.Util.option_map (Aws.Json.lookup j "ResourcePath") String.of_json
    ; fully_qualified_domain_name =
        Aws.Util.option_map (Aws.Json.lookup j "FullyQualifiedDomainName") String.of_json
    ; search_string =
        Aws.Util.option_map (Aws.Json.lookup j "SearchString") String.of_json
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
        HealthCheckRegionList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Regions"))
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
end

module TooManyVPCAssociationAuthorizations = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module GetHostedZoneCountRequest = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ListTrafficPolicyVersionsRequest = struct
  type t =
    { id : String.t
    ; traffic_policy_version_marker : String.t option
    ; max_items : String.t option
    }

  let make ~id ?traffic_policy_version_marker ?max_items () =
    { id; traffic_policy_version_marker; max_items }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; traffic_policy_version_marker =
          Aws.Util.option_bind (Aws.Xml.member "trafficpolicyversion" xml) String.parse
      ; max_items = Aws.Util.option_bind (Aws.Xml.member "maxitems" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_items (fun f ->
               Aws.Query.Pair ("maxitems", String.to_query f))
         ; Aws.Util.option_map v.traffic_policy_version_marker (fun f ->
               Aws.Query.Pair ("trafficpolicyversion", String.to_query f))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_items (fun f -> "maxitems", String.to_json f)
         ; Aws.Util.option_map v.traffic_policy_version_marker (fun f ->
               "trafficpolicyversion", String.to_json f)
         ; Some ("Id", String.to_json v.id)
         ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; traffic_policy_version_marker =
        Aws.Util.option_map (Aws.Json.lookup j "trafficpolicyversion") String.of_json
    ; max_items = Aws.Util.option_map (Aws.Json.lookup j "maxitems") String.of_json
    }
end

module NotAuthorizedException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
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
      { continent_code =
          Aws.Util.option_bind (Aws.Xml.member "continentcode" xml) String.parse
      ; country_code =
          Aws.Util.option_bind (Aws.Xml.member "countrycode" xml) String.parse
      ; subdivision_code =
          Aws.Util.option_bind (Aws.Xml.member "subdivisioncode" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.subdivision_code (fun f ->
               Aws.Query.Pair ("subdivisioncode", String.to_query f))
         ; Aws.Util.option_map v.country_code (fun f ->
               Aws.Query.Pair ("countrycode", String.to_query f))
         ; Aws.Util.option_map v.continent_code (fun f ->
               Aws.Query.Pair ("continentcode", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.subdivision_code (fun f ->
               "subdivisioncode", String.to_json f)
         ; Aws.Util.option_map v.country_code (fun f -> "countrycode", String.to_json f)
         ; Aws.Util.option_map v.continent_code (fun f ->
               "continentcode", String.to_json f)
         ])

  let of_json j =
    { continent_code =
        Aws.Util.option_map (Aws.Json.lookup j "continentcode") String.of_json
    ; country_code = Aws.Util.option_map (Aws.Json.lookup j "countrycode") String.of_json
    ; subdivision_code =
        Aws.Util.option_map (Aws.Json.lookup j "subdivisioncode") String.of_json
    }
end

module QueryLoggingConfigAlreadyExists = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module ListTrafficPolicyInstancesByPolicyRequest = struct
  type t =
    { traffic_policy_id : String.t
    ; traffic_policy_version : Integer.t
    ; hosted_zone_id_marker : String.t option
    ; traffic_policy_instance_name_marker : String.t option
    ; traffic_policy_instance_type_marker : RRType.t option
    ; max_items : String.t option
    }

  let make
      ~traffic_policy_id
      ~traffic_policy_version
      ?hosted_zone_id_marker
      ?traffic_policy_instance_name_marker
      ?traffic_policy_instance_type_marker
      ?max_items
      () =
    { traffic_policy_id
    ; traffic_policy_version
    ; hosted_zone_id_marker
    ; traffic_policy_instance_name_marker
    ; traffic_policy_instance_type_marker
    ; max_items
    }

  let parse xml =
    Some
      { traffic_policy_id =
          Aws.Xml.required
            "id"
            (Aws.Util.option_bind (Aws.Xml.member "id" xml) String.parse)
      ; traffic_policy_version =
          Aws.Xml.required
            "version"
            (Aws.Util.option_bind (Aws.Xml.member "version" xml) Integer.parse)
      ; hosted_zone_id_marker =
          Aws.Util.option_bind (Aws.Xml.member "hostedzoneid" xml) String.parse
      ; traffic_policy_instance_name_marker =
          Aws.Util.option_bind
            (Aws.Xml.member "trafficpolicyinstancename" xml)
            String.parse
      ; traffic_policy_instance_type_marker =
          Aws.Util.option_bind
            (Aws.Xml.member "trafficpolicyinstancetype" xml)
            RRType.parse
      ; max_items = Aws.Util.option_bind (Aws.Xml.member "maxitems" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_items (fun f ->
               Aws.Query.Pair ("maxitems", String.to_query f))
         ; Aws.Util.option_map v.traffic_policy_instance_type_marker (fun f ->
               Aws.Query.Pair ("trafficpolicyinstancetype", RRType.to_query f))
         ; Aws.Util.option_map v.traffic_policy_instance_name_marker (fun f ->
               Aws.Query.Pair ("trafficpolicyinstancename", String.to_query f))
         ; Aws.Util.option_map v.hosted_zone_id_marker (fun f ->
               Aws.Query.Pair ("hostedzoneid", String.to_query f))
         ; Some (Aws.Query.Pair ("version", Integer.to_query v.traffic_policy_version))
         ; Some (Aws.Query.Pair ("id", String.to_query v.traffic_policy_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_items (fun f -> "maxitems", String.to_json f)
         ; Aws.Util.option_map v.traffic_policy_instance_type_marker (fun f ->
               "trafficpolicyinstancetype", RRType.to_json f)
         ; Aws.Util.option_map v.traffic_policy_instance_name_marker (fun f ->
               "trafficpolicyinstancename", String.to_json f)
         ; Aws.Util.option_map v.hosted_zone_id_marker (fun f ->
               "hostedzoneid", String.to_json f)
         ; Some ("version", Integer.to_json v.traffic_policy_version)
         ; Some ("id", String.to_json v.traffic_policy_id)
         ])

  let of_json j =
    { traffic_policy_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "id"))
    ; traffic_policy_version =
        Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "version"))
    ; hosted_zone_id_marker =
        Aws.Util.option_map (Aws.Json.lookup j "hostedzoneid") String.of_json
    ; traffic_policy_instance_name_marker =
        Aws.Util.option_map (Aws.Json.lookup j "trafficpolicyinstancename") String.of_json
    ; traffic_policy_instance_type_marker =
        Aws.Util.option_map (Aws.Json.lookup j "trafficpolicyinstancetype") RRType.of_json
    ; max_items = Aws.Util.option_map (Aws.Json.lookup j "maxitems") String.of_json
    }
end

module InvalidChangeBatch = struct
  type t =
    { messages : ErrorMessages.t
    ; message : String.t option
    }

  let make ?(messages = []) ?message () = { messages; message }

  let parse xml =
    Some
      { messages =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "messages" xml) ErrorMessages.parse)
      ; message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ; Some (Aws.Query.Pair ("messages.member", ErrorMessages.to_query v.messages))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f)
         ; Some ("messages", ErrorMessages.to_json v.messages)
         ])

  let of_json j =
    { messages =
        ErrorMessages.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "messages"))
    ; message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json
    }
end

module ChangeTagsForResourceResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ChangeResourceRecordSetsResponse = struct
  type t = { change_info : ChangeInfo.t }

  let make ~change_info () = { change_info }

  let parse xml =
    Some
      { change_info =
          Aws.Xml.required
            "ChangeInfo"
            (Aws.Util.option_bind (Aws.Xml.member "ChangeInfo" xml) ChangeInfo.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("ChangeInfo", ChangeInfo.to_query v.change_info)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("ChangeInfo", ChangeInfo.to_json v.change_info) ])

  let of_json j =
    { change_info =
        ChangeInfo.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ChangeInfo"))
    }
end

module GetReusableDelegationSetResponse = struct
  type t = { delegation_set : DelegationSet.t }

  let make ~delegation_set () = { delegation_set }

  let parse xml =
    Some
      { delegation_set =
          Aws.Xml.required
            "DelegationSet"
            (Aws.Util.option_bind
               (Aws.Xml.member "DelegationSet" xml)
               DelegationSet.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("DelegationSet", DelegationSet.to_query v.delegation_set))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("DelegationSet", DelegationSet.to_json v.delegation_set) ])

  let of_json j =
    { delegation_set =
        DelegationSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DelegationSet"))
    }
end

module CreateQueryLoggingConfigRequest = struct
  type t =
    { hosted_zone_id : String.t
    ; cloud_watch_logs_log_group_arn : String.t
    }

  let make ~hosted_zone_id ~cloud_watch_logs_log_group_arn () =
    { hosted_zone_id; cloud_watch_logs_log_group_arn }

  let parse xml =
    Some
      { hosted_zone_id =
          Aws.Xml.required
            "HostedZoneId"
            (Aws.Util.option_bind (Aws.Xml.member "HostedZoneId" xml) String.parse)
      ; cloud_watch_logs_log_group_arn =
          Aws.Xml.required
            "CloudWatchLogsLogGroupArn"
            (Aws.Util.option_bind
               (Aws.Xml.member "CloudWatchLogsLogGroupArn" xml)
               String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "CloudWatchLogsLogGroupArn"
                , String.to_query v.cloud_watch_logs_log_group_arn ))
         ; Some (Aws.Query.Pair ("HostedZoneId", String.to_query v.hosted_zone_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ("CloudWatchLogsLogGroupArn", String.to_json v.cloud_watch_logs_log_group_arn)
         ; Some ("HostedZoneId", String.to_json v.hosted_zone_id)
         ])

  let of_json j =
    { hosted_zone_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZoneId"))
    ; cloud_watch_logs_log_group_arn =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CloudWatchLogsLogGroupArn"))
    }
end

module NoSuchChange = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module TestDNSAnswerRequest = struct
  type t =
    { hosted_zone_id : String.t
    ; record_name : String.t
    ; record_type : RRType.t
    ; resolver_i_p : String.t option
    ; e_d_n_s0_client_subnet_i_p : String.t option
    ; e_d_n_s0_client_subnet_mask : String.t option
    }

  let make
      ~hosted_zone_id
      ~record_name
      ~record_type
      ?resolver_i_p
      ?e_d_n_s0_client_subnet_i_p
      ?e_d_n_s0_client_subnet_mask
      () =
    { hosted_zone_id
    ; record_name
    ; record_type
    ; resolver_i_p
    ; e_d_n_s0_client_subnet_i_p
    ; e_d_n_s0_client_subnet_mask
    }

  let parse xml =
    Some
      { hosted_zone_id =
          Aws.Xml.required
            "hostedzoneid"
            (Aws.Util.option_bind (Aws.Xml.member "hostedzoneid" xml) String.parse)
      ; record_name =
          Aws.Xml.required
            "recordname"
            (Aws.Util.option_bind (Aws.Xml.member "recordname" xml) String.parse)
      ; record_type =
          Aws.Xml.required
            "recordtype"
            (Aws.Util.option_bind (Aws.Xml.member "recordtype" xml) RRType.parse)
      ; resolver_i_p = Aws.Util.option_bind (Aws.Xml.member "resolverip" xml) String.parse
      ; e_d_n_s0_client_subnet_i_p =
          Aws.Util.option_bind (Aws.Xml.member "edns0clientsubnetip" xml) String.parse
      ; e_d_n_s0_client_subnet_mask =
          Aws.Util.option_bind (Aws.Xml.member "edns0clientsubnetmask" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.e_d_n_s0_client_subnet_mask (fun f ->
               Aws.Query.Pair ("edns0clientsubnetmask", String.to_query f))
         ; Aws.Util.option_map v.e_d_n_s0_client_subnet_i_p (fun f ->
               Aws.Query.Pair ("edns0clientsubnetip", String.to_query f))
         ; Aws.Util.option_map v.resolver_i_p (fun f ->
               Aws.Query.Pair ("resolverip", String.to_query f))
         ; Some (Aws.Query.Pair ("recordtype", RRType.to_query v.record_type))
         ; Some (Aws.Query.Pair ("recordname", String.to_query v.record_name))
         ; Some (Aws.Query.Pair ("hostedzoneid", String.to_query v.hosted_zone_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.e_d_n_s0_client_subnet_mask (fun f ->
               "edns0clientsubnetmask", String.to_json f)
         ; Aws.Util.option_map v.e_d_n_s0_client_subnet_i_p (fun f ->
               "edns0clientsubnetip", String.to_json f)
         ; Aws.Util.option_map v.resolver_i_p (fun f -> "resolverip", String.to_json f)
         ; Some ("recordtype", RRType.to_json v.record_type)
         ; Some ("recordname", String.to_json v.record_name)
         ; Some ("hostedzoneid", String.to_json v.hosted_zone_id)
         ])

  let of_json j =
    { hosted_zone_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "hostedzoneid"))
    ; record_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "recordname"))
    ; record_type =
        RRType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "recordtype"))
    ; resolver_i_p = Aws.Util.option_map (Aws.Json.lookup j "resolverip") String.of_json
    ; e_d_n_s0_client_subnet_i_p =
        Aws.Util.option_map (Aws.Json.lookup j "edns0clientsubnetip") String.of_json
    ; e_d_n_s0_client_subnet_mask =
        Aws.Util.option_map (Aws.Json.lookup j "edns0clientsubnetmask") String.of_json
    }
end

module HealthCheckObservations = struct
  type t = HealthCheckObservation.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map
         HealthCheckObservation.parse
         (Aws.Xml.members "HealthCheckObservation" xml))

  let to_query v = Aws.Query.to_query_list HealthCheckObservation.to_query v

  let to_json v = `List (List.map HealthCheckObservation.to_json v)

  let of_json j = Aws.Json.to_list HealthCheckObservation.of_json j
end

module CheckerIpRanges = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
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
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; start_record_name = Aws.Util.option_bind (Aws.Xml.member "name" xml) String.parse
      ; start_record_type = Aws.Util.option_bind (Aws.Xml.member "type" xml) RRType.parse
      ; start_record_identifier =
          Aws.Util.option_bind (Aws.Xml.member "identifier" xml) String.parse
      ; max_items = Aws.Util.option_bind (Aws.Xml.member "maxitems" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_items (fun f ->
               Aws.Query.Pair ("maxitems", String.to_query f))
         ; Aws.Util.option_map v.start_record_identifier (fun f ->
               Aws.Query.Pair ("identifier", String.to_query f))
         ; Aws.Util.option_map v.start_record_type (fun f ->
               Aws.Query.Pair ("type", RRType.to_query f))
         ; Aws.Util.option_map v.start_record_name (fun f ->
               Aws.Query.Pair ("name", String.to_query f))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.hosted_zone_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_items (fun f -> "maxitems", String.to_json f)
         ; Aws.Util.option_map v.start_record_identifier (fun f ->
               "identifier", String.to_json f)
         ; Aws.Util.option_map v.start_record_type (fun f -> "type", RRType.to_json f)
         ; Aws.Util.option_map v.start_record_name (fun f -> "name", String.to_json f)
         ; Some ("Id", String.to_json v.hosted_zone_id)
         ])

  let of_json j =
    { hosted_zone_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; start_record_name = Aws.Util.option_map (Aws.Json.lookup j "name") String.of_json
    ; start_record_type = Aws.Util.option_map (Aws.Json.lookup j "type") RRType.of_json
    ; start_record_identifier =
        Aws.Util.option_map (Aws.Json.lookup j "identifier") String.of_json
    ; max_items = Aws.Util.option_map (Aws.Json.lookup j "maxitems") String.of_json
    }
end

module InvalidPaginationToken = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module GetHealthCheckRequest = struct
  type t = { health_check_id : String.t }

  let make ~health_check_id () = { health_check_id }

  let parse xml =
    Some
      { health_check_id =
          Aws.Xml.required
            "HealthCheckId"
            (Aws.Util.option_bind (Aws.Xml.member "HealthCheckId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("HealthCheckId", String.to_query v.health_check_id)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("HealthCheckId", String.to_json v.health_check_id) ])

  let of_json j =
    { health_check_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheckId"))
    }
end

module NoSuchTrafficPolicyInstance = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module TooManyTrafficPolicyVersionsForCurrentPolicy = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module PublicZoneVPCAssociation = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module LimitsExceeded = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module GetTrafficPolicyResponse = struct
  type t = { traffic_policy : TrafficPolicy.t }

  let make ~traffic_policy () = { traffic_policy }

  let parse xml =
    Some
      { traffic_policy =
          Aws.Xml.required
            "TrafficPolicy"
            (Aws.Util.option_bind
               (Aws.Xml.member "TrafficPolicy" xml)
               TrafficPolicy.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("TrafficPolicy", TrafficPolicy.to_query v.traffic_policy))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("TrafficPolicy", TrafficPolicy.to_json v.traffic_policy) ])

  let of_json j =
    { traffic_policy =
        TrafficPolicy.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicy"))
    }
end

module GetHealthCheckLastFailureReasonRequest = struct
  type t = { health_check_id : String.t }

  let make ~health_check_id () = { health_check_id }

  let parse xml =
    Some
      { health_check_id =
          Aws.Xml.required
            "HealthCheckId"
            (Aws.Util.option_bind (Aws.Xml.member "HealthCheckId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("HealthCheckId", String.to_query v.health_check_id)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("HealthCheckId", String.to_json v.health_check_id) ])

  let of_json j =
    { health_check_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheckId"))
    }
end

module GetCheckerIpRangesRequest = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module HostedZoneNotEmpty = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
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
          Aws.Xml.required
            "ResourceType"
            (Aws.Util.option_bind
               (Aws.Xml.member "ResourceType" xml)
               TagResourceType.parse)
      ; resource_id =
          Aws.Xml.required
            "ResourceId"
            (Aws.Util.option_bind (Aws.Xml.member "ResourceId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("ResourceId", String.to_query v.resource_id))
         ; Some
             (Aws.Query.Pair ("ResourceType", TagResourceType.to_query v.resource_type))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ResourceId", String.to_json v.resource_id)
         ; Some ("ResourceType", TagResourceType.to_json v.resource_type)
         ])

  let of_json j =
    { resource_type =
        TagResourceType.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceType"))
    ; resource_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceId"))
    }
end

module ListVPCAssociationAuthorizationsRequest = struct
  type t =
    { hosted_zone_id : String.t
    ; next_token : String.t option
    ; max_results : String.t option
    }

  let make ~hosted_zone_id ?next_token ?max_results () =
    { hosted_zone_id; next_token; max_results }

  let parse xml =
    Some
      { hosted_zone_id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "nexttoken" xml) String.parse
      ; max_results = Aws.Util.option_bind (Aws.Xml.member "maxresults" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_results (fun f ->
               Aws.Query.Pair ("maxresults", String.to_query f))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("nexttoken", String.to_query f))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.hosted_zone_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_results (fun f -> "maxresults", String.to_json f)
         ; Aws.Util.option_map v.next_token (fun f -> "nexttoken", String.to_json f)
         ; Some ("Id", String.to_json v.hosted_zone_id)
         ])

  let of_json j =
    { hosted_zone_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nexttoken") String.of_json
    ; max_results = Aws.Util.option_map (Aws.Json.lookup j "maxresults") String.of_json
    }
end

module CreateTrafficPolicyInstanceRequest = struct
  type t =
    { hosted_zone_id : String.t
    ; name : String.t
    ; t_t_l : Long.t
    ; traffic_policy_id : String.t
    ; traffic_policy_version : Integer.t
    }

  let make ~hosted_zone_id ~name ~t_t_l ~traffic_policy_id ~traffic_policy_version () =
    { hosted_zone_id; name; t_t_l; traffic_policy_id; traffic_policy_version }

  let parse xml =
    Some
      { hosted_zone_id =
          Aws.Xml.required
            "HostedZoneId"
            (Aws.Util.option_bind (Aws.Xml.member "HostedZoneId" xml) String.parse)
      ; name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      ; t_t_l =
          Aws.Xml.required
            "TTL"
            (Aws.Util.option_bind (Aws.Xml.member "TTL" xml) Long.parse)
      ; traffic_policy_id =
          Aws.Xml.required
            "TrafficPolicyId"
            (Aws.Util.option_bind (Aws.Xml.member "TrafficPolicyId" xml) String.parse)
      ; traffic_policy_version =
          Aws.Xml.required
            "TrafficPolicyVersion"
            (Aws.Util.option_bind
               (Aws.Xml.member "TrafficPolicyVersion" xml)
               Integer.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("TrafficPolicyVersion", Integer.to_query v.traffic_policy_version))
         ; Some (Aws.Query.Pair ("TrafficPolicyId", String.to_query v.traffic_policy_id))
         ; Some (Aws.Query.Pair ("TTL", Long.to_query v.t_t_l))
         ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
         ; Some (Aws.Query.Pair ("HostedZoneId", String.to_query v.hosted_zone_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("TrafficPolicyVersion", Integer.to_json v.traffic_policy_version)
         ; Some ("TrafficPolicyId", String.to_json v.traffic_policy_id)
         ; Some ("TTL", Long.to_json v.t_t_l)
         ; Some ("Name", String.to_json v.name)
         ; Some ("HostedZoneId", String.to_json v.hosted_zone_id)
         ])

  let of_json j =
    { hosted_zone_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZoneId"))
    ; name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    ; t_t_l = Long.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TTL"))
    ; traffic_policy_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyId"))
    ; traffic_policy_version =
        Integer.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyVersion"))
    }
end

module GetHealthCheckStatusRequest = struct
  type t = { health_check_id : String.t }

  let make ~health_check_id () = { health_check_id }

  let parse xml =
    Some
      { health_check_id =
          Aws.Xml.required
            "HealthCheckId"
            (Aws.Util.option_bind (Aws.Xml.member "HealthCheckId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("HealthCheckId", String.to_query v.health_check_id)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("HealthCheckId", String.to_json v.health_check_id) ])

  let of_json j =
    { health_check_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheckId"))
    }
end

module GetHealthCheckResponse = struct
  type t = { health_check : HealthCheck.t }

  let make ~health_check () = { health_check }

  let parse xml =
    Some
      { health_check =
          Aws.Xml.required
            "HealthCheck"
            (Aws.Util.option_bind (Aws.Xml.member "HealthCheck" xml) HealthCheck.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("HealthCheck", HealthCheck.to_query v.health_check)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("HealthCheck", HealthCheck.to_json v.health_check) ])

  let of_json j =
    { health_check =
        HealthCheck.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheck"))
    }
end

module DeleteTrafficPolicyRequest = struct
  type t =
    { id : String.t
    ; version : Integer.t
    }

  let make ~id ~version () = { id; version }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; version =
          Aws.Xml.required
            "Version"
            (Aws.Util.option_bind (Aws.Xml.member "Version" xml) Integer.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Version", Integer.to_query v.version))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Version", Integer.to_json v.version); Some ("Id", String.to_json v.id) ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; version = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Version"))
    }
end

module NoSuchDelegationSet = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module InvalidInput = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module ThrottlingException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module ListTrafficPolicyInstancesResponse = struct
  type t =
    { traffic_policy_instances : TrafficPolicyInstances.t
    ; hosted_zone_id_marker : String.t option
    ; traffic_policy_instance_name_marker : String.t option
    ; traffic_policy_instance_type_marker : RRType.t option
    ; is_truncated : Boolean.t
    ; max_items : String.t
    }

  let make
      ~traffic_policy_instances
      ?hosted_zone_id_marker
      ?traffic_policy_instance_name_marker
      ?traffic_policy_instance_type_marker
      ~is_truncated
      ~max_items
      () =
    { traffic_policy_instances
    ; hosted_zone_id_marker
    ; traffic_policy_instance_name_marker
    ; traffic_policy_instance_type_marker
    ; is_truncated
    ; max_items
    }

  let parse xml =
    Some
      { traffic_policy_instances =
          Aws.Xml.required
            "TrafficPolicyInstances"
            (Aws.Util.option_bind
               (Aws.Xml.member "TrafficPolicyInstances" xml)
               TrafficPolicyInstances.parse)
      ; hosted_zone_id_marker =
          Aws.Util.option_bind (Aws.Xml.member "HostedZoneIdMarker" xml) String.parse
      ; traffic_policy_instance_name_marker =
          Aws.Util.option_bind
            (Aws.Xml.member "TrafficPolicyInstanceNameMarker" xml)
            String.parse
      ; traffic_policy_instance_type_marker =
          Aws.Util.option_bind
            (Aws.Xml.member "TrafficPolicyInstanceTypeMarker" xml)
            RRType.parse
      ; is_truncated =
          Aws.Xml.required
            "IsTruncated"
            (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml) Boolean.parse)
      ; max_items =
          Aws.Xml.required
            "MaxItems"
            (Aws.Util.option_bind (Aws.Xml.member "MaxItems" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("MaxItems", String.to_query v.max_items))
         ; Some (Aws.Query.Pair ("IsTruncated", Boolean.to_query v.is_truncated))
         ; Aws.Util.option_map v.traffic_policy_instance_type_marker (fun f ->
               Aws.Query.Pair ("TrafficPolicyInstanceTypeMarker", RRType.to_query f))
         ; Aws.Util.option_map v.traffic_policy_instance_name_marker (fun f ->
               Aws.Query.Pair ("TrafficPolicyInstanceNameMarker", String.to_query f))
         ; Aws.Util.option_map v.hosted_zone_id_marker (fun f ->
               Aws.Query.Pair ("HostedZoneIdMarker", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "TrafficPolicyInstances.member"
                , TrafficPolicyInstances.to_query v.traffic_policy_instances ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("MaxItems", String.to_json v.max_items)
         ; Some ("IsTruncated", Boolean.to_json v.is_truncated)
         ; Aws.Util.option_map v.traffic_policy_instance_type_marker (fun f ->
               "TrafficPolicyInstanceTypeMarker", RRType.to_json f)
         ; Aws.Util.option_map v.traffic_policy_instance_name_marker (fun f ->
               "TrafficPolicyInstanceNameMarker", String.to_json f)
         ; Aws.Util.option_map v.hosted_zone_id_marker (fun f ->
               "HostedZoneIdMarker", String.to_json f)
         ; Some
             ( "TrafficPolicyInstances"
             , TrafficPolicyInstances.to_json v.traffic_policy_instances )
         ])

  let of_json j =
    { traffic_policy_instances =
        TrafficPolicyInstances.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyInstances"))
    ; hosted_zone_id_marker =
        Aws.Util.option_map (Aws.Json.lookup j "HostedZoneIdMarker") String.of_json
    ; traffic_policy_instance_name_marker =
        Aws.Util.option_map
          (Aws.Json.lookup j "TrafficPolicyInstanceNameMarker")
          String.of_json
    ; traffic_policy_instance_type_marker =
        Aws.Util.option_map
          (Aws.Json.lookup j "TrafficPolicyInstanceTypeMarker")
          RRType.of_json
    ; is_truncated =
        Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IsTruncated"))
    ; max_items = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxItems"))
    }
end

module ListHostedZonesByVPCResponse = struct
  type t =
    { hosted_zone_summaries : HostedZoneSummaries.t
    ; max_items : String.t
    ; next_token : String.t option
    }

  let make ~hosted_zone_summaries ~max_items ?next_token () =
    { hosted_zone_summaries; max_items; next_token }

  let parse xml =
    Some
      { hosted_zone_summaries =
          Aws.Xml.required
            "HostedZoneSummaries"
            (Aws.Util.option_bind
               (Aws.Xml.member "HostedZoneSummaries" xml)
               HostedZoneSummaries.parse)
      ; max_items =
          Aws.Xml.required
            "MaxItems"
            (Aws.Util.option_bind (Aws.Xml.member "MaxItems" xml) String.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some (Aws.Query.Pair ("MaxItems", String.to_query v.max_items))
         ; Some
             (Aws.Query.Pair
                ( "HostedZoneSummaries.member"
                , HostedZoneSummaries.to_query v.hosted_zone_summaries ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("MaxItems", String.to_json v.max_items)
         ; Some
             ("HostedZoneSummaries", HostedZoneSummaries.to_json v.hosted_zone_summaries)
         ])

  let of_json j =
    { hosted_zone_summaries =
        HostedZoneSummaries.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZoneSummaries"))
    ; max_items = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxItems"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module GetCheckerIpRangesResponse = struct
  type t = { checker_ip_ranges : CheckerIpRanges.t }

  let make ~checker_ip_ranges () = { checker_ip_ranges }

  let parse xml =
    Some
      { checker_ip_ranges =
          Aws.Xml.required
            "CheckerIpRanges"
            (Aws.Util.option_bind
               (Aws.Xml.member "CheckerIpRanges" xml)
               CheckerIpRanges.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("CheckerIpRanges.member", CheckerIpRanges.to_query v.checker_ip_ranges))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("CheckerIpRanges", CheckerIpRanges.to_json v.checker_ip_ranges) ])

  let of_json j =
    { checker_ip_ranges =
        CheckerIpRanges.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CheckerIpRanges"))
    }
end

module GetHealthCheckStatusResponse = struct
  type t = { health_check_observations : HealthCheckObservations.t }

  let make ~health_check_observations () = { health_check_observations }

  let parse xml =
    Some
      { health_check_observations =
          Aws.Xml.required
            "HealthCheckObservations"
            (Aws.Util.option_bind
               (Aws.Xml.member "HealthCheckObservations" xml)
               HealthCheckObservations.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "HealthCheckObservations.member"
                , HealthCheckObservations.to_query v.health_check_observations ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "HealthCheckObservations"
             , HealthCheckObservations.to_json v.health_check_observations )
         ])

  let of_json j =
    { health_check_observations =
        HealthCheckObservations.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheckObservations"))
    }
end

module GetQueryLoggingConfigResponse = struct
  type t = { query_logging_config : QueryLoggingConfig.t }

  let make ~query_logging_config () = { query_logging_config }

  let parse xml =
    Some
      { query_logging_config =
          Aws.Xml.required
            "QueryLoggingConfig"
            (Aws.Util.option_bind
               (Aws.Xml.member "QueryLoggingConfig" xml)
               QueryLoggingConfig.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("QueryLoggingConfig", QueryLoggingConfig.to_query v.query_logging_config))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("QueryLoggingConfig", QueryLoggingConfig.to_json v.query_logging_config)
         ])

  let of_json j =
    { query_logging_config =
        QueryLoggingConfig.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "QueryLoggingConfig"))
    }
end

module GetHostedZoneLimitRequest = struct
  type t =
    { type_ : HostedZoneLimitType.t
    ; hosted_zone_id : String.t
    }

  let make ~type_ ~hosted_zone_id () = { type_; hosted_zone_id }

  let parse xml =
    Some
      { type_ =
          Aws.Xml.required
            "Type"
            (Aws.Util.option_bind (Aws.Xml.member "Type" xml) HostedZoneLimitType.parse)
      ; hosted_zone_id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Id", String.to_query v.hosted_zone_id))
         ; Some (Aws.Query.Pair ("Type", HostedZoneLimitType.to_query v.type_))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Id", String.to_json v.hosted_zone_id)
         ; Some ("Type", HostedZoneLimitType.to_json v.type_)
         ])

  let of_json j =
    { type_ =
        HostedZoneLimitType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Type"))
    ; hosted_zone_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    }
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
          Aws.Xml.required
            "HostedZone"
            (Aws.Util.option_bind (Aws.Xml.member "HostedZone" xml) HostedZone.parse)
      ; change_info =
          Aws.Xml.required
            "ChangeInfo"
            (Aws.Util.option_bind (Aws.Xml.member "ChangeInfo" xml) ChangeInfo.parse)
      ; delegation_set =
          Aws.Xml.required
            "DelegationSet"
            (Aws.Util.option_bind
               (Aws.Xml.member "DelegationSet" xml)
               DelegationSet.parse)
      ; v_p_c = Aws.Util.option_bind (Aws.Xml.member "VPC" xml) VPC.parse
      ; location =
          Aws.Xml.required
            "Location"
            (Aws.Util.option_bind (Aws.Xml.member "Location" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Location", String.to_query v.location))
         ; Aws.Util.option_map v.v_p_c (fun f -> Aws.Query.Pair ("VPC", VPC.to_query f))
         ; Some
             (Aws.Query.Pair ("DelegationSet", DelegationSet.to_query v.delegation_set))
         ; Some (Aws.Query.Pair ("ChangeInfo", ChangeInfo.to_query v.change_info))
         ; Some (Aws.Query.Pair ("HostedZone", HostedZone.to_query v.hosted_zone))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Location", String.to_json v.location)
         ; Aws.Util.option_map v.v_p_c (fun f -> "VPC", VPC.to_json f)
         ; Some ("DelegationSet", DelegationSet.to_json v.delegation_set)
         ; Some ("ChangeInfo", ChangeInfo.to_json v.change_info)
         ; Some ("HostedZone", HostedZone.to_json v.hosted_zone)
         ])

  let of_json j =
    { hosted_zone =
        HostedZone.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZone"))
    ; change_info =
        ChangeInfo.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ChangeInfo"))
    ; delegation_set =
        DelegationSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DelegationSet"))
    ; v_p_c = Aws.Util.option_map (Aws.Json.lookup j "VPC") VPC.of_json
    ; location = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Location"))
    }
end

module InvalidArgument = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module DeleteTrafficPolicyInstanceResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ListReusableDelegationSetsRequest = struct
  type t =
    { marker : String.t option
    ; max_items : String.t option
    }

  let make ?marker ?max_items () = { marker; max_items }

  let parse xml =
    Some
      { marker = Aws.Util.option_bind (Aws.Xml.member "marker" xml) String.parse
      ; max_items = Aws.Util.option_bind (Aws.Xml.member "maxitems" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_items (fun f ->
               Aws.Query.Pair ("maxitems", String.to_query f))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_items (fun f -> "maxitems", String.to_json f)
         ; Aws.Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Aws.Util.option_map (Aws.Json.lookup j "marker") String.of_json
    ; max_items = Aws.Util.option_map (Aws.Json.lookup j "maxitems") String.of_json
    }
end

module GetTrafficPolicyInstanceRequest = struct
  type t = { id : String.t }

  let make ~id () = { id }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt [ Some (Aws.Query.Pair ("Id", String.to_query v.id)) ])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [ Some ("Id", String.to_json v.id) ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id")) }
end

module GetAccountLimitRequest = struct
  type t = { type_ : AccountLimitType.t }

  let make ~type_ () = { type_ }

  let parse xml =
    Some
      { type_ =
          Aws.Xml.required
            "Type"
            (Aws.Util.option_bind (Aws.Xml.member "Type" xml) AccountLimitType.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Type", AccountLimitType.to_query v.type_)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("Type", AccountLimitType.to_json v.type_) ])

  let of_json j =
    { type_ = AccountLimitType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Type"))
    }
end

module DelegationSetAlreadyCreated = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module GetHostedZoneRequest = struct
  type t = { id : String.t }

  let make ~id () = { id }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt [ Some (Aws.Query.Pair ("Id", String.to_query v.id)) ])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [ Some ("Id", String.to_json v.id) ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id")) }
end

module TooManyTrafficPolicyInstances = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module DeleteVPCAssociationAuthorizationRequest = struct
  type t =
    { hosted_zone_id : String.t
    ; v_p_c : VPC.t
    }

  let make ~hosted_zone_id ~v_p_c () = { hosted_zone_id; v_p_c }

  let parse xml =
    Some
      { hosted_zone_id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; v_p_c =
          Aws.Xml.required
            "VPC"
            (Aws.Util.option_bind (Aws.Xml.member "VPC" xml) VPC.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("VPC", VPC.to_query v.v_p_c))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.hosted_zone_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("VPC", VPC.to_json v.v_p_c)
         ; Some ("Id", String.to_json v.hosted_zone_id)
         ])

  let of_json j =
    { hosted_zone_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; v_p_c = VPC.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VPC"))
    }
end

module VPCAssociationAuthorizationNotFound = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module GetHealthCheckLastFailureReasonResponse = struct
  type t = { health_check_observations : HealthCheckObservations.t }

  let make ~health_check_observations () = { health_check_observations }

  let parse xml =
    Some
      { health_check_observations =
          Aws.Xml.required
            "HealthCheckObservations"
            (Aws.Util.option_bind
               (Aws.Xml.member "HealthCheckObservations" xml)
               HealthCheckObservations.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "HealthCheckObservations.member"
                , HealthCheckObservations.to_query v.health_check_observations ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "HealthCheckObservations"
             , HealthCheckObservations.to_json v.health_check_observations )
         ])

  let of_json j =
    { health_check_observations =
        HealthCheckObservations.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheckObservations"))
    }
end

module GetGeoLocationResponse = struct
  type t = { geo_location_details : GeoLocationDetails.t }

  let make ~geo_location_details () = { geo_location_details }

  let parse xml =
    Some
      { geo_location_details =
          Aws.Xml.required
            "GeoLocationDetails"
            (Aws.Util.option_bind
               (Aws.Xml.member "GeoLocationDetails" xml)
               GeoLocationDetails.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("GeoLocationDetails", GeoLocationDetails.to_query v.geo_location_details))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("GeoLocationDetails", GeoLocationDetails.to_json v.geo_location_details)
         ])

  let of_json j =
    { geo_location_details =
        GeoLocationDetails.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "GeoLocationDetails"))
    }
end

module GetChangeRequest = struct
  type t = { id : String.t }

  let make ~id () = { id }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt [ Some (Aws.Query.Pair ("Id", String.to_query v.id)) ])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [ Some ("Id", String.to_json v.id) ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id")) }
end

module DelegationSetAlreadyReusable = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module TrafficPolicies = struct
  type t = TrafficPolicy.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map TrafficPolicy.parse (Aws.Xml.members "TrafficPolicy" xml))

  let to_query v = Aws.Query.to_query_list TrafficPolicy.to_query v

  let to_json v = `List (List.map TrafficPolicy.to_json v)

  let of_json j = Aws.Json.to_list TrafficPolicy.of_json j
end

module ListTrafficPolicyVersionsResponse = struct
  type t =
    { traffic_policies : TrafficPolicies.t
    ; is_truncated : Boolean.t
    ; traffic_policy_version_marker : String.t
    ; max_items : String.t
    }

  let make ~traffic_policies ~is_truncated ~traffic_policy_version_marker ~max_items () =
    { traffic_policies; is_truncated; traffic_policy_version_marker; max_items }

  let parse xml =
    Some
      { traffic_policies =
          Aws.Xml.required
            "TrafficPolicies"
            (Aws.Util.option_bind
               (Aws.Xml.member "TrafficPolicies" xml)
               TrafficPolicies.parse)
      ; is_truncated =
          Aws.Xml.required
            "IsTruncated"
            (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml) Boolean.parse)
      ; traffic_policy_version_marker =
          Aws.Xml.required
            "TrafficPolicyVersionMarker"
            (Aws.Util.option_bind
               (Aws.Xml.member "TrafficPolicyVersionMarker" xml)
               String.parse)
      ; max_items =
          Aws.Xml.required
            "MaxItems"
            (Aws.Util.option_bind (Aws.Xml.member "MaxItems" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("MaxItems", String.to_query v.max_items))
         ; Some
             (Aws.Query.Pair
                ( "TrafficPolicyVersionMarker"
                , String.to_query v.traffic_policy_version_marker ))
         ; Some (Aws.Query.Pair ("IsTruncated", Boolean.to_query v.is_truncated))
         ; Some
             (Aws.Query.Pair
                ("TrafficPolicies.member", TrafficPolicies.to_query v.traffic_policies))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("MaxItems", String.to_json v.max_items)
         ; Some
             ("TrafficPolicyVersionMarker", String.to_json v.traffic_policy_version_marker)
         ; Some ("IsTruncated", Boolean.to_json v.is_truncated)
         ; Some ("TrafficPolicies", TrafficPolicies.to_json v.traffic_policies)
         ])

  let of_json j =
    { traffic_policies =
        TrafficPolicies.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicies"))
    ; is_truncated =
        Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IsTruncated"))
    ; traffic_policy_version_marker =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyVersionMarker"))
    ; max_items = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxItems"))
    }
end

module AssociateVPCWithHostedZoneResponse = struct
  type t = { change_info : ChangeInfo.t }

  let make ~change_info () = { change_info }

  let parse xml =
    Some
      { change_info =
          Aws.Xml.required
            "ChangeInfo"
            (Aws.Util.option_bind (Aws.Xml.member "ChangeInfo" xml) ChangeInfo.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("ChangeInfo", ChangeInfo.to_query v.change_info)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("ChangeInfo", ChangeInfo.to_json v.change_info) ])

  let of_json j =
    { change_info =
        ChangeInfo.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ChangeInfo"))
    }
end

module ListHealthChecksRequest = struct
  type t =
    { marker : String.t option
    ; max_items : String.t option
    }

  let make ?marker ?max_items () = { marker; max_items }

  let parse xml =
    Some
      { marker = Aws.Util.option_bind (Aws.Xml.member "marker" xml) String.parse
      ; max_items = Aws.Util.option_bind (Aws.Xml.member "maxitems" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_items (fun f ->
               Aws.Query.Pair ("maxitems", String.to_query f))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_items (fun f -> "maxitems", String.to_json f)
         ; Aws.Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Aws.Util.option_map (Aws.Json.lookup j "marker") String.of_json
    ; max_items = Aws.Util.option_map (Aws.Json.lookup j "maxitems") String.of_json
    }
end

module GetTrafficPolicyInstanceResponse = struct
  type t = { traffic_policy_instance : TrafficPolicyInstance.t }

  let make ~traffic_policy_instance () = { traffic_policy_instance }

  let parse xml =
    Some
      { traffic_policy_instance =
          Aws.Xml.required
            "TrafficPolicyInstance"
            (Aws.Util.option_bind
               (Aws.Xml.member "TrafficPolicyInstance" xml)
               TrafficPolicyInstance.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "TrafficPolicyInstance"
                , TrafficPolicyInstance.to_query v.traffic_policy_instance ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "TrafficPolicyInstance"
             , TrafficPolicyInstance.to_json v.traffic_policy_instance )
         ])

  let of_json j =
    { traffic_policy_instance =
        TrafficPolicyInstance.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyInstance"))
    }
end

module DeleteQueryLoggingConfigResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module HostedZoneNotFound = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module DeleteReusableDelegationSetResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
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
          Aws.Xml.required
            "HostedZones"
            (Aws.Util.option_bind (Aws.Xml.member "HostedZones" xml) HostedZones.parse)
      ; d_n_s_name = Aws.Util.option_bind (Aws.Xml.member "DNSName" xml) String.parse
      ; hosted_zone_id =
          Aws.Util.option_bind (Aws.Xml.member "HostedZoneId" xml) String.parse
      ; is_truncated =
          Aws.Xml.required
            "IsTruncated"
            (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml) Boolean.parse)
      ; next_d_n_s_name =
          Aws.Util.option_bind (Aws.Xml.member "NextDNSName" xml) String.parse
      ; next_hosted_zone_id =
          Aws.Util.option_bind (Aws.Xml.member "NextHostedZoneId" xml) String.parse
      ; max_items =
          Aws.Xml.required
            "MaxItems"
            (Aws.Util.option_bind (Aws.Xml.member "MaxItems" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("MaxItems", String.to_query v.max_items))
         ; Aws.Util.option_map v.next_hosted_zone_id (fun f ->
               Aws.Query.Pair ("NextHostedZoneId", String.to_query f))
         ; Aws.Util.option_map v.next_d_n_s_name (fun f ->
               Aws.Query.Pair ("NextDNSName", String.to_query f))
         ; Some (Aws.Query.Pair ("IsTruncated", Boolean.to_query v.is_truncated))
         ; Aws.Util.option_map v.hosted_zone_id (fun f ->
               Aws.Query.Pair ("HostedZoneId", String.to_query f))
         ; Aws.Util.option_map v.d_n_s_name (fun f ->
               Aws.Query.Pair ("DNSName", String.to_query f))
         ; Some
             (Aws.Query.Pair ("HostedZones.member", HostedZones.to_query v.hosted_zones))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("MaxItems", String.to_json v.max_items)
         ; Aws.Util.option_map v.next_hosted_zone_id (fun f ->
               "NextHostedZoneId", String.to_json f)
         ; Aws.Util.option_map v.next_d_n_s_name (fun f ->
               "NextDNSName", String.to_json f)
         ; Some ("IsTruncated", Boolean.to_json v.is_truncated)
         ; Aws.Util.option_map v.hosted_zone_id (fun f ->
               "HostedZoneId", String.to_json f)
         ; Aws.Util.option_map v.d_n_s_name (fun f -> "DNSName", String.to_json f)
         ; Some ("HostedZones", HostedZones.to_json v.hosted_zones)
         ])

  let of_json j =
    { hosted_zones =
        HostedZones.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZones"))
    ; d_n_s_name = Aws.Util.option_map (Aws.Json.lookup j "DNSName") String.of_json
    ; hosted_zone_id =
        Aws.Util.option_map (Aws.Json.lookup j "HostedZoneId") String.of_json
    ; is_truncated =
        Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IsTruncated"))
    ; next_d_n_s_name =
        Aws.Util.option_map (Aws.Json.lookup j "NextDNSName") String.of_json
    ; next_hosted_zone_id =
        Aws.Util.option_map (Aws.Json.lookup j "NextHostedZoneId") String.of_json
    ; max_items = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxItems"))
    }
end
