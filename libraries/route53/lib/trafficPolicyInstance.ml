open Aws.BaseTypes

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
          (Aws.Util.option_bind (Aws.Xml.member "TrafficPolicyVersion" xml) Integer.parse)
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
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyVersion"))
  ; traffic_policy_type =
      RRType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficPolicyType"))
  }
