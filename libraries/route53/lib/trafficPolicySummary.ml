open Aws.BaseTypes

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
           (Aws.Query.Pair ("TrafficPolicyCount", Integer.to_query v.traffic_policy_count))
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
