open Aws.BaseTypes

type t =
  { target : String.t
  ; interval : Integer.t
  ; timeout : Integer.t
  ; unhealthy_threshold : Integer.t
  ; healthy_threshold : Integer.t
  }

let make ~target ~interval ~timeout ~unhealthy_threshold ~healthy_threshold () =
  { target; interval; timeout; unhealthy_threshold; healthy_threshold }

let parse xml =
  Some
    { target =
        Aws.Xml.required
          "Target"
          (Aws.Util.option_bind (Aws.Xml.member "Target" xml) String.parse)
    ; interval =
        Aws.Xml.required
          "Interval"
          (Aws.Util.option_bind (Aws.Xml.member "Interval" xml) Integer.parse)
    ; timeout =
        Aws.Xml.required
          "Timeout"
          (Aws.Util.option_bind (Aws.Xml.member "Timeout" xml) Integer.parse)
    ; unhealthy_threshold =
        Aws.Xml.required
          "UnhealthyThreshold"
          (Aws.Util.option_bind (Aws.Xml.member "UnhealthyThreshold" xml) Integer.parse)
    ; healthy_threshold =
        Aws.Xml.required
          "HealthyThreshold"
          (Aws.Util.option_bind (Aws.Xml.member "HealthyThreshold" xml) Integer.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("HealthyThreshold", Integer.to_query v.healthy_threshold))
       ; Some
           (Aws.Query.Pair ("UnhealthyThreshold", Integer.to_query v.unhealthy_threshold))
       ; Some (Aws.Query.Pair ("Timeout", Integer.to_query v.timeout))
       ; Some (Aws.Query.Pair ("Interval", Integer.to_query v.interval))
       ; Some (Aws.Query.Pair ("Target", String.to_query v.target))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("HealthyThreshold", Integer.to_json v.healthy_threshold)
       ; Some ("UnhealthyThreshold", Integer.to_json v.unhealthy_threshold)
       ; Some ("Timeout", Integer.to_json v.timeout)
       ; Some ("Interval", Integer.to_json v.interval)
       ; Some ("Target", String.to_json v.target)
       ])

let of_json j =
  { target = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Target"))
  ; interval = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Interval"))
  ; timeout = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Timeout"))
  ; unhealthy_threshold =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UnhealthyThreshold"))
  ; healthy_threshold =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthyThreshold"))
  }
