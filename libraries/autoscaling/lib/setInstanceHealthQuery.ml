open Aws.BaseTypes

type t =
  { instance_id : String.t
  ; health_status : String.t
  ; should_respect_grace_period : Boolean.t option
  }

let make ~instance_id ~health_status ?should_respect_grace_period () =
  { instance_id; health_status; should_respect_grace_period }

let parse xml =
  Some
    { instance_id =
        Aws.Xml.required
          "InstanceId"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
    ; health_status =
        Aws.Xml.required
          "HealthStatus"
          (Aws.Util.option_bind (Aws.Xml.member "HealthStatus" xml) String.parse)
    ; should_respect_grace_period =
        Aws.Util.option_bind (Aws.Xml.member "ShouldRespectGracePeriod" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.should_respect_grace_period (fun f ->
             Aws.Query.Pair ("ShouldRespectGracePeriod", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("HealthStatus", String.to_query v.health_status))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.should_respect_grace_period (fun f ->
             "ShouldRespectGracePeriod", Boolean.to_json f)
       ; Some ("HealthStatus", String.to_json v.health_status)
       ; Some ("InstanceId", String.to_json v.instance_id)
       ])

let of_json j =
  { instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  ; health_status =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthStatus"))
  ; should_respect_grace_period =
      Aws.Util.option_map (Aws.Json.lookup j "ShouldRespectGracePeriod") Boolean.of_json
  }
