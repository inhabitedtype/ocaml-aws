open Aws.BaseTypes

type t =
  { instance_id : String.t
  ; cpu_credits : String.t option
  }

let make ~instance_id ?cpu_credits () = { instance_id; cpu_credits }

let parse xml =
  Some
    { instance_id =
        Aws.Xml.required
          "InstanceId"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
    ; cpu_credits = Aws.Util.option_bind (Aws.Xml.member "CpuCredits" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cpu_credits (fun f ->
             Aws.Query.Pair ("CpuCredits", String.to_query f))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cpu_credits (fun f -> "CpuCredits", String.to_json f)
       ; Some ("InstanceId", String.to_json v.instance_id)
       ])

let of_json j =
  { instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  ; cpu_credits = Aws.Util.option_map (Aws.Json.lookup j "CpuCredits") String.of_json
  }
