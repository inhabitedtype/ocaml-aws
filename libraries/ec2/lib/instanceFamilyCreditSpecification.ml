open Aws.BaseTypes

type t =
  { instance_family : UnlimitedSupportedInstanceFamily.t option
  ; cpu_credits : String.t option
  }

let make ?instance_family ?cpu_credits () = { instance_family; cpu_credits }

let parse xml =
  Some
    { instance_family =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceFamily" xml)
          UnlimitedSupportedInstanceFamily.parse
    ; cpu_credits = Aws.Util.option_bind (Aws.Xml.member "cpuCredits" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cpu_credits (fun f ->
             Aws.Query.Pair ("CpuCredits", String.to_query f))
       ; Aws.Util.option_map v.instance_family (fun f ->
             Aws.Query.Pair ("InstanceFamily", UnlimitedSupportedInstanceFamily.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cpu_credits (fun f -> "cpuCredits", String.to_json f)
       ; Aws.Util.option_map v.instance_family (fun f ->
             "instanceFamily", UnlimitedSupportedInstanceFamily.to_json f)
       ])

let of_json j =
  { instance_family =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceFamily")
        UnlimitedSupportedInstanceFamily.of_json
  ; cpu_credits = Aws.Util.option_map (Aws.Json.lookup j "cpuCredits") String.of_json
  }
