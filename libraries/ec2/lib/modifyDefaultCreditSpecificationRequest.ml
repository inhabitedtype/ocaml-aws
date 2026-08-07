open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; instance_family : UnlimitedSupportedInstanceFamily.t
  ; cpu_credits : String.t
  }

let make ?dry_run ~instance_family ~cpu_credits () =
  { dry_run; instance_family; cpu_credits }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; instance_family =
        Aws.Xml.required
          "InstanceFamily"
          (Aws.Util.option_bind
             (Aws.Xml.member "InstanceFamily" xml)
             UnlimitedSupportedInstanceFamily.parse)
    ; cpu_credits =
        Aws.Xml.required
          "CpuCredits"
          (Aws.Util.option_bind (Aws.Xml.member "CpuCredits" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("CpuCredits", String.to_query v.cpu_credits))
       ; Some
           (Aws.Query.Pair
              ( "InstanceFamily"
              , UnlimitedSupportedInstanceFamily.to_query v.instance_family ))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("CpuCredits", String.to_json v.cpu_credits)
       ; Some
           ("InstanceFamily", UnlimitedSupportedInstanceFamily.to_json v.instance_family)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; instance_family =
      UnlimitedSupportedInstanceFamily.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceFamily"))
  ; cpu_credits = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CpuCredits"))
  }
