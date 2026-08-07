open Aws.BaseTypes

type t =
  { group_name : String.t
  ; opt_in_status : ModifyAvailabilityZoneOptInStatus.t
  ; dry_run : Boolean.t option
  }

let make ~group_name ~opt_in_status ?dry_run () = { group_name; opt_in_status; dry_run }

let parse xml =
  Some
    { group_name =
        Aws.Xml.required
          "GroupName"
          (Aws.Util.option_bind (Aws.Xml.member "GroupName" xml) String.parse)
    ; opt_in_status =
        Aws.Xml.required
          "OptInStatus"
          (Aws.Util.option_bind
             (Aws.Xml.member "OptInStatus" xml)
             ModifyAvailabilityZoneOptInStatus.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("OptInStatus", ModifyAvailabilityZoneOptInStatus.to_query v.opt_in_status))
       ; Some (Aws.Query.Pair ("GroupName", String.to_query v.group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("OptInStatus", ModifyAvailabilityZoneOptInStatus.to_json v.opt_in_status)
       ; Some ("GroupName", String.to_json v.group_name)
       ])

let of_json j =
  { group_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GroupName"))
  ; opt_in_status =
      ModifyAvailabilityZoneOptInStatus.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "OptInStatus"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
