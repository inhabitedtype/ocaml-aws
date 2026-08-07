open Aws.BaseTypes

type t =
  { instance_ids : InstanceIdStringList.t
  ; additional_info : String.t option
  ; dry_run : Boolean.t option
  }

let make ~instance_ids ?additional_info ?dry_run () =
  { instance_ids; additional_info; dry_run }

let parse xml =
  Some
    { instance_ids =
        Aws.Xml.required
          "InstanceId"
          (Aws.Util.option_bind
             (Aws.Xml.member "InstanceId" xml)
             InstanceIdStringList.parse)
    ; additional_info =
        Aws.Util.option_bind (Aws.Xml.member "additionalInfo" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.additional_info (fun f ->
             Aws.Query.Pair ("AdditionalInfo", String.to_query f))
       ; Some
           (Aws.Query.Pair ("InstanceId", InstanceIdStringList.to_query v.instance_ids))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.additional_info (fun f ->
             "additionalInfo", String.to_json f)
       ; Some ("InstanceId", InstanceIdStringList.to_json v.instance_ids)
       ])

let of_json j =
  { instance_ids =
      InstanceIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  ; additional_info =
      Aws.Util.option_map (Aws.Json.lookup j "additionalInfo") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
