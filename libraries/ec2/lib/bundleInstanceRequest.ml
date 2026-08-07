open Aws.BaseTypes

type t =
  { instance_id : String.t
  ; storage : Storage.t
  ; dry_run : Boolean.t option
  }

let make ~instance_id ~storage ?dry_run () = { instance_id; storage; dry_run }

let parse xml =
  Some
    { instance_id =
        Aws.Xml.required
          "InstanceId"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
    ; storage =
        Aws.Xml.required
          "Storage"
          (Aws.Util.option_bind (Aws.Xml.member "Storage" xml) Storage.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("Storage", Storage.to_query v.storage))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some ("Storage", Storage.to_json v.storage)
       ; Some ("InstanceId", String.to_json v.instance_id)
       ])

let of_json j =
  { instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  ; storage = Storage.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Storage"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
