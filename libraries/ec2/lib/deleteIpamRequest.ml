open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; ipam_id : String.t
  ; cascade : Boolean.t option
  }

let make ?dry_run ~ipam_id ?cascade () = { dry_run; ipam_id; cascade }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipam_id =
        Aws.Xml.required
          "IpamId"
          (Aws.Util.option_bind (Aws.Xml.member "IpamId" xml) String.parse)
    ; cascade = Aws.Util.option_bind (Aws.Xml.member "Cascade" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cascade (fun f ->
             Aws.Query.Pair ("Cascade", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("IpamId", String.to_query v.ipam_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cascade (fun f -> "Cascade", Boolean.to_json f)
       ; Some ("IpamId", String.to_json v.ipam_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipam_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamId"))
  ; cascade = Aws.Util.option_map (Aws.Json.lookup j "Cascade") Boolean.of_json
  }
