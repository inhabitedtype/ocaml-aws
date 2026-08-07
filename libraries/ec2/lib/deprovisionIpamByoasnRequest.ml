open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; ipam_id : String.t
  ; asn : String.t
  }

let make ?dry_run ~ipam_id ~asn () = { dry_run; ipam_id; asn }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipam_id =
        Aws.Xml.required
          "IpamId"
          (Aws.Util.option_bind (Aws.Xml.member "IpamId" xml) String.parse)
    ; asn =
        Aws.Xml.required
          "Asn"
          (Aws.Util.option_bind (Aws.Xml.member "Asn" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Asn", String.to_query v.asn))
       ; Some (Aws.Query.Pair ("IpamId", String.to_query v.ipam_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Asn", String.to_json v.asn)
       ; Some ("IpamId", String.to_json v.ipam_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipam_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamId"))
  ; asn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Asn"))
  }
