open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; public_ip : String.t
  }

let make ?dry_run ~public_ip () = { dry_run; public_ip }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; public_ip =
        Aws.Xml.required
          "publicIp"
          (Aws.Util.option_bind (Aws.Xml.member "publicIp" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("PublicIp", String.to_query v.public_ip))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("publicIp", String.to_json v.public_ip)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; public_ip = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "publicIp"))
  }
