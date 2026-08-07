open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; asn : String.t
  ; cidr : String.t
  }

let make ?dry_run ~asn ~cidr () = { dry_run; asn; cidr }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; asn =
        Aws.Xml.required
          "Asn"
          (Aws.Util.option_bind (Aws.Xml.member "Asn" xml) String.parse)
    ; cidr =
        Aws.Xml.required
          "Cidr"
          (Aws.Util.option_bind (Aws.Xml.member "Cidr" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Cidr", String.to_query v.cidr))
       ; Some (Aws.Query.Pair ("Asn", String.to_query v.asn))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Cidr", String.to_json v.cidr)
       ; Some ("Asn", String.to_json v.asn)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; asn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Asn"))
  ; cidr = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Cidr"))
  }
