open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; service_id : String.t
  ; payer_responsibility : PayerResponsibility.t
  }

let make ?dry_run ~service_id ~payer_responsibility () =
  { dry_run; service_id; payer_responsibility }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; service_id =
        Aws.Xml.required
          "ServiceId"
          (Aws.Util.option_bind (Aws.Xml.member "ServiceId" xml) String.parse)
    ; payer_responsibility =
        Aws.Xml.required
          "PayerResponsibility"
          (Aws.Util.option_bind
             (Aws.Xml.member "PayerResponsibility" xml)
             PayerResponsibility.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("PayerResponsibility", PayerResponsibility.to_query v.payer_responsibility))
       ; Some (Aws.Query.Pair ("ServiceId", String.to_query v.service_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("PayerResponsibility", PayerResponsibility.to_json v.payer_responsibility)
       ; Some ("ServiceId", String.to_json v.service_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; service_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ServiceId"))
  ; payer_responsibility =
      PayerResponsibility.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PayerResponsibility"))
  }
