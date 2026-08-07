open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; client_token : String.t option
  ; instance_credit_specifications : InstanceCreditSpecificationListRequest.t
  }

let make ?dry_run ?client_token ~instance_credit_specifications () =
  { dry_run; client_token; instance_credit_specifications }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; instance_credit_specifications =
        Aws.Xml.required
          "InstanceCreditSpecification"
          (Aws.Util.option_bind
             (Aws.Xml.member "InstanceCreditSpecification" xml)
             InstanceCreditSpecificationListRequest.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "InstanceCreditSpecification"
              , InstanceCreditSpecificationListRequest.to_query
                  v.instance_credit_specifications ))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "InstanceCreditSpecification"
           , InstanceCreditSpecificationListRequest.to_json
               v.instance_credit_specifications )
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; instance_credit_specifications =
      InstanceCreditSpecificationListRequest.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceCreditSpecification"))
  }
