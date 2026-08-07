open Aws.BaseTypes

type t =
  { verified_access_instance_id : String.t
  ; dry_run : Boolean.t option
  ; client_token : String.t option
  }

let make ~verified_access_instance_id ?dry_run ?client_token () =
  { verified_access_instance_id; dry_run; client_token }

let parse xml =
  Some
    { verified_access_instance_id =
        Aws.Xml.required
          "VerifiedAccessInstanceId"
          (Aws.Util.option_bind
             (Aws.Xml.member "VerifiedAccessInstanceId" xml)
             String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("VerifiedAccessInstanceId", String.to_query v.verified_access_instance_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("VerifiedAccessInstanceId", String.to_json v.verified_access_instance_id)
       ])

let of_json j =
  { verified_access_instance_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VerifiedAccessInstanceId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  }
