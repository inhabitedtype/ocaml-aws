open Aws.BaseTypes

type t =
  { client_token : String.t option
  ; dry_run : Boolean.t option
  ; secondary_network_id : String.t
  }

let make ?client_token ?dry_run ~secondary_network_id () =
  { client_token; dry_run; secondary_network_id }

let parse xml =
  Some
    { client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; secondary_network_id =
        Aws.Xml.required
          "SecondaryNetworkId"
          (Aws.Util.option_bind (Aws.Xml.member "SecondaryNetworkId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("SecondaryNetworkId", String.to_query v.secondary_network_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("SecondaryNetworkId", String.to_json v.secondary_network_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ])

let of_json j =
  { client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; secondary_network_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SecondaryNetworkId"))
  }
