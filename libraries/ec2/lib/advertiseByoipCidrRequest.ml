open Aws.BaseTypes

type t =
  { cidr : String.t
  ; asn : String.t option
  ; dry_run : Boolean.t option
  ; network_border_group : String.t option
  }

let make ~cidr ?asn ?dry_run ?network_border_group () =
  { cidr; asn; dry_run; network_border_group }

let parse xml =
  Some
    { cidr =
        Aws.Xml.required
          "Cidr"
          (Aws.Util.option_bind (Aws.Xml.member "Cidr" xml) String.parse)
    ; asn = Aws.Util.option_bind (Aws.Xml.member "Asn" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; network_border_group =
        Aws.Util.option_bind (Aws.Xml.member "NetworkBorderGroup" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_border_group (fun f ->
             Aws.Query.Pair ("NetworkBorderGroup", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.asn (fun f -> Aws.Query.Pair ("Asn", String.to_query f))
       ; Some (Aws.Query.Pair ("Cidr", String.to_query v.cidr))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_border_group (fun f ->
             "NetworkBorderGroup", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.asn (fun f -> "Asn", String.to_json f)
       ; Some ("Cidr", String.to_json v.cidr)
       ])

let of_json j =
  { cidr = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Cidr"))
  ; asn = Aws.Util.option_map (Aws.Json.lookup j "Asn") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; network_border_group =
      Aws.Util.option_map (Aws.Json.lookup j "NetworkBorderGroup") String.of_json
  }
