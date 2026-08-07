open Aws.BaseTypes

type t =
  { asn : String.t option
  ; cidr : String.t option
  ; status_message : String.t option
  ; state : AsnAssociationState.t option
  }

let make ?asn ?cidr ?status_message ?state () = { asn; cidr; status_message; state }

let parse xml =
  Some
    { asn = Aws.Util.option_bind (Aws.Xml.member "asn" xml) String.parse
    ; cidr = Aws.Util.option_bind (Aws.Xml.member "cidr" xml) String.parse
    ; status_message =
        Aws.Util.option_bind (Aws.Xml.member "statusMessage" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) AsnAssociationState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", AsnAssociationState.to_query f))
       ; Aws.Util.option_map v.status_message (fun f ->
             Aws.Query.Pair ("StatusMessage", String.to_query f))
       ; Aws.Util.option_map v.cidr (fun f -> Aws.Query.Pair ("Cidr", String.to_query f))
       ; Aws.Util.option_map v.asn (fun f -> Aws.Query.Pair ("Asn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f -> "state", AsnAssociationState.to_json f)
       ; Aws.Util.option_map v.status_message (fun f -> "statusMessage", String.to_json f)
       ; Aws.Util.option_map v.cidr (fun f -> "cidr", String.to_json f)
       ; Aws.Util.option_map v.asn (fun f -> "asn", String.to_json f)
       ])

let of_json j =
  { asn = Aws.Util.option_map (Aws.Json.lookup j "asn") String.of_json
  ; cidr = Aws.Util.option_map (Aws.Json.lookup j "cidr") String.of_json
  ; status_message =
      Aws.Util.option_map (Aws.Json.lookup j "statusMessage") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") AsnAssociationState.of_json
  }
