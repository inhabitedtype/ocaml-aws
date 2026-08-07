open Aws.BaseTypes

type t =
  { cidr : String.t option
  ; description : String.t option
  ; asn_associations : AsnAssociationSet.t
  ; status_message : String.t option
  ; state : ByoipCidrState.t option
  ; network_border_group : String.t option
  ; advertisement_type : String.t option
  }

let make
    ?cidr
    ?description
    ?(asn_associations = [])
    ?status_message
    ?state
    ?network_border_group
    ?advertisement_type
    () =
  { cidr
  ; description
  ; asn_associations
  ; status_message
  ; state
  ; network_border_group
  ; advertisement_type
  }

let parse xml =
  Some
    { cidr = Aws.Util.option_bind (Aws.Xml.member "cidr" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; asn_associations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "asnAssociationSet" xml)
             AsnAssociationSet.parse)
    ; status_message =
        Aws.Util.option_bind (Aws.Xml.member "statusMessage" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) ByoipCidrState.parse
    ; network_border_group =
        Aws.Util.option_bind (Aws.Xml.member "networkBorderGroup" xml) String.parse
    ; advertisement_type =
        Aws.Util.option_bind (Aws.Xml.member "advertisementType" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.advertisement_type (fun f ->
             Aws.Query.Pair ("AdvertisementType", String.to_query f))
       ; Aws.Util.option_map v.network_border_group (fun f ->
             Aws.Query.Pair ("NetworkBorderGroup", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", ByoipCidrState.to_query f))
       ; Aws.Util.option_map v.status_message (fun f ->
             Aws.Query.Pair ("StatusMessage", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AsnAssociationSet", AsnAssociationSet.to_query v.asn_associations))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.cidr (fun f -> Aws.Query.Pair ("Cidr", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.advertisement_type (fun f ->
             "advertisementType", String.to_json f)
       ; Aws.Util.option_map v.network_border_group (fun f ->
             "networkBorderGroup", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", ByoipCidrState.to_json f)
       ; Aws.Util.option_map v.status_message (fun f -> "statusMessage", String.to_json f)
       ; Some ("asnAssociationSet", AsnAssociationSet.to_json v.asn_associations)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.cidr (fun f -> "cidr", String.to_json f)
       ])

let of_json j =
  { cidr = Aws.Util.option_map (Aws.Json.lookup j "cidr") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; asn_associations =
      AsnAssociationSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "asnAssociationSet"))
  ; status_message =
      Aws.Util.option_map (Aws.Json.lookup j "statusMessage") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") ByoipCidrState.of_json
  ; network_border_group =
      Aws.Util.option_map (Aws.Json.lookup j "networkBorderGroup") String.of_json
  ; advertisement_type =
      Aws.Util.option_map (Aws.Json.lookup j "advertisementType") String.of_json
  }
