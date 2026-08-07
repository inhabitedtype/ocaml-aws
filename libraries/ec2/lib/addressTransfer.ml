open Aws.BaseTypes

type t =
  { public_ip : String.t option
  ; allocation_id : String.t option
  ; transfer_account_id : String.t option
  ; transfer_offer_expiration_timestamp : DateTime.t option
  ; transfer_offer_accepted_timestamp : DateTime.t option
  ; address_transfer_status : AddressTransferStatus.t option
  }

let make
    ?public_ip
    ?allocation_id
    ?transfer_account_id
    ?transfer_offer_expiration_timestamp
    ?transfer_offer_accepted_timestamp
    ?address_transfer_status
    () =
  { public_ip
  ; allocation_id
  ; transfer_account_id
  ; transfer_offer_expiration_timestamp
  ; transfer_offer_accepted_timestamp
  ; address_transfer_status
  }

let parse xml =
  Some
    { public_ip = Aws.Util.option_bind (Aws.Xml.member "publicIp" xml) String.parse
    ; allocation_id =
        Aws.Util.option_bind (Aws.Xml.member "allocationId" xml) String.parse
    ; transfer_account_id =
        Aws.Util.option_bind (Aws.Xml.member "transferAccountId" xml) String.parse
    ; transfer_offer_expiration_timestamp =
        Aws.Util.option_bind
          (Aws.Xml.member "transferOfferExpirationTimestamp" xml)
          DateTime.parse
    ; transfer_offer_accepted_timestamp =
        Aws.Util.option_bind
          (Aws.Xml.member "transferOfferAcceptedTimestamp" xml)
          DateTime.parse
    ; address_transfer_status =
        Aws.Util.option_bind
          (Aws.Xml.member "addressTransferStatus" xml)
          AddressTransferStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.address_transfer_status (fun f ->
             Aws.Query.Pair ("AddressTransferStatus", AddressTransferStatus.to_query f))
       ; Aws.Util.option_map v.transfer_offer_accepted_timestamp (fun f ->
             Aws.Query.Pair ("TransferOfferAcceptedTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.transfer_offer_expiration_timestamp (fun f ->
             Aws.Query.Pair ("TransferOfferExpirationTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.transfer_account_id (fun f ->
             Aws.Query.Pair ("TransferAccountId", String.to_query f))
       ; Aws.Util.option_map v.allocation_id (fun f ->
             Aws.Query.Pair ("AllocationId", String.to_query f))
       ; Aws.Util.option_map v.public_ip (fun f ->
             Aws.Query.Pair ("PublicIp", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.address_transfer_status (fun f ->
             "addressTransferStatus", AddressTransferStatus.to_json f)
       ; Aws.Util.option_map v.transfer_offer_accepted_timestamp (fun f ->
             "transferOfferAcceptedTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.transfer_offer_expiration_timestamp (fun f ->
             "transferOfferExpirationTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.transfer_account_id (fun f ->
             "transferAccountId", String.to_json f)
       ; Aws.Util.option_map v.allocation_id (fun f -> "allocationId", String.to_json f)
       ; Aws.Util.option_map v.public_ip (fun f -> "publicIp", String.to_json f)
       ])

let of_json j =
  { public_ip = Aws.Util.option_map (Aws.Json.lookup j "publicIp") String.of_json
  ; allocation_id = Aws.Util.option_map (Aws.Json.lookup j "allocationId") String.of_json
  ; transfer_account_id =
      Aws.Util.option_map (Aws.Json.lookup j "transferAccountId") String.of_json
  ; transfer_offer_expiration_timestamp =
      Aws.Util.option_map
        (Aws.Json.lookup j "transferOfferExpirationTimestamp")
        DateTime.of_json
  ; transfer_offer_accepted_timestamp =
      Aws.Util.option_map
        (Aws.Json.lookup j "transferOfferAcceptedTimestamp")
        DateTime.of_json
  ; address_transfer_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "addressTransferStatus")
        AddressTransferStatus.of_json
  }
