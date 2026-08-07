open Aws.BaseTypes

type t =
  { capacity_reservation_cancellation_quote_id : String.t option
  ; capacity_reservation_id : String.t option
  ; create_time : DateTime.t option
  ; expiration_time : DateTime.t option
  ; quote_state : CapacityReservationCancellationQuoteState.t option
  ; current_configuration : CapacityReservationConfiguration.t option
  ; cancellation_terms : CancellationTermsSet.t
  ; tags : TagList.t
  }

let make
    ?capacity_reservation_cancellation_quote_id
    ?capacity_reservation_id
    ?create_time
    ?expiration_time
    ?quote_state
    ?current_configuration
    ?(cancellation_terms = [])
    ?(tags = [])
    () =
  { capacity_reservation_cancellation_quote_id
  ; capacity_reservation_id
  ; create_time
  ; expiration_time
  ; quote_state
  ; current_configuration
  ; cancellation_terms
  ; tags
  }

let parse xml =
  Some
    { capacity_reservation_cancellation_quote_id =
        Aws.Util.option_bind
          (Aws.Xml.member "capacityReservationCancellationQuoteId" xml)
          String.parse
    ; capacity_reservation_id =
        Aws.Util.option_bind (Aws.Xml.member "capacityReservationId" xml) String.parse
    ; create_time = Aws.Util.option_bind (Aws.Xml.member "createTime" xml) DateTime.parse
    ; expiration_time =
        Aws.Util.option_bind (Aws.Xml.member "expirationTime" xml) DateTime.parse
    ; quote_state =
        Aws.Util.option_bind
          (Aws.Xml.member "quoteState" xml)
          CapacityReservationCancellationQuoteState.parse
    ; current_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "currentConfiguration" xml)
          CapacityReservationConfiguration.parse
    ; cancellation_terms =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "cancellationTermSet" xml)
             CancellationTermsSet.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Some
           (Aws.Query.Pair
              ("CancellationTermSet", CancellationTermsSet.to_query v.cancellation_terms))
       ; Aws.Util.option_map v.current_configuration (fun f ->
             Aws.Query.Pair
               ("CurrentConfiguration", CapacityReservationConfiguration.to_query f))
       ; Aws.Util.option_map v.quote_state (fun f ->
             Aws.Query.Pair
               ("QuoteState", CapacityReservationCancellationQuoteState.to_query f))
       ; Aws.Util.option_map v.expiration_time (fun f ->
             Aws.Query.Pair ("ExpirationTime", DateTime.to_query f))
       ; Aws.Util.option_map v.create_time (fun f ->
             Aws.Query.Pair ("CreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.capacity_reservation_id (fun f ->
             Aws.Query.Pair ("CapacityReservationId", String.to_query f))
       ; Aws.Util.option_map v.capacity_reservation_cancellation_quote_id (fun f ->
             Aws.Query.Pair ("CapacityReservationCancellationQuoteId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Some ("cancellationTermSet", CancellationTermsSet.to_json v.cancellation_terms)
       ; Aws.Util.option_map v.current_configuration (fun f ->
             "currentConfiguration", CapacityReservationConfiguration.to_json f)
       ; Aws.Util.option_map v.quote_state (fun f ->
             "quoteState", CapacityReservationCancellationQuoteState.to_json f)
       ; Aws.Util.option_map v.expiration_time (fun f ->
             "expirationTime", DateTime.to_json f)
       ; Aws.Util.option_map v.create_time (fun f -> "createTime", DateTime.to_json f)
       ; Aws.Util.option_map v.capacity_reservation_id (fun f ->
             "capacityReservationId", String.to_json f)
       ; Aws.Util.option_map v.capacity_reservation_cancellation_quote_id (fun f ->
             "capacityReservationCancellationQuoteId", String.to_json f)
       ])

let of_json j =
  { capacity_reservation_cancellation_quote_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "capacityReservationCancellationQuoteId")
        String.of_json
  ; capacity_reservation_id =
      Aws.Util.option_map (Aws.Json.lookup j "capacityReservationId") String.of_json
  ; create_time = Aws.Util.option_map (Aws.Json.lookup j "createTime") DateTime.of_json
  ; expiration_time =
      Aws.Util.option_map (Aws.Json.lookup j "expirationTime") DateTime.of_json
  ; quote_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "quoteState")
        CapacityReservationCancellationQuoteState.of_json
  ; current_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "currentConfiguration")
        CapacityReservationConfiguration.of_json
  ; cancellation_terms =
      CancellationTermsSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "cancellationTermSet"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
