open Aws.BaseTypes

type t =
  { client_token : String.t option
  ; currency_code : CurrencyCodeValues.t option
  ; host_id_set : RequestHostIdSet.t
  ; limit_price : String.t option
  ; offering_id : String.t
  ; tag_specifications : TagSpecificationList.t
  }

let make
    ?client_token
    ?currency_code
    ~host_id_set
    ?limit_price
    ~offering_id
    ?(tag_specifications = [])
    () =
  { client_token
  ; currency_code
  ; host_id_set
  ; limit_price
  ; offering_id
  ; tag_specifications
  }

let parse xml =
  Some
    { client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; currency_code =
        Aws.Util.option_bind (Aws.Xml.member "CurrencyCode" xml) CurrencyCodeValues.parse
    ; host_id_set =
        Aws.Xml.required
          "HostIdSet"
          (Aws.Util.option_bind (Aws.Xml.member "HostIdSet" xml) RequestHostIdSet.parse)
    ; limit_price = Aws.Util.option_bind (Aws.Xml.member "LimitPrice" xml) String.parse
    ; offering_id =
        Aws.Xml.required
          "OfferingId"
          (Aws.Util.option_bind (Aws.Xml.member "OfferingId" xml) String.parse)
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Some (Aws.Query.Pair ("OfferingId", String.to_query v.offering_id))
       ; Aws.Util.option_map v.limit_price (fun f ->
             Aws.Query.Pair ("LimitPrice", String.to_query f))
       ; Some (Aws.Query.Pair ("HostIdSet", RequestHostIdSet.to_query v.host_id_set))
       ; Aws.Util.option_map v.currency_code (fun f ->
             Aws.Query.Pair ("CurrencyCode", CurrencyCodeValues.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Some ("OfferingId", String.to_json v.offering_id)
       ; Aws.Util.option_map v.limit_price (fun f -> "LimitPrice", String.to_json f)
       ; Some ("HostIdSet", RequestHostIdSet.to_json v.host_id_set)
       ; Aws.Util.option_map v.currency_code (fun f ->
             "CurrencyCode", CurrencyCodeValues.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ])

let of_json j =
  { client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; currency_code =
      Aws.Util.option_map (Aws.Json.lookup j "CurrencyCode") CurrencyCodeValues.of_json
  ; host_id_set =
      RequestHostIdSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostIdSet"))
  ; limit_price = Aws.Util.option_map (Aws.Json.lookup j "LimitPrice") String.of_json
  ; offering_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OfferingId"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  }
