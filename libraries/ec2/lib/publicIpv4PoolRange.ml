open Aws.BaseTypes

type t =
  { first_address : String.t option
  ; last_address : String.t option
  ; address_count : Integer.t option
  ; available_address_count : Integer.t option
  }

let make ?first_address ?last_address ?address_count ?available_address_count () =
  { first_address; last_address; address_count; available_address_count }

let parse xml =
  Some
    { first_address =
        Aws.Util.option_bind (Aws.Xml.member "firstAddress" xml) String.parse
    ; last_address = Aws.Util.option_bind (Aws.Xml.member "lastAddress" xml) String.parse
    ; address_count =
        Aws.Util.option_bind (Aws.Xml.member "addressCount" xml) Integer.parse
    ; available_address_count =
        Aws.Util.option_bind (Aws.Xml.member "availableAddressCount" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.available_address_count (fun f ->
             Aws.Query.Pair ("AvailableAddressCount", Integer.to_query f))
       ; Aws.Util.option_map v.address_count (fun f ->
             Aws.Query.Pair ("AddressCount", Integer.to_query f))
       ; Aws.Util.option_map v.last_address (fun f ->
             Aws.Query.Pair ("LastAddress", String.to_query f))
       ; Aws.Util.option_map v.first_address (fun f ->
             Aws.Query.Pair ("FirstAddress", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.available_address_count (fun f ->
             "availableAddressCount", Integer.to_json f)
       ; Aws.Util.option_map v.address_count (fun f -> "addressCount", Integer.to_json f)
       ; Aws.Util.option_map v.last_address (fun f -> "lastAddress", String.to_json f)
       ; Aws.Util.option_map v.first_address (fun f -> "firstAddress", String.to_json f)
       ])

let of_json j =
  { first_address = Aws.Util.option_map (Aws.Json.lookup j "firstAddress") String.of_json
  ; last_address = Aws.Util.option_map (Aws.Json.lookup j "lastAddress") String.of_json
  ; address_count = Aws.Util.option_map (Aws.Json.lookup j "addressCount") Integer.of_json
  ; available_address_count =
      Aws.Util.option_map (Aws.Json.lookup j "availableAddressCount") Integer.of_json
  }
