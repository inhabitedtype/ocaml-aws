open Aws.BaseTypes

type t =
  { custom_key_stores : CustomKeyStoresList.t
  ; next_marker : String.t option
  ; truncated : Boolean.t option
  }

let make ?(custom_key_stores = []) ?next_marker ?truncated () =
  { custom_key_stores; next_marker; truncated }

let parse xml =
  Some
    { custom_key_stores =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "CustomKeyStores" xml)
             CustomKeyStoresList.parse)
    ; next_marker = Aws.Util.option_bind (Aws.Xml.member "NextMarker" xml) String.parse
    ; truncated = Aws.Util.option_bind (Aws.Xml.member "Truncated" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.truncated (fun f ->
             Aws.Query.Pair ("Truncated", Boolean.to_query f))
       ; Aws.Util.option_map v.next_marker (fun f ->
             Aws.Query.Pair ("NextMarker", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("CustomKeyStores.member", CustomKeyStoresList.to_query v.custom_key_stores))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.truncated (fun f -> "Truncated", Boolean.to_json f)
       ; Aws.Util.option_map v.next_marker (fun f -> "NextMarker", String.to_json f)
       ; Some ("CustomKeyStores", CustomKeyStoresList.to_json v.custom_key_stores)
       ])

let of_json j =
  { custom_key_stores =
      CustomKeyStoresList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CustomKeyStores"))
  ; next_marker = Aws.Util.option_map (Aws.Json.lookup j "NextMarker") String.of_json
  ; truncated = Aws.Util.option_map (Aws.Json.lookup j "Truncated") Boolean.of_json
  }
