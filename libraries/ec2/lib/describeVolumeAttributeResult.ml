open Aws.BaseTypes

type t =
  { auto_enable_i_o : AttributeBooleanValue.t option
  ; product_codes : ProductCodeList.t
  ; volume_id : String.t option
  }

let make ?auto_enable_i_o ?(product_codes = []) ?volume_id () =
  { auto_enable_i_o; product_codes; volume_id }

let parse xml =
  Some
    { auto_enable_i_o =
        Aws.Util.option_bind
          (Aws.Xml.member "autoEnableIO" xml)
          AttributeBooleanValue.parse
    ; product_codes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "productCodes" xml) ProductCodeList.parse)
    ; volume_id = Aws.Util.option_bind (Aws.Xml.member "volumeId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.volume_id (fun f ->
             Aws.Query.Pair ("VolumeId", String.to_query f))
       ; Some (Aws.Query.Pair ("ProductCodes", ProductCodeList.to_query v.product_codes))
       ; Aws.Util.option_map v.auto_enable_i_o (fun f ->
             Aws.Query.Pair ("AutoEnableIO", AttributeBooleanValue.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.volume_id (fun f -> "volumeId", String.to_json f)
       ; Some ("productCodes", ProductCodeList.to_json v.product_codes)
       ; Aws.Util.option_map v.auto_enable_i_o (fun f ->
             "autoEnableIO", AttributeBooleanValue.to_json f)
       ])

let of_json j =
  { auto_enable_i_o =
      Aws.Util.option_map (Aws.Json.lookup j "autoEnableIO") AttributeBooleanValue.of_json
  ; product_codes =
      ProductCodeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "productCodes"))
  ; volume_id = Aws.Util.option_map (Aws.Json.lookup j "volumeId") String.of_json
  }
