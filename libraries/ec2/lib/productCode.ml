open Aws.BaseTypes

type t =
  { product_code_id : String.t option
  ; product_code_type : ProductCodeValues.t option
  }

let make ?product_code_id ?product_code_type () = { product_code_id; product_code_type }

let parse xml =
  Some
    { product_code_id =
        Aws.Util.option_bind (Aws.Xml.member "productCode" xml) String.parse
    ; product_code_type =
        Aws.Util.option_bind (Aws.Xml.member "type" xml) ProductCodeValues.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.product_code_type (fun f ->
             Aws.Query.Pair ("Type", ProductCodeValues.to_query f))
       ; Aws.Util.option_map v.product_code_id (fun f ->
             Aws.Query.Pair ("ProductCode", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.product_code_type (fun f ->
             "type", ProductCodeValues.to_json f)
       ; Aws.Util.option_map v.product_code_id (fun f -> "productCode", String.to_json f)
       ])

let of_json j =
  { product_code_id = Aws.Util.option_map (Aws.Json.lookup j "productCode") String.of_json
  ; product_code_type =
      Aws.Util.option_map (Aws.Json.lookup j "type") ProductCodeValues.of_json
  }
