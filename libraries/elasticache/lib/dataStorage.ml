open Aws.BaseTypes

type t =
  { maximum : Integer.t option
  ; minimum : Integer.t option
  ; unit : DataStorageUnit.t
  }

let make ?maximum ?minimum ~unit () = { maximum; minimum; unit }

let parse xml =
  Some
    { maximum = Aws.Util.option_bind (Aws.Xml.member "Maximum" xml) Integer.parse
    ; minimum = Aws.Util.option_bind (Aws.Xml.member "Minimum" xml) Integer.parse
    ; unit =
        Aws.Xml.required
          "Unit"
          (Aws.Util.option_bind (Aws.Xml.member "Unit" xml) DataStorageUnit.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Unit", DataStorageUnit.to_query v.unit))
       ; Aws.Util.option_map v.minimum (fun f ->
             Aws.Query.Pair ("Minimum", Integer.to_query f))
       ; Aws.Util.option_map v.maximum (fun f ->
             Aws.Query.Pair ("Maximum", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Unit", DataStorageUnit.to_json v.unit)
       ; Aws.Util.option_map v.minimum (fun f -> "Minimum", Integer.to_json f)
       ; Aws.Util.option_map v.maximum (fun f -> "Maximum", Integer.to_json f)
       ])

let of_json j =
  { maximum = Aws.Util.option_map (Aws.Json.lookup j "Maximum") Integer.of_json
  ; minimum = Aws.Util.option_map (Aws.Json.lookup j "Minimum") Integer.of_json
  ; unit = DataStorageUnit.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Unit"))
  }
