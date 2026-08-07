open Aws.BaseTypes

type t =
  { name : String.t option
  ; field_selectors : AdvancedFieldSelectors.t
  }

let make ?name ~field_selectors () = { name; field_selectors }

let parse xml =
  Some
    { name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; field_selectors =
        Aws.Xml.required
          "FieldSelectors"
          (Aws.Util.option_bind
             (Aws.Xml.member "FieldSelectors" xml)
             AdvancedFieldSelectors.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("FieldSelectors.member", AdvancedFieldSelectors.to_query v.field_selectors))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("FieldSelectors", AdvancedFieldSelectors.to_json v.field_selectors)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ])

let of_json j =
  { name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; field_selectors =
      AdvancedFieldSelectors.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "FieldSelectors"))
  }
