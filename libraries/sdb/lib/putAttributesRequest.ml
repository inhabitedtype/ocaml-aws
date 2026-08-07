open Aws.BaseTypes

type t =
  { domain_name : String.t
  ; item_name : String.t
  ; attributes : ReplaceableAttributeList.t
  ; expected : UpdateCondition.t option
  }

let make ~domain_name ~item_name ~attributes ?expected () =
  { domain_name; item_name; attributes; expected }

let parse xml =
  Some
    { domain_name =
        Aws.Xml.required
          "DomainName"
          (Aws.Util.option_bind (Aws.Xml.member "DomainName" xml) String.parse)
    ; item_name =
        Aws.Xml.required
          "ItemName"
          (Aws.Util.option_bind (Aws.Xml.member "ItemName" xml) String.parse)
    ; attributes = Aws.Xml.required "Attributes" (ReplaceableAttributeList.parse xml)
    ; expected =
        Aws.Util.option_bind (Aws.Xml.member "Expected" xml) UpdateCondition.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.expected (fun f ->
             Aws.Query.Pair ("Expected", UpdateCondition.to_query f))
       ; Some
           (Aws.Query.Pair
              ("Attributes.member", ReplaceableAttributeList.to_query v.attributes))
       ; Some (Aws.Query.Pair ("ItemName", String.to_query v.item_name))
       ; Some (Aws.Query.Pair ("DomainName", String.to_query v.domain_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.expected (fun f -> "Expected", UpdateCondition.to_json f)
       ; Some ("Attributes", ReplaceableAttributeList.to_json v.attributes)
       ; Some ("ItemName", String.to_json v.item_name)
       ; Some ("DomainName", String.to_json v.domain_name)
       ])

let of_json j =
  { domain_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DomainName"))
  ; item_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ItemName"))
  ; attributes =
      ReplaceableAttributeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Attributes"))
  ; expected = Aws.Util.option_map (Aws.Json.lookup j "Expected") UpdateCondition.of_json
  }
