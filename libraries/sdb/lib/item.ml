open Aws.BaseTypes

type t =
  { name : String.t
  ; alternate_name_encoding : String.t option
  ; attributes : AttributeList.t
  }

let make ~name ?alternate_name_encoding ~attributes () =
  { name; alternate_name_encoding; attributes }

let parse xml =
  Some
    { name =
        Aws.Xml.required
          "Name"
          (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
    ; alternate_name_encoding =
        Aws.Util.option_bind (Aws.Xml.member "AlternateNameEncoding" xml) String.parse
    ; attributes = Aws.Xml.required "Attributes" (AttributeList.parse xml)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Attributes.member", AttributeList.to_query v.attributes))
       ; Aws.Util.option_map v.alternate_name_encoding (fun f ->
             Aws.Query.Pair ("AlternateNameEncoding", String.to_query f))
       ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Attributes", AttributeList.to_json v.attributes)
       ; Aws.Util.option_map v.alternate_name_encoding (fun f ->
             "AlternateNameEncoding", String.to_json f)
       ; Some ("Name", String.to_json v.name)
       ])

let of_json j =
  { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
  ; alternate_name_encoding =
      Aws.Util.option_map (Aws.Json.lookup j "AlternateNameEncoding") String.of_json
  ; attributes =
      AttributeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Attributes"))
  }
