open Aws.BaseTypes

type t =
  { domain_name : String.t
  ; item_name : String.t
  ; attribute_names : AttributeNameList.t
  ; consistent_read : Boolean.t option
  }

let make ~domain_name ~item_name ?(attribute_names = []) ?consistent_read () =
  { domain_name; item_name; attribute_names; consistent_read }

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
    ; attribute_names = Aws.Util.of_option [] (AttributeNameList.parse xml)
    ; consistent_read =
        Aws.Util.option_bind (Aws.Xml.member "ConsistentRead" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.consistent_read (fun f ->
             Aws.Query.Pair ("ConsistentRead", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AttributeNames.member", AttributeNameList.to_query v.attribute_names))
       ; Some (Aws.Query.Pair ("ItemName", String.to_query v.item_name))
       ; Some (Aws.Query.Pair ("DomainName", String.to_query v.domain_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.consistent_read (fun f ->
             "ConsistentRead", Boolean.to_json f)
       ; Some ("AttributeNames", AttributeNameList.to_json v.attribute_names)
       ; Some ("ItemName", String.to_json v.item_name)
       ; Some ("DomainName", String.to_json v.domain_name)
       ])

let of_json j =
  { domain_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DomainName"))
  ; item_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ItemName"))
  ; attribute_names =
      AttributeNameList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AttributeNames"))
  ; consistent_read =
      Aws.Util.option_map (Aws.Json.lookup j "ConsistentRead") Boolean.of_json
  }
