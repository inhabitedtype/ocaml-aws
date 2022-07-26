open Aws.BaseTypes

type calendar = CalendarLib.Calendar.t

module ReplaceableAttribute = struct
  type t =
    { name : String.t
    ; value : String.t
    ; replace : Boolean.t option
    }

  let make ~name ~value ?replace () = { name; value; replace }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      ; value =
          Aws.Xml.required
            "Value"
            (Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse)
      ; replace = Aws.Util.option_bind (Aws.Xml.member "Replace" xml) Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.replace (fun f ->
               Aws.Query.Pair ("Replace", Boolean.to_query f))
         ; Some (Aws.Query.Pair ("Value", String.to_query v.value))
         ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.replace (fun f -> "Replace", Boolean.to_json f)
         ; Some ("Value", String.to_json v.value)
         ; Some ("Name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    ; value = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Value"))
    ; replace = Aws.Util.option_map (Aws.Json.lookup j "Replace") Boolean.of_json
    }
end

module NumberDomainAttributesExceeded = struct
  type t = { box_usage : Float.t option }

  let make ?box_usage () = { box_usage }

  let parse xml =
    Some { box_usage = Aws.Util.option_bind (Aws.Xml.member "BoxUsage" xml) Float.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f ->
               Aws.Query.Pair ("BoxUsage", Float.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f -> "BoxUsage", Float.to_json f) ])

  let of_json j =
    { box_usage = Aws.Util.option_map (Aws.Json.lookup j "BoxUsage") Float.of_json }
end

module NumberSubmittedItemsExceeded = struct
  type t = { box_usage : Float.t option }

  let make ?box_usage () = { box_usage }

  let parse xml =
    Some { box_usage = Aws.Util.option_bind (Aws.Xml.member "BoxUsage" xml) Float.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f ->
               Aws.Query.Pair ("BoxUsage", Float.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f -> "BoxUsage", Float.to_json f) ])

  let of_json j =
    { box_usage = Aws.Util.option_map (Aws.Json.lookup j "BoxUsage") Float.of_json }
end

module DeleteDomainRequest = struct
  type t = { domain_name : String.t }

  let make ~domain_name () = { domain_name }

  let parse xml =
    Some
      { domain_name =
          Aws.Xml.required
            "DomainName"
            (Aws.Util.option_bind (Aws.Xml.member "DomainName" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("DomainName", String.to_query v.domain_name)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("DomainName", String.to_json v.domain_name) ])

  let of_json j =
    { domain_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DomainName"))
    }
end

module NumberItemAttributesExceeded = struct
  type t = { box_usage : Float.t option }

  let make ?box_usage () = { box_usage }

  let parse xml =
    Some { box_usage = Aws.Util.option_bind (Aws.Xml.member "BoxUsage" xml) Float.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f ->
               Aws.Query.Pair ("BoxUsage", Float.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f -> "BoxUsage", Float.to_json f) ])

  let of_json j =
    { box_usage = Aws.Util.option_map (Aws.Json.lookup j "BoxUsage") Float.of_json }
end

module TooManyRequestedAttributes = struct
  type t = { box_usage : Float.t option }

  let make ?box_usage () = { box_usage }

  let parse xml =
    Some { box_usage = Aws.Util.option_bind (Aws.Xml.member "BoxUsage" xml) Float.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f ->
               Aws.Query.Pair ("BoxUsage", Float.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f -> "BoxUsage", Float.to_json f) ])

  let of_json j =
    { box_usage = Aws.Util.option_map (Aws.Json.lookup j "BoxUsage") Float.of_json }
end

module InvalidQueryExpression = struct
  type t = { box_usage : Float.t option }

  let make ?box_usage () = { box_usage }

  let parse xml =
    Some { box_usage = Aws.Util.option_bind (Aws.Xml.member "BoxUsage" xml) Float.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f ->
               Aws.Query.Pair ("BoxUsage", Float.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f -> "BoxUsage", Float.to_json f) ])

  let of_json j =
    { box_usage = Aws.Util.option_map (Aws.Json.lookup j "BoxUsage") Float.of_json }
end

module DomainNameList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "DomainName" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module Attribute = struct
  type t =
    { name : String.t
    ; alternate_name_encoding : String.t option
    ; value : String.t
    ; alternate_value_encoding : String.t option
    }

  let make ~name ?alternate_name_encoding ~value ?alternate_value_encoding () =
    { name; alternate_name_encoding; value; alternate_value_encoding }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      ; alternate_name_encoding =
          Aws.Util.option_bind (Aws.Xml.member "AlternateNameEncoding" xml) String.parse
      ; value =
          Aws.Xml.required
            "Value"
            (Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse)
      ; alternate_value_encoding =
          Aws.Util.option_bind (Aws.Xml.member "AlternateValueEncoding" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.alternate_value_encoding (fun f ->
               Aws.Query.Pair ("AlternateValueEncoding", String.to_query f))
         ; Some (Aws.Query.Pair ("Value", String.to_query v.value))
         ; Aws.Util.option_map v.alternate_name_encoding (fun f ->
               Aws.Query.Pair ("AlternateNameEncoding", String.to_query f))
         ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.alternate_value_encoding (fun f ->
               "AlternateValueEncoding", String.to_json f)
         ; Some ("Value", String.to_json v.value)
         ; Aws.Util.option_map v.alternate_name_encoding (fun f ->
               "AlternateNameEncoding", String.to_json f)
         ; Some ("Name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    ; alternate_name_encoding =
        Aws.Util.option_map (Aws.Json.lookup j "AlternateNameEncoding") String.of_json
    ; value = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Value"))
    ; alternate_value_encoding =
        Aws.Util.option_map (Aws.Json.lookup j "AlternateValueEncoding") String.of_json
    }
end

module AttributeList = struct
  type t = Attribute.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Attribute.parse (Aws.Xml.members "Attribute" xml))

  let to_query v = Aws.Query.to_query_list Attribute.to_query v

  let to_json v = `List (List.map Attribute.to_json v)

  let of_json j = Aws.Json.to_list Attribute.of_json j
end

module Item = struct
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
         [ Some
             (Aws.Query.Pair ("Attributes.member", AttributeList.to_query v.attributes))
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
end

module ListDomainsResult = struct
  type t =
    { domain_names : DomainNameList.t
    ; next_token : String.t option
    }

  let make ?(domain_names = []) ?next_token () = { domain_names; next_token }

  let parse xml =
    Some
      { domain_names = Aws.Util.of_option [] (DomainNameList.parse xml)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair ("DomainNames.member", DomainNameList.to_query v.domain_names))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("DomainNames", DomainNameList.to_json v.domain_names)
         ])

  let of_json j =
    { domain_names =
        DomainNameList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DomainNames"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module InvalidNumberPredicates = struct
  type t = { box_usage : Float.t option }

  let make ?box_usage () = { box_usage }

  let parse xml =
    Some { box_usage = Aws.Util.option_bind (Aws.Xml.member "BoxUsage" xml) Float.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f ->
               Aws.Query.Pair ("BoxUsage", Float.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f -> "BoxUsage", Float.to_json f) ])

  let of_json j =
    { box_usage = Aws.Util.option_map (Aws.Json.lookup j "BoxUsage") Float.of_json }
end

module InvalidNumberValueTests = struct
  type t = { box_usage : Float.t option }

  let make ?box_usage () = { box_usage }

  let parse xml =
    Some { box_usage = Aws.Util.option_bind (Aws.Xml.member "BoxUsage" xml) Float.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f ->
               Aws.Query.Pair ("BoxUsage", Float.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f -> "BoxUsage", Float.to_json f) ])

  let of_json j =
    { box_usage = Aws.Util.option_map (Aws.Json.lookup j "BoxUsage") Float.of_json }
end

module AttributeDoesNotExist = struct
  type t = { box_usage : Float.t option }

  let make ?box_usage () = { box_usage }

  let parse xml =
    Some { box_usage = Aws.Util.option_bind (Aws.Xml.member "BoxUsage" xml) Float.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f ->
               Aws.Query.Pair ("BoxUsage", Float.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f -> "BoxUsage", Float.to_json f) ])

  let of_json j =
    { box_usage = Aws.Util.option_map (Aws.Json.lookup j "BoxUsage") Float.of_json }
end

module DeletableItem = struct
  type t =
    { name : String.t
    ; attributes : AttributeList.t
    }

  let make ~name ?(attributes = []) () = { name; attributes }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "ItemName"
            (Aws.Util.option_bind (Aws.Xml.member "ItemName" xml) String.parse)
      ; attributes = Aws.Util.of_option [] (AttributeList.parse xml)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("Attributes.member", AttributeList.to_query v.attributes))
         ; Some (Aws.Query.Pair ("ItemName", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Attributes", AttributeList.to_json v.attributes)
         ; Some ("ItemName", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ItemName"))
    ; attributes =
        AttributeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Attributes"))
    }
end

module DeletableItemList = struct
  type t = DeletableItem.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map DeletableItem.parse (Aws.Xml.members "Item" xml))

  let to_query v = Aws.Query.to_query_list DeletableItem.to_query v

  let to_json v = `List (List.map DeletableItem.to_json v)

  let of_json j = Aws.Json.to_list DeletableItem.of_json j
end

module ReplaceableAttributeList = struct
  type t = ReplaceableAttribute.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map ReplaceableAttribute.parse (Aws.Xml.members "Attribute" xml))

  let to_query v = Aws.Query.to_query_list ReplaceableAttribute.to_query v

  let to_json v = `List (List.map ReplaceableAttribute.to_json v)

  let of_json j = Aws.Json.to_list ReplaceableAttribute.of_json j
end

module ReplaceableItem = struct
  type t =
    { name : String.t
    ; attributes : ReplaceableAttributeList.t
    }

  let make ~name ~attributes () = { name; attributes }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "ItemName"
            (Aws.Util.option_bind (Aws.Xml.member "ItemName" xml) String.parse)
      ; attributes = Aws.Xml.required "Attributes" (ReplaceableAttributeList.parse xml)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("Attributes.member", ReplaceableAttributeList.to_query v.attributes))
         ; Some (Aws.Query.Pair ("ItemName", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Attributes", ReplaceableAttributeList.to_json v.attributes)
         ; Some ("ItemName", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ItemName"))
    ; attributes =
        ReplaceableAttributeList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Attributes"))
    }
end

module CreateDomainRequest = struct
  type t = { domain_name : String.t }

  let make ~domain_name () = { domain_name }

  let parse xml =
    Some
      { domain_name =
          Aws.Xml.required
            "DomainName"
            (Aws.Util.option_bind (Aws.Xml.member "DomainName" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("DomainName", String.to_query v.domain_name)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("DomainName", String.to_json v.domain_name) ])

  let of_json j =
    { domain_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DomainName"))
    }
end

module InvalidNextToken = struct
  type t = { box_usage : Float.t option }

  let make ?box_usage () = { box_usage }

  let parse xml =
    Some { box_usage = Aws.Util.option_bind (Aws.Xml.member "BoxUsage" xml) Float.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f ->
               Aws.Query.Pair ("BoxUsage", Float.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f -> "BoxUsage", Float.to_json f) ])

  let of_json j =
    { box_usage = Aws.Util.option_map (Aws.Json.lookup j "BoxUsage") Float.of_json }
end

module RequestTimeout = struct
  type t = { box_usage : Float.t option }

  let make ?box_usage () = { box_usage }

  let parse xml =
    Some { box_usage = Aws.Util.option_bind (Aws.Xml.member "BoxUsage" xml) Float.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f ->
               Aws.Query.Pair ("BoxUsage", Float.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f -> "BoxUsage", Float.to_json f) ])

  let of_json j =
    { box_usage = Aws.Util.option_map (Aws.Json.lookup j "BoxUsage") Float.of_json }
end

module DomainMetadataResult = struct
  type t =
    { item_count : Integer.t option
    ; item_names_size_bytes : Long.t option
    ; attribute_name_count : Integer.t option
    ; attribute_names_size_bytes : Long.t option
    ; attribute_value_count : Integer.t option
    ; attribute_values_size_bytes : Long.t option
    ; timestamp : Integer.t option
    }

  let make
      ?item_count
      ?item_names_size_bytes
      ?attribute_name_count
      ?attribute_names_size_bytes
      ?attribute_value_count
      ?attribute_values_size_bytes
      ?timestamp
      () =
    { item_count
    ; item_names_size_bytes
    ; attribute_name_count
    ; attribute_names_size_bytes
    ; attribute_value_count
    ; attribute_values_size_bytes
    ; timestamp
    }

  let parse xml =
    Some
      { item_count = Aws.Util.option_bind (Aws.Xml.member "ItemCount" xml) Integer.parse
      ; item_names_size_bytes =
          Aws.Util.option_bind (Aws.Xml.member "ItemNamesSizeBytes" xml) Long.parse
      ; attribute_name_count =
          Aws.Util.option_bind (Aws.Xml.member "AttributeNameCount" xml) Integer.parse
      ; attribute_names_size_bytes =
          Aws.Util.option_bind (Aws.Xml.member "AttributeNamesSizeBytes" xml) Long.parse
      ; attribute_value_count =
          Aws.Util.option_bind (Aws.Xml.member "AttributeValueCount" xml) Integer.parse
      ; attribute_values_size_bytes =
          Aws.Util.option_bind (Aws.Xml.member "AttributeValuesSizeBytes" xml) Long.parse
      ; timestamp = Aws.Util.option_bind (Aws.Xml.member "Timestamp" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.timestamp (fun f ->
               Aws.Query.Pair ("Timestamp", Integer.to_query f))
         ; Aws.Util.option_map v.attribute_values_size_bytes (fun f ->
               Aws.Query.Pair ("AttributeValuesSizeBytes", Long.to_query f))
         ; Aws.Util.option_map v.attribute_value_count (fun f ->
               Aws.Query.Pair ("AttributeValueCount", Integer.to_query f))
         ; Aws.Util.option_map v.attribute_names_size_bytes (fun f ->
               Aws.Query.Pair ("AttributeNamesSizeBytes", Long.to_query f))
         ; Aws.Util.option_map v.attribute_name_count (fun f ->
               Aws.Query.Pair ("AttributeNameCount", Integer.to_query f))
         ; Aws.Util.option_map v.item_names_size_bytes (fun f ->
               Aws.Query.Pair ("ItemNamesSizeBytes", Long.to_query f))
         ; Aws.Util.option_map v.item_count (fun f ->
               Aws.Query.Pair ("ItemCount", Integer.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.timestamp (fun f -> "Timestamp", Integer.to_json f)
         ; Aws.Util.option_map v.attribute_values_size_bytes (fun f ->
               "AttributeValuesSizeBytes", Long.to_json f)
         ; Aws.Util.option_map v.attribute_value_count (fun f ->
               "AttributeValueCount", Integer.to_json f)
         ; Aws.Util.option_map v.attribute_names_size_bytes (fun f ->
               "AttributeNamesSizeBytes", Long.to_json f)
         ; Aws.Util.option_map v.attribute_name_count (fun f ->
               "AttributeNameCount", Integer.to_json f)
         ; Aws.Util.option_map v.item_names_size_bytes (fun f ->
               "ItemNamesSizeBytes", Long.to_json f)
         ; Aws.Util.option_map v.item_count (fun f -> "ItemCount", Integer.to_json f)
         ])

  let of_json j =
    { item_count = Aws.Util.option_map (Aws.Json.lookup j "ItemCount") Integer.of_json
    ; item_names_size_bytes =
        Aws.Util.option_map (Aws.Json.lookup j "ItemNamesSizeBytes") Long.of_json
    ; attribute_name_count =
        Aws.Util.option_map (Aws.Json.lookup j "AttributeNameCount") Integer.of_json
    ; attribute_names_size_bytes =
        Aws.Util.option_map (Aws.Json.lookup j "AttributeNamesSizeBytes") Long.of_json
    ; attribute_value_count =
        Aws.Util.option_map (Aws.Json.lookup j "AttributeValueCount") Integer.of_json
    ; attribute_values_size_bytes =
        Aws.Util.option_map (Aws.Json.lookup j "AttributeValuesSizeBytes") Long.of_json
    ; timestamp = Aws.Util.option_map (Aws.Json.lookup j "Timestamp") Integer.of_json
    }
end

module SelectRequest = struct
  type t =
    { select_expression : String.t
    ; next_token : String.t option
    ; consistent_read : Boolean.t option
    }

  let make ~select_expression ?next_token ?consistent_read () =
    { select_expression; next_token; consistent_read }

  let parse xml =
    Some
      { select_expression =
          Aws.Xml.required
            "SelectExpression"
            (Aws.Util.option_bind (Aws.Xml.member "SelectExpression" xml) String.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      ; consistent_read =
          Aws.Util.option_bind (Aws.Xml.member "ConsistentRead" xml) Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.consistent_read (fun f ->
               Aws.Query.Pair ("ConsistentRead", Boolean.to_query f))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some (Aws.Query.Pair ("SelectExpression", String.to_query v.select_expression))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.consistent_read (fun f ->
               "ConsistentRead", Boolean.to_json f)
         ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("SelectExpression", String.to_json v.select_expression)
         ])

  let of_json j =
    { select_expression =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SelectExpression"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    ; consistent_read =
        Aws.Util.option_map (Aws.Json.lookup j "ConsistentRead") Boolean.of_json
    }
end

module ListDomainsRequest = struct
  type t =
    { max_number_of_domains : Integer.t option
    ; next_token : String.t option
    }

  let make ?max_number_of_domains ?next_token () = { max_number_of_domains; next_token }

  let parse xml =
    Some
      { max_number_of_domains =
          Aws.Util.option_bind (Aws.Xml.member "MaxNumberOfDomains" xml) Integer.parse
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Aws.Util.option_map v.max_number_of_domains (fun f ->
               Aws.Query.Pair ("MaxNumberOfDomains", Integer.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Aws.Util.option_map v.max_number_of_domains (fun f ->
               "MaxNumberOfDomains", Integer.to_json f)
         ])

  let of_json j =
    { max_number_of_domains =
        Aws.Util.option_map (Aws.Json.lookup j "MaxNumberOfDomains") Integer.of_json
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module ItemList = struct
  type t = Item.t list

  let make elems () = elems

  let parse xml = Aws.Util.option_all (List.map Item.parse (Aws.Xml.members "Item" xml))

  let to_query v = Aws.Query.to_query_list Item.to_query v

  let to_json v = `List (List.map Item.to_json v)

  let of_json j = Aws.Json.to_list Item.of_json j
end

module SelectResult = struct
  type t =
    { items : ItemList.t
    ; next_token : String.t option
    }

  let make ?(items = []) ?next_token () = { items; next_token }

  let parse xml =
    Some
      { items = Aws.Util.of_option [] (ItemList.parse xml)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some (Aws.Query.Pair ("Items.member", ItemList.to_query v.items))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("Items", ItemList.to_json v.items)
         ])

  let of_json j =
    { items = ItemList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Items"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module DomainMetadataRequest = struct
  type t = { domain_name : String.t }

  let make ~domain_name () = { domain_name }

  let parse xml =
    Some
      { domain_name =
          Aws.Xml.required
            "DomainName"
            (Aws.Util.option_bind (Aws.Xml.member "DomainName" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("DomainName", String.to_query v.domain_name)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("DomainName", String.to_json v.domain_name) ])

  let of_json j =
    { domain_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DomainName"))
    }
end

module NoSuchDomain = struct
  type t = { box_usage : Float.t option }

  let make ?box_usage () = { box_usage }

  let parse xml =
    Some { box_usage = Aws.Util.option_bind (Aws.Xml.member "BoxUsage" xml) Float.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f ->
               Aws.Query.Pair ("BoxUsage", Float.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f -> "BoxUsage", Float.to_json f) ])

  let of_json j =
    { box_usage = Aws.Util.option_map (Aws.Json.lookup j "BoxUsage") Float.of_json }
end

module NumberDomainsExceeded = struct
  type t = { box_usage : Float.t option }

  let make ?box_usage () = { box_usage }

  let parse xml =
    Some { box_usage = Aws.Util.option_bind (Aws.Xml.member "BoxUsage" xml) Float.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f ->
               Aws.Query.Pair ("BoxUsage", Float.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f -> "BoxUsage", Float.to_json f) ])

  let of_json j =
    { box_usage = Aws.Util.option_map (Aws.Json.lookup j "BoxUsage") Float.of_json }
end

module BatchDeleteAttributesRequest = struct
  type t =
    { domain_name : String.t
    ; items : DeletableItemList.t
    }

  let make ~domain_name ~items () = { domain_name; items }

  let parse xml =
    Some
      { domain_name =
          Aws.Xml.required
            "DomainName"
            (Aws.Util.option_bind (Aws.Xml.member "DomainName" xml) String.parse)
      ; items = Aws.Xml.required "Items" (DeletableItemList.parse xml)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Items.member", DeletableItemList.to_query v.items))
         ; Some (Aws.Query.Pair ("DomainName", String.to_query v.domain_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Items", DeletableItemList.to_json v.items)
         ; Some ("DomainName", String.to_json v.domain_name)
         ])

  let of_json j =
    { domain_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DomainName"))
    ; items =
        DeletableItemList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Items"))
    }
end

module AttributeNameList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "AttributeName" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module NumberSubmittedAttributesExceeded = struct
  type t = { box_usage : Float.t option }

  let make ?box_usage () = { box_usage }

  let parse xml =
    Some { box_usage = Aws.Util.option_bind (Aws.Xml.member "BoxUsage" xml) Float.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f ->
               Aws.Query.Pair ("BoxUsage", Float.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f -> "BoxUsage", Float.to_json f) ])

  let of_json j =
    { box_usage = Aws.Util.option_map (Aws.Json.lookup j "BoxUsage") Float.of_json }
end

module DuplicateItemName = struct
  type t = { box_usage : Float.t option }

  let make ?box_usage () = { box_usage }

  let parse xml =
    Some { box_usage = Aws.Util.option_bind (Aws.Xml.member "BoxUsage" xml) Float.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f ->
               Aws.Query.Pair ("BoxUsage", Float.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f -> "BoxUsage", Float.to_json f) ])

  let of_json j =
    { box_usage = Aws.Util.option_map (Aws.Json.lookup j "BoxUsage") Float.of_json }
end

module UpdateCondition = struct
  type t =
    { name : String.t option
    ; value : String.t option
    ; exists : Boolean.t option
    }

  let make ?name ?value ?exists () = { name; value; exists }

  let parse xml =
    Some
      { name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
      ; value = Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse
      ; exists = Aws.Util.option_bind (Aws.Xml.member "Exists" xml) Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.exists (fun f ->
               Aws.Query.Pair ("Exists", Boolean.to_query f))
         ; Aws.Util.option_map v.value (fun f ->
               Aws.Query.Pair ("Value", String.to_query f))
         ; Aws.Util.option_map v.name (fun f ->
               Aws.Query.Pair ("Name", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.exists (fun f -> "Exists", Boolean.to_json f)
         ; Aws.Util.option_map v.value (fun f -> "Value", String.to_json f)
         ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
         ])

  let of_json j =
    { name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
    ; value = Aws.Util.option_map (Aws.Json.lookup j "Value") String.of_json
    ; exists = Aws.Util.option_map (Aws.Json.lookup j "Exists") Boolean.of_json
    }
end

module MissingParameter = struct
  type t = { box_usage : Float.t option }

  let make ?box_usage () = { box_usage }

  let parse xml =
    Some { box_usage = Aws.Util.option_bind (Aws.Xml.member "BoxUsage" xml) Float.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f ->
               Aws.Query.Pair ("BoxUsage", Float.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f -> "BoxUsage", Float.to_json f) ])

  let of_json j =
    { box_usage = Aws.Util.option_map (Aws.Json.lookup j "BoxUsage") Float.of_json }
end

module InvalidParameterValue = struct
  type t = { box_usage : Float.t option }

  let make ?box_usage () = { box_usage }

  let parse xml =
    Some { box_usage = Aws.Util.option_bind (Aws.Xml.member "BoxUsage" xml) Float.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f ->
               Aws.Query.Pair ("BoxUsage", Float.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f -> "BoxUsage", Float.to_json f) ])

  let of_json j =
    { box_usage = Aws.Util.option_map (Aws.Json.lookup j "BoxUsage") Float.of_json }
end

module PutAttributesRequest = struct
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
    { domain_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DomainName"))
    ; item_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ItemName"))
    ; attributes =
        ReplaceableAttributeList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Attributes"))
    ; expected =
        Aws.Util.option_map (Aws.Json.lookup j "Expected") UpdateCondition.of_json
    }
end

module GetAttributesResult = struct
  type t = { attributes : AttributeList.t }

  let make ?(attributes = []) () = { attributes }

  let parse xml = Some { attributes = Aws.Util.of_option [] (AttributeList.parse xml) }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("Attributes.member", AttributeList.to_query v.attributes))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Attributes", AttributeList.to_json v.attributes) ])

  let of_json j =
    { attributes =
        AttributeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Attributes"))
    }
end

module GetAttributesRequest = struct
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
    { domain_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DomainName"))
    ; item_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ItemName"))
    ; attribute_names =
        AttributeNameList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AttributeNames"))
    ; consistent_read =
        Aws.Util.option_map (Aws.Json.lookup j "ConsistentRead") Boolean.of_json
    }
end

module ReplaceableItemList = struct
  type t = ReplaceableItem.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map ReplaceableItem.parse (Aws.Xml.members "Item" xml))

  let to_query v = Aws.Query.to_query_list ReplaceableItem.to_query v

  let to_json v = `List (List.map ReplaceableItem.to_json v)

  let of_json j = Aws.Json.to_list ReplaceableItem.of_json j
end

module BatchPutAttributesRequest = struct
  type t =
    { domain_name : String.t
    ; items : ReplaceableItemList.t
    }

  let make ~domain_name ~items () = { domain_name; items }

  let parse xml =
    Some
      { domain_name =
          Aws.Xml.required
            "DomainName"
            (Aws.Util.option_bind (Aws.Xml.member "DomainName" xml) String.parse)
      ; items = Aws.Xml.required "Items" (ReplaceableItemList.parse xml)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Items.member", ReplaceableItemList.to_query v.items))
         ; Some (Aws.Query.Pair ("DomainName", String.to_query v.domain_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Items", ReplaceableItemList.to_json v.items)
         ; Some ("DomainName", String.to_json v.domain_name)
         ])

  let of_json j =
    { domain_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DomainName"))
    ; items =
        ReplaceableItemList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Items"))
    }
end

module NumberDomainBytesExceeded = struct
  type t = { box_usage : Float.t option }

  let make ?box_usage () = { box_usage }

  let parse xml =
    Some { box_usage = Aws.Util.option_bind (Aws.Xml.member "BoxUsage" xml) Float.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f ->
               Aws.Query.Pair ("BoxUsage", Float.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.box_usage (fun f -> "BoxUsage", Float.to_json f) ])

  let of_json j =
    { box_usage = Aws.Util.option_map (Aws.Json.lookup j "BoxUsage") Float.of_json }
end

module DeleteAttributesRequest = struct
  type t =
    { domain_name : String.t
    ; item_name : String.t
    ; attributes : AttributeList.t
    ; expected : UpdateCondition.t option
    }

  let make ~domain_name ~item_name ?(attributes = []) ?expected () =
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
      ; attributes = Aws.Util.of_option [] (AttributeList.parse xml)
      ; expected =
          Aws.Util.option_bind (Aws.Xml.member "Expected" xml) UpdateCondition.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.expected (fun f ->
               Aws.Query.Pair ("Expected", UpdateCondition.to_query f))
         ; Some
             (Aws.Query.Pair ("Attributes.member", AttributeList.to_query v.attributes))
         ; Some (Aws.Query.Pair ("ItemName", String.to_query v.item_name))
         ; Some (Aws.Query.Pair ("DomainName", String.to_query v.domain_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.expected (fun f -> "Expected", UpdateCondition.to_json f)
         ; Some ("Attributes", AttributeList.to_json v.attributes)
         ; Some ("ItemName", String.to_json v.item_name)
         ; Some ("DomainName", String.to_json v.domain_name)
         ])

  let of_json j =
    { domain_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DomainName"))
    ; item_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ItemName"))
    ; attributes =
        AttributeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Attributes"))
    ; expected =
        Aws.Util.option_map (Aws.Json.lookup j "Expected") UpdateCondition.of_json
    }
end
