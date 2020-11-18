open Aws
open Aws.BaseTypes
open CalendarLib
type calendar = Calendar.t
module ReplaceableAttribute =
  struct
    type t = {
      name: String.t ;
      value: String.t ;
      replace: Boolean.t option }
    let make ~name  ~value  ?replace  () = { name; value; replace }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          value =
            (Xml.required "Value"
               (Util.option_bind (Xml.member "Value" xml) String.parse));
          replace =
            (Util.option_bind (Xml.member "Replace" xml) Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.replace
              (fun f -> Query.Pair ("Replace", (Boolean.to_query f)));
           Some (Query.Pair ("Value", (String.to_query v.value)));
           Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.replace
              (fun f -> ("replace", (Boolean.to_json f)));
           Some ("value", (String.to_json v.value));
           Some ("name", (String.to_json v.name))])
    let of_json j =
      {
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        value = (String.of_json (Util.of_option_exn (Json.lookup j "value")));
        replace = (Util.option_map (Json.lookup j "replace") Boolean.of_json)
      }
  end
module Attribute =
  struct
    type t =
      {
      name: String.t ;
      alternate_name_encoding: String.t option ;
      value: String.t ;
      alternate_value_encoding: String.t option }
    let make ~name  ?alternate_name_encoding  ~value 
      ?alternate_value_encoding  () =
      { name; alternate_name_encoding; value; alternate_value_encoding }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          alternate_name_encoding =
            (Util.option_bind (Xml.member "AlternateNameEncoding" xml)
               String.parse);
          value =
            (Xml.required "Value"
               (Util.option_bind (Xml.member "Value" xml) String.parse));
          alternate_value_encoding =
            (Util.option_bind (Xml.member "AlternateValueEncoding" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.alternate_value_encoding
              (fun f ->
                 Query.Pair ("AlternateValueEncoding", (String.to_query f)));
           Some (Query.Pair ("Value", (String.to_query v.value)));
           Util.option_map v.alternate_name_encoding
             (fun f ->
                Query.Pair ("AlternateNameEncoding", (String.to_query f)));
           Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.alternate_value_encoding
              (fun f -> ("alternate_value_encoding", (String.to_json f)));
           Some ("value", (String.to_json v.value));
           Util.option_map v.alternate_name_encoding
             (fun f -> ("alternate_name_encoding", (String.to_json f)));
           Some ("name", (String.to_json v.name))])
    let of_json j =
      {
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        alternate_name_encoding =
          (Util.option_map (Json.lookup j "alternate_name_encoding")
             String.of_json);
        value = (String.of_json (Util.of_option_exn (Json.lookup j "value")));
        alternate_value_encoding =
          (Util.option_map (Json.lookup j "alternate_value_encoding")
             String.of_json)
      }
  end
module ReplaceableAttributeList =
  struct
    type t = ReplaceableAttribute.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ReplaceableAttribute.parse (Xml.members "Attribute" xml))
    let to_query v = Query.to_query_list ReplaceableAttribute.to_query v
    let to_json v = `List (List.map ReplaceableAttribute.to_json v)
    let of_json j = Json.to_list ReplaceableAttribute.of_json j
  end
module AttributeList =
  struct
    type t = Attribute.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map Attribute.parse (Xml.members "Attribute" xml))
    let to_query v = Query.to_query_list Attribute.to_query v
    let to_json v = `List (List.map Attribute.to_json v)
    let of_json j = Json.to_list Attribute.of_json j
  end
module ReplaceableItem =
  struct
    type t = {
      name: String.t ;
      attributes: ReplaceableAttributeList.t }
    let make ~name  ~attributes  () = { name; attributes }
    let parse xml =
      Some
        {
          name =
            (Xml.required "ItemName"
               (Util.option_bind (Xml.member "ItemName" xml) String.parse));
          attributes =
            (Xml.required "Attributes" (ReplaceableAttributeList.parse xml))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Attributes.member",
                   (ReplaceableAttributeList.to_query v.attributes)));
           Some (Query.Pair ("ItemName", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("attributes", (ReplaceableAttributeList.to_json v.attributes));
           Some ("name", (String.to_json v.name))])
    let of_json j =
      {
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        attributes =
          (ReplaceableAttributeList.of_json
             (Util.of_option_exn (Json.lookup j "attributes")))
      }
  end
module DeletableItem =
  struct
    type t = {
      name: String.t ;
      attributes: AttributeList.t }
    let make ~name  ?(attributes= [])  () = { name; attributes }
    let parse xml =
      Some
        {
          name =
            (Xml.required "ItemName"
               (Util.option_bind (Xml.member "ItemName" xml) String.parse));
          attributes = (Util.of_option [] (AttributeList.parse xml))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Attributes.member", (AttributeList.to_query v.attributes)));
           Some (Query.Pair ("ItemName", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("attributes", (AttributeList.to_json v.attributes));
           Some ("name", (String.to_json v.name))])
    let of_json j =
      {
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        attributes =
          (AttributeList.of_json
             (Util.of_option_exn (Json.lookup j "attributes")))
      }
  end
module Item =
  struct
    type t =
      {
      name: String.t ;
      alternate_name_encoding: String.t option ;
      attributes: AttributeList.t }
    let make ~name  ?alternate_name_encoding  ~attributes  () =
      { name; alternate_name_encoding; attributes }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          alternate_name_encoding =
            (Util.option_bind (Xml.member "AlternateNameEncoding" xml)
               String.parse);
          attributes = (Xml.required "Attributes" (AttributeList.parse xml))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Attributes.member", (AttributeList.to_query v.attributes)));
           Util.option_map v.alternate_name_encoding
             (fun f ->
                Query.Pair ("AlternateNameEncoding", (String.to_query f)));
           Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("attributes", (AttributeList.to_json v.attributes));
           Util.option_map v.alternate_name_encoding
             (fun f -> ("alternate_name_encoding", (String.to_json f)));
           Some ("name", (String.to_json v.name))])
    let of_json j =
      {
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        alternate_name_encoding =
          (Util.option_map (Json.lookup j "alternate_name_encoding")
             String.of_json);
        attributes =
          (AttributeList.of_json
             (Util.of_option_exn (Json.lookup j "attributes")))
      }
  end
module UpdateCondition =
  struct
    type t =
      {
      name: String.t option ;
      value: String.t option ;
      exists: Boolean.t option }
    let make ?name  ?value  ?exists  () = { name; value; exists }
    let parse xml =
      Some
        {
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          value = (Util.option_bind (Xml.member "Value" xml) String.parse);
          exists = (Util.option_bind (Xml.member "Exists" xml) Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.exists
              (fun f -> Query.Pair ("Exists", (Boolean.to_query f)));
           Util.option_map v.value
             (fun f -> Query.Pair ("Value", (String.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.exists
              (fun f -> ("exists", (Boolean.to_json f)));
           Util.option_map v.value (fun f -> ("value", (String.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)))])
    let of_json j =
      {
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        value = (Util.option_map (Json.lookup j "value") String.of_json);
        exists = (Util.option_map (Json.lookup j "exists") Boolean.of_json)
      }
  end
module ReplaceableItemList =
  struct
    type t = ReplaceableItem.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ReplaceableItem.parse (Xml.members "Item" xml))
    let to_query v = Query.to_query_list ReplaceableItem.to_query v
    let to_json v = `List (List.map ReplaceableItem.to_json v)
    let of_json j = Json.to_list ReplaceableItem.of_json j
  end
module AttributeNameList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map String.parse (Xml.members "AttributeName" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module DeletableItemList =
  struct
    type t = DeletableItem.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map DeletableItem.parse (Xml.members "Item" xml))
    let to_query v = Query.to_query_list DeletableItem.to_query v
    let to_json v = `List (List.map DeletableItem.to_json v)
    let of_json j = Json.to_list DeletableItem.of_json j
  end
module ItemList =
  struct
    type t = Item.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Item.parse (Xml.members "Item" xml))
    let to_query v = Query.to_query_list Item.to_query v
    let to_json v = `List (List.map Item.to_json v)
    let of_json j = Json.to_list Item.of_json j
  end
module DomainNameList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "DomainName" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module DeleteAttributesRequest =
  struct
    type t =
      {
      domain_name: String.t ;
      item_name: String.t ;
      attributes: AttributeList.t ;
      expected: UpdateCondition.t option }
    let make ~domain_name  ~item_name  ?(attributes= [])  ?expected  () =
      { domain_name; item_name; attributes; expected }
    let parse xml =
      Some
        {
          domain_name =
            (Xml.required "DomainName"
               (Util.option_bind (Xml.member "DomainName" xml) String.parse));
          item_name =
            (Xml.required "ItemName"
               (Util.option_bind (Xml.member "ItemName" xml) String.parse));
          attributes = (Util.of_option [] (AttributeList.parse xml));
          expected =
            (Util.option_bind (Xml.member "Expected" xml)
               UpdateCondition.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.expected
              (fun f -> Query.Pair ("Expected", (UpdateCondition.to_query f)));
           Some
             (Query.Pair
                ("Attributes.member", (AttributeList.to_query v.attributes)));
           Some (Query.Pair ("ItemName", (String.to_query v.item_name)));
           Some (Query.Pair ("DomainName", (String.to_query v.domain_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.expected
              (fun f -> ("expected", (UpdateCondition.to_json f)));
           Some ("attributes", (AttributeList.to_json v.attributes));
           Some ("item_name", (String.to_json v.item_name));
           Some ("domain_name", (String.to_json v.domain_name))])
    let of_json j =
      {
        domain_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "domain_name")));
        item_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "item_name")));
        attributes =
          (AttributeList.of_json
             (Util.of_option_exn (Json.lookup j "attributes")));
        expected =
          (Util.option_map (Json.lookup j "expected") UpdateCondition.of_json)
      }
  end
module NumberDomainBytesExceeded =
  struct
    type t = {
      box_usage: Float.t option }
    let make ?box_usage  () = { box_usage }
    let parse xml =
      Some
        {
          box_usage =
            (Util.option_bind (Xml.member "BoxUsage" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> Query.Pair ("BoxUsage", (Float.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> ("box_usage", (Float.to_json f)))])
    let of_json j =
      {
        box_usage =
          (Util.option_map (Json.lookup j "box_usage") Float.of_json)
      }
  end
module BatchPutAttributesRequest =
  struct
    type t = {
      domain_name: String.t ;
      items: ReplaceableItemList.t }
    let make ~domain_name  ~items  () = { domain_name; items }
    let parse xml =
      Some
        {
          domain_name =
            (Xml.required "DomainName"
               (Util.option_bind (Xml.member "DomainName" xml) String.parse));
          items = (Xml.required "Items" (ReplaceableItemList.parse xml))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Items.member", (ReplaceableItemList.to_query v.items)));
           Some (Query.Pair ("DomainName", (String.to_query v.domain_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("items", (ReplaceableItemList.to_json v.items));
           Some ("domain_name", (String.to_json v.domain_name))])
    let of_json j =
      {
        domain_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "domain_name")));
        items =
          (ReplaceableItemList.of_json
             (Util.of_option_exn (Json.lookup j "items")))
      }
  end
module GetAttributesRequest =
  struct
    type t =
      {
      domain_name: String.t ;
      item_name: String.t ;
      attribute_names: AttributeNameList.t ;
      consistent_read: Boolean.t option }
    let make ~domain_name  ~item_name  ?(attribute_names= []) 
      ?consistent_read  () =
      { domain_name; item_name; attribute_names; consistent_read }
    let parse xml =
      Some
        {
          domain_name =
            (Xml.required "DomainName"
               (Util.option_bind (Xml.member "DomainName" xml) String.parse));
          item_name =
            (Xml.required "ItemName"
               (Util.option_bind (Xml.member "ItemName" xml) String.parse));
          attribute_names = (Util.of_option [] (AttributeNameList.parse xml));
          consistent_read =
            (Util.option_bind (Xml.member "ConsistentRead" xml) Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.consistent_read
              (fun f -> Query.Pair ("ConsistentRead", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("AttributeNames.member",
                  (AttributeNameList.to_query v.attribute_names)));
           Some (Query.Pair ("ItemName", (String.to_query v.item_name)));
           Some (Query.Pair ("DomainName", (String.to_query v.domain_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.consistent_read
              (fun f -> ("consistent_read", (Boolean.to_json f)));
           Some
             ("attribute_names",
               (AttributeNameList.to_json v.attribute_names));
           Some ("item_name", (String.to_json v.item_name));
           Some ("domain_name", (String.to_json v.domain_name))])
    let of_json j =
      {
        domain_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "domain_name")));
        item_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "item_name")));
        attribute_names =
          (AttributeNameList.of_json
             (Util.of_option_exn (Json.lookup j "attribute_names")));
        consistent_read =
          (Util.option_map (Json.lookup j "consistent_read") Boolean.of_json)
      }
  end
module GetAttributesResult =
  struct
    type t = {
      attributes: AttributeList.t }
    let make ?(attributes= [])  () = { attributes }
    let parse xml =
      Some { attributes = (Util.of_option [] (AttributeList.parse xml)) }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Attributes.member", (AttributeList.to_query v.attributes)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("attributes", (AttributeList.to_json v.attributes))])
    let of_json j =
      {
        attributes =
          (AttributeList.of_json
             (Util.of_option_exn (Json.lookup j "attributes")))
      }
  end
module PutAttributesRequest =
  struct
    type t =
      {
      domain_name: String.t ;
      item_name: String.t ;
      attributes: ReplaceableAttributeList.t ;
      expected: UpdateCondition.t option }
    let make ~domain_name  ~item_name  ~attributes  ?expected  () =
      { domain_name; item_name; attributes; expected }
    let parse xml =
      Some
        {
          domain_name =
            (Xml.required "DomainName"
               (Util.option_bind (Xml.member "DomainName" xml) String.parse));
          item_name =
            (Xml.required "ItemName"
               (Util.option_bind (Xml.member "ItemName" xml) String.parse));
          attributes =
            (Xml.required "Attributes" (ReplaceableAttributeList.parse xml));
          expected =
            (Util.option_bind (Xml.member "Expected" xml)
               UpdateCondition.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.expected
              (fun f -> Query.Pair ("Expected", (UpdateCondition.to_query f)));
           Some
             (Query.Pair
                ("Attributes.member",
                  (ReplaceableAttributeList.to_query v.attributes)));
           Some (Query.Pair ("ItemName", (String.to_query v.item_name)));
           Some (Query.Pair ("DomainName", (String.to_query v.domain_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.expected
              (fun f -> ("expected", (UpdateCondition.to_json f)));
           Some
             ("attributes", (ReplaceableAttributeList.to_json v.attributes));
           Some ("item_name", (String.to_json v.item_name));
           Some ("domain_name", (String.to_json v.domain_name))])
    let of_json j =
      {
        domain_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "domain_name")));
        item_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "item_name")));
        attributes =
          (ReplaceableAttributeList.of_json
             (Util.of_option_exn (Json.lookup j "attributes")));
        expected =
          (Util.option_map (Json.lookup j "expected") UpdateCondition.of_json)
      }
  end
module InvalidParameterValue =
  struct
    type t = {
      box_usage: Float.t option }
    let make ?box_usage  () = { box_usage }
    let parse xml =
      Some
        {
          box_usage =
            (Util.option_bind (Xml.member "BoxUsage" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> Query.Pair ("BoxUsage", (Float.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> ("box_usage", (Float.to_json f)))])
    let of_json j =
      {
        box_usage =
          (Util.option_map (Json.lookup j "box_usage") Float.of_json)
      }
  end
module MissingParameter =
  struct
    type t = {
      box_usage: Float.t option }
    let make ?box_usage  () = { box_usage }
    let parse xml =
      Some
        {
          box_usage =
            (Util.option_bind (Xml.member "BoxUsage" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> Query.Pair ("BoxUsage", (Float.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> ("box_usage", (Float.to_json f)))])
    let of_json j =
      {
        box_usage =
          (Util.option_map (Json.lookup j "box_usage") Float.of_json)
      }
  end
module DuplicateItemName =
  struct
    type t = {
      box_usage: Float.t option }
    let make ?box_usage  () = { box_usage }
    let parse xml =
      Some
        {
          box_usage =
            (Util.option_bind (Xml.member "BoxUsage" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> Query.Pair ("BoxUsage", (Float.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> ("box_usage", (Float.to_json f)))])
    let of_json j =
      {
        box_usage =
          (Util.option_map (Json.lookup j "box_usage") Float.of_json)
      }
  end
module NumberSubmittedAttributesExceeded =
  struct
    type t = {
      box_usage: Float.t option }
    let make ?box_usage  () = { box_usage }
    let parse xml =
      Some
        {
          box_usage =
            (Util.option_bind (Xml.member "BoxUsage" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> Query.Pair ("BoxUsage", (Float.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> ("box_usage", (Float.to_json f)))])
    let of_json j =
      {
        box_usage =
          (Util.option_map (Json.lookup j "box_usage") Float.of_json)
      }
  end
module BatchDeleteAttributesRequest =
  struct
    type t = {
      domain_name: String.t ;
      items: DeletableItemList.t }
    let make ~domain_name  ~items  () = { domain_name; items }
    let parse xml =
      Some
        {
          domain_name =
            (Xml.required "DomainName"
               (Util.option_bind (Xml.member "DomainName" xml) String.parse));
          items = (Xml.required "Items" (DeletableItemList.parse xml))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Items.member", (DeletableItemList.to_query v.items)));
           Some (Query.Pair ("DomainName", (String.to_query v.domain_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("items", (DeletableItemList.to_json v.items));
           Some ("domain_name", (String.to_json v.domain_name))])
    let of_json j =
      {
        domain_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "domain_name")));
        items =
          (DeletableItemList.of_json
             (Util.of_option_exn (Json.lookup j "items")))
      }
  end
module NumberDomainsExceeded =
  struct
    type t = {
      box_usage: Float.t option }
    let make ?box_usage  () = { box_usage }
    let parse xml =
      Some
        {
          box_usage =
            (Util.option_bind (Xml.member "BoxUsage" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> Query.Pair ("BoxUsage", (Float.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> ("box_usage", (Float.to_json f)))])
    let of_json j =
      {
        box_usage =
          (Util.option_map (Json.lookup j "box_usage") Float.of_json)
      }
  end
module NoSuchDomain =
  struct
    type t = {
      box_usage: Float.t option }
    let make ?box_usage  () = { box_usage }
    let parse xml =
      Some
        {
          box_usage =
            (Util.option_bind (Xml.member "BoxUsage" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> Query.Pair ("BoxUsage", (Float.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> ("box_usage", (Float.to_json f)))])
    let of_json j =
      {
        box_usage =
          (Util.option_map (Json.lookup j "box_usage") Float.of_json)
      }
  end
module DomainMetadataRequest =
  struct
    type t = {
      domain_name: String.t }
    let make ~domain_name  () = { domain_name }
    let parse xml =
      Some
        {
          domain_name =
            (Xml.required "DomainName"
               (Util.option_bind (Xml.member "DomainName" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("DomainName", (String.to_query v.domain_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("domain_name", (String.to_json v.domain_name))])
    let of_json j =
      {
        domain_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "domain_name")))
      }
  end
module SelectResult =
  struct
    type t = {
      items: ItemList.t ;
      next_token: String.t option }
    let make ?(items= [])  ?next_token  () = { items; next_token }
    let parse xml =
      Some
        {
          items = (Util.of_option [] (ItemList.parse xml));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some (Query.Pair ("Items.member", (ItemList.to_query v.items)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some ("items", (ItemList.to_json v.items))])
    let of_json j =
      {
        items =
          (ItemList.of_json (Util.of_option_exn (Json.lookup j "items")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module ListDomainsRequest =
  struct
    type t =
      {
      max_number_of_domains: Integer.t option ;
      next_token: String.t option }
    let make ?max_number_of_domains  ?next_token  () =
      { max_number_of_domains; next_token }
    let parse xml =
      Some
        {
          max_number_of_domains =
            (Util.option_bind (Xml.member "MaxNumberOfDomains" xml)
               Integer.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.max_number_of_domains
             (fun f ->
                Query.Pair ("MaxNumberOfDomains", (Integer.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.max_number_of_domains
             (fun f -> ("max_number_of_domains", (Integer.to_json f)))])
    let of_json j =
      {
        max_number_of_domains =
          (Util.option_map (Json.lookup j "max_number_of_domains")
             Integer.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module SelectRequest =
  struct
    type t =
      {
      select_expression: String.t ;
      next_token: String.t option ;
      consistent_read: Boolean.t option }
    let make ~select_expression  ?next_token  ?consistent_read  () =
      { select_expression; next_token; consistent_read }
    let parse xml =
      Some
        {
          select_expression =
            (Xml.required "SelectExpression"
               (Util.option_bind (Xml.member "SelectExpression" xml)
                  String.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          consistent_read =
            (Util.option_bind (Xml.member "ConsistentRead" xml) Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.consistent_read
              (fun f -> Query.Pair ("ConsistentRead", (Boolean.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("SelectExpression", (String.to_query v.select_expression)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.consistent_read
              (fun f -> ("consistent_read", (Boolean.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Some ("select_expression", (String.to_json v.select_expression))])
    let of_json j =
      {
        select_expression =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "select_expression")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        consistent_read =
          (Util.option_map (Json.lookup j "consistent_read") Boolean.of_json)
      }
  end
module DomainMetadataResult =
  struct
    type t =
      {
      item_count: Integer.t option ;
      item_names_size_bytes: Long.t option ;
      attribute_name_count: Integer.t option ;
      attribute_names_size_bytes: Long.t option ;
      attribute_value_count: Integer.t option ;
      attribute_values_size_bytes: Long.t option ;
      timestamp: Integer.t option }
    let make ?item_count  ?item_names_size_bytes  ?attribute_name_count 
      ?attribute_names_size_bytes  ?attribute_value_count 
      ?attribute_values_size_bytes  ?timestamp  () =
      {
        item_count;
        item_names_size_bytes;
        attribute_name_count;
        attribute_names_size_bytes;
        attribute_value_count;
        attribute_values_size_bytes;
        timestamp
      }
    let parse xml =
      Some
        {
          item_count =
            (Util.option_bind (Xml.member "ItemCount" xml) Integer.parse);
          item_names_size_bytes =
            (Util.option_bind (Xml.member "ItemNamesSizeBytes" xml)
               Long.parse);
          attribute_name_count =
            (Util.option_bind (Xml.member "AttributeNameCount" xml)
               Integer.parse);
          attribute_names_size_bytes =
            (Util.option_bind (Xml.member "AttributeNamesSizeBytes" xml)
               Long.parse);
          attribute_value_count =
            (Util.option_bind (Xml.member "AttributeValueCount" xml)
               Integer.parse);
          attribute_values_size_bytes =
            (Util.option_bind (Xml.member "AttributeValuesSizeBytes" xml)
               Long.parse);
          timestamp =
            (Util.option_bind (Xml.member "Timestamp" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.timestamp
              (fun f -> Query.Pair ("Timestamp", (Integer.to_query f)));
           Util.option_map v.attribute_values_size_bytes
             (fun f ->
                Query.Pair ("AttributeValuesSizeBytes", (Long.to_query f)));
           Util.option_map v.attribute_value_count
             (fun f ->
                Query.Pair ("AttributeValueCount", (Integer.to_query f)));
           Util.option_map v.attribute_names_size_bytes
             (fun f ->
                Query.Pair ("AttributeNamesSizeBytes", (Long.to_query f)));
           Util.option_map v.attribute_name_count
             (fun f ->
                Query.Pair ("AttributeNameCount", (Integer.to_query f)));
           Util.option_map v.item_names_size_bytes
             (fun f -> Query.Pair ("ItemNamesSizeBytes", (Long.to_query f)));
           Util.option_map v.item_count
             (fun f -> Query.Pair ("ItemCount", (Integer.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.timestamp
              (fun f -> ("timestamp", (Integer.to_json f)));
           Util.option_map v.attribute_values_size_bytes
             (fun f -> ("attribute_values_size_bytes", (Long.to_json f)));
           Util.option_map v.attribute_value_count
             (fun f -> ("attribute_value_count", (Integer.to_json f)));
           Util.option_map v.attribute_names_size_bytes
             (fun f -> ("attribute_names_size_bytes", (Long.to_json f)));
           Util.option_map v.attribute_name_count
             (fun f -> ("attribute_name_count", (Integer.to_json f)));
           Util.option_map v.item_names_size_bytes
             (fun f -> ("item_names_size_bytes", (Long.to_json f)));
           Util.option_map v.item_count
             (fun f -> ("item_count", (Integer.to_json f)))])
    let of_json j =
      {
        item_count =
          (Util.option_map (Json.lookup j "item_count") Integer.of_json);
        item_names_size_bytes =
          (Util.option_map (Json.lookup j "item_names_size_bytes")
             Long.of_json);
        attribute_name_count =
          (Util.option_map (Json.lookup j "attribute_name_count")
             Integer.of_json);
        attribute_names_size_bytes =
          (Util.option_map (Json.lookup j "attribute_names_size_bytes")
             Long.of_json);
        attribute_value_count =
          (Util.option_map (Json.lookup j "attribute_value_count")
             Integer.of_json);
        attribute_values_size_bytes =
          (Util.option_map (Json.lookup j "attribute_values_size_bytes")
             Long.of_json);
        timestamp =
          (Util.option_map (Json.lookup j "timestamp") Integer.of_json)
      }
  end
module RequestTimeout =
  struct
    type t = {
      box_usage: Float.t option }
    let make ?box_usage  () = { box_usage }
    let parse xml =
      Some
        {
          box_usage =
            (Util.option_bind (Xml.member "BoxUsage" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> Query.Pair ("BoxUsage", (Float.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> ("box_usage", (Float.to_json f)))])
    let of_json j =
      {
        box_usage =
          (Util.option_map (Json.lookup j "box_usage") Float.of_json)
      }
  end
module InvalidNextToken =
  struct
    type t = {
      box_usage: Float.t option }
    let make ?box_usage  () = { box_usage }
    let parse xml =
      Some
        {
          box_usage =
            (Util.option_bind (Xml.member "BoxUsage" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> Query.Pair ("BoxUsage", (Float.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> ("box_usage", (Float.to_json f)))])
    let of_json j =
      {
        box_usage =
          (Util.option_map (Json.lookup j "box_usage") Float.of_json)
      }
  end
module CreateDomainRequest =
  struct
    type t = {
      domain_name: String.t }
    let make ~domain_name  () = { domain_name }
    let parse xml =
      Some
        {
          domain_name =
            (Xml.required "DomainName"
               (Util.option_bind (Xml.member "DomainName" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("DomainName", (String.to_query v.domain_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("domain_name", (String.to_json v.domain_name))])
    let of_json j =
      {
        domain_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "domain_name")))
      }
  end
module AttributeDoesNotExist =
  struct
    type t = {
      box_usage: Float.t option }
    let make ?box_usage  () = { box_usage }
    let parse xml =
      Some
        {
          box_usage =
            (Util.option_bind (Xml.member "BoxUsage" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> Query.Pair ("BoxUsage", (Float.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> ("box_usage", (Float.to_json f)))])
    let of_json j =
      {
        box_usage =
          (Util.option_map (Json.lookup j "box_usage") Float.of_json)
      }
  end
module InvalidNumberValueTests =
  struct
    type t = {
      box_usage: Float.t option }
    let make ?box_usage  () = { box_usage }
    let parse xml =
      Some
        {
          box_usage =
            (Util.option_bind (Xml.member "BoxUsage" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> Query.Pair ("BoxUsage", (Float.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> ("box_usage", (Float.to_json f)))])
    let of_json j =
      {
        box_usage =
          (Util.option_map (Json.lookup j "box_usage") Float.of_json)
      }
  end
module InvalidNumberPredicates =
  struct
    type t = {
      box_usage: Float.t option }
    let make ?box_usage  () = { box_usage }
    let parse xml =
      Some
        {
          box_usage =
            (Util.option_bind (Xml.member "BoxUsage" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> Query.Pair ("BoxUsage", (Float.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> ("box_usage", (Float.to_json f)))])
    let of_json j =
      {
        box_usage =
          (Util.option_map (Json.lookup j "box_usage") Float.of_json)
      }
  end
module ListDomainsResult =
  struct
    type t = {
      domain_names: DomainNameList.t ;
      next_token: String.t option }
    let make ?(domain_names= [])  ?next_token  () =
      { domain_names; next_token }
    let parse xml =
      Some
        {
          domain_names = (Util.of_option [] (DomainNameList.parse xml));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("DomainNames.member",
                  (DomainNameList.to_query v.domain_names)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some ("domain_names", (DomainNameList.to_json v.domain_names))])
    let of_json j =
      {
        domain_names =
          (DomainNameList.of_json
             (Util.of_option_exn (Json.lookup j "domain_names")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module InvalidQueryExpression =
  struct
    type t = {
      box_usage: Float.t option }
    let make ?box_usage  () = { box_usage }
    let parse xml =
      Some
        {
          box_usage =
            (Util.option_bind (Xml.member "BoxUsage" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> Query.Pair ("BoxUsage", (Float.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> ("box_usage", (Float.to_json f)))])
    let of_json j =
      {
        box_usage =
          (Util.option_map (Json.lookup j "box_usage") Float.of_json)
      }
  end
module TooManyRequestedAttributes =
  struct
    type t = {
      box_usage: Float.t option }
    let make ?box_usage  () = { box_usage }
    let parse xml =
      Some
        {
          box_usage =
            (Util.option_bind (Xml.member "BoxUsage" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> Query.Pair ("BoxUsage", (Float.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> ("box_usage", (Float.to_json f)))])
    let of_json j =
      {
        box_usage =
          (Util.option_map (Json.lookup j "box_usage") Float.of_json)
      }
  end
module NumberItemAttributesExceeded =
  struct
    type t = {
      box_usage: Float.t option }
    let make ?box_usage  () = { box_usage }
    let parse xml =
      Some
        {
          box_usage =
            (Util.option_bind (Xml.member "BoxUsage" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> Query.Pair ("BoxUsage", (Float.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> ("box_usage", (Float.to_json f)))])
    let of_json j =
      {
        box_usage =
          (Util.option_map (Json.lookup j "box_usage") Float.of_json)
      }
  end
module DeleteDomainRequest =
  struct
    type t = {
      domain_name: String.t }
    let make ~domain_name  () = { domain_name }
    let parse xml =
      Some
        {
          domain_name =
            (Xml.required "DomainName"
               (Util.option_bind (Xml.member "DomainName" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("DomainName", (String.to_query v.domain_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("domain_name", (String.to_json v.domain_name))])
    let of_json j =
      {
        domain_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "domain_name")))
      }
  end
module NumberSubmittedItemsExceeded =
  struct
    type t = {
      box_usage: Float.t option }
    let make ?box_usage  () = { box_usage }
    let parse xml =
      Some
        {
          box_usage =
            (Util.option_bind (Xml.member "BoxUsage" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> Query.Pair ("BoxUsage", (Float.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> ("box_usage", (Float.to_json f)))])
    let of_json j =
      {
        box_usage =
          (Util.option_map (Json.lookup j "box_usage") Float.of_json)
      }
  end
module NumberDomainAttributesExceeded =
  struct
    type t = {
      box_usage: Float.t option }
    let make ?box_usage  () = { box_usage }
    let parse xml =
      Some
        {
          box_usage =
            (Util.option_bind (Xml.member "BoxUsage" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> Query.Pair ("BoxUsage", (Float.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.box_usage
              (fun f -> ("box_usage", (Float.to_json f)))])
    let of_json j =
      {
        box_usage =
          (Util.option_map (Json.lookup j "box_usage") Float.of_json)
      }
  end