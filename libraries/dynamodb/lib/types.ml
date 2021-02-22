open Aws
open Aws.BaseTypes
open CalendarLib
type calendar = Calendar.t
module BinarySetAttributeValue =
  struct
    type t = Blob.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Blob.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Blob.to_query v
    let to_json v = `List (List.map Blob.to_json v)
    let of_json j = Json.to_list Blob.of_json j
  end
module NumberSetAttributeValue =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module StringSetAttributeValue =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module MapAttributeValue =
  struct
    type t = (String.t, AttributeValue.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Query.to_query_hashtbl String.to_string AttributeValue.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (AttributeValue.to_json v))
                  :: acc) v [])
    let of_json j = Json.to_hashtbl String.of_string AttributeValue.of_json j
  end
module ListAttributeValue =
  struct
    type t = AttributeValue.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map AttributeValue.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list AttributeValue.to_query v
    let to_json v = `List (List.map AttributeValue.to_json v)
    let of_json j = Json.to_list AttributeValue.of_json j
  end
module AttributeValue =
  struct
    type t =
      {
      s: String.t option ;
      n: String.t option ;
      b: Blob.t option ;
      s_s: StringSetAttributeValue.t ;
      n_s: NumberSetAttributeValue.t ;
      b_s: BinarySetAttributeValue.t ;
      m: MapAttributeValue.t option ;
      l: ListAttributeValue.t ;
      n_u_l_l: Boolean.t option ;
      b_o_o_l: Boolean.t option }
    let make ?s  ?n  ?b  ?(s_s= [])  ?(n_s= [])  ?(b_s= [])  ?m  ?(l= []) 
      ?n_u_l_l  ?b_o_o_l  () =
      { s; n; b; s_s; n_s; b_s; m; l; n_u_l_l; b_o_o_l }
    let parse xml =
      Some
        {
          s = (Util.option_bind (Xml.member "S" xml) String.parse);
          n = (Util.option_bind (Xml.member "N" xml) String.parse);
          b = (Util.option_bind (Xml.member "B" xml) Blob.parse);
          s_s =
            (Util.of_option []
               (Util.option_bind (Xml.member "SS" xml)
                  StringSetAttributeValue.parse));
          n_s =
            (Util.of_option []
               (Util.option_bind (Xml.member "NS" xml)
                  NumberSetAttributeValue.parse));
          b_s =
            (Util.of_option []
               (Util.option_bind (Xml.member "BS" xml)
                  BinarySetAttributeValue.parse));
          m = (Util.option_bind (Xml.member "M" xml) MapAttributeValue.parse);
          l =
            (Util.of_option []
               (Util.option_bind (Xml.member "L" xml)
                  ListAttributeValue.parse));
          n_u_l_l = (Util.option_bind (Xml.member "NULL" xml) Boolean.parse);
          b_o_o_l = (Util.option_bind (Xml.member "BOOL" xml) Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.b_o_o_l
              (fun f -> Query.Pair ("BOOL", (Boolean.to_query f)));
           Util.option_map v.n_u_l_l
             (fun f -> Query.Pair ("NULL", (Boolean.to_query f)));
           Some (Query.Pair ("L.member", (ListAttributeValue.to_query v.l)));
           Util.option_map v.m
             (fun f -> Query.Pair ("M", (MapAttributeValue.to_query f)));
           Some
             (Query.Pair
                ("BS.member", (BinarySetAttributeValue.to_query v.b_s)));
           Some
             (Query.Pair
                ("NS.member", (NumberSetAttributeValue.to_query v.n_s)));
           Some
             (Query.Pair
                ("SS.member", (StringSetAttributeValue.to_query v.s_s)));
           Util.option_map v.b (fun f -> Query.Pair ("B", (Blob.to_query f)));
           Util.option_map v.n
             (fun f -> Query.Pair ("N", (String.to_query f)));
           Util.option_map v.s
             (fun f -> Query.Pair ("S", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.b_o_o_l
              (fun f -> ("b_o_o_l", (Boolean.to_json f)));
           Util.option_map v.n_u_l_l
             (fun f -> ("n_u_l_l", (Boolean.to_json f)));
           Some ("l", (ListAttributeValue.to_json v.l));
           Util.option_map v.m
             (fun f -> ("m", (MapAttributeValue.to_json f)));
           Some ("b_s", (BinarySetAttributeValue.to_json v.b_s));
           Some ("n_s", (NumberSetAttributeValue.to_json v.n_s));
           Some ("s_s", (StringSetAttributeValue.to_json v.s_s));
           Util.option_map v.b (fun f -> ("b", (Blob.to_json f)));
           Util.option_map v.n (fun f -> ("n", (String.to_json f)));
           Util.option_map v.s (fun f -> ("s", (String.to_json f)))])
    let of_json j =
      {
        s = (Util.option_map (Json.lookup j "s") String.of_json);
        n = (Util.option_map (Json.lookup j "n") String.of_json);
        b = (Util.option_map (Json.lookup j "b") Blob.of_json);
        s_s =
          (StringSetAttributeValue.of_json
             (Util.of_option_exn (Json.lookup j "s_s")));
        n_s =
          (NumberSetAttributeValue.of_json
             (Util.of_option_exn (Json.lookup j "n_s")));
        b_s =
          (BinarySetAttributeValue.of_json
             (Util.of_option_exn (Json.lookup j "b_s")));
        m = (Util.option_map (Json.lookup j "m") MapAttributeValue.of_json);
        l =
          (ListAttributeValue.of_json
             (Util.of_option_exn (Json.lookup j "l")));
        n_u_l_l = (Util.option_map (Json.lookup j "n_u_l_l") Boolean.of_json);
        b_o_o_l = (Util.option_map (Json.lookup j "b_o_o_l") Boolean.of_json)
      }
  end
module KeyType =
  struct
    type t =
      | HASH 
      | RANGE 
    let str_to_t = [("RANGE", RANGE); ("HASH", HASH)]
    let t_to_str = [(RANGE, "RANGE"); (HASH, "HASH")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module Key =
  struct
    type t = (String.t, AttributeValue.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Query.to_query_hashtbl String.to_string AttributeValue.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (AttributeValue.to_json v))
                  :: acc) v [])
    let of_json j = Json.to_hashtbl String.of_string AttributeValue.of_json j
  end
module PutItemInputAttributeMap =
  struct
    type t = (String.t, AttributeValue.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Query.to_query_hashtbl String.to_string AttributeValue.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (AttributeValue.to_json v))
                  :: acc) v [])
    let of_json j = Json.to_hashtbl String.of_string AttributeValue.of_json j
  end
module KeySchemaElement =
  struct
    type t = {
      attribute_name: String.t ;
      key_type: KeyType.t }
    let make ~attribute_name  ~key_type  () = { attribute_name; key_type }
    let parse xml =
      Some
        {
          attribute_name =
            (Xml.required "AttributeName"
               (Util.option_bind (Xml.member "AttributeName" xml)
                  String.parse));
          key_type =
            (Xml.required "KeyType"
               (Util.option_bind (Xml.member "KeyType" xml) KeyType.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("KeyType", (KeyType.to_query v.key_type)));
           Some
             (Query.Pair
                ("AttributeName", (String.to_query v.attribute_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("key_type", (KeyType.to_json v.key_type));
           Some ("attribute_name", (String.to_json v.attribute_name))])
    let of_json j =
      {
        attribute_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "attribute_name")));
        key_type =
          (KeyType.of_json (Util.of_option_exn (Json.lookup j "key_type")))
      }
  end
module NonKeyAttributeNameList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module ProjectionType =
  struct
    type t =
      | ALL 
      | KEYS_ONLY 
      | INCLUDE 
    let str_to_t =
      [("INCLUDE", INCLUDE); ("KEYS_ONLY", KEYS_ONLY); ("ALL", ALL)]
    let t_to_str =
      [(INCLUDE, "INCLUDE"); (KEYS_ONLY, "KEYS_ONLY"); (ALL, "ALL")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module DeleteRequest =
  struct
    type t = {
      key: Key.t }
    let make ~key  () = { key }
    let parse xml =
      Some
        {
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) Key.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Key", (Key.to_query v.key)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("key", (Key.to_json v.key))])
    let of_json j =
      { key = (Key.of_json (Util.of_option_exn (Json.lookup j "key"))) }
  end
module PutRequest =
  struct
    type t = {
      item: PutItemInputAttributeMap.t }
    let make ~item  () = { item }
    let parse xml =
      Some
        {
          item =
            (Xml.required "Item"
               (Util.option_bind (Xml.member "Item" xml)
                  PutItemInputAttributeMap.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Item", (PutItemInputAttributeMap.to_query v.item)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("item", (PutItemInputAttributeMap.to_json v.item))])
    let of_json j =
      {
        item =
          (PutItemInputAttributeMap.of_json
             (Util.of_option_exn (Json.lookup j "item")))
      }
  end
module Capacity =
  struct
    type t = {
      capacity_units: Double.t option }
    let make ?capacity_units  () = { capacity_units }
    let parse xml =
      Some
        {
          capacity_units =
            (Util.option_bind (Xml.member "CapacityUnits" xml) Double.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.capacity_units
              (fun f -> Query.Pair ("CapacityUnits", (Double.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.capacity_units
              (fun f -> ("capacity_units", (Double.to_json f)))])
    let of_json j =
      {
        capacity_units =
          (Util.option_map (Json.lookup j "capacity_units") Double.of_json)
      }
  end
module ItemCollectionKeyAttributeMap =
  struct
    type t = (String.t, AttributeValue.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Query.to_query_hashtbl String.to_string AttributeValue.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (AttributeValue.to_json v))
                  :: acc) v [])
    let of_json j = Json.to_hashtbl String.of_string AttributeValue.of_json j
  end
module ItemCollectionSizeEstimateRange =
  struct
    type t = Double.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Double.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Double.to_query v
    let to_json v = `List (List.map Double.to_json v)
    let of_json j = Json.to_list Double.of_json j
  end
module ScalarAttributeType =
  struct
    type t =
      | S 
      | N 
      | B 
    let str_to_t = [("B", B); ("N", N); ("S", S)]
    let t_to_str = [(B, "B"); (N, "N"); (S, "S")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module IndexStatus =
  struct
    type t =
      | CREATING 
      | UPDATING 
      | DELETING 
      | ACTIVE 
    let str_to_t =
      [("ACTIVE", ACTIVE);
      ("DELETING", DELETING);
      ("UPDATING", UPDATING);
      ("CREATING", CREATING)]
    let t_to_str =
      [(ACTIVE, "ACTIVE");
      (DELETING, "DELETING");
      (UPDATING, "UPDATING");
      (CREATING, "CREATING")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module KeySchema =
  struct
    type t = KeySchemaElement.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map KeySchemaElement.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list KeySchemaElement.to_query v
    let to_json v = `List (List.map KeySchemaElement.to_json v)
    let of_json j = Json.to_list KeySchemaElement.of_json j
  end
module Projection =
  struct
    type t =
      {
      projection_type: ProjectionType.t option ;
      non_key_attributes: NonKeyAttributeNameList.t }
    let make ?projection_type  ?(non_key_attributes= [])  () =
      { projection_type; non_key_attributes }
    let parse xml =
      Some
        {
          projection_type =
            (Util.option_bind (Xml.member "ProjectionType" xml)
               ProjectionType.parse);
          non_key_attributes =
            (Util.of_option []
               (Util.option_bind (Xml.member "NonKeyAttributes" xml)
                  NonKeyAttributeNameList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("NonKeyAttributes.member",
                   (NonKeyAttributeNameList.to_query v.non_key_attributes)));
           Util.option_map v.projection_type
             (fun f ->
                Query.Pair ("ProjectionType", (ProjectionType.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("non_key_attributes",
                (NonKeyAttributeNameList.to_json v.non_key_attributes));
           Util.option_map v.projection_type
             (fun f -> ("projection_type", (ProjectionType.to_json f)))])
    let of_json j =
      {
        projection_type =
          (Util.option_map (Json.lookup j "projection_type")
             ProjectionType.of_json);
        non_key_attributes =
          (NonKeyAttributeNameList.of_json
             (Util.of_option_exn (Json.lookup j "non_key_attributes")))
      }
  end
module ProvisionedThroughputDescription =
  struct
    type t =
      {
      last_increase_date_time: DateTime.t option ;
      last_decrease_date_time: DateTime.t option ;
      number_of_decreases_today: Long.t option ;
      read_capacity_units: Long.t option ;
      write_capacity_units: Long.t option }
    let make ?last_increase_date_time  ?last_decrease_date_time 
      ?number_of_decreases_today  ?read_capacity_units  ?write_capacity_units
       () =
      {
        last_increase_date_time;
        last_decrease_date_time;
        number_of_decreases_today;
        read_capacity_units;
        write_capacity_units
      }
    let parse xml =
      Some
        {
          last_increase_date_time =
            (Util.option_bind (Xml.member "LastIncreaseDateTime" xml)
               DateTime.parse);
          last_decrease_date_time =
            (Util.option_bind (Xml.member "LastDecreaseDateTime" xml)
               DateTime.parse);
          number_of_decreases_today =
            (Util.option_bind (Xml.member "NumberOfDecreasesToday" xml)
               Long.parse);
          read_capacity_units =
            (Util.option_bind (Xml.member "ReadCapacityUnits" xml) Long.parse);
          write_capacity_units =
            (Util.option_bind (Xml.member "WriteCapacityUnits" xml)
               Long.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.write_capacity_units
              (fun f -> Query.Pair ("WriteCapacityUnits", (Long.to_query f)));
           Util.option_map v.read_capacity_units
             (fun f -> Query.Pair ("ReadCapacityUnits", (Long.to_query f)));
           Util.option_map v.number_of_decreases_today
             (fun f ->
                Query.Pair ("NumberOfDecreasesToday", (Long.to_query f)));
           Util.option_map v.last_decrease_date_time
             (fun f ->
                Query.Pair ("LastDecreaseDateTime", (DateTime.to_query f)));
           Util.option_map v.last_increase_date_time
             (fun f ->
                Query.Pair ("LastIncreaseDateTime", (DateTime.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.write_capacity_units
              (fun f -> ("write_capacity_units", (Long.to_json f)));
           Util.option_map v.read_capacity_units
             (fun f -> ("read_capacity_units", (Long.to_json f)));
           Util.option_map v.number_of_decreases_today
             (fun f -> ("number_of_decreases_today", (Long.to_json f)));
           Util.option_map v.last_decrease_date_time
             (fun f -> ("last_decrease_date_time", (DateTime.to_json f)));
           Util.option_map v.last_increase_date_time
             (fun f -> ("last_increase_date_time", (DateTime.to_json f)))])
    let of_json j =
      {
        last_increase_date_time =
          (Util.option_map (Json.lookup j "last_increase_date_time")
             DateTime.of_json);
        last_decrease_date_time =
          (Util.option_map (Json.lookup j "last_decrease_date_time")
             DateTime.of_json);
        number_of_decreases_today =
          (Util.option_map (Json.lookup j "number_of_decreases_today")
             Long.of_json);
        read_capacity_units =
          (Util.option_map (Json.lookup j "read_capacity_units") Long.of_json);
        write_capacity_units =
          (Util.option_map (Json.lookup j "write_capacity_units")
             Long.of_json)
      }
  end
module ProvisionedThroughput =
  struct
    type t = {
      read_capacity_units: Long.t ;
      write_capacity_units: Long.t }
    let make ~read_capacity_units  ~write_capacity_units  () =
      { read_capacity_units; write_capacity_units }
    let parse xml =
      Some
        {
          read_capacity_units =
            (Xml.required "ReadCapacityUnits"
               (Util.option_bind (Xml.member "ReadCapacityUnits" xml)
                  Long.parse));
          write_capacity_units =
            (Xml.required "WriteCapacityUnits"
               (Util.option_bind (Xml.member "WriteCapacityUnits" xml)
                  Long.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("WriteCapacityUnits",
                   (Long.to_query v.write_capacity_units)));
           Some
             (Query.Pair
                ("ReadCapacityUnits", (Long.to_query v.read_capacity_units)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("write_capacity_units", (Long.to_json v.write_capacity_units));
           Some ("read_capacity_units", (Long.to_json v.read_capacity_units))])
    let of_json j =
      {
        read_capacity_units =
          (Long.of_json
             (Util.of_option_exn (Json.lookup j "read_capacity_units")));
        write_capacity_units =
          (Long.of_json
             (Util.of_option_exn (Json.lookup j "write_capacity_units")))
      }
  end
module AttributeAction =
  struct
    type t =
      | ADD 
      | PUT 
      | DELETE 
    let str_to_t = [("DELETE", DELETE); ("PUT", PUT); ("ADD", ADD)]
    let t_to_str = [(DELETE, "DELETE"); (PUT, "PUT"); (ADD, "ADD")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module AttributeValueList =
  struct
    type t = AttributeValue.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map AttributeValue.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list AttributeValue.to_query v
    let to_json v = `List (List.map AttributeValue.to_json v)
    let of_json j = Json.to_list AttributeValue.of_json j
  end
module ComparisonOperator =
  struct
    type t =
      | EQ 
      | NE 
      | IN 
      | LE 
      | LT 
      | GE 
      | GT 
      | BETWEEN 
      | NOT_NULL 
      | NULL 
      | CONTAINS 
      | NOT_CONTAINS 
      | BEGINS_WITH 
    let str_to_t =
      [("BEGINS_WITH", BEGINS_WITH);
      ("NOT_CONTAINS", NOT_CONTAINS);
      ("CONTAINS", CONTAINS);
      ("NULL", NULL);
      ("NOT_NULL", NOT_NULL);
      ("BETWEEN", BETWEEN);
      ("GT", GT);
      ("GE", GE);
      ("LT", LT);
      ("LE", LE);
      ("IN", IN);
      ("NE", NE);
      ("EQ", EQ)]
    let t_to_str =
      [(BEGINS_WITH, "BEGINS_WITH");
      (NOT_CONTAINS, "NOT_CONTAINS");
      (CONTAINS, "CONTAINS");
      (NULL, "NULL");
      (NOT_NULL, "NOT_NULL");
      (BETWEEN, "BETWEEN");
      (GT, "GT");
      (GE, "GE");
      (LT, "LT");
      (LE, "LE");
      (IN, "IN");
      (NE, "NE");
      (EQ, "EQ")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module WriteRequest =
  struct
    type t =
      {
      put_request: PutRequest.t option ;
      delete_request: DeleteRequest.t option }
    let make ?put_request  ?delete_request  () =
      { put_request; delete_request }
    let parse xml =
      Some
        {
          put_request =
            (Util.option_bind (Xml.member "PutRequest" xml) PutRequest.parse);
          delete_request =
            (Util.option_bind (Xml.member "DeleteRequest" xml)
               DeleteRequest.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.delete_request
              (fun f ->
                 Query.Pair ("DeleteRequest", (DeleteRequest.to_query f)));
           Util.option_map v.put_request
             (fun f -> Query.Pair ("PutRequest", (PutRequest.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.delete_request
              (fun f -> ("delete_request", (DeleteRequest.to_json f)));
           Util.option_map v.put_request
             (fun f -> ("put_request", (PutRequest.to_json f)))])
    let of_json j =
      {
        put_request =
          (Util.option_map (Json.lookup j "put_request") PutRequest.of_json);
        delete_request =
          (Util.option_map (Json.lookup j "delete_request")
             DeleteRequest.of_json)
      }
  end
module SecondaryIndexesCapacityMap =
  struct
    type t = (String.t, Capacity.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Query.to_query_hashtbl String.to_string Capacity.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (Capacity.to_json v)) ::
                  acc) v [])
    let of_json j = Json.to_hashtbl String.of_string Capacity.of_json j
  end
module ItemCollectionMetrics =
  struct
    type t =
      {
      item_collection_key: ItemCollectionKeyAttributeMap.t option ;
      size_estimate_range_g_b: ItemCollectionSizeEstimateRange.t }
    let make ?item_collection_key  ?(size_estimate_range_g_b= [])  () =
      { item_collection_key; size_estimate_range_g_b }
    let parse xml =
      Some
        {
          item_collection_key =
            (Util.option_bind (Xml.member "ItemCollectionKey" xml)
               ItemCollectionKeyAttributeMap.parse);
          size_estimate_range_g_b =
            (Util.of_option []
               (Util.option_bind (Xml.member "SizeEstimateRangeGB" xml)
                  ItemCollectionSizeEstimateRange.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("SizeEstimateRangeGB.member",
                   (ItemCollectionSizeEstimateRange.to_query
                      v.size_estimate_range_g_b)));
           Util.option_map v.item_collection_key
             (fun f ->
                Query.Pair
                  ("ItemCollectionKey",
                    (ItemCollectionKeyAttributeMap.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("size_estimate_range_g_b",
                (ItemCollectionSizeEstimateRange.to_json
                   v.size_estimate_range_g_b));
           Util.option_map v.item_collection_key
             (fun f ->
                ("item_collection_key",
                  (ItemCollectionKeyAttributeMap.to_json f)))])
    let of_json j =
      {
        item_collection_key =
          (Util.option_map (Json.lookup j "item_collection_key")
             ItemCollectionKeyAttributeMap.of_json);
        size_estimate_range_g_b =
          (ItemCollectionSizeEstimateRange.of_json
             (Util.of_option_exn (Json.lookup j "size_estimate_range_g_b")))
      }
  end
module AttributeDefinition =
  struct
    type t =
      {
      attribute_name: String.t ;
      attribute_type: ScalarAttributeType.t }
    let make ~attribute_name  ~attribute_type  () =
      { attribute_name; attribute_type }
    let parse xml =
      Some
        {
          attribute_name =
            (Xml.required "AttributeName"
               (Util.option_bind (Xml.member "AttributeName" xml)
                  String.parse));
          attribute_type =
            (Xml.required "AttributeType"
               (Util.option_bind (Xml.member "AttributeType" xml)
                  ScalarAttributeType.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("AttributeType",
                   (ScalarAttributeType.to_query v.attribute_type)));
           Some
             (Query.Pair
                ("AttributeName", (String.to_query v.attribute_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("attribute_type",
                (ScalarAttributeType.to_json v.attribute_type));
           Some ("attribute_name", (String.to_json v.attribute_name))])
    let of_json j =
      {
        attribute_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "attribute_name")));
        attribute_type =
          (ScalarAttributeType.of_json
             (Util.of_option_exn (Json.lookup j "attribute_type")))
      }
  end
module GlobalSecondaryIndexDescription =
  struct
    type t =
      {
      index_name: String.t option ;
      key_schema: KeySchema.t ;
      projection: Projection.t option ;
      index_status: IndexStatus.t option ;
      backfilling: Boolean.t option ;
      provisioned_throughput: ProvisionedThroughputDescription.t option ;
      index_size_bytes: Long.t option ;
      item_count: Long.t option ;
      index_arn: String.t option }
    let make ?index_name  ?(key_schema= [])  ?projection  ?index_status 
      ?backfilling  ?provisioned_throughput  ?index_size_bytes  ?item_count 
      ?index_arn  () =
      {
        index_name;
        key_schema;
        projection;
        index_status;
        backfilling;
        provisioned_throughput;
        index_size_bytes;
        item_count;
        index_arn
      }
    let parse xml =
      Some
        {
          index_name =
            (Util.option_bind (Xml.member "IndexName" xml) String.parse);
          key_schema =
            (Util.of_option []
               (Util.option_bind (Xml.member "KeySchema" xml) KeySchema.parse));
          projection =
            (Util.option_bind (Xml.member "Projection" xml) Projection.parse);
          index_status =
            (Util.option_bind (Xml.member "IndexStatus" xml)
               IndexStatus.parse);
          backfilling =
            (Util.option_bind (Xml.member "Backfilling" xml) Boolean.parse);
          provisioned_throughput =
            (Util.option_bind (Xml.member "ProvisionedThroughput" xml)
               ProvisionedThroughputDescription.parse);
          index_size_bytes =
            (Util.option_bind (Xml.member "IndexSizeBytes" xml) Long.parse);
          item_count =
            (Util.option_bind (Xml.member "ItemCount" xml) Long.parse);
          index_arn =
            (Util.option_bind (Xml.member "IndexArn" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.index_arn
              (fun f -> Query.Pair ("IndexArn", (String.to_query f)));
           Util.option_map v.item_count
             (fun f -> Query.Pair ("ItemCount", (Long.to_query f)));
           Util.option_map v.index_size_bytes
             (fun f -> Query.Pair ("IndexSizeBytes", (Long.to_query f)));
           Util.option_map v.provisioned_throughput
             (fun f ->
                Query.Pair
                  ("ProvisionedThroughput",
                    (ProvisionedThroughputDescription.to_query f)));
           Util.option_map v.backfilling
             (fun f -> Query.Pair ("Backfilling", (Boolean.to_query f)));
           Util.option_map v.index_status
             (fun f -> Query.Pair ("IndexStatus", (IndexStatus.to_query f)));
           Util.option_map v.projection
             (fun f -> Query.Pair ("Projection", (Projection.to_query f)));
           Some
             (Query.Pair
                ("KeySchema.member", (KeySchema.to_query v.key_schema)));
           Util.option_map v.index_name
             (fun f -> Query.Pair ("IndexName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.index_arn
              (fun f -> ("index_arn", (String.to_json f)));
           Util.option_map v.item_count
             (fun f -> ("item_count", (Long.to_json f)));
           Util.option_map v.index_size_bytes
             (fun f -> ("index_size_bytes", (Long.to_json f)));
           Util.option_map v.provisioned_throughput
             (fun f ->
                ("provisioned_throughput",
                  (ProvisionedThroughputDescription.to_json f)));
           Util.option_map v.backfilling
             (fun f -> ("backfilling", (Boolean.to_json f)));
           Util.option_map v.index_status
             (fun f -> ("index_status", (IndexStatus.to_json f)));
           Util.option_map v.projection
             (fun f -> ("projection", (Projection.to_json f)));
           Some ("key_schema", (KeySchema.to_json v.key_schema));
           Util.option_map v.index_name
             (fun f -> ("index_name", (String.to_json f)))])
    let of_json j =
      {
        index_name =
          (Util.option_map (Json.lookup j "index_name") String.of_json);
        key_schema =
          (KeySchema.of_json
             (Util.of_option_exn (Json.lookup j "key_schema")));
        projection =
          (Util.option_map (Json.lookup j "projection") Projection.of_json);
        index_status =
          (Util.option_map (Json.lookup j "index_status") IndexStatus.of_json);
        backfilling =
          (Util.option_map (Json.lookup j "backfilling") Boolean.of_json);
        provisioned_throughput =
          (Util.option_map (Json.lookup j "provisioned_throughput")
             ProvisionedThroughputDescription.of_json);
        index_size_bytes =
          (Util.option_map (Json.lookup j "index_size_bytes") Long.of_json);
        item_count =
          (Util.option_map (Json.lookup j "item_count") Long.of_json);
        index_arn =
          (Util.option_map (Json.lookup j "index_arn") String.of_json)
      }
  end
module LocalSecondaryIndexDescription =
  struct
    type t =
      {
      index_name: String.t option ;
      key_schema: KeySchema.t ;
      projection: Projection.t option ;
      index_size_bytes: Long.t option ;
      item_count: Long.t option ;
      index_arn: String.t option }
    let make ?index_name  ?(key_schema= [])  ?projection  ?index_size_bytes 
      ?item_count  ?index_arn  () =
      {
        index_name;
        key_schema;
        projection;
        index_size_bytes;
        item_count;
        index_arn
      }
    let parse xml =
      Some
        {
          index_name =
            (Util.option_bind (Xml.member "IndexName" xml) String.parse);
          key_schema =
            (Util.of_option []
               (Util.option_bind (Xml.member "KeySchema" xml) KeySchema.parse));
          projection =
            (Util.option_bind (Xml.member "Projection" xml) Projection.parse);
          index_size_bytes =
            (Util.option_bind (Xml.member "IndexSizeBytes" xml) Long.parse);
          item_count =
            (Util.option_bind (Xml.member "ItemCount" xml) Long.parse);
          index_arn =
            (Util.option_bind (Xml.member "IndexArn" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.index_arn
              (fun f -> Query.Pair ("IndexArn", (String.to_query f)));
           Util.option_map v.item_count
             (fun f -> Query.Pair ("ItemCount", (Long.to_query f)));
           Util.option_map v.index_size_bytes
             (fun f -> Query.Pair ("IndexSizeBytes", (Long.to_query f)));
           Util.option_map v.projection
             (fun f -> Query.Pair ("Projection", (Projection.to_query f)));
           Some
             (Query.Pair
                ("KeySchema.member", (KeySchema.to_query v.key_schema)));
           Util.option_map v.index_name
             (fun f -> Query.Pair ("IndexName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.index_arn
              (fun f -> ("index_arn", (String.to_json f)));
           Util.option_map v.item_count
             (fun f -> ("item_count", (Long.to_json f)));
           Util.option_map v.index_size_bytes
             (fun f -> ("index_size_bytes", (Long.to_json f)));
           Util.option_map v.projection
             (fun f -> ("projection", (Projection.to_json f)));
           Some ("key_schema", (KeySchema.to_json v.key_schema));
           Util.option_map v.index_name
             (fun f -> ("index_name", (String.to_json f)))])
    let of_json j =
      {
        index_name =
          (Util.option_map (Json.lookup j "index_name") String.of_json);
        key_schema =
          (KeySchema.of_json
             (Util.of_option_exn (Json.lookup j "key_schema")));
        projection =
          (Util.option_map (Json.lookup j "projection") Projection.of_json);
        index_size_bytes =
          (Util.option_map (Json.lookup j "index_size_bytes") Long.of_json);
        item_count =
          (Util.option_map (Json.lookup j "item_count") Long.of_json);
        index_arn =
          (Util.option_map (Json.lookup j "index_arn") String.of_json)
      }
  end
module StreamViewType =
  struct
    type t =
      | NEW_IMAGE 
      | OLD_IMAGE 
      | NEW_AND_OLD_IMAGES 
      | KEYS_ONLY 
    let str_to_t =
      [("KEYS_ONLY", KEYS_ONLY);
      ("NEW_AND_OLD_IMAGES", NEW_AND_OLD_IMAGES);
      ("OLD_IMAGE", OLD_IMAGE);
      ("NEW_IMAGE", NEW_IMAGE)]
    let t_to_str =
      [(KEYS_ONLY, "KEYS_ONLY");
      (NEW_AND_OLD_IMAGES, "NEW_AND_OLD_IMAGES");
      (OLD_IMAGE, "OLD_IMAGE");
      (NEW_IMAGE, "NEW_IMAGE")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module AttributeNameList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module ExpressionAttributeNameMap =
  struct
    type t = (String.t, String.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Query.to_query_hashtbl String.to_string String.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (String.to_json v)) :: acc)
           v [])
    let of_json j = Json.to_hashtbl String.of_string String.of_json j
  end
module KeyList =
  struct
    type t = Key.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Key.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Key.to_query v
    let to_json v = `List (List.map Key.to_json v)
    let of_json j = Json.to_list Key.of_json j
  end
module AttributeMap =
  struct
    type t = (String.t, AttributeValue.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Query.to_query_hashtbl String.to_string AttributeValue.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (AttributeValue.to_json v))
                  :: acc) v [])
    let of_json j = Json.to_hashtbl String.of_string AttributeValue.of_json j
  end
module CreateGlobalSecondaryIndexAction =
  struct
    type t =
      {
      index_name: String.t ;
      key_schema: KeySchema.t ;
      projection: Projection.t ;
      provisioned_throughput: ProvisionedThroughput.t }
    let make ~index_name  ~key_schema  ~projection  ~provisioned_throughput 
      () = { index_name; key_schema; projection; provisioned_throughput }
    let parse xml =
      Some
        {
          index_name =
            (Xml.required "IndexName"
               (Util.option_bind (Xml.member "IndexName" xml) String.parse));
          key_schema =
            (Xml.required "KeySchema"
               (Util.option_bind (Xml.member "KeySchema" xml) KeySchema.parse));
          projection =
            (Xml.required "Projection"
               (Util.option_bind (Xml.member "Projection" xml)
                  Projection.parse));
          provisioned_throughput =
            (Xml.required "ProvisionedThroughput"
               (Util.option_bind (Xml.member "ProvisionedThroughput" xml)
                  ProvisionedThroughput.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ProvisionedThroughput",
                   (ProvisionedThroughput.to_query v.provisioned_throughput)));
           Some
             (Query.Pair ("Projection", (Projection.to_query v.projection)));
           Some
             (Query.Pair
                ("KeySchema.member", (KeySchema.to_query v.key_schema)));
           Some (Query.Pair ("IndexName", (String.to_query v.index_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("provisioned_throughput",
                (ProvisionedThroughput.to_json v.provisioned_throughput));
           Some ("projection", (Projection.to_json v.projection));
           Some ("key_schema", (KeySchema.to_json v.key_schema));
           Some ("index_name", (String.to_json v.index_name))])
    let of_json j =
      {
        index_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "index_name")));
        key_schema =
          (KeySchema.of_json
             (Util.of_option_exn (Json.lookup j "key_schema")));
        projection =
          (Projection.of_json
             (Util.of_option_exn (Json.lookup j "projection")));
        provisioned_throughput =
          (ProvisionedThroughput.of_json
             (Util.of_option_exn (Json.lookup j "provisioned_throughput")))
      }
  end
module DeleteGlobalSecondaryIndexAction =
  struct
    type t = {
      index_name: String.t }
    let make ~index_name  () = { index_name }
    let parse xml =
      Some
        {
          index_name =
            (Xml.required "IndexName"
               (Util.option_bind (Xml.member "IndexName" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("IndexName", (String.to_query v.index_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("index_name", (String.to_json v.index_name))])
    let of_json j =
      {
        index_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "index_name")))
      }
  end
module UpdateGlobalSecondaryIndexAction =
  struct
    type t =
      {
      index_name: String.t ;
      provisioned_throughput: ProvisionedThroughput.t }
    let make ~index_name  ~provisioned_throughput  () =
      { index_name; provisioned_throughput }
    let parse xml =
      Some
        {
          index_name =
            (Xml.required "IndexName"
               (Util.option_bind (Xml.member "IndexName" xml) String.parse));
          provisioned_throughput =
            (Xml.required "ProvisionedThroughput"
               (Util.option_bind (Xml.member "ProvisionedThroughput" xml)
                  ProvisionedThroughput.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ProvisionedThroughput",
                   (ProvisionedThroughput.to_query v.provisioned_throughput)));
           Some (Query.Pair ("IndexName", (String.to_query v.index_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("provisioned_throughput",
                (ProvisionedThroughput.to_json v.provisioned_throughput));
           Some ("index_name", (String.to_json v.index_name))])
    let of_json j =
      {
        index_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "index_name")));
        provisioned_throughput =
          (ProvisionedThroughput.of_json
             (Util.of_option_exn (Json.lookup j "provisioned_throughput")))
      }
  end
module AttributeValueUpdate =
  struct
    type t =
      {
      value: AttributeValue.t option ;
      action: AttributeAction.t option }
    let make ?value  ?action  () = { value; action }
    let parse xml =
      Some
        {
          value =
            (Util.option_bind (Xml.member "Value" xml) AttributeValue.parse);
          action =
            (Util.option_bind (Xml.member "Action" xml) AttributeAction.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.action
              (fun f -> Query.Pair ("Action", (AttributeAction.to_query f)));
           Util.option_map v.value
             (fun f -> Query.Pair ("Value", (AttributeValue.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.action
              (fun f -> ("action", (AttributeAction.to_json f)));
           Util.option_map v.value
             (fun f -> ("value", (AttributeValue.to_json f)))])
    let of_json j =
      {
        value =
          (Util.option_map (Json.lookup j "value") AttributeValue.of_json);
        action =
          (Util.option_map (Json.lookup j "action") AttributeAction.of_json)
      }
  end
module ExpectedAttributeValue =
  struct
    type t =
      {
      value: AttributeValue.t option ;
      exists: Boolean.t option ;
      comparison_operator: ComparisonOperator.t option ;
      attribute_value_list: AttributeValueList.t }
    let make ?value  ?exists  ?comparison_operator  ?(attribute_value_list=
      [])  () = { value; exists; comparison_operator; attribute_value_list }
    let parse xml =
      Some
        {
          value =
            (Util.option_bind (Xml.member "Value" xml) AttributeValue.parse);
          exists = (Util.option_bind (Xml.member "Exists" xml) Boolean.parse);
          comparison_operator =
            (Util.option_bind (Xml.member "ComparisonOperator" xml)
               ComparisonOperator.parse);
          attribute_value_list =
            (Util.of_option []
               (Util.option_bind (Xml.member "AttributeValueList" xml)
                  AttributeValueList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("AttributeValueList.member",
                   (AttributeValueList.to_query v.attribute_value_list)));
           Util.option_map v.comparison_operator
             (fun f ->
                Query.Pair
                  ("ComparisonOperator", (ComparisonOperator.to_query f)));
           Util.option_map v.exists
             (fun f -> Query.Pair ("Exists", (Boolean.to_query f)));
           Util.option_map v.value
             (fun f -> Query.Pair ("Value", (AttributeValue.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("attribute_value_list",
                (AttributeValueList.to_json v.attribute_value_list));
           Util.option_map v.comparison_operator
             (fun f ->
                ("comparison_operator", (ComparisonOperator.to_json f)));
           Util.option_map v.exists
             (fun f -> ("exists", (Boolean.to_json f)));
           Util.option_map v.value
             (fun f -> ("value", (AttributeValue.to_json f)))])
    let of_json j =
      {
        value =
          (Util.option_map (Json.lookup j "value") AttributeValue.of_json);
        exists = (Util.option_map (Json.lookup j "exists") Boolean.of_json);
        comparison_operator =
          (Util.option_map (Json.lookup j "comparison_operator")
             ComparisonOperator.of_json);
        attribute_value_list =
          (AttributeValueList.of_json
             (Util.of_option_exn (Json.lookup j "attribute_value_list")))
      }
  end
module WriteRequests =
  struct
    type t = WriteRequest.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map WriteRequest.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list WriteRequest.to_query v
    let to_json v = `List (List.map WriteRequest.to_json v)
    let of_json j = Json.to_list WriteRequest.of_json j
  end
module ConsumedCapacity =
  struct
    type t =
      {
      table_name: String.t option ;
      capacity_units: Double.t option ;
      table: Capacity.t option ;
      local_secondary_indexes: SecondaryIndexesCapacityMap.t option ;
      global_secondary_indexes: SecondaryIndexesCapacityMap.t option }
    let make ?table_name  ?capacity_units  ?table  ?local_secondary_indexes 
      ?global_secondary_indexes  () =
      {
        table_name;
        capacity_units;
        table;
        local_secondary_indexes;
        global_secondary_indexes
      }
    let parse xml =
      Some
        {
          table_name =
            (Util.option_bind (Xml.member "TableName" xml) String.parse);
          capacity_units =
            (Util.option_bind (Xml.member "CapacityUnits" xml) Double.parse);
          table = (Util.option_bind (Xml.member "Table" xml) Capacity.parse);
          local_secondary_indexes =
            (Util.option_bind (Xml.member "LocalSecondaryIndexes" xml)
               SecondaryIndexesCapacityMap.parse);
          global_secondary_indexes =
            (Util.option_bind (Xml.member "GlobalSecondaryIndexes" xml)
               SecondaryIndexesCapacityMap.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.global_secondary_indexes
              (fun f ->
                 Query.Pair
                   ("GlobalSecondaryIndexes",
                     (SecondaryIndexesCapacityMap.to_query f)));
           Util.option_map v.local_secondary_indexes
             (fun f ->
                Query.Pair
                  ("LocalSecondaryIndexes",
                    (SecondaryIndexesCapacityMap.to_query f)));
           Util.option_map v.table
             (fun f -> Query.Pair ("Table", (Capacity.to_query f)));
           Util.option_map v.capacity_units
             (fun f -> Query.Pair ("CapacityUnits", (Double.to_query f)));
           Util.option_map v.table_name
             (fun f -> Query.Pair ("TableName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.global_secondary_indexes
              (fun f ->
                 ("global_secondary_indexes",
                   (SecondaryIndexesCapacityMap.to_json f)));
           Util.option_map v.local_secondary_indexes
             (fun f ->
                ("local_secondary_indexes",
                  (SecondaryIndexesCapacityMap.to_json f)));
           Util.option_map v.table (fun f -> ("table", (Capacity.to_json f)));
           Util.option_map v.capacity_units
             (fun f -> ("capacity_units", (Double.to_json f)));
           Util.option_map v.table_name
             (fun f -> ("table_name", (String.to_json f)))])
    let of_json j =
      {
        table_name =
          (Util.option_map (Json.lookup j "table_name") String.of_json);
        capacity_units =
          (Util.option_map (Json.lookup j "capacity_units") Double.of_json);
        table = (Util.option_map (Json.lookup j "table") Capacity.of_json);
        local_secondary_indexes =
          (Util.option_map (Json.lookup j "local_secondary_indexes")
             SecondaryIndexesCapacityMap.of_json);
        global_secondary_indexes =
          (Util.option_map (Json.lookup j "global_secondary_indexes")
             SecondaryIndexesCapacityMap.of_json)
      }
  end
module ItemCollectionMetricsMultiple =
  struct
    type t = ItemCollectionMetrics.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ItemCollectionMetrics.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list ItemCollectionMetrics.to_query v
    let to_json v = `List (List.map ItemCollectionMetrics.to_json v)
    let of_json j = Json.to_list ItemCollectionMetrics.of_json j
  end
module AttributeDefinitions =
  struct
    type t = AttributeDefinition.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map AttributeDefinition.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list AttributeDefinition.to_query v
    let to_json v = `List (List.map AttributeDefinition.to_json v)
    let of_json j = Json.to_list AttributeDefinition.of_json j
  end
module GlobalSecondaryIndexDescriptionList =
  struct
    type t = GlobalSecondaryIndexDescription.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map GlobalSecondaryIndexDescription.parse
           (Xml.members "member" xml))
    let to_query v =
      Query.to_query_list GlobalSecondaryIndexDescription.to_query v
    let to_json v =
      `List (List.map GlobalSecondaryIndexDescription.to_json v)
    let of_json j = Json.to_list GlobalSecondaryIndexDescription.of_json j
  end
module LocalSecondaryIndexDescriptionList =
  struct
    type t = LocalSecondaryIndexDescription.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map LocalSecondaryIndexDescription.parse
           (Xml.members "member" xml))
    let to_query v =
      Query.to_query_list LocalSecondaryIndexDescription.to_query v
    let to_json v = `List (List.map LocalSecondaryIndexDescription.to_json v)
    let of_json j = Json.to_list LocalSecondaryIndexDescription.of_json j
  end
module StreamSpecification =
  struct
    type t =
      {
      stream_enabled: Boolean.t option ;
      stream_view_type: StreamViewType.t option }
    let make ?stream_enabled  ?stream_view_type  () =
      { stream_enabled; stream_view_type }
    let parse xml =
      Some
        {
          stream_enabled =
            (Util.option_bind (Xml.member "StreamEnabled" xml) Boolean.parse);
          stream_view_type =
            (Util.option_bind (Xml.member "StreamViewType" xml)
               StreamViewType.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.stream_view_type
              (fun f ->
                 Query.Pair ("StreamViewType", (StreamViewType.to_query f)));
           Util.option_map v.stream_enabled
             (fun f -> Query.Pair ("StreamEnabled", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stream_view_type
              (fun f -> ("stream_view_type", (StreamViewType.to_json f)));
           Util.option_map v.stream_enabled
             (fun f -> ("stream_enabled", (Boolean.to_json f)))])
    let of_json j =
      {
        stream_enabled =
          (Util.option_map (Json.lookup j "stream_enabled") Boolean.of_json);
        stream_view_type =
          (Util.option_map (Json.lookup j "stream_view_type")
             StreamViewType.of_json)
      }
  end
module TableStatus =
  struct
    type t =
      | CREATING 
      | UPDATING 
      | DELETING 
      | ACTIVE 
    let str_to_t =
      [("ACTIVE", ACTIVE);
      ("DELETING", DELETING);
      ("UPDATING", UPDATING);
      ("CREATING", CREATING)]
    let t_to_str =
      [(ACTIVE, "ACTIVE");
      (DELETING, "DELETING");
      (UPDATING, "UPDATING");
      (CREATING, "CREATING")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module GlobalSecondaryIndex =
  struct
    type t =
      {
      index_name: String.t ;
      key_schema: KeySchema.t ;
      projection: Projection.t ;
      provisioned_throughput: ProvisionedThroughput.t }
    let make ~index_name  ~key_schema  ~projection  ~provisioned_throughput 
      () = { index_name; key_schema; projection; provisioned_throughput }
    let parse xml =
      Some
        {
          index_name =
            (Xml.required "IndexName"
               (Util.option_bind (Xml.member "IndexName" xml) String.parse));
          key_schema =
            (Xml.required "KeySchema"
               (Util.option_bind (Xml.member "KeySchema" xml) KeySchema.parse));
          projection =
            (Xml.required "Projection"
               (Util.option_bind (Xml.member "Projection" xml)
                  Projection.parse));
          provisioned_throughput =
            (Xml.required "ProvisionedThroughput"
               (Util.option_bind (Xml.member "ProvisionedThroughput" xml)
                  ProvisionedThroughput.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ProvisionedThroughput",
                   (ProvisionedThroughput.to_query v.provisioned_throughput)));
           Some
             (Query.Pair ("Projection", (Projection.to_query v.projection)));
           Some
             (Query.Pair
                ("KeySchema.member", (KeySchema.to_query v.key_schema)));
           Some (Query.Pair ("IndexName", (String.to_query v.index_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("provisioned_throughput",
                (ProvisionedThroughput.to_json v.provisioned_throughput));
           Some ("projection", (Projection.to_json v.projection));
           Some ("key_schema", (KeySchema.to_json v.key_schema));
           Some ("index_name", (String.to_json v.index_name))])
    let of_json j =
      {
        index_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "index_name")));
        key_schema =
          (KeySchema.of_json
             (Util.of_option_exn (Json.lookup j "key_schema")));
        projection =
          (Projection.of_json
             (Util.of_option_exn (Json.lookup j "projection")));
        provisioned_throughput =
          (ProvisionedThroughput.of_json
             (Util.of_option_exn (Json.lookup j "provisioned_throughput")))
      }
  end
module LocalSecondaryIndex =
  struct
    type t =
      {
      index_name: String.t ;
      key_schema: KeySchema.t ;
      projection: Projection.t }
    let make ~index_name  ~key_schema  ~projection  () =
      { index_name; key_schema; projection }
    let parse xml =
      Some
        {
          index_name =
            (Xml.required "IndexName"
               (Util.option_bind (Xml.member "IndexName" xml) String.parse));
          key_schema =
            (Xml.required "KeySchema"
               (Util.option_bind (Xml.member "KeySchema" xml) KeySchema.parse));
          projection =
            (Xml.required "Projection"
               (Util.option_bind (Xml.member "Projection" xml)
                  Projection.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Projection", (Projection.to_query v.projection)));
           Some
             (Query.Pair
                ("KeySchema.member", (KeySchema.to_query v.key_schema)));
           Some (Query.Pair ("IndexName", (String.to_query v.index_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("projection", (Projection.to_json v.projection));
           Some ("key_schema", (KeySchema.to_json v.key_schema));
           Some ("index_name", (String.to_json v.index_name))])
    let of_json j =
      {
        index_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "index_name")));
        key_schema =
          (KeySchema.of_json
             (Util.of_option_exn (Json.lookup j "key_schema")));
        projection =
          (Projection.of_json
             (Util.of_option_exn (Json.lookup j "projection")))
      }
  end
module Condition =
  struct
    type t =
      {
      attribute_value_list: AttributeValueList.t ;
      comparison_operator: ComparisonOperator.t }
    let make ?(attribute_value_list= [])  ~comparison_operator  () =
      { attribute_value_list; comparison_operator }
    let parse xml =
      Some
        {
          attribute_value_list =
            (Util.of_option []
               (Util.option_bind (Xml.member "AttributeValueList" xml)
                  AttributeValueList.parse));
          comparison_operator =
            (Xml.required "ComparisonOperator"
               (Util.option_bind (Xml.member "ComparisonOperator" xml)
                  ComparisonOperator.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ComparisonOperator",
                   (ComparisonOperator.to_query v.comparison_operator)));
           Some
             (Query.Pair
                ("AttributeValueList.member",
                  (AttributeValueList.to_query v.attribute_value_list)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("comparison_operator",
                (ComparisonOperator.to_json v.comparison_operator));
           Some
             ("attribute_value_list",
               (AttributeValueList.to_json v.attribute_value_list))])
    let of_json j =
      {
        attribute_value_list =
          (AttributeValueList.of_json
             (Util.of_option_exn (Json.lookup j "attribute_value_list")));
        comparison_operator =
          (ComparisonOperator.of_json
             (Util.of_option_exn (Json.lookup j "comparison_operator")))
      }
  end
module KeysAndAttributes =
  struct
    type t =
      {
      keys: KeyList.t ;
      attributes_to_get: AttributeNameList.t ;
      consistent_read: Boolean.t option ;
      projection_expression: String.t option ;
      expression_attribute_names: ExpressionAttributeNameMap.t option }
    let make ~keys  ?(attributes_to_get= [])  ?consistent_read 
      ?projection_expression  ?expression_attribute_names  () =
      {
        keys;
        attributes_to_get;
        consistent_read;
        projection_expression;
        expression_attribute_names
      }
    let parse xml =
      Some
        {
          keys =
            (Xml.required "Keys"
               (Util.option_bind (Xml.member "Keys" xml) KeyList.parse));
          attributes_to_get =
            (Util.of_option []
               (Util.option_bind (Xml.member "AttributesToGet" xml)
                  AttributeNameList.parse));
          consistent_read =
            (Util.option_bind (Xml.member "ConsistentRead" xml) Boolean.parse);
          projection_expression =
            (Util.option_bind (Xml.member "ProjectionExpression" xml)
               String.parse);
          expression_attribute_names =
            (Util.option_bind (Xml.member "ExpressionAttributeNames" xml)
               ExpressionAttributeNameMap.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.expression_attribute_names
              (fun f ->
                 Query.Pair
                   ("ExpressionAttributeNames",
                     (ExpressionAttributeNameMap.to_query f)));
           Util.option_map v.projection_expression
             (fun f ->
                Query.Pair ("ProjectionExpression", (String.to_query f)));
           Util.option_map v.consistent_read
             (fun f -> Query.Pair ("ConsistentRead", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("AttributesToGet.member",
                  (AttributeNameList.to_query v.attributes_to_get)));
           Some (Query.Pair ("Keys.member", (KeyList.to_query v.keys)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.expression_attribute_names
              (fun f ->
                 ("expression_attribute_names",
                   (ExpressionAttributeNameMap.to_json f)));
           Util.option_map v.projection_expression
             (fun f -> ("projection_expression", (String.to_json f)));
           Util.option_map v.consistent_read
             (fun f -> ("consistent_read", (Boolean.to_json f)));
           Some
             ("attributes_to_get",
               (AttributeNameList.to_json v.attributes_to_get));
           Some ("keys", (KeyList.to_json v.keys))])
    let of_json j =
      {
        keys = (KeyList.of_json (Util.of_option_exn (Json.lookup j "keys")));
        attributes_to_get =
          (AttributeNameList.of_json
             (Util.of_option_exn (Json.lookup j "attributes_to_get")));
        consistent_read =
          (Util.option_map (Json.lookup j "consistent_read") Boolean.of_json);
        projection_expression =
          (Util.option_map (Json.lookup j "projection_expression")
             String.of_json);
        expression_attribute_names =
          (Util.option_map (Json.lookup j "expression_attribute_names")
             ExpressionAttributeNameMap.of_json)
      }
  end
module ItemList =
  struct
    type t = AttributeMap.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map AttributeMap.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list AttributeMap.to_query v
    let to_json v = `List (List.map AttributeMap.to_json v)
    let of_json j = Json.to_list AttributeMap.of_json j
  end
module GlobalSecondaryIndexUpdate =
  struct
    type t =
      {
      update: UpdateGlobalSecondaryIndexAction.t option ;
      create: CreateGlobalSecondaryIndexAction.t option ;
      delete: DeleteGlobalSecondaryIndexAction.t option }
    let make ?update  ?create  ?delete  () = { update; create; delete }
    let parse xml =
      Some
        {
          update =
            (Util.option_bind (Xml.member "Update" xml)
               UpdateGlobalSecondaryIndexAction.parse);
          create =
            (Util.option_bind (Xml.member "Create" xml)
               CreateGlobalSecondaryIndexAction.parse);
          delete =
            (Util.option_bind (Xml.member "Delete" xml)
               DeleteGlobalSecondaryIndexAction.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.delete
              (fun f ->
                 Query.Pair
                   ("Delete", (DeleteGlobalSecondaryIndexAction.to_query f)));
           Util.option_map v.create
             (fun f ->
                Query.Pair
                  ("Create", (CreateGlobalSecondaryIndexAction.to_query f)));
           Util.option_map v.update
             (fun f ->
                Query.Pair
                  ("Update", (UpdateGlobalSecondaryIndexAction.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.delete
              (fun f ->
                 ("delete", (DeleteGlobalSecondaryIndexAction.to_json f)));
           Util.option_map v.create
             (fun f ->
                ("create", (CreateGlobalSecondaryIndexAction.to_json f)));
           Util.option_map v.update
             (fun f ->
                ("update", (UpdateGlobalSecondaryIndexAction.to_json f)))])
    let of_json j =
      {
        update =
          (Util.option_map (Json.lookup j "update")
             UpdateGlobalSecondaryIndexAction.of_json);
        create =
          (Util.option_map (Json.lookup j "create")
             CreateGlobalSecondaryIndexAction.of_json);
        delete =
          (Util.option_map (Json.lookup j "delete")
             DeleteGlobalSecondaryIndexAction.of_json)
      }
  end
module AttributeUpdates =
  struct
    type t = (String.t, AttributeValueUpdate.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Query.to_query_hashtbl String.to_string AttributeValueUpdate.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc ->
                  ((String.to_string k), (AttributeValueUpdate.to_json v)) ::
                  acc) v [])
    let of_json j =
      Json.to_hashtbl String.of_string AttributeValueUpdate.of_json j
  end
module ConditionalOperator =
  struct
    type t =
      | AND 
      | OR 
    let str_to_t = [("OR", OR); ("AND", AND)]
    let t_to_str = [(OR, "OR"); (AND, "AND")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module ExpectedAttributeMap =
  struct
    type t = (String.t, ExpectedAttributeValue.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Query.to_query_hashtbl String.to_string ExpectedAttributeValue.to_query
        v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc ->
                  ((String.to_string k), (ExpectedAttributeValue.to_json v))
                  :: acc) v [])
    let of_json j =
      Json.to_hashtbl String.of_string ExpectedAttributeValue.of_json j
  end
module ExpressionAttributeValueMap =
  struct
    type t = (String.t, AttributeValue.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Query.to_query_hashtbl String.to_string AttributeValue.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (AttributeValue.to_json v))
                  :: acc) v [])
    let of_json j = Json.to_hashtbl String.of_string AttributeValue.of_json j
  end
module ReturnConsumedCapacity =
  struct
    type t =
      | INDEXES 
      | TOTAL 
      | NONE 
    let str_to_t = [("NONE", NONE); ("TOTAL", TOTAL); ("INDEXES", INDEXES)]
    let t_to_str = [(NONE, "NONE"); (TOTAL, "TOTAL"); (INDEXES, "INDEXES")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module ReturnItemCollectionMetrics =
  struct
    type t =
      | SIZE 
      | NONE 
    let str_to_t = [("NONE", NONE); ("SIZE", SIZE)]
    let t_to_str = [(NONE, "NONE"); (SIZE, "SIZE")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module ReturnValue =
  struct
    type t =
      | NONE 
      | ALL_OLD 
      | UPDATED_OLD 
      | ALL_NEW 
      | UPDATED_NEW 
    let str_to_t =
      [("UPDATED_NEW", UPDATED_NEW);
      ("ALL_NEW", ALL_NEW);
      ("UPDATED_OLD", UPDATED_OLD);
      ("ALL_OLD", ALL_OLD);
      ("NONE", NONE)]
    let t_to_str =
      [(UPDATED_NEW, "UPDATED_NEW");
      (ALL_NEW, "ALL_NEW");
      (UPDATED_OLD, "UPDATED_OLD");
      (ALL_OLD, "ALL_OLD");
      (NONE, "NONE")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module BatchWriteItemRequestMap =
  struct
    type t = (String.t, WriteRequests.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Query.to_query_hashtbl String.to_string WriteRequests.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (WriteRequests.to_json v))
                  :: acc) v [])
    let of_json j = Json.to_hashtbl String.of_string WriteRequests.of_json j
  end
module ConsumedCapacityMultiple =
  struct
    type t = ConsumedCapacity.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ConsumedCapacity.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list ConsumedCapacity.to_query v
    let to_json v = `List (List.map ConsumedCapacity.to_json v)
    let of_json j = Json.to_list ConsumedCapacity.of_json j
  end
module ItemCollectionMetricsPerTable =
  struct
    type t = (String.t, ItemCollectionMetricsMultiple.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Query.to_query_hashtbl String.to_string
        ItemCollectionMetricsMultiple.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc ->
                  ((String.to_string k),
                    (ItemCollectionMetricsMultiple.to_json v))
                  :: acc) v [])
    let of_json j =
      Json.to_hashtbl String.of_string ItemCollectionMetricsMultiple.of_json
        j
  end
module TableDescription =
  struct
    type t =
      {
      attribute_definitions: AttributeDefinitions.t ;
      table_name: String.t option ;
      key_schema: KeySchema.t ;
      table_status: TableStatus.t option ;
      creation_date_time: DateTime.t option ;
      provisioned_throughput: ProvisionedThroughputDescription.t option ;
      table_size_bytes: Long.t option ;
      item_count: Long.t option ;
      table_arn: String.t option ;
      local_secondary_indexes: LocalSecondaryIndexDescriptionList.t ;
      global_secondary_indexes: GlobalSecondaryIndexDescriptionList.t ;
      stream_specification: StreamSpecification.t option ;
      latest_stream_label: String.t option ;
      latest_stream_arn: String.t option }
    let make ?(attribute_definitions= [])  ?table_name  ?(key_schema= []) 
      ?table_status  ?creation_date_time  ?provisioned_throughput 
      ?table_size_bytes  ?item_count  ?table_arn  ?(local_secondary_indexes=
      [])  ?(global_secondary_indexes= [])  ?stream_specification 
      ?latest_stream_label  ?latest_stream_arn  () =
      {
        attribute_definitions;
        table_name;
        key_schema;
        table_status;
        creation_date_time;
        provisioned_throughput;
        table_size_bytes;
        item_count;
        table_arn;
        local_secondary_indexes;
        global_secondary_indexes;
        stream_specification;
        latest_stream_label;
        latest_stream_arn
      }
    let parse xml =
      Some
        {
          attribute_definitions =
            (Util.of_option []
               (Util.option_bind (Xml.member "AttributeDefinitions" xml)
                  AttributeDefinitions.parse));
          table_name =
            (Util.option_bind (Xml.member "TableName" xml) String.parse);
          key_schema =
            (Util.of_option []
               (Util.option_bind (Xml.member "KeySchema" xml) KeySchema.parse));
          table_status =
            (Util.option_bind (Xml.member "TableStatus" xml)
               TableStatus.parse);
          creation_date_time =
            (Util.option_bind (Xml.member "CreationDateTime" xml)
               DateTime.parse);
          provisioned_throughput =
            (Util.option_bind (Xml.member "ProvisionedThroughput" xml)
               ProvisionedThroughputDescription.parse);
          table_size_bytes =
            (Util.option_bind (Xml.member "TableSizeBytes" xml) Long.parse);
          item_count =
            (Util.option_bind (Xml.member "ItemCount" xml) Long.parse);
          table_arn =
            (Util.option_bind (Xml.member "TableArn" xml) String.parse);
          local_secondary_indexes =
            (Util.of_option []
               (Util.option_bind (Xml.member "LocalSecondaryIndexes" xml)
                  LocalSecondaryIndexDescriptionList.parse));
          global_secondary_indexes =
            (Util.of_option []
               (Util.option_bind (Xml.member "GlobalSecondaryIndexes" xml)
                  GlobalSecondaryIndexDescriptionList.parse));
          stream_specification =
            (Util.option_bind (Xml.member "StreamSpecification" xml)
               StreamSpecification.parse);
          latest_stream_label =
            (Util.option_bind (Xml.member "LatestStreamLabel" xml)
               String.parse);
          latest_stream_arn =
            (Util.option_bind (Xml.member "LatestStreamArn" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.latest_stream_arn
              (fun f -> Query.Pair ("LatestStreamArn", (String.to_query f)));
           Util.option_map v.latest_stream_label
             (fun f -> Query.Pair ("LatestStreamLabel", (String.to_query f)));
           Util.option_map v.stream_specification
             (fun f ->
                Query.Pair
                  ("StreamSpecification", (StreamSpecification.to_query f)));
           Some
             (Query.Pair
                ("GlobalSecondaryIndexes.member",
                  (GlobalSecondaryIndexDescriptionList.to_query
                     v.global_secondary_indexes)));
           Some
             (Query.Pair
                ("LocalSecondaryIndexes.member",
                  (LocalSecondaryIndexDescriptionList.to_query
                     v.local_secondary_indexes)));
           Util.option_map v.table_arn
             (fun f -> Query.Pair ("TableArn", (String.to_query f)));
           Util.option_map v.item_count
             (fun f -> Query.Pair ("ItemCount", (Long.to_query f)));
           Util.option_map v.table_size_bytes
             (fun f -> Query.Pair ("TableSizeBytes", (Long.to_query f)));
           Util.option_map v.provisioned_throughput
             (fun f ->
                Query.Pair
                  ("ProvisionedThroughput",
                    (ProvisionedThroughputDescription.to_query f)));
           Util.option_map v.creation_date_time
             (fun f -> Query.Pair ("CreationDateTime", (DateTime.to_query f)));
           Util.option_map v.table_status
             (fun f -> Query.Pair ("TableStatus", (TableStatus.to_query f)));
           Some
             (Query.Pair
                ("KeySchema.member", (KeySchema.to_query v.key_schema)));
           Util.option_map v.table_name
             (fun f -> Query.Pair ("TableName", (String.to_query f)));
           Some
             (Query.Pair
                ("AttributeDefinitions.member",
                  (AttributeDefinitions.to_query v.attribute_definitions)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.latest_stream_arn
              (fun f -> ("latest_stream_arn", (String.to_json f)));
           Util.option_map v.latest_stream_label
             (fun f -> ("latest_stream_label", (String.to_json f)));
           Util.option_map v.stream_specification
             (fun f ->
                ("stream_specification", (StreamSpecification.to_json f)));
           Some
             ("global_secondary_indexes",
               (GlobalSecondaryIndexDescriptionList.to_json
                  v.global_secondary_indexes));
           Some
             ("local_secondary_indexes",
               (LocalSecondaryIndexDescriptionList.to_json
                  v.local_secondary_indexes));
           Util.option_map v.table_arn
             (fun f -> ("table_arn", (String.to_json f)));
           Util.option_map v.item_count
             (fun f -> ("item_count", (Long.to_json f)));
           Util.option_map v.table_size_bytes
             (fun f -> ("table_size_bytes", (Long.to_json f)));
           Util.option_map v.provisioned_throughput
             (fun f ->
                ("provisioned_throughput",
                  (ProvisionedThroughputDescription.to_json f)));
           Util.option_map v.creation_date_time
             (fun f -> ("creation_date_time", (DateTime.to_json f)));
           Util.option_map v.table_status
             (fun f -> ("table_status", (TableStatus.to_json f)));
           Some ("key_schema", (KeySchema.to_json v.key_schema));
           Util.option_map v.table_name
             (fun f -> ("table_name", (String.to_json f)));
           Some
             ("attribute_definitions",
               (AttributeDefinitions.to_json v.attribute_definitions))])
    let of_json j =
      {
        attribute_definitions =
          (AttributeDefinitions.of_json
             (Util.of_option_exn (Json.lookup j "attribute_definitions")));
        table_name =
          (Util.option_map (Json.lookup j "table_name") String.of_json);
        key_schema =
          (KeySchema.of_json
             (Util.of_option_exn (Json.lookup j "key_schema")));
        table_status =
          (Util.option_map (Json.lookup j "table_status") TableStatus.of_json);
        creation_date_time =
          (Util.option_map (Json.lookup j "creation_date_time")
             DateTime.of_json);
        provisioned_throughput =
          (Util.option_map (Json.lookup j "provisioned_throughput")
             ProvisionedThroughputDescription.of_json);
        table_size_bytes =
          (Util.option_map (Json.lookup j "table_size_bytes") Long.of_json);
        item_count =
          (Util.option_map (Json.lookup j "item_count") Long.of_json);
        table_arn =
          (Util.option_map (Json.lookup j "table_arn") String.of_json);
        local_secondary_indexes =
          (LocalSecondaryIndexDescriptionList.of_json
             (Util.of_option_exn (Json.lookup j "local_secondary_indexes")));
        global_secondary_indexes =
          (GlobalSecondaryIndexDescriptionList.of_json
             (Util.of_option_exn (Json.lookup j "global_secondary_indexes")));
        stream_specification =
          (Util.option_map (Json.lookup j "stream_specification")
             StreamSpecification.of_json);
        latest_stream_label =
          (Util.option_map (Json.lookup j "latest_stream_label")
             String.of_json);
        latest_stream_arn =
          (Util.option_map (Json.lookup j "latest_stream_arn") String.of_json)
      }
  end
module GlobalSecondaryIndexList =
  struct
    type t = GlobalSecondaryIndex.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map GlobalSecondaryIndex.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list GlobalSecondaryIndex.to_query v
    let to_json v = `List (List.map GlobalSecondaryIndex.to_json v)
    let of_json j = Json.to_list GlobalSecondaryIndex.of_json j
  end
module LocalSecondaryIndexList =
  struct
    type t = LocalSecondaryIndex.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map LocalSecondaryIndex.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list LocalSecondaryIndex.to_query v
    let to_json v = `List (List.map LocalSecondaryIndex.to_json v)
    let of_json j = Json.to_list LocalSecondaryIndex.of_json j
  end
module TableNameList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module FilterConditionMap =
  struct
    type t = (String.t, Condition.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Query.to_query_hashtbl String.to_string Condition.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (Condition.to_json v)) ::
                  acc) v [])
    let of_json j = Json.to_hashtbl String.of_string Condition.of_json j
  end
module KeyConditions =
  struct
    type t = (String.t, Condition.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Query.to_query_hashtbl String.to_string Condition.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (Condition.to_json v)) ::
                  acc) v [])
    let of_json j = Json.to_hashtbl String.of_string Condition.of_json j
  end
module Select =
  struct
    type t =
      | ALL_ATTRIBUTES 
      | ALL_PROJECTED_ATTRIBUTES 
      | SPECIFIC_ATTRIBUTES 
      | COUNT 
    let str_to_t =
      [("COUNT", COUNT);
      ("SPECIFIC_ATTRIBUTES", SPECIFIC_ATTRIBUTES);
      ("ALL_PROJECTED_ATTRIBUTES", ALL_PROJECTED_ATTRIBUTES);
      ("ALL_ATTRIBUTES", ALL_ATTRIBUTES)]
    let t_to_str =
      [(COUNT, "COUNT");
      (SPECIFIC_ATTRIBUTES, "SPECIFIC_ATTRIBUTES");
      (ALL_PROJECTED_ATTRIBUTES, "ALL_PROJECTED_ATTRIBUTES");
      (ALL_ATTRIBUTES, "ALL_ATTRIBUTES")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module BatchGetRequestMap =
  struct
    type t = (String.t, KeysAndAttributes.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Query.to_query_hashtbl String.to_string KeysAndAttributes.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc ->
                  ((String.to_string k), (KeysAndAttributes.to_json v)) ::
                  acc) v [])
    let of_json j =
      Json.to_hashtbl String.of_string KeysAndAttributes.of_json j
  end
module BatchGetResponseMap =
  struct
    type t = (String.t, ItemList.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Query.to_query_hashtbl String.to_string ItemList.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (ItemList.to_json v)) ::
                  acc) v [])
    let of_json j = Json.to_hashtbl String.of_string ItemList.of_json j
  end
module GlobalSecondaryIndexUpdateList =
  struct
    type t = GlobalSecondaryIndexUpdate.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map GlobalSecondaryIndexUpdate.parse (Xml.members "member" xml))
    let to_query v =
      Query.to_query_list GlobalSecondaryIndexUpdate.to_query v
    let to_json v = `List (List.map GlobalSecondaryIndexUpdate.to_json v)
    let of_json j = Json.to_list GlobalSecondaryIndexUpdate.of_json j
  end
module QueryOutput =
  struct
    type t =
      {
      items: ItemList.t ;
      count: Integer.t option ;
      scanned_count: Integer.t option ;
      last_evaluated_key: Key.t option ;
      consumed_capacity: ConsumedCapacity.t option }
    let make ?(items= [])  ?count  ?scanned_count  ?last_evaluated_key 
      ?consumed_capacity  () =
      { items; count; scanned_count; last_evaluated_key; consumed_capacity }
    let parse xml =
      Some
        {
          items =
            (Util.of_option []
               (Util.option_bind (Xml.member "Items" xml) ItemList.parse));
          count = (Util.option_bind (Xml.member "Count" xml) Integer.parse);
          scanned_count =
            (Util.option_bind (Xml.member "ScannedCount" xml) Integer.parse);
          last_evaluated_key =
            (Util.option_bind (Xml.member "LastEvaluatedKey" xml) Key.parse);
          consumed_capacity =
            (Util.option_bind (Xml.member "ConsumedCapacity" xml)
               ConsumedCapacity.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.consumed_capacity
              (fun f ->
                 Query.Pair
                   ("ConsumedCapacity", (ConsumedCapacity.to_query f)));
           Util.option_map v.last_evaluated_key
             (fun f -> Query.Pair ("LastEvaluatedKey", (Key.to_query f)));
           Util.option_map v.scanned_count
             (fun f -> Query.Pair ("ScannedCount", (Integer.to_query f)));
           Util.option_map v.count
             (fun f -> Query.Pair ("Count", (Integer.to_query f)));
           Some (Query.Pair ("Items.member", (ItemList.to_query v.items)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.consumed_capacity
              (fun f -> ("consumed_capacity", (ConsumedCapacity.to_json f)));
           Util.option_map v.last_evaluated_key
             (fun f -> ("last_evaluated_key", (Key.to_json f)));
           Util.option_map v.scanned_count
             (fun f -> ("scanned_count", (Integer.to_json f)));
           Util.option_map v.count (fun f -> ("count", (Integer.to_json f)));
           Some ("items", (ItemList.to_json v.items))])
    let of_json j =
      {
        items =
          (ItemList.of_json (Util.of_option_exn (Json.lookup j "items")));
        count = (Util.option_map (Json.lookup j "count") Integer.of_json);
        scanned_count =
          (Util.option_map (Json.lookup j "scanned_count") Integer.of_json);
        last_evaluated_key =
          (Util.option_map (Json.lookup j "last_evaluated_key") Key.of_json);
        consumed_capacity =
          (Util.option_map (Json.lookup j "consumed_capacity")
             ConsumedCapacity.of_json)
      }
  end
module UpdateItemInput =
  struct
    type t =
      {
      table_name: String.t ;
      key: Key.t ;
      attribute_updates: AttributeUpdates.t option ;
      expected: ExpectedAttributeMap.t option ;
      conditional_operator: ConditionalOperator.t option ;
      return_values: ReturnValue.t option ;
      return_consumed_capacity: ReturnConsumedCapacity.t option ;
      return_item_collection_metrics: ReturnItemCollectionMetrics.t option ;
      update_expression: String.t option ;
      condition_expression: String.t option ;
      expression_attribute_names: ExpressionAttributeNameMap.t option ;
      expression_attribute_values: ExpressionAttributeValueMap.t option }
    let make ~table_name  ~key  ?attribute_updates  ?expected 
      ?conditional_operator  ?return_values  ?return_consumed_capacity 
      ?return_item_collection_metrics  ?update_expression 
      ?condition_expression  ?expression_attribute_names 
      ?expression_attribute_values  () =
      {
        table_name;
        key;
        attribute_updates;
        expected;
        conditional_operator;
        return_values;
        return_consumed_capacity;
        return_item_collection_metrics;
        update_expression;
        condition_expression;
        expression_attribute_names;
        expression_attribute_values
      }
    let parse xml =
      Some
        {
          table_name =
            (Xml.required "TableName"
               (Util.option_bind (Xml.member "TableName" xml) String.parse));
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) Key.parse));
          attribute_updates =
            (Util.option_bind (Xml.member "AttributeUpdates" xml)
               AttributeUpdates.parse);
          expected =
            (Util.option_bind (Xml.member "Expected" xml)
               ExpectedAttributeMap.parse);
          conditional_operator =
            (Util.option_bind (Xml.member "ConditionalOperator" xml)
               ConditionalOperator.parse);
          return_values =
            (Util.option_bind (Xml.member "ReturnValues" xml)
               ReturnValue.parse);
          return_consumed_capacity =
            (Util.option_bind (Xml.member "ReturnConsumedCapacity" xml)
               ReturnConsumedCapacity.parse);
          return_item_collection_metrics =
            (Util.option_bind (Xml.member "ReturnItemCollectionMetrics" xml)
               ReturnItemCollectionMetrics.parse);
          update_expression =
            (Util.option_bind (Xml.member "UpdateExpression" xml)
               String.parse);
          condition_expression =
            (Util.option_bind (Xml.member "ConditionExpression" xml)
               String.parse);
          expression_attribute_names =
            (Util.option_bind (Xml.member "ExpressionAttributeNames" xml)
               ExpressionAttributeNameMap.parse);
          expression_attribute_values =
            (Util.option_bind (Xml.member "ExpressionAttributeValues" xml)
               ExpressionAttributeValueMap.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.expression_attribute_values
              (fun f ->
                 Query.Pair
                   ("ExpressionAttributeValues",
                     (ExpressionAttributeValueMap.to_query f)));
           Util.option_map v.expression_attribute_names
             (fun f ->
                Query.Pair
                  ("ExpressionAttributeNames",
                    (ExpressionAttributeNameMap.to_query f)));
           Util.option_map v.condition_expression
             (fun f ->
                Query.Pair ("ConditionExpression", (String.to_query f)));
           Util.option_map v.update_expression
             (fun f -> Query.Pair ("UpdateExpression", (String.to_query f)));
           Util.option_map v.return_item_collection_metrics
             (fun f ->
                Query.Pair
                  ("ReturnItemCollectionMetrics",
                    (ReturnItemCollectionMetrics.to_query f)));
           Util.option_map v.return_consumed_capacity
             (fun f ->
                Query.Pair
                  ("ReturnConsumedCapacity",
                    (ReturnConsumedCapacity.to_query f)));
           Util.option_map v.return_values
             (fun f -> Query.Pair ("ReturnValues", (ReturnValue.to_query f)));
           Util.option_map v.conditional_operator
             (fun f ->
                Query.Pair
                  ("ConditionalOperator", (ConditionalOperator.to_query f)));
           Util.option_map v.expected
             (fun f ->
                Query.Pair ("Expected", (ExpectedAttributeMap.to_query f)));
           Util.option_map v.attribute_updates
             (fun f ->
                Query.Pair
                  ("AttributeUpdates", (AttributeUpdates.to_query f)));
           Some (Query.Pair ("Key", (Key.to_query v.key)));
           Some (Query.Pair ("TableName", (String.to_query v.table_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.expression_attribute_values
              (fun f ->
                 ("expression_attribute_values",
                   (ExpressionAttributeValueMap.to_json f)));
           Util.option_map v.expression_attribute_names
             (fun f ->
                ("expression_attribute_names",
                  (ExpressionAttributeNameMap.to_json f)));
           Util.option_map v.condition_expression
             (fun f -> ("condition_expression", (String.to_json f)));
           Util.option_map v.update_expression
             (fun f -> ("update_expression", (String.to_json f)));
           Util.option_map v.return_item_collection_metrics
             (fun f ->
                ("return_item_collection_metrics",
                  (ReturnItemCollectionMetrics.to_json f)));
           Util.option_map v.return_consumed_capacity
             (fun f ->
                ("return_consumed_capacity",
                  (ReturnConsumedCapacity.to_json f)));
           Util.option_map v.return_values
             (fun f -> ("return_values", (ReturnValue.to_json f)));
           Util.option_map v.conditional_operator
             (fun f ->
                ("conditional_operator", (ConditionalOperator.to_json f)));
           Util.option_map v.expected
             (fun f -> ("expected", (ExpectedAttributeMap.to_json f)));
           Util.option_map v.attribute_updates
             (fun f -> ("attribute_updates", (AttributeUpdates.to_json f)));
           Some ("key", (Key.to_json v.key));
           Some ("table_name", (String.to_json v.table_name))])
    let of_json j =
      {
        table_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "table_name")));
        key = (Key.of_json (Util.of_option_exn (Json.lookup j "key")));
        attribute_updates =
          (Util.option_map (Json.lookup j "attribute_updates")
             AttributeUpdates.of_json);
        expected =
          (Util.option_map (Json.lookup j "expected")
             ExpectedAttributeMap.of_json);
        conditional_operator =
          (Util.option_map (Json.lookup j "conditional_operator")
             ConditionalOperator.of_json);
        return_values =
          (Util.option_map (Json.lookup j "return_values")
             ReturnValue.of_json);
        return_consumed_capacity =
          (Util.option_map (Json.lookup j "return_consumed_capacity")
             ReturnConsumedCapacity.of_json);
        return_item_collection_metrics =
          (Util.option_map (Json.lookup j "return_item_collection_metrics")
             ReturnItemCollectionMetrics.of_json);
        update_expression =
          (Util.option_map (Json.lookup j "update_expression") String.of_json);
        condition_expression =
          (Util.option_map (Json.lookup j "condition_expression")
             String.of_json);
        expression_attribute_names =
          (Util.option_map (Json.lookup j "expression_attribute_names")
             ExpressionAttributeNameMap.of_json);
        expression_attribute_values =
          (Util.option_map (Json.lookup j "expression_attribute_values")
             ExpressionAttributeValueMap.of_json)
      }
  end
module BatchWriteItemOutput =
  struct
    type t =
      {
      unprocessed_items: BatchWriteItemRequestMap.t option ;
      item_collection_metrics: ItemCollectionMetricsPerTable.t option ;
      consumed_capacity: ConsumedCapacityMultiple.t }
    let make ?unprocessed_items  ?item_collection_metrics 
      ?(consumed_capacity= [])  () =
      { unprocessed_items; item_collection_metrics; consumed_capacity }
    let parse xml =
      Some
        {
          unprocessed_items =
            (Util.option_bind (Xml.member "UnprocessedItems" xml)
               BatchWriteItemRequestMap.parse);
          item_collection_metrics =
            (Util.option_bind (Xml.member "ItemCollectionMetrics" xml)
               ItemCollectionMetricsPerTable.parse);
          consumed_capacity =
            (Util.of_option []
               (Util.option_bind (Xml.member "ConsumedCapacity" xml)
                  ConsumedCapacityMultiple.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ConsumedCapacity.member",
                   (ConsumedCapacityMultiple.to_query v.consumed_capacity)));
           Util.option_map v.item_collection_metrics
             (fun f ->
                Query.Pair
                  ("ItemCollectionMetrics",
                    (ItemCollectionMetricsPerTable.to_query f)));
           Util.option_map v.unprocessed_items
             (fun f ->
                Query.Pair
                  ("UnprocessedItems", (BatchWriteItemRequestMap.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("consumed_capacity",
                (ConsumedCapacityMultiple.to_json v.consumed_capacity));
           Util.option_map v.item_collection_metrics
             (fun f ->
                ("item_collection_metrics",
                  (ItemCollectionMetricsPerTable.to_json f)));
           Util.option_map v.unprocessed_items
             (fun f ->
                ("unprocessed_items", (BatchWriteItemRequestMap.to_json f)))])
    let of_json j =
      {
        unprocessed_items =
          (Util.option_map (Json.lookup j "unprocessed_items")
             BatchWriteItemRequestMap.of_json);
        item_collection_metrics =
          (Util.option_map (Json.lookup j "item_collection_metrics")
             ItemCollectionMetricsPerTable.of_json);
        consumed_capacity =
          (ConsumedCapacityMultiple.of_json
             (Util.of_option_exn (Json.lookup j "consumed_capacity")))
      }
  end
module ListTablesInput =
  struct
    type t =
      {
      exclusive_start_table_name: String.t option ;
      limit: Integer.t option }
    let make ?exclusive_start_table_name  ?limit  () =
      { exclusive_start_table_name; limit }
    let parse xml =
      Some
        {
          exclusive_start_table_name =
            (Util.option_bind (Xml.member "ExclusiveStartTableName" xml)
               String.parse);
          limit = (Util.option_bind (Xml.member "Limit" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.limit
              (fun f -> Query.Pair ("Limit", (Integer.to_query f)));
           Util.option_map v.exclusive_start_table_name
             (fun f ->
                Query.Pair ("ExclusiveStartTableName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.limit (fun f -> ("limit", (Integer.to_json f)));
           Util.option_map v.exclusive_start_table_name
             (fun f -> ("exclusive_start_table_name", (String.to_json f)))])
    let of_json j =
      {
        exclusive_start_table_name =
          (Util.option_map (Json.lookup j "exclusive_start_table_name")
             String.of_json);
        limit = (Util.option_map (Json.lookup j "limit") Integer.of_json)
      }
  end
module DeleteTableOutput =
  struct
    type t = {
      table_description: TableDescription.t option }
    let make ?table_description  () = { table_description }
    let parse xml =
      Some
        {
          table_description =
            (Util.option_bind (Xml.member "TableDescription" xml)
               TableDescription.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.table_description
              (fun f ->
                 Query.Pair
                   ("TableDescription", (TableDescription.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.table_description
              (fun f -> ("table_description", (TableDescription.to_json f)))])
    let of_json j =
      {
        table_description =
          (Util.option_map (Json.lookup j "table_description")
             TableDescription.of_json)
      }
  end
module DeleteItemOutput =
  struct
    type t =
      {
      attributes: AttributeMap.t option ;
      consumed_capacity: ConsumedCapacity.t option ;
      item_collection_metrics: ItemCollectionMetrics.t option }
    let make ?attributes  ?consumed_capacity  ?item_collection_metrics  () =
      { attributes; consumed_capacity; item_collection_metrics }
    let parse xml =
      Some
        {
          attributes =
            (Util.option_bind (Xml.member "Attributes" xml)
               AttributeMap.parse);
          consumed_capacity =
            (Util.option_bind (Xml.member "ConsumedCapacity" xml)
               ConsumedCapacity.parse);
          item_collection_metrics =
            (Util.option_bind (Xml.member "ItemCollectionMetrics" xml)
               ItemCollectionMetrics.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.item_collection_metrics
              (fun f ->
                 Query.Pair
                   ("ItemCollectionMetrics",
                     (ItemCollectionMetrics.to_query f)));
           Util.option_map v.consumed_capacity
             (fun f ->
                Query.Pair
                  ("ConsumedCapacity", (ConsumedCapacity.to_query f)));
           Util.option_map v.attributes
             (fun f -> Query.Pair ("Attributes", (AttributeMap.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.item_collection_metrics
              (fun f ->
                 ("item_collection_metrics",
                   (ItemCollectionMetrics.to_json f)));
           Util.option_map v.consumed_capacity
             (fun f -> ("consumed_capacity", (ConsumedCapacity.to_json f)));
           Util.option_map v.attributes
             (fun f -> ("attributes", (AttributeMap.to_json f)))])
    let of_json j =
      {
        attributes =
          (Util.option_map (Json.lookup j "attributes") AttributeMap.of_json);
        consumed_capacity =
          (Util.option_map (Json.lookup j "consumed_capacity")
             ConsumedCapacity.of_json);
        item_collection_metrics =
          (Util.option_map (Json.lookup j "item_collection_metrics")
             ItemCollectionMetrics.of_json)
      }
  end
module GetItemInput =
  struct
    type t =
      {
      table_name: String.t ;
      key: Key.t ;
      attributes_to_get: AttributeNameList.t ;
      consistent_read: Boolean.t option ;
      return_consumed_capacity: ReturnConsumedCapacity.t option ;
      projection_expression: String.t option ;
      expression_attribute_names: ExpressionAttributeNameMap.t option }
    let make ~table_name  ~key  ?(attributes_to_get= [])  ?consistent_read 
      ?return_consumed_capacity  ?projection_expression 
      ?expression_attribute_names  () =
      {
        table_name;
        key;
        attributes_to_get;
        consistent_read;
        return_consumed_capacity;
        projection_expression;
        expression_attribute_names
      }
    let parse xml =
      Some
        {
          table_name =
            (Xml.required "TableName"
               (Util.option_bind (Xml.member "TableName" xml) String.parse));
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) Key.parse));
          attributes_to_get =
            (Util.of_option []
               (Util.option_bind (Xml.member "AttributesToGet" xml)
                  AttributeNameList.parse));
          consistent_read =
            (Util.option_bind (Xml.member "ConsistentRead" xml) Boolean.parse);
          return_consumed_capacity =
            (Util.option_bind (Xml.member "ReturnConsumedCapacity" xml)
               ReturnConsumedCapacity.parse);
          projection_expression =
            (Util.option_bind (Xml.member "ProjectionExpression" xml)
               String.parse);
          expression_attribute_names =
            (Util.option_bind (Xml.member "ExpressionAttributeNames" xml)
               ExpressionAttributeNameMap.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.expression_attribute_names
              (fun f ->
                 Query.Pair
                   ("ExpressionAttributeNames",
                     (ExpressionAttributeNameMap.to_query f)));
           Util.option_map v.projection_expression
             (fun f ->
                Query.Pair ("ProjectionExpression", (String.to_query f)));
           Util.option_map v.return_consumed_capacity
             (fun f ->
                Query.Pair
                  ("ReturnConsumedCapacity",
                    (ReturnConsumedCapacity.to_query f)));
           Util.option_map v.consistent_read
             (fun f -> Query.Pair ("ConsistentRead", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("AttributesToGet.member",
                  (AttributeNameList.to_query v.attributes_to_get)));
           Some (Query.Pair ("Key", (Key.to_query v.key)));
           Some (Query.Pair ("TableName", (String.to_query v.table_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.expression_attribute_names
              (fun f ->
                 ("expression_attribute_names",
                   (ExpressionAttributeNameMap.to_json f)));
           Util.option_map v.projection_expression
             (fun f -> ("projection_expression", (String.to_json f)));
           Util.option_map v.return_consumed_capacity
             (fun f ->
                ("return_consumed_capacity",
                  (ReturnConsumedCapacity.to_json f)));
           Util.option_map v.consistent_read
             (fun f -> ("consistent_read", (Boolean.to_json f)));
           Some
             ("attributes_to_get",
               (AttributeNameList.to_json v.attributes_to_get));
           Some ("key", (Key.to_json v.key));
           Some ("table_name", (String.to_json v.table_name))])
    let of_json j =
      {
        table_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "table_name")));
        key = (Key.of_json (Util.of_option_exn (Json.lookup j "key")));
        attributes_to_get =
          (AttributeNameList.of_json
             (Util.of_option_exn (Json.lookup j "attributes_to_get")));
        consistent_read =
          (Util.option_map (Json.lookup j "consistent_read") Boolean.of_json);
        return_consumed_capacity =
          (Util.option_map (Json.lookup j "return_consumed_capacity")
             ReturnConsumedCapacity.of_json);
        projection_expression =
          (Util.option_map (Json.lookup j "projection_expression")
             String.of_json);
        expression_attribute_names =
          (Util.option_map (Json.lookup j "expression_attribute_names")
             ExpressionAttributeNameMap.of_json)
      }
  end
module GetItemOutput =
  struct
    type t =
      {
      item: AttributeMap.t option ;
      consumed_capacity: ConsumedCapacity.t option }
    let make ?item  ?consumed_capacity  () = { item; consumed_capacity }
    let parse xml =
      Some
        {
          item =
            (Util.option_bind (Xml.member "Item" xml) AttributeMap.parse);
          consumed_capacity =
            (Util.option_bind (Xml.member "ConsumedCapacity" xml)
               ConsumedCapacity.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.consumed_capacity
              (fun f ->
                 Query.Pair
                   ("ConsumedCapacity", (ConsumedCapacity.to_query f)));
           Util.option_map v.item
             (fun f -> Query.Pair ("Item", (AttributeMap.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.consumed_capacity
              (fun f -> ("consumed_capacity", (ConsumedCapacity.to_json f)));
           Util.option_map v.item
             (fun f -> ("item", (AttributeMap.to_json f)))])
    let of_json j =
      {
        item = (Util.option_map (Json.lookup j "item") AttributeMap.of_json);
        consumed_capacity =
          (Util.option_map (Json.lookup j "consumed_capacity")
             ConsumedCapacity.of_json)
      }
  end
module UpdateItemOutput =
  struct
    type t =
      {
      attributes: AttributeMap.t option ;
      consumed_capacity: ConsumedCapacity.t option ;
      item_collection_metrics: ItemCollectionMetrics.t option }
    let make ?attributes  ?consumed_capacity  ?item_collection_metrics  () =
      { attributes; consumed_capacity; item_collection_metrics }
    let parse xml =
      Some
        {
          attributes =
            (Util.option_bind (Xml.member "Attributes" xml)
               AttributeMap.parse);
          consumed_capacity =
            (Util.option_bind (Xml.member "ConsumedCapacity" xml)
               ConsumedCapacity.parse);
          item_collection_metrics =
            (Util.option_bind (Xml.member "ItemCollectionMetrics" xml)
               ItemCollectionMetrics.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.item_collection_metrics
              (fun f ->
                 Query.Pair
                   ("ItemCollectionMetrics",
                     (ItemCollectionMetrics.to_query f)));
           Util.option_map v.consumed_capacity
             (fun f ->
                Query.Pair
                  ("ConsumedCapacity", (ConsumedCapacity.to_query f)));
           Util.option_map v.attributes
             (fun f -> Query.Pair ("Attributes", (AttributeMap.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.item_collection_metrics
              (fun f ->
                 ("item_collection_metrics",
                   (ItemCollectionMetrics.to_json f)));
           Util.option_map v.consumed_capacity
             (fun f -> ("consumed_capacity", (ConsumedCapacity.to_json f)));
           Util.option_map v.attributes
             (fun f -> ("attributes", (AttributeMap.to_json f)))])
    let of_json j =
      {
        attributes =
          (Util.option_map (Json.lookup j "attributes") AttributeMap.of_json);
        consumed_capacity =
          (Util.option_map (Json.lookup j "consumed_capacity")
             ConsumedCapacity.of_json);
        item_collection_metrics =
          (Util.option_map (Json.lookup j "item_collection_metrics")
             ItemCollectionMetrics.of_json)
      }
  end
module ResourceInUseException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module CreateTableInput =
  struct
    type t =
      {
      attribute_definitions: AttributeDefinitions.t ;
      table_name: String.t ;
      key_schema: KeySchema.t ;
      local_secondary_indexes: LocalSecondaryIndexList.t ;
      global_secondary_indexes: GlobalSecondaryIndexList.t ;
      provisioned_throughput: ProvisionedThroughput.t ;
      stream_specification: StreamSpecification.t option }
    let make ~attribute_definitions  ~table_name  ~key_schema 
      ?(local_secondary_indexes= [])  ?(global_secondary_indexes= []) 
      ~provisioned_throughput  ?stream_specification  () =
      {
        attribute_definitions;
        table_name;
        key_schema;
        local_secondary_indexes;
        global_secondary_indexes;
        provisioned_throughput;
        stream_specification
      }
    let parse xml =
      Some
        {
          attribute_definitions =
            (Xml.required "AttributeDefinitions"
               (Util.option_bind (Xml.member "AttributeDefinitions" xml)
                  AttributeDefinitions.parse));
          table_name =
            (Xml.required "TableName"
               (Util.option_bind (Xml.member "TableName" xml) String.parse));
          key_schema =
            (Xml.required "KeySchema"
               (Util.option_bind (Xml.member "KeySchema" xml) KeySchema.parse));
          local_secondary_indexes =
            (Util.of_option []
               (Util.option_bind (Xml.member "LocalSecondaryIndexes" xml)
                  LocalSecondaryIndexList.parse));
          global_secondary_indexes =
            (Util.of_option []
               (Util.option_bind (Xml.member "GlobalSecondaryIndexes" xml)
                  GlobalSecondaryIndexList.parse));
          provisioned_throughput =
            (Xml.required "ProvisionedThroughput"
               (Util.option_bind (Xml.member "ProvisionedThroughput" xml)
                  ProvisionedThroughput.parse));
          stream_specification =
            (Util.option_bind (Xml.member "StreamSpecification" xml)
               StreamSpecification.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.stream_specification
              (fun f ->
                 Query.Pair
                   ("StreamSpecification", (StreamSpecification.to_query f)));
           Some
             (Query.Pair
                ("ProvisionedThroughput",
                  (ProvisionedThroughput.to_query v.provisioned_throughput)));
           Some
             (Query.Pair
                ("GlobalSecondaryIndexes.member",
                  (GlobalSecondaryIndexList.to_query
                     v.global_secondary_indexes)));
           Some
             (Query.Pair
                ("LocalSecondaryIndexes.member",
                  (LocalSecondaryIndexList.to_query v.local_secondary_indexes)));
           Some
             (Query.Pair
                ("KeySchema.member", (KeySchema.to_query v.key_schema)));
           Some (Query.Pair ("TableName", (String.to_query v.table_name)));
           Some
             (Query.Pair
                ("AttributeDefinitions.member",
                  (AttributeDefinitions.to_query v.attribute_definitions)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stream_specification
              (fun f ->
                 ("stream_specification", (StreamSpecification.to_json f)));
           Some
             ("provisioned_throughput",
               (ProvisionedThroughput.to_json v.provisioned_throughput));
           Some
             ("global_secondary_indexes",
               (GlobalSecondaryIndexList.to_json v.global_secondary_indexes));
           Some
             ("local_secondary_indexes",
               (LocalSecondaryIndexList.to_json v.local_secondary_indexes));
           Some ("key_schema", (KeySchema.to_json v.key_schema));
           Some ("table_name", (String.to_json v.table_name));
           Some
             ("attribute_definitions",
               (AttributeDefinitions.to_json v.attribute_definitions))])
    let of_json j =
      {
        attribute_definitions =
          (AttributeDefinitions.of_json
             (Util.of_option_exn (Json.lookup j "attribute_definitions")));
        table_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "table_name")));
        key_schema =
          (KeySchema.of_json
             (Util.of_option_exn (Json.lookup j "key_schema")));
        local_secondary_indexes =
          (LocalSecondaryIndexList.of_json
             (Util.of_option_exn (Json.lookup j "local_secondary_indexes")));
        global_secondary_indexes =
          (GlobalSecondaryIndexList.of_json
             (Util.of_option_exn (Json.lookup j "global_secondary_indexes")));
        provisioned_throughput =
          (ProvisionedThroughput.of_json
             (Util.of_option_exn (Json.lookup j "provisioned_throughput")));
        stream_specification =
          (Util.option_map (Json.lookup j "stream_specification")
             StreamSpecification.of_json)
      }
  end
module DescribeTableInput =
  struct
    type t = {
      table_name: String.t }
    let make ~table_name  () = { table_name }
    let parse xml =
      Some
        {
          table_name =
            (Xml.required "TableName"
               (Util.option_bind (Xml.member "TableName" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("TableName", (String.to_query v.table_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("table_name", (String.to_json v.table_name))])
    let of_json j =
      {
        table_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "table_name")))
      }
  end
module BatchWriteItemInput =
  struct
    type t =
      {
      request_items: BatchWriteItemRequestMap.t ;
      return_consumed_capacity: ReturnConsumedCapacity.t option ;
      return_item_collection_metrics: ReturnItemCollectionMetrics.t option }
    let make ~request_items  ?return_consumed_capacity 
      ?return_item_collection_metrics  () =
      {
        request_items;
        return_consumed_capacity;
        return_item_collection_metrics
      }
    let parse xml =
      Some
        {
          request_items =
            (Xml.required "RequestItems"
               (Util.option_bind (Xml.member "RequestItems" xml)
                  BatchWriteItemRequestMap.parse));
          return_consumed_capacity =
            (Util.option_bind (Xml.member "ReturnConsumedCapacity" xml)
               ReturnConsumedCapacity.parse);
          return_item_collection_metrics =
            (Util.option_bind (Xml.member "ReturnItemCollectionMetrics" xml)
               ReturnItemCollectionMetrics.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.return_item_collection_metrics
              (fun f ->
                 Query.Pair
                   ("ReturnItemCollectionMetrics",
                     (ReturnItemCollectionMetrics.to_query f)));
           Util.option_map v.return_consumed_capacity
             (fun f ->
                Query.Pair
                  ("ReturnConsumedCapacity",
                    (ReturnConsumedCapacity.to_query f)));
           Some
             (Query.Pair
                ("RequestItems",
                  (BatchWriteItemRequestMap.to_query v.request_items)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.return_item_collection_metrics
              (fun f ->
                 ("return_item_collection_metrics",
                   (ReturnItemCollectionMetrics.to_json f)));
           Util.option_map v.return_consumed_capacity
             (fun f ->
                ("return_consumed_capacity",
                  (ReturnConsumedCapacity.to_json f)));
           Some
             ("request_items",
               (BatchWriteItemRequestMap.to_json v.request_items))])
    let of_json j =
      {
        request_items =
          (BatchWriteItemRequestMap.of_json
             (Util.of_option_exn (Json.lookup j "request_items")));
        return_consumed_capacity =
          (Util.option_map (Json.lookup j "return_consumed_capacity")
             ReturnConsumedCapacity.of_json);
        return_item_collection_metrics =
          (Util.option_map (Json.lookup j "return_item_collection_metrics")
             ReturnItemCollectionMetrics.of_json)
      }
  end
module CreateTableOutput =
  struct
    type t = {
      table_description: TableDescription.t option }
    let make ?table_description  () = { table_description }
    let parse xml =
      Some
        {
          table_description =
            (Util.option_bind (Xml.member "TableDescription" xml)
               TableDescription.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.table_description
              (fun f ->
                 Query.Pair
                   ("TableDescription", (TableDescription.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.table_description
              (fun f -> ("table_description", (TableDescription.to_json f)))])
    let of_json j =
      {
        table_description =
          (Util.option_map (Json.lookup j "table_description")
             TableDescription.of_json)
      }
  end
module ListTablesOutput =
  struct
    type t =
      {
      table_names: TableNameList.t ;
      last_evaluated_table_name: String.t option }
    let make ?(table_names= [])  ?last_evaluated_table_name  () =
      { table_names; last_evaluated_table_name }
    let parse xml =
      Some
        {
          table_names =
            (Util.of_option []
               (Util.option_bind (Xml.member "TableNames" xml)
                  TableNameList.parse));
          last_evaluated_table_name =
            (Util.option_bind (Xml.member "LastEvaluatedTableName" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.last_evaluated_table_name
              (fun f ->
                 Query.Pair ("LastEvaluatedTableName", (String.to_query f)));
           Some
             (Query.Pair
                ("TableNames.member", (TableNameList.to_query v.table_names)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.last_evaluated_table_name
              (fun f -> ("last_evaluated_table_name", (String.to_json f)));
           Some ("table_names", (TableNameList.to_json v.table_names))])
    let of_json j =
      {
        table_names =
          (TableNameList.of_json
             (Util.of_option_exn (Json.lookup j "table_names")));
        last_evaluated_table_name =
          (Util.option_map (Json.lookup j "last_evaluated_table_name")
             String.of_json)
      }
  end
module InternalServerError =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module PutItemOutput =
  struct
    type t =
      {
      attributes: AttributeMap.t option ;
      consumed_capacity: ConsumedCapacity.t option ;
      item_collection_metrics: ItemCollectionMetrics.t option }
    let make ?attributes  ?consumed_capacity  ?item_collection_metrics  () =
      { attributes; consumed_capacity; item_collection_metrics }
    let parse xml =
      Some
        {
          attributes =
            (Util.option_bind (Xml.member "Attributes" xml)
               AttributeMap.parse);
          consumed_capacity =
            (Util.option_bind (Xml.member "ConsumedCapacity" xml)
               ConsumedCapacity.parse);
          item_collection_metrics =
            (Util.option_bind (Xml.member "ItemCollectionMetrics" xml)
               ItemCollectionMetrics.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.item_collection_metrics
              (fun f ->
                 Query.Pair
                   ("ItemCollectionMetrics",
                     (ItemCollectionMetrics.to_query f)));
           Util.option_map v.consumed_capacity
             (fun f ->
                Query.Pair
                  ("ConsumedCapacity", (ConsumedCapacity.to_query f)));
           Util.option_map v.attributes
             (fun f -> Query.Pair ("Attributes", (AttributeMap.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.item_collection_metrics
              (fun f ->
                 ("item_collection_metrics",
                   (ItemCollectionMetrics.to_json f)));
           Util.option_map v.consumed_capacity
             (fun f -> ("consumed_capacity", (ConsumedCapacity.to_json f)));
           Util.option_map v.attributes
             (fun f -> ("attributes", (AttributeMap.to_json f)))])
    let of_json j =
      {
        attributes =
          (Util.option_map (Json.lookup j "attributes") AttributeMap.of_json);
        consumed_capacity =
          (Util.option_map (Json.lookup j "consumed_capacity")
             ConsumedCapacity.of_json);
        item_collection_metrics =
          (Util.option_map (Json.lookup j "item_collection_metrics")
             ItemCollectionMetrics.of_json)
      }
  end
module ConditionalCheckFailedException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ScanOutput =
  struct
    type t =
      {
      items: ItemList.t ;
      count: Integer.t option ;
      scanned_count: Integer.t option ;
      last_evaluated_key: Key.t option ;
      consumed_capacity: ConsumedCapacity.t option }
    let make ?(items= [])  ?count  ?scanned_count  ?last_evaluated_key 
      ?consumed_capacity  () =
      { items; count; scanned_count; last_evaluated_key; consumed_capacity }
    let parse xml =
      Some
        {
          items =
            (Util.of_option []
               (Util.option_bind (Xml.member "Items" xml) ItemList.parse));
          count = (Util.option_bind (Xml.member "Count" xml) Integer.parse);
          scanned_count =
            (Util.option_bind (Xml.member "ScannedCount" xml) Integer.parse);
          last_evaluated_key =
            (Util.option_bind (Xml.member "LastEvaluatedKey" xml) Key.parse);
          consumed_capacity =
            (Util.option_bind (Xml.member "ConsumedCapacity" xml)
               ConsumedCapacity.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.consumed_capacity
              (fun f ->
                 Query.Pair
                   ("ConsumedCapacity", (ConsumedCapacity.to_query f)));
           Util.option_map v.last_evaluated_key
             (fun f -> Query.Pair ("LastEvaluatedKey", (Key.to_query f)));
           Util.option_map v.scanned_count
             (fun f -> Query.Pair ("ScannedCount", (Integer.to_query f)));
           Util.option_map v.count
             (fun f -> Query.Pair ("Count", (Integer.to_query f)));
           Some (Query.Pair ("Items.member", (ItemList.to_query v.items)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.consumed_capacity
              (fun f -> ("consumed_capacity", (ConsumedCapacity.to_json f)));
           Util.option_map v.last_evaluated_key
             (fun f -> ("last_evaluated_key", (Key.to_json f)));
           Util.option_map v.scanned_count
             (fun f -> ("scanned_count", (Integer.to_json f)));
           Util.option_map v.count (fun f -> ("count", (Integer.to_json f)));
           Some ("items", (ItemList.to_json v.items))])
    let of_json j =
      {
        items =
          (ItemList.of_json (Util.of_option_exn (Json.lookup j "items")));
        count = (Util.option_map (Json.lookup j "count") Integer.of_json);
        scanned_count =
          (Util.option_map (Json.lookup j "scanned_count") Integer.of_json);
        last_evaluated_key =
          (Util.option_map (Json.lookup j "last_evaluated_key") Key.of_json);
        consumed_capacity =
          (Util.option_map (Json.lookup j "consumed_capacity")
             ConsumedCapacity.of_json)
      }
  end
module DeleteTableInput =
  struct
    type t = {
      table_name: String.t }
    let make ~table_name  () = { table_name }
    let parse xml =
      Some
        {
          table_name =
            (Xml.required "TableName"
               (Util.option_bind (Xml.member "TableName" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("TableName", (String.to_query v.table_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("table_name", (String.to_json v.table_name))])
    let of_json j =
      {
        table_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "table_name")))
      }
  end
module QueryInput =
  struct
    type t =
      {
      table_name: String.t ;
      index_name: String.t option ;
      select: Select.t option ;
      attributes_to_get: AttributeNameList.t ;
      limit: Integer.t option ;
      consistent_read: Boolean.t option ;
      key_conditions: KeyConditions.t option ;
      query_filter: FilterConditionMap.t option ;
      conditional_operator: ConditionalOperator.t option ;
      scan_index_forward: Boolean.t option ;
      exclusive_start_key: Key.t option ;
      return_consumed_capacity: ReturnConsumedCapacity.t option ;
      projection_expression: String.t option ;
      filter_expression: String.t option ;
      key_condition_expression: String.t option ;
      expression_attribute_names: ExpressionAttributeNameMap.t option ;
      expression_attribute_values: ExpressionAttributeValueMap.t option }
    let make ~table_name  ?index_name  ?select  ?(attributes_to_get= []) 
      ?limit  ?consistent_read  ?key_conditions  ?query_filter 
      ?conditional_operator  ?scan_index_forward  ?exclusive_start_key 
      ?return_consumed_capacity  ?projection_expression  ?filter_expression 
      ?key_condition_expression  ?expression_attribute_names 
      ?expression_attribute_values  () =
      {
        table_name;
        index_name;
        select;
        attributes_to_get;
        limit;
        consistent_read;
        key_conditions;
        query_filter;
        conditional_operator;
        scan_index_forward;
        exclusive_start_key;
        return_consumed_capacity;
        projection_expression;
        filter_expression;
        key_condition_expression;
        expression_attribute_names;
        expression_attribute_values
      }
    let parse xml =
      Some
        {
          table_name =
            (Xml.required "TableName"
               (Util.option_bind (Xml.member "TableName" xml) String.parse));
          index_name =
            (Util.option_bind (Xml.member "IndexName" xml) String.parse);
          select = (Util.option_bind (Xml.member "Select" xml) Select.parse);
          attributes_to_get =
            (Util.of_option []
               (Util.option_bind (Xml.member "AttributesToGet" xml)
                  AttributeNameList.parse));
          limit = (Util.option_bind (Xml.member "Limit" xml) Integer.parse);
          consistent_read =
            (Util.option_bind (Xml.member "ConsistentRead" xml) Boolean.parse);
          key_conditions =
            (Util.option_bind (Xml.member "KeyConditions" xml)
               KeyConditions.parse);
          query_filter =
            (Util.option_bind (Xml.member "QueryFilter" xml)
               FilterConditionMap.parse);
          conditional_operator =
            (Util.option_bind (Xml.member "ConditionalOperator" xml)
               ConditionalOperator.parse);
          scan_index_forward =
            (Util.option_bind (Xml.member "ScanIndexForward" xml)
               Boolean.parse);
          exclusive_start_key =
            (Util.option_bind (Xml.member "ExclusiveStartKey" xml) Key.parse);
          return_consumed_capacity =
            (Util.option_bind (Xml.member "ReturnConsumedCapacity" xml)
               ReturnConsumedCapacity.parse);
          projection_expression =
            (Util.option_bind (Xml.member "ProjectionExpression" xml)
               String.parse);
          filter_expression =
            (Util.option_bind (Xml.member "FilterExpression" xml)
               String.parse);
          key_condition_expression =
            (Util.option_bind (Xml.member "KeyConditionExpression" xml)
               String.parse);
          expression_attribute_names =
            (Util.option_bind (Xml.member "ExpressionAttributeNames" xml)
               ExpressionAttributeNameMap.parse);
          expression_attribute_values =
            (Util.option_bind (Xml.member "ExpressionAttributeValues" xml)
               ExpressionAttributeValueMap.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.expression_attribute_values
              (fun f ->
                 Query.Pair
                   ("ExpressionAttributeValues",
                     (ExpressionAttributeValueMap.to_query f)));
           Util.option_map v.expression_attribute_names
             (fun f ->
                Query.Pair
                  ("ExpressionAttributeNames",
                    (ExpressionAttributeNameMap.to_query f)));
           Util.option_map v.key_condition_expression
             (fun f ->
                Query.Pair ("KeyConditionExpression", (String.to_query f)));
           Util.option_map v.filter_expression
             (fun f -> Query.Pair ("FilterExpression", (String.to_query f)));
           Util.option_map v.projection_expression
             (fun f ->
                Query.Pair ("ProjectionExpression", (String.to_query f)));
           Util.option_map v.return_consumed_capacity
             (fun f ->
                Query.Pair
                  ("ReturnConsumedCapacity",
                    (ReturnConsumedCapacity.to_query f)));
           Util.option_map v.exclusive_start_key
             (fun f -> Query.Pair ("ExclusiveStartKey", (Key.to_query f)));
           Util.option_map v.scan_index_forward
             (fun f -> Query.Pair ("ScanIndexForward", (Boolean.to_query f)));
           Util.option_map v.conditional_operator
             (fun f ->
                Query.Pair
                  ("ConditionalOperator", (ConditionalOperator.to_query f)));
           Util.option_map v.query_filter
             (fun f ->
                Query.Pair ("QueryFilter", (FilterConditionMap.to_query f)));
           Util.option_map v.key_conditions
             (fun f ->
                Query.Pair ("KeyConditions", (KeyConditions.to_query f)));
           Util.option_map v.consistent_read
             (fun f -> Query.Pair ("ConsistentRead", (Boolean.to_query f)));
           Util.option_map v.limit
             (fun f -> Query.Pair ("Limit", (Integer.to_query f)));
           Some
             (Query.Pair
                ("AttributesToGet.member",
                  (AttributeNameList.to_query v.attributes_to_get)));
           Util.option_map v.select
             (fun f -> Query.Pair ("Select", (Select.to_query f)));
           Util.option_map v.index_name
             (fun f -> Query.Pair ("IndexName", (String.to_query f)));
           Some (Query.Pair ("TableName", (String.to_query v.table_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.expression_attribute_values
              (fun f ->
                 ("expression_attribute_values",
                   (ExpressionAttributeValueMap.to_json f)));
           Util.option_map v.expression_attribute_names
             (fun f ->
                ("expression_attribute_names",
                  (ExpressionAttributeNameMap.to_json f)));
           Util.option_map v.key_condition_expression
             (fun f -> ("key_condition_expression", (String.to_json f)));
           Util.option_map v.filter_expression
             (fun f -> ("filter_expression", (String.to_json f)));
           Util.option_map v.projection_expression
             (fun f -> ("projection_expression", (String.to_json f)));
           Util.option_map v.return_consumed_capacity
             (fun f ->
                ("return_consumed_capacity",
                  (ReturnConsumedCapacity.to_json f)));
           Util.option_map v.exclusive_start_key
             (fun f -> ("exclusive_start_key", (Key.to_json f)));
           Util.option_map v.scan_index_forward
             (fun f -> ("scan_index_forward", (Boolean.to_json f)));
           Util.option_map v.conditional_operator
             (fun f ->
                ("conditional_operator", (ConditionalOperator.to_json f)));
           Util.option_map v.query_filter
             (fun f -> ("query_filter", (FilterConditionMap.to_json f)));
           Util.option_map v.key_conditions
             (fun f -> ("key_conditions", (KeyConditions.to_json f)));
           Util.option_map v.consistent_read
             (fun f -> ("consistent_read", (Boolean.to_json f)));
           Util.option_map v.limit (fun f -> ("limit", (Integer.to_json f)));
           Some
             ("attributes_to_get",
               (AttributeNameList.to_json v.attributes_to_get));
           Util.option_map v.select (fun f -> ("select", (Select.to_json f)));
           Util.option_map v.index_name
             (fun f -> ("index_name", (String.to_json f)));
           Some ("table_name", (String.to_json v.table_name))])
    let of_json j =
      {
        table_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "table_name")));
        index_name =
          (Util.option_map (Json.lookup j "index_name") String.of_json);
        select = (Util.option_map (Json.lookup j "select") Select.of_json);
        attributes_to_get =
          (AttributeNameList.of_json
             (Util.of_option_exn (Json.lookup j "attributes_to_get")));
        limit = (Util.option_map (Json.lookup j "limit") Integer.of_json);
        consistent_read =
          (Util.option_map (Json.lookup j "consistent_read") Boolean.of_json);
        key_conditions =
          (Util.option_map (Json.lookup j "key_conditions")
             KeyConditions.of_json);
        query_filter =
          (Util.option_map (Json.lookup j "query_filter")
             FilterConditionMap.of_json);
        conditional_operator =
          (Util.option_map (Json.lookup j "conditional_operator")
             ConditionalOperator.of_json);
        scan_index_forward =
          (Util.option_map (Json.lookup j "scan_index_forward")
             Boolean.of_json);
        exclusive_start_key =
          (Util.option_map (Json.lookup j "exclusive_start_key") Key.of_json);
        return_consumed_capacity =
          (Util.option_map (Json.lookup j "return_consumed_capacity")
             ReturnConsumedCapacity.of_json);
        projection_expression =
          (Util.option_map (Json.lookup j "projection_expression")
             String.of_json);
        filter_expression =
          (Util.option_map (Json.lookup j "filter_expression") String.of_json);
        key_condition_expression =
          (Util.option_map (Json.lookup j "key_condition_expression")
             String.of_json);
        expression_attribute_names =
          (Util.option_map (Json.lookup j "expression_attribute_names")
             ExpressionAttributeNameMap.of_json);
        expression_attribute_values =
          (Util.option_map (Json.lookup j "expression_attribute_values")
             ExpressionAttributeValueMap.of_json)
      }
  end
module BatchGetItemInput =
  struct
    type t =
      {
      request_items: BatchGetRequestMap.t ;
      return_consumed_capacity: ReturnConsumedCapacity.t option }
    let make ~request_items  ?return_consumed_capacity  () =
      { request_items; return_consumed_capacity }
    let parse xml =
      Some
        {
          request_items =
            (Xml.required "RequestItems"
               (Util.option_bind (Xml.member "RequestItems" xml)
                  BatchGetRequestMap.parse));
          return_consumed_capacity =
            (Util.option_bind (Xml.member "ReturnConsumedCapacity" xml)
               ReturnConsumedCapacity.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.return_consumed_capacity
              (fun f ->
                 Query.Pair
                   ("ReturnConsumedCapacity",
                     (ReturnConsumedCapacity.to_query f)));
           Some
             (Query.Pair
                ("RequestItems",
                  (BatchGetRequestMap.to_query v.request_items)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.return_consumed_capacity
              (fun f ->
                 ("return_consumed_capacity",
                   (ReturnConsumedCapacity.to_json f)));
           Some
             ("request_items", (BatchGetRequestMap.to_json v.request_items))])
    let of_json j =
      {
        request_items =
          (BatchGetRequestMap.of_json
             (Util.of_option_exn (Json.lookup j "request_items")));
        return_consumed_capacity =
          (Util.option_map (Json.lookup j "return_consumed_capacity")
             ReturnConsumedCapacity.of_json)
      }
  end
module ScanInput =
  struct
    type t =
      {
      table_name: String.t ;
      index_name: String.t option ;
      attributes_to_get: AttributeNameList.t ;
      limit: Integer.t option ;
      select: Select.t option ;
      scan_filter: FilterConditionMap.t option ;
      conditional_operator: ConditionalOperator.t option ;
      exclusive_start_key: Key.t option ;
      return_consumed_capacity: ReturnConsumedCapacity.t option ;
      total_segments: Integer.t option ;
      segment: Integer.t option ;
      projection_expression: String.t option ;
      filter_expression: String.t option ;
      expression_attribute_names: ExpressionAttributeNameMap.t option ;
      expression_attribute_values: ExpressionAttributeValueMap.t option ;
      consistent_read: Boolean.t option }
    let make ~table_name  ?index_name  ?(attributes_to_get= [])  ?limit 
      ?select  ?scan_filter  ?conditional_operator  ?exclusive_start_key 
      ?return_consumed_capacity  ?total_segments  ?segment 
      ?projection_expression  ?filter_expression  ?expression_attribute_names
       ?expression_attribute_values  ?consistent_read  () =
      {
        table_name;
        index_name;
        attributes_to_get;
        limit;
        select;
        scan_filter;
        conditional_operator;
        exclusive_start_key;
        return_consumed_capacity;
        total_segments;
        segment;
        projection_expression;
        filter_expression;
        expression_attribute_names;
        expression_attribute_values;
        consistent_read
      }
    let parse xml =
      Some
        {
          table_name =
            (Xml.required "TableName"
               (Util.option_bind (Xml.member "TableName" xml) String.parse));
          index_name =
            (Util.option_bind (Xml.member "IndexName" xml) String.parse);
          attributes_to_get =
            (Util.of_option []
               (Util.option_bind (Xml.member "AttributesToGet" xml)
                  AttributeNameList.parse));
          limit = (Util.option_bind (Xml.member "Limit" xml) Integer.parse);
          select = (Util.option_bind (Xml.member "Select" xml) Select.parse);
          scan_filter =
            (Util.option_bind (Xml.member "ScanFilter" xml)
               FilterConditionMap.parse);
          conditional_operator =
            (Util.option_bind (Xml.member "ConditionalOperator" xml)
               ConditionalOperator.parse);
          exclusive_start_key =
            (Util.option_bind (Xml.member "ExclusiveStartKey" xml) Key.parse);
          return_consumed_capacity =
            (Util.option_bind (Xml.member "ReturnConsumedCapacity" xml)
               ReturnConsumedCapacity.parse);
          total_segments =
            (Util.option_bind (Xml.member "TotalSegments" xml) Integer.parse);
          segment =
            (Util.option_bind (Xml.member "Segment" xml) Integer.parse);
          projection_expression =
            (Util.option_bind (Xml.member "ProjectionExpression" xml)
               String.parse);
          filter_expression =
            (Util.option_bind (Xml.member "FilterExpression" xml)
               String.parse);
          expression_attribute_names =
            (Util.option_bind (Xml.member "ExpressionAttributeNames" xml)
               ExpressionAttributeNameMap.parse);
          expression_attribute_values =
            (Util.option_bind (Xml.member "ExpressionAttributeValues" xml)
               ExpressionAttributeValueMap.parse);
          consistent_read =
            (Util.option_bind (Xml.member "ConsistentRead" xml) Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.consistent_read
              (fun f -> Query.Pair ("ConsistentRead", (Boolean.to_query f)));
           Util.option_map v.expression_attribute_values
             (fun f ->
                Query.Pair
                  ("ExpressionAttributeValues",
                    (ExpressionAttributeValueMap.to_query f)));
           Util.option_map v.expression_attribute_names
             (fun f ->
                Query.Pair
                  ("ExpressionAttributeNames",
                    (ExpressionAttributeNameMap.to_query f)));
           Util.option_map v.filter_expression
             (fun f -> Query.Pair ("FilterExpression", (String.to_query f)));
           Util.option_map v.projection_expression
             (fun f ->
                Query.Pair ("ProjectionExpression", (String.to_query f)));
           Util.option_map v.segment
             (fun f -> Query.Pair ("Segment", (Integer.to_query f)));
           Util.option_map v.total_segments
             (fun f -> Query.Pair ("TotalSegments", (Integer.to_query f)));
           Util.option_map v.return_consumed_capacity
             (fun f ->
                Query.Pair
                  ("ReturnConsumedCapacity",
                    (ReturnConsumedCapacity.to_query f)));
           Util.option_map v.exclusive_start_key
             (fun f -> Query.Pair ("ExclusiveStartKey", (Key.to_query f)));
           Util.option_map v.conditional_operator
             (fun f ->
                Query.Pair
                  ("ConditionalOperator", (ConditionalOperator.to_query f)));
           Util.option_map v.scan_filter
             (fun f ->
                Query.Pair ("ScanFilter", (FilterConditionMap.to_query f)));
           Util.option_map v.select
             (fun f -> Query.Pair ("Select", (Select.to_query f)));
           Util.option_map v.limit
             (fun f -> Query.Pair ("Limit", (Integer.to_query f)));
           Some
             (Query.Pair
                ("AttributesToGet.member",
                  (AttributeNameList.to_query v.attributes_to_get)));
           Util.option_map v.index_name
             (fun f -> Query.Pair ("IndexName", (String.to_query f)));
           Some (Query.Pair ("TableName", (String.to_query v.table_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.consistent_read
              (fun f -> ("consistent_read", (Boolean.to_json f)));
           Util.option_map v.expression_attribute_values
             (fun f ->
                ("expression_attribute_values",
                  (ExpressionAttributeValueMap.to_json f)));
           Util.option_map v.expression_attribute_names
             (fun f ->
                ("expression_attribute_names",
                  (ExpressionAttributeNameMap.to_json f)));
           Util.option_map v.filter_expression
             (fun f -> ("filter_expression", (String.to_json f)));
           Util.option_map v.projection_expression
             (fun f -> ("projection_expression", (String.to_json f)));
           Util.option_map v.segment
             (fun f -> ("segment", (Integer.to_json f)));
           Util.option_map v.total_segments
             (fun f -> ("total_segments", (Integer.to_json f)));
           Util.option_map v.return_consumed_capacity
             (fun f ->
                ("return_consumed_capacity",
                  (ReturnConsumedCapacity.to_json f)));
           Util.option_map v.exclusive_start_key
             (fun f -> ("exclusive_start_key", (Key.to_json f)));
           Util.option_map v.conditional_operator
             (fun f ->
                ("conditional_operator", (ConditionalOperator.to_json f)));
           Util.option_map v.scan_filter
             (fun f -> ("scan_filter", (FilterConditionMap.to_json f)));
           Util.option_map v.select (fun f -> ("select", (Select.to_json f)));
           Util.option_map v.limit (fun f -> ("limit", (Integer.to_json f)));
           Some
             ("attributes_to_get",
               (AttributeNameList.to_json v.attributes_to_get));
           Util.option_map v.index_name
             (fun f -> ("index_name", (String.to_json f)));
           Some ("table_name", (String.to_json v.table_name))])
    let of_json j =
      {
        table_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "table_name")));
        index_name =
          (Util.option_map (Json.lookup j "index_name") String.of_json);
        attributes_to_get =
          (AttributeNameList.of_json
             (Util.of_option_exn (Json.lookup j "attributes_to_get")));
        limit = (Util.option_map (Json.lookup j "limit") Integer.of_json);
        select = (Util.option_map (Json.lookup j "select") Select.of_json);
        scan_filter =
          (Util.option_map (Json.lookup j "scan_filter")
             FilterConditionMap.of_json);
        conditional_operator =
          (Util.option_map (Json.lookup j "conditional_operator")
             ConditionalOperator.of_json);
        exclusive_start_key =
          (Util.option_map (Json.lookup j "exclusive_start_key") Key.of_json);
        return_consumed_capacity =
          (Util.option_map (Json.lookup j "return_consumed_capacity")
             ReturnConsumedCapacity.of_json);
        total_segments =
          (Util.option_map (Json.lookup j "total_segments") Integer.of_json);
        segment = (Util.option_map (Json.lookup j "segment") Integer.of_json);
        projection_expression =
          (Util.option_map (Json.lookup j "projection_expression")
             String.of_json);
        filter_expression =
          (Util.option_map (Json.lookup j "filter_expression") String.of_json);
        expression_attribute_names =
          (Util.option_map (Json.lookup j "expression_attribute_names")
             ExpressionAttributeNameMap.of_json);
        expression_attribute_values =
          (Util.option_map (Json.lookup j "expression_attribute_values")
             ExpressionAttributeValueMap.of_json);
        consistent_read =
          (Util.option_map (Json.lookup j "consistent_read") Boolean.of_json)
      }
  end
module DeleteItemInput =
  struct
    type t =
      {
      table_name: String.t ;
      key: Key.t ;
      expected: ExpectedAttributeMap.t option ;
      conditional_operator: ConditionalOperator.t option ;
      return_values: ReturnValue.t option ;
      return_consumed_capacity: ReturnConsumedCapacity.t option ;
      return_item_collection_metrics: ReturnItemCollectionMetrics.t option ;
      condition_expression: String.t option ;
      expression_attribute_names: ExpressionAttributeNameMap.t option ;
      expression_attribute_values: ExpressionAttributeValueMap.t option }
    let make ~table_name  ~key  ?expected  ?conditional_operator 
      ?return_values  ?return_consumed_capacity 
      ?return_item_collection_metrics  ?condition_expression 
      ?expression_attribute_names  ?expression_attribute_values  () =
      {
        table_name;
        key;
        expected;
        conditional_operator;
        return_values;
        return_consumed_capacity;
        return_item_collection_metrics;
        condition_expression;
        expression_attribute_names;
        expression_attribute_values
      }
    let parse xml =
      Some
        {
          table_name =
            (Xml.required "TableName"
               (Util.option_bind (Xml.member "TableName" xml) String.parse));
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) Key.parse));
          expected =
            (Util.option_bind (Xml.member "Expected" xml)
               ExpectedAttributeMap.parse);
          conditional_operator =
            (Util.option_bind (Xml.member "ConditionalOperator" xml)
               ConditionalOperator.parse);
          return_values =
            (Util.option_bind (Xml.member "ReturnValues" xml)
               ReturnValue.parse);
          return_consumed_capacity =
            (Util.option_bind (Xml.member "ReturnConsumedCapacity" xml)
               ReturnConsumedCapacity.parse);
          return_item_collection_metrics =
            (Util.option_bind (Xml.member "ReturnItemCollectionMetrics" xml)
               ReturnItemCollectionMetrics.parse);
          condition_expression =
            (Util.option_bind (Xml.member "ConditionExpression" xml)
               String.parse);
          expression_attribute_names =
            (Util.option_bind (Xml.member "ExpressionAttributeNames" xml)
               ExpressionAttributeNameMap.parse);
          expression_attribute_values =
            (Util.option_bind (Xml.member "ExpressionAttributeValues" xml)
               ExpressionAttributeValueMap.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.expression_attribute_values
              (fun f ->
                 Query.Pair
                   ("ExpressionAttributeValues",
                     (ExpressionAttributeValueMap.to_query f)));
           Util.option_map v.expression_attribute_names
             (fun f ->
                Query.Pair
                  ("ExpressionAttributeNames",
                    (ExpressionAttributeNameMap.to_query f)));
           Util.option_map v.condition_expression
             (fun f ->
                Query.Pair ("ConditionExpression", (String.to_query f)));
           Util.option_map v.return_item_collection_metrics
             (fun f ->
                Query.Pair
                  ("ReturnItemCollectionMetrics",
                    (ReturnItemCollectionMetrics.to_query f)));
           Util.option_map v.return_consumed_capacity
             (fun f ->
                Query.Pair
                  ("ReturnConsumedCapacity",
                    (ReturnConsumedCapacity.to_query f)));
           Util.option_map v.return_values
             (fun f -> Query.Pair ("ReturnValues", (ReturnValue.to_query f)));
           Util.option_map v.conditional_operator
             (fun f ->
                Query.Pair
                  ("ConditionalOperator", (ConditionalOperator.to_query f)));
           Util.option_map v.expected
             (fun f ->
                Query.Pair ("Expected", (ExpectedAttributeMap.to_query f)));
           Some (Query.Pair ("Key", (Key.to_query v.key)));
           Some (Query.Pair ("TableName", (String.to_query v.table_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.expression_attribute_values
              (fun f ->
                 ("expression_attribute_values",
                   (ExpressionAttributeValueMap.to_json f)));
           Util.option_map v.expression_attribute_names
             (fun f ->
                ("expression_attribute_names",
                  (ExpressionAttributeNameMap.to_json f)));
           Util.option_map v.condition_expression
             (fun f -> ("condition_expression", (String.to_json f)));
           Util.option_map v.return_item_collection_metrics
             (fun f ->
                ("return_item_collection_metrics",
                  (ReturnItemCollectionMetrics.to_json f)));
           Util.option_map v.return_consumed_capacity
             (fun f ->
                ("return_consumed_capacity",
                  (ReturnConsumedCapacity.to_json f)));
           Util.option_map v.return_values
             (fun f -> ("return_values", (ReturnValue.to_json f)));
           Util.option_map v.conditional_operator
             (fun f ->
                ("conditional_operator", (ConditionalOperator.to_json f)));
           Util.option_map v.expected
             (fun f -> ("expected", (ExpectedAttributeMap.to_json f)));
           Some ("key", (Key.to_json v.key));
           Some ("table_name", (String.to_json v.table_name))])
    let of_json j =
      {
        table_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "table_name")));
        key = (Key.of_json (Util.of_option_exn (Json.lookup j "key")));
        expected =
          (Util.option_map (Json.lookup j "expected")
             ExpectedAttributeMap.of_json);
        conditional_operator =
          (Util.option_map (Json.lookup j "conditional_operator")
             ConditionalOperator.of_json);
        return_values =
          (Util.option_map (Json.lookup j "return_values")
             ReturnValue.of_json);
        return_consumed_capacity =
          (Util.option_map (Json.lookup j "return_consumed_capacity")
             ReturnConsumedCapacity.of_json);
        return_item_collection_metrics =
          (Util.option_map (Json.lookup j "return_item_collection_metrics")
             ReturnItemCollectionMetrics.of_json);
        condition_expression =
          (Util.option_map (Json.lookup j "condition_expression")
             String.of_json);
        expression_attribute_names =
          (Util.option_map (Json.lookup j "expression_attribute_names")
             ExpressionAttributeNameMap.of_json);
        expression_attribute_values =
          (Util.option_map (Json.lookup j "expression_attribute_values")
             ExpressionAttributeValueMap.of_json)
      }
  end
module LimitExceededException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ResourceNotFoundException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module PutItemInput =
  struct
    type t =
      {
      table_name: String.t ;
      item: PutItemInputAttributeMap.t ;
      expected: ExpectedAttributeMap.t option ;
      return_values: ReturnValue.t option ;
      return_consumed_capacity: ReturnConsumedCapacity.t option ;
      return_item_collection_metrics: ReturnItemCollectionMetrics.t option ;
      conditional_operator: ConditionalOperator.t option ;
      condition_expression: String.t option ;
      expression_attribute_names: ExpressionAttributeNameMap.t option ;
      expression_attribute_values: ExpressionAttributeValueMap.t option }
    let make ~table_name  ~item  ?expected  ?return_values 
      ?return_consumed_capacity  ?return_item_collection_metrics 
      ?conditional_operator  ?condition_expression 
      ?expression_attribute_names  ?expression_attribute_values  () =
      {
        table_name;
        item;
        expected;
        return_values;
        return_consumed_capacity;
        return_item_collection_metrics;
        conditional_operator;
        condition_expression;
        expression_attribute_names;
        expression_attribute_values
      }
    let parse xml =
      Some
        {
          table_name =
            (Xml.required "TableName"
               (Util.option_bind (Xml.member "TableName" xml) String.parse));
          item =
            (Xml.required "Item"
               (Util.option_bind (Xml.member "Item" xml)
                  PutItemInputAttributeMap.parse));
          expected =
            (Util.option_bind (Xml.member "Expected" xml)
               ExpectedAttributeMap.parse);
          return_values =
            (Util.option_bind (Xml.member "ReturnValues" xml)
               ReturnValue.parse);
          return_consumed_capacity =
            (Util.option_bind (Xml.member "ReturnConsumedCapacity" xml)
               ReturnConsumedCapacity.parse);
          return_item_collection_metrics =
            (Util.option_bind (Xml.member "ReturnItemCollectionMetrics" xml)
               ReturnItemCollectionMetrics.parse);
          conditional_operator =
            (Util.option_bind (Xml.member "ConditionalOperator" xml)
               ConditionalOperator.parse);
          condition_expression =
            (Util.option_bind (Xml.member "ConditionExpression" xml)
               String.parse);
          expression_attribute_names =
            (Util.option_bind (Xml.member "ExpressionAttributeNames" xml)
               ExpressionAttributeNameMap.parse);
          expression_attribute_values =
            (Util.option_bind (Xml.member "ExpressionAttributeValues" xml)
               ExpressionAttributeValueMap.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.expression_attribute_values
              (fun f ->
                 Query.Pair
                   ("ExpressionAttributeValues",
                     (ExpressionAttributeValueMap.to_query f)));
           Util.option_map v.expression_attribute_names
             (fun f ->
                Query.Pair
                  ("ExpressionAttributeNames",
                    (ExpressionAttributeNameMap.to_query f)));
           Util.option_map v.condition_expression
             (fun f ->
                Query.Pair ("ConditionExpression", (String.to_query f)));
           Util.option_map v.conditional_operator
             (fun f ->
                Query.Pair
                  ("ConditionalOperator", (ConditionalOperator.to_query f)));
           Util.option_map v.return_item_collection_metrics
             (fun f ->
                Query.Pair
                  ("ReturnItemCollectionMetrics",
                    (ReturnItemCollectionMetrics.to_query f)));
           Util.option_map v.return_consumed_capacity
             (fun f ->
                Query.Pair
                  ("ReturnConsumedCapacity",
                    (ReturnConsumedCapacity.to_query f)));
           Util.option_map v.return_values
             (fun f -> Query.Pair ("ReturnValues", (ReturnValue.to_query f)));
           Util.option_map v.expected
             (fun f ->
                Query.Pair ("Expected", (ExpectedAttributeMap.to_query f)));
           Some
             (Query.Pair ("Item", (PutItemInputAttributeMap.to_query v.item)));
           Some (Query.Pair ("TableName", (String.to_query v.table_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.expression_attribute_values
              (fun f ->
                 ("expression_attribute_values",
                   (ExpressionAttributeValueMap.to_json f)));
           Util.option_map v.expression_attribute_names
             (fun f ->
                ("expression_attribute_names",
                  (ExpressionAttributeNameMap.to_json f)));
           Util.option_map v.condition_expression
             (fun f -> ("condition_expression", (String.to_json f)));
           Util.option_map v.conditional_operator
             (fun f ->
                ("conditional_operator", (ConditionalOperator.to_json f)));
           Util.option_map v.return_item_collection_metrics
             (fun f ->
                ("return_item_collection_metrics",
                  (ReturnItemCollectionMetrics.to_json f)));
           Util.option_map v.return_consumed_capacity
             (fun f ->
                ("return_consumed_capacity",
                  (ReturnConsumedCapacity.to_json f)));
           Util.option_map v.return_values
             (fun f -> ("return_values", (ReturnValue.to_json f)));
           Util.option_map v.expected
             (fun f -> ("expected", (ExpectedAttributeMap.to_json f)));
           Some ("item", (PutItemInputAttributeMap.to_json v.item));
           Some ("table_name", (String.to_json v.table_name))])
    let of_json j =
      {
        table_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "table_name")));
        item =
          (PutItemInputAttributeMap.of_json
             (Util.of_option_exn (Json.lookup j "item")));
        expected =
          (Util.option_map (Json.lookup j "expected")
             ExpectedAttributeMap.of_json);
        return_values =
          (Util.option_map (Json.lookup j "return_values")
             ReturnValue.of_json);
        return_consumed_capacity =
          (Util.option_map (Json.lookup j "return_consumed_capacity")
             ReturnConsumedCapacity.of_json);
        return_item_collection_metrics =
          (Util.option_map (Json.lookup j "return_item_collection_metrics")
             ReturnItemCollectionMetrics.of_json);
        conditional_operator =
          (Util.option_map (Json.lookup j "conditional_operator")
             ConditionalOperator.of_json);
        condition_expression =
          (Util.option_map (Json.lookup j "condition_expression")
             String.of_json);
        expression_attribute_names =
          (Util.option_map (Json.lookup j "expression_attribute_names")
             ExpressionAttributeNameMap.of_json);
        expression_attribute_values =
          (Util.option_map (Json.lookup j "expression_attribute_values")
             ExpressionAttributeValueMap.of_json)
      }
  end
module BatchGetItemOutput =
  struct
    type t =
      {
      responses: BatchGetResponseMap.t option ;
      unprocessed_keys: BatchGetRequestMap.t option ;
      consumed_capacity: ConsumedCapacityMultiple.t }
    let make ?responses  ?unprocessed_keys  ?(consumed_capacity= [])  () =
      { responses; unprocessed_keys; consumed_capacity }
    let parse xml =
      Some
        {
          responses =
            (Util.option_bind (Xml.member "Responses" xml)
               BatchGetResponseMap.parse);
          unprocessed_keys =
            (Util.option_bind (Xml.member "UnprocessedKeys" xml)
               BatchGetRequestMap.parse);
          consumed_capacity =
            (Util.of_option []
               (Util.option_bind (Xml.member "ConsumedCapacity" xml)
                  ConsumedCapacityMultiple.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ConsumedCapacity.member",
                   (ConsumedCapacityMultiple.to_query v.consumed_capacity)));
           Util.option_map v.unprocessed_keys
             (fun f ->
                Query.Pair
                  ("UnprocessedKeys", (BatchGetRequestMap.to_query f)));
           Util.option_map v.responses
             (fun f ->
                Query.Pair ("Responses", (BatchGetResponseMap.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("consumed_capacity",
                (ConsumedCapacityMultiple.to_json v.consumed_capacity));
           Util.option_map v.unprocessed_keys
             (fun f -> ("unprocessed_keys", (BatchGetRequestMap.to_json f)));
           Util.option_map v.responses
             (fun f -> ("responses", (BatchGetResponseMap.to_json f)))])
    let of_json j =
      {
        responses =
          (Util.option_map (Json.lookup j "responses")
             BatchGetResponseMap.of_json);
        unprocessed_keys =
          (Util.option_map (Json.lookup j "unprocessed_keys")
             BatchGetRequestMap.of_json);
        consumed_capacity =
          (ConsumedCapacityMultiple.of_json
             (Util.of_option_exn (Json.lookup j "consumed_capacity")))
      }
  end
module ItemCollectionSizeLimitExceededException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module DescribeTableOutput =
  struct
    type t = {
      table: TableDescription.t option }
    let make ?table  () = { table }
    let parse xml =
      Some
        {
          table =
            (Util.option_bind (Xml.member "Table" xml) TableDescription.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.table
              (fun f -> Query.Pair ("Table", (TableDescription.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.table
              (fun f -> ("table", (TableDescription.to_json f)))])
    let of_json j =
      {
        table =
          (Util.option_map (Json.lookup j "table") TableDescription.of_json)
      }
  end
module ProvisionedThroughputExceededException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module UpdateTableOutput =
  struct
    type t = {
      table_description: TableDescription.t option }
    let make ?table_description  () = { table_description }
    let parse xml =
      Some
        {
          table_description =
            (Util.option_bind (Xml.member "TableDescription" xml)
               TableDescription.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.table_description
              (fun f ->
                 Query.Pair
                   ("TableDescription", (TableDescription.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.table_description
              (fun f -> ("table_description", (TableDescription.to_json f)))])
    let of_json j =
      {
        table_description =
          (Util.option_map (Json.lookup j "table_description")
             TableDescription.of_json)
      }
  end
module UpdateTableInput =
  struct
    type t =
      {
      attribute_definitions: AttributeDefinitions.t ;
      table_name: String.t ;
      provisioned_throughput: ProvisionedThroughput.t option ;
      global_secondary_index_updates: GlobalSecondaryIndexUpdateList.t ;
      stream_specification: StreamSpecification.t option }
    let make ?(attribute_definitions= [])  ~table_name 
      ?provisioned_throughput  ?(global_secondary_index_updates= []) 
      ?stream_specification  () =
      {
        attribute_definitions;
        table_name;
        provisioned_throughput;
        global_secondary_index_updates;
        stream_specification
      }
    let parse xml =
      Some
        {
          attribute_definitions =
            (Util.of_option []
               (Util.option_bind (Xml.member "AttributeDefinitions" xml)
                  AttributeDefinitions.parse));
          table_name =
            (Xml.required "TableName"
               (Util.option_bind (Xml.member "TableName" xml) String.parse));
          provisioned_throughput =
            (Util.option_bind (Xml.member "ProvisionedThroughput" xml)
               ProvisionedThroughput.parse);
          global_secondary_index_updates =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "GlobalSecondaryIndexUpdates" xml)
                  GlobalSecondaryIndexUpdateList.parse));
          stream_specification =
            (Util.option_bind (Xml.member "StreamSpecification" xml)
               StreamSpecification.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.stream_specification
              (fun f ->
                 Query.Pair
                   ("StreamSpecification", (StreamSpecification.to_query f)));
           Some
             (Query.Pair
                ("GlobalSecondaryIndexUpdates.member",
                  (GlobalSecondaryIndexUpdateList.to_query
                     v.global_secondary_index_updates)));
           Util.option_map v.provisioned_throughput
             (fun f ->
                Query.Pair
                  ("ProvisionedThroughput",
                    (ProvisionedThroughput.to_query f)));
           Some (Query.Pair ("TableName", (String.to_query v.table_name)));
           Some
             (Query.Pair
                ("AttributeDefinitions.member",
                  (AttributeDefinitions.to_query v.attribute_definitions)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stream_specification
              (fun f ->
                 ("stream_specification", (StreamSpecification.to_json f)));
           Some
             ("global_secondary_index_updates",
               (GlobalSecondaryIndexUpdateList.to_json
                  v.global_secondary_index_updates));
           Util.option_map v.provisioned_throughput
             (fun f ->
                ("provisioned_throughput", (ProvisionedThroughput.to_json f)));
           Some ("table_name", (String.to_json v.table_name));
           Some
             ("attribute_definitions",
               (AttributeDefinitions.to_json v.attribute_definitions))])
    let of_json j =
      {
        attribute_definitions =
          (AttributeDefinitions.of_json
             (Util.of_option_exn (Json.lookup j "attribute_definitions")));
        table_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "table_name")));
        provisioned_throughput =
          (Util.option_map (Json.lookup j "provisioned_throughput")
             ProvisionedThroughput.of_json);
        global_secondary_index_updates =
          (GlobalSecondaryIndexUpdateList.of_json
             (Util.of_option_exn
                (Json.lookup j "global_secondary_index_updates")));
        stream_specification =
          (Util.option_map (Json.lookup j "stream_specification")
             StreamSpecification.of_json)
      }
  end