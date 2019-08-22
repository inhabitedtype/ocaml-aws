open Aws
open Aws.BaseTypes
open CalendarLib
type calendar = Calendar.t
module AssociationStatusName =
  struct
    type t =
      | Pending 
      | Success 
      | Failed 
    let str_to_t =
      [("Failed", Failed); ("Success", Success); ("Pending", Pending)]
    let t_to_str =
      [(Failed, "Failed"); (Success, "Success"); (Pending, "Pending")]
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
module AssociationFilterKey =
  struct
    type t =
      | InstanceId 
      | Name 
    let str_to_t = [("Name", Name); ("InstanceId", InstanceId)]
    let t_to_str = [(Name, "Name"); (InstanceId, "InstanceId")]
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
module DocumentFilterKey =
  struct
    type t =
      | Name 
    let str_to_t = [("Name", Name)]
    let t_to_str = [(Name, "Name")]
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
module AssociationStatus =
  struct
    type t =
      {
      date: DateTime.t ;
      name: AssociationStatusName.t ;
      message: String.t ;
      additional_info: String.t option }
    let make ~date  ~name  ~message  ?additional_info  () =
      { date; name; message; additional_info }
    let parse xml =
      Some
        {
          date =
            (Xml.required "Date"
               (Util.option_bind (Xml.member "Date" xml) DateTime.parse));
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml)
                  AssociationStatusName.parse));
          message =
            (Xml.required "Message"
               (Util.option_bind (Xml.member "Message" xml) String.parse));
          additional_info =
            (Util.option_bind (Xml.member "AdditionalInfo" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.additional_info
              (fun f -> Query.Pair ("AdditionalInfo", (String.to_query f)));
           Some (Query.Pair ("Message", (String.to_query v.message)));
           Some
             (Query.Pair ("Name", (AssociationStatusName.to_query v.name)));
           Some (Query.Pair ("Date", (DateTime.to_query v.date)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.additional_info
              (fun f -> ("additional_info", (String.to_json f)));
           Some ("message", (String.to_json v.message));
           Some ("name", (AssociationStatusName.to_json v.name));
           Some ("date", (DateTime.to_json v.date))])
    let of_json j =
      {
        date = (DateTime.of_json (Util.of_option_exn (Json.lookup j "date")));
        name =
          (AssociationStatusName.of_json
             (Util.of_option_exn (Json.lookup j "name")));
        message =
          (String.of_json (Util.of_option_exn (Json.lookup j "message")));
        additional_info =
          (Util.option_map (Json.lookup j "additional_info") String.of_json)
      }
  end
module CreateAssociationBatchRequestEntry =
  struct
    type t = {
      name: String.t option ;
      instance_id: String.t option }
    let make ?name  ?instance_id  () = { name; instance_id }
    let parse xml =
      Some
        {
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          instance_id =
            (Util.option_bind (Xml.member "InstanceId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.instance_id
              (fun f -> Query.Pair ("InstanceId", (String.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.instance_id
              (fun f -> ("instance_id", (String.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)))])
    let of_json j =
      {
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        instance_id =
          (Util.option_map (Json.lookup j "instance_id") String.of_json)
      }
  end
module Fault =
  struct
    type t =
      | Client 
      | Server 
      | Unknown 
    let str_to_t =
      [("Unknown", Unknown); ("Server", Server); ("Client", Client)]
    let t_to_str =
      [(Unknown, "Unknown"); (Server, "Server"); (Client, "Client")]
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
module AssociationFilter =
  struct
    type t = {
      key: AssociationFilterKey.t ;
      value: String.t }
    let make ~key  ~value  () = { key; value }
    let parse xml =
      Some
        {
          key =
            (Xml.required "key"
               (Util.option_bind (Xml.member "key" xml)
                  AssociationFilterKey.parse));
          value =
            (Xml.required "value"
               (Util.option_bind (Xml.member "value" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("value", (String.to_query v.value)));
           Some (Query.Pair ("key", (AssociationFilterKey.to_query v.key)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("value", (String.to_json v.value));
           Some ("key", (AssociationFilterKey.to_json v.key))])
    let of_json j =
      {
        key =
          (AssociationFilterKey.of_json
             (Util.of_option_exn (Json.lookup j "key")));
        value = (String.of_json (Util.of_option_exn (Json.lookup j "value")))
      }
  end
module Association =
  struct
    type t = {
      name: String.t option ;
      instance_id: String.t option }
    let make ?name  ?instance_id  () = { name; instance_id }
    let parse xml =
      Some
        {
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          instance_id =
            (Util.option_bind (Xml.member "InstanceId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.instance_id
              (fun f -> Query.Pair ("InstanceId", (String.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.instance_id
              (fun f -> ("instance_id", (String.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)))])
    let of_json j =
      {
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        instance_id =
          (Util.option_map (Json.lookup j "instance_id") String.of_json)
      }
  end
module DocumentIdentifier =
  struct
    type t = {
      name: String.t option }
    let make ?name  () = { name }
    let parse xml =
      Some { name = (Util.option_bind (Xml.member "Name" xml) String.parse) }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.name
              (fun f -> Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.name (fun f -> ("name", (String.to_json f)))])
    let of_json j =
      { name = (Util.option_map (Json.lookup j "name") String.of_json) }
  end
module DocumentFilter =
  struct
    type t = {
      key: DocumentFilterKey.t ;
      value: String.t }
    let make ~key  ~value  () = { key; value }
    let parse xml =
      Some
        {
          key =
            (Xml.required "key"
               (Util.option_bind (Xml.member "key" xml)
                  DocumentFilterKey.parse));
          value =
            (Xml.required "value"
               (Util.option_bind (Xml.member "value" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("value", (String.to_query v.value)));
           Some (Query.Pair ("key", (DocumentFilterKey.to_query v.key)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("value", (String.to_json v.value));
           Some ("key", (DocumentFilterKey.to_json v.key))])
    let of_json j =
      {
        key =
          (DocumentFilterKey.of_json
             (Util.of_option_exn (Json.lookup j "key")));
        value = (String.of_json (Util.of_option_exn (Json.lookup j "value")))
      }
  end
module DocumentStatus =
  struct
    type t =
      | Creating 
      | Active 
      | Deleting 
    let str_to_t =
      [("Deleting", Deleting); ("Active", Active); ("Creating", Creating)]
    let t_to_str =
      [(Deleting, "Deleting"); (Active, "Active"); (Creating, "Creating")]
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
module AssociationDescription =
  struct
    type t =
      {
      name: String.t option ;
      instance_id: String.t option ;
      date: DateTime.t option ;
      status: AssociationStatus.t option }
    let make ?name  ?instance_id  ?date  ?status  () =
      { name; instance_id; date; status }
    let parse xml =
      Some
        {
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          instance_id =
            (Util.option_bind (Xml.member "InstanceId" xml) String.parse);
          date = (Util.option_bind (Xml.member "Date" xml) DateTime.parse);
          status =
            (Util.option_bind (Xml.member "Status" xml)
               AssociationStatus.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> Query.Pair ("Status", (AssociationStatus.to_query f)));
           Util.option_map v.date
             (fun f -> Query.Pair ("Date", (DateTime.to_query f)));
           Util.option_map v.instance_id
             (fun f -> Query.Pair ("InstanceId", (String.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> ("status", (AssociationStatus.to_json f)));
           Util.option_map v.date (fun f -> ("date", (DateTime.to_json f)));
           Util.option_map v.instance_id
             (fun f -> ("instance_id", (String.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)))])
    let of_json j =
      {
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        instance_id =
          (Util.option_map (Json.lookup j "instance_id") String.of_json);
        date = (Util.option_map (Json.lookup j "date") DateTime.of_json);
        status =
          (Util.option_map (Json.lookup j "status") AssociationStatus.of_json)
      }
  end
module FailedCreateAssociation =
  struct
    type t =
      {
      entry: CreateAssociationBatchRequestEntry.t option ;
      message: String.t option ;
      fault: Fault.t option }
    let make ?entry  ?message  ?fault  () = { entry; message; fault }
    let parse xml =
      Some
        {
          entry =
            (Util.option_bind (Xml.member "Entry" xml)
               CreateAssociationBatchRequestEntry.parse);
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse);
          fault = (Util.option_bind (Xml.member "Fault" xml) Fault.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.fault
              (fun f -> Query.Pair ("Fault", (Fault.to_query f)));
           Util.option_map v.message
             (fun f -> Query.Pair ("Message", (String.to_query f)));
           Util.option_map v.entry
             (fun f ->
                Query.Pair
                  ("Entry", (CreateAssociationBatchRequestEntry.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.fault (fun f -> ("fault", (Fault.to_json f)));
           Util.option_map v.message
             (fun f -> ("message", (String.to_json f)));
           Util.option_map v.entry
             (fun f ->
                ("entry", (CreateAssociationBatchRequestEntry.to_json f)))])
    let of_json j =
      {
        entry =
          (Util.option_map (Json.lookup j "entry")
             CreateAssociationBatchRequestEntry.of_json);
        message = (Util.option_map (Json.lookup j "message") String.of_json);
        fault = (Util.option_map (Json.lookup j "fault") Fault.of_json)
      }
  end
module AssociationFilterList =
  struct
    type t = AssociationFilter.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map AssociationFilter.parse
           (Xml.members "AssociationFilter" xml))
    let to_query v = Query.to_query_list AssociationFilter.to_query v
    let to_json v = `List (List.map AssociationFilter.to_json v)
    let of_json j = Json.to_list AssociationFilter.of_json j
  end
module AssociationList =
  struct
    type t = Association.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map Association.parse (Xml.members "Association" xml))
    let to_query v = Query.to_query_list Association.to_query v
    let to_json v = `List (List.map Association.to_json v)
    let of_json j = Json.to_list Association.of_json j
  end
module DocumentIdentifierList =
  struct
    type t = DocumentIdentifier.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DocumentIdentifier.parse
           (Xml.members "DocumentIdentifier" xml))
    let to_query v = Query.to_query_list DocumentIdentifier.to_query v
    let to_json v = `List (List.map DocumentIdentifier.to_json v)
    let of_json j = Json.to_list DocumentIdentifier.of_json j
  end
module DocumentFilterList =
  struct
    type t = DocumentFilter.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DocumentFilter.parse (Xml.members "DocumentFilter" xml))
    let to_query v = Query.to_query_list DocumentFilter.to_query v
    let to_json v = `List (List.map DocumentFilter.to_json v)
    let of_json j = Json.to_list DocumentFilter.of_json j
  end
module DocumentDescription =
  struct
    type t =
      {
      sha1: String.t option ;
      name: String.t option ;
      created_date: DateTime.t option ;
      status: DocumentStatus.t option }
    let make ?sha1  ?name  ?created_date  ?status  () =
      { sha1; name; created_date; status }
    let parse xml =
      Some
        {
          sha1 = (Util.option_bind (Xml.member "Sha1" xml) String.parse);
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          created_date =
            (Util.option_bind (Xml.member "CreatedDate" xml) DateTime.parse);
          status =
            (Util.option_bind (Xml.member "Status" xml) DocumentStatus.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> Query.Pair ("Status", (DocumentStatus.to_query f)));
           Util.option_map v.created_date
             (fun f -> Query.Pair ("CreatedDate", (DateTime.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)));
           Util.option_map v.sha1
             (fun f -> Query.Pair ("Sha1", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> ("status", (DocumentStatus.to_json f)));
           Util.option_map v.created_date
             (fun f -> ("created_date", (DateTime.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)));
           Util.option_map v.sha1 (fun f -> ("sha1", (String.to_json f)))])
    let of_json j =
      {
        sha1 = (Util.option_map (Json.lookup j "sha1") String.of_json);
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        created_date =
          (Util.option_map (Json.lookup j "created_date") DateTime.of_json);
        status =
          (Util.option_map (Json.lookup j "status") DocumentStatus.of_json)
      }
  end
module AssociationDescriptionList =
  struct
    type t = AssociationDescription.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map AssociationDescription.parse
           (Xml.members "AssociationDescription" xml))
    let to_query v = Query.to_query_list AssociationDescription.to_query v
    let to_json v = `List (List.map AssociationDescription.to_json v)
    let of_json j = Json.to_list AssociationDescription.of_json j
  end
module FailedCreateAssociationList =
  struct
    type t = FailedCreateAssociation.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map FailedCreateAssociation.parse
           (Xml.members "FailedCreateAssociationEntry" xml))
    let to_query v = Query.to_query_list FailedCreateAssociation.to_query v
    let to_json v = `List (List.map FailedCreateAssociation.to_json v)
    let of_json j = Json.to_list FailedCreateAssociation.of_json j
  end
module CreateAssociationBatchRequestEntries =
  struct
    type t = CreateAssociationBatchRequestEntry.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map CreateAssociationBatchRequestEntry.parse
           (Xml.members "entries" xml))
    let to_query v =
      Query.to_query_list CreateAssociationBatchRequestEntry.to_query v
    let to_json v =
      `List (List.map CreateAssociationBatchRequestEntry.to_json v)
    let of_json j = Json.to_list CreateAssociationBatchRequestEntry.of_json j
  end
module CreateDocumentRequest =
  struct
    type t = {
      content: String.t ;
      name: String.t }
    let make ~content  ~name  () = { content; name }
    let parse xml =
      Some
        {
          content =
            (Xml.required "Content"
               (Util.option_bind (Xml.member "Content" xml) String.parse));
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Name", (String.to_query v.name)));
           Some (Query.Pair ("Content", (String.to_query v.content)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("name", (String.to_json v.name));
           Some ("content", (String.to_json v.content))])
    let of_json j =
      {
        content =
          (String.of_json (Util.of_option_exn (Json.lookup j "content")));
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")))
      }
  end
module ListAssociationsRequest =
  struct
    type t =
      {
      association_filter_list: AssociationFilterList.t ;
      max_results: Integer.t option ;
      next_token: String.t option }
    let make ~association_filter_list  ?max_results  ?next_token  () =
      { association_filter_list; max_results; next_token }
    let parse xml =
      Some
        {
          association_filter_list =
            (Xml.required "AssociationFilterList"
               (Util.option_bind (Xml.member "AssociationFilterList" xml)
                  AssociationFilterList.parse));
          max_results =
            (Util.option_bind (Xml.member "MaxResults" xml) Integer.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.max_results
             (fun f -> Query.Pair ("MaxResults", (Integer.to_query f)));
           Some
             (Query.Pair
                ("AssociationFilterList.member",
                  (AssociationFilterList.to_query v.association_filter_list)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.max_results
             (fun f -> ("max_results", (Integer.to_json f)));
           Some
             ("association_filter_list",
               (AssociationFilterList.to_json v.association_filter_list))])
    let of_json j =
      {
        association_filter_list =
          (AssociationFilterList.of_json
             (Util.of_option_exn (Json.lookup j "association_filter_list")));
        max_results =
          (Util.option_map (Json.lookup j "max_results") Integer.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module MaxDocumentSizeExceeded =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module UpdateAssociationStatusResult =
  struct
    type t = {
      association_description: AssociationDescription.t option }
    let make ?association_description  () = { association_description }
    let parse xml =
      Some
        {
          association_description =
            (Util.option_bind (Xml.member "AssociationDescription" xml)
               AssociationDescription.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.association_description
              (fun f ->
                 Query.Pair
                   ("AssociationDescription",
                     (AssociationDescription.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.association_description
              (fun f ->
                 ("association_description",
                   (AssociationDescription.to_json f)))])
    let of_json j =
      {
        association_description =
          (Util.option_map (Json.lookup j "association_description")
             AssociationDescription.of_json)
      }
  end
module InvalidInstanceId =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ListAssociationsResult =
  struct
    type t = {
      associations: AssociationList.t ;
      next_token: String.t option }
    let make ?(associations= [])  ?next_token  () =
      { associations; next_token }
    let parse xml =
      Some
        {
          associations =
            (Util.of_option []
               (Util.option_bind (Xml.member "Associations" xml)
                  AssociationList.parse));
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
                ("Associations.member",
                  (AssociationList.to_query v.associations)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some ("associations", (AssociationList.to_json v.associations))])
    let of_json j =
      {
        associations =
          (AssociationList.of_json
             (Util.of_option_exn (Json.lookup j "associations")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module AssociatedInstances =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DeleteDocumentRequest =
  struct
    type t = {
      name: String.t }
    let make ~name  () = { name }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("name", (String.to_json v.name))])
    let of_json j =
      { name = (String.of_json (Util.of_option_exn (Json.lookup j "name"))) }
  end
module DocumentAlreadyExists =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeAssociationResult =
  struct
    type t = {
      association_description: AssociationDescription.t option }
    let make ?association_description  () = { association_description }
    let parse xml =
      Some
        {
          association_description =
            (Util.option_bind (Xml.member "AssociationDescription" xml)
               AssociationDescription.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.association_description
              (fun f ->
                 Query.Pair
                   ("AssociationDescription",
                     (AssociationDescription.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.association_description
              (fun f ->
                 ("association_description",
                   (AssociationDescription.to_json f)))])
    let of_json j =
      {
        association_description =
          (Util.option_map (Json.lookup j "association_description")
             AssociationDescription.of_json)
      }
  end
module ListDocumentsResult =
  struct
    type t =
      {
      document_identifiers: DocumentIdentifierList.t ;
      next_token: String.t option }
    let make ?(document_identifiers= [])  ?next_token  () =
      { document_identifiers; next_token }
    let parse xml =
      Some
        {
          document_identifiers =
            (Util.of_option []
               (Util.option_bind (Xml.member "DocumentIdentifiers" xml)
                  DocumentIdentifierList.parse));
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
                ("DocumentIdentifiers.member",
                  (DocumentIdentifierList.to_query v.document_identifiers)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some
             ("document_identifiers",
               (DocumentIdentifierList.to_json v.document_identifiers))])
    let of_json j =
      {
        document_identifiers =
          (DocumentIdentifierList.of_json
             (Util.of_option_exn (Json.lookup j "document_identifiers")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module DescribeAssociationRequest =
  struct
    type t = {
      name: String.t ;
      instance_id: String.t }
    let make ~name  ~instance_id  () = { name; instance_id }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          instance_id =
            (Xml.required "InstanceId"
               (Util.option_bind (Xml.member "InstanceId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("InstanceId", (String.to_query v.instance_id)));
           Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("instance_id", (String.to_json v.instance_id));
           Some ("name", (String.to_json v.name))])
    let of_json j =
      {
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        instance_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "instance_id")))
      }
  end
module AssociationDoesNotExist =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeDocumentRequest =
  struct
    type t = {
      name: String.t }
    let make ~name  () = { name }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("name", (String.to_json v.name))])
    let of_json j =
      { name = (String.of_json (Util.of_option_exn (Json.lookup j "name"))) }
  end
module CreateAssociationRequest =
  struct
    type t = {
      name: String.t ;
      instance_id: String.t }
    let make ~name  ~instance_id  () = { name; instance_id }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          instance_id =
            (Xml.required "InstanceId"
               (Util.option_bind (Xml.member "InstanceId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("InstanceId", (String.to_query v.instance_id)));
           Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("instance_id", (String.to_json v.instance_id));
           Some ("name", (String.to_json v.name))])
    let of_json j =
      {
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        instance_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "instance_id")))
      }
  end
module DeleteAssociationResult =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ListDocumentsRequest =
  struct
    type t =
      {
      document_filter_list: DocumentFilterList.t ;
      max_results: Integer.t option ;
      next_token: String.t option }
    let make ?(document_filter_list= [])  ?max_results  ?next_token  () =
      { document_filter_list; max_results; next_token }
    let parse xml =
      Some
        {
          document_filter_list =
            (Util.of_option []
               (Util.option_bind (Xml.member "DocumentFilterList" xml)
                  DocumentFilterList.parse));
          max_results =
            (Util.option_bind (Xml.member "MaxResults" xml) Integer.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.max_results
             (fun f -> Query.Pair ("MaxResults", (Integer.to_query f)));
           Some
             (Query.Pair
                ("DocumentFilterList.member",
                  (DocumentFilterList.to_query v.document_filter_list)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.max_results
             (fun f -> ("max_results", (Integer.to_json f)));
           Some
             ("document_filter_list",
               (DocumentFilterList.to_json v.document_filter_list))])
    let of_json j =
      {
        document_filter_list =
          (DocumentFilterList.of_json
             (Util.of_option_exn (Json.lookup j "document_filter_list")));
        max_results =
          (Util.option_map (Json.lookup j "max_results") Integer.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module InternalServerError =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module AssociationLimitExceeded =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeDocumentResult =
  struct
    type t = {
      document: DocumentDescription.t option }
    let make ?document  () = { document }
    let parse xml =
      Some
        {
          document =
            (Util.option_bind (Xml.member "Document" xml)
               DocumentDescription.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.document
              (fun f ->
                 Query.Pair ("Document", (DocumentDescription.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.document
              (fun f -> ("document", (DocumentDescription.to_json f)))])
    let of_json j =
      {
        document =
          (Util.option_map (Json.lookup j "document")
             DocumentDescription.of_json)
      }
  end
module UpdateAssociationStatusRequest =
  struct
    type t =
      {
      name: String.t ;
      instance_id: String.t ;
      association_status: AssociationStatus.t }
    let make ~name  ~instance_id  ~association_status  () =
      { name; instance_id; association_status }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          instance_id =
            (Xml.required "InstanceId"
               (Util.option_bind (Xml.member "InstanceId" xml) String.parse));
          association_status =
            (Xml.required "AssociationStatus"
               (Util.option_bind (Xml.member "AssociationStatus" xml)
                  AssociationStatus.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("AssociationStatus",
                   (AssociationStatus.to_query v.association_status)));
           Some (Query.Pair ("InstanceId", (String.to_query v.instance_id)));
           Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("association_status",
                (AssociationStatus.to_json v.association_status));
           Some ("instance_id", (String.to_json v.instance_id));
           Some ("name", (String.to_json v.name))])
    let of_json j =
      {
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        instance_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "instance_id")));
        association_status =
          (AssociationStatus.of_json
             (Util.of_option_exn (Json.lookup j "association_status")))
      }
  end
module InvalidNextToken =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidDocument =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module AssociationAlreadyExists =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module GetDocumentResult =
  struct
    type t = {
      name: String.t option ;
      content: String.t option }
    let make ?name  ?content  () = { name; content }
    let parse xml =
      Some
        {
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          content =
            (Util.option_bind (Xml.member "Content" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.content
              (fun f -> Query.Pair ("Content", (String.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.content
              (fun f -> ("content", (String.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)))])
    let of_json j =
      {
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        content = (Util.option_map (Json.lookup j "content") String.of_json)
      }
  end
module TooManyUpdates =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CreateAssociationBatchResult =
  struct
    type t =
      {
      successful: AssociationDescriptionList.t ;
      failed: FailedCreateAssociationList.t }
    let make ?(successful= [])  ?(failed= [])  () = { successful; failed }
    let parse xml =
      Some
        {
          successful =
            (Util.of_option []
               (Util.option_bind (Xml.member "Successful" xml)
                  AssociationDescriptionList.parse));
          failed =
            (Util.of_option []
               (Util.option_bind (Xml.member "Failed" xml)
                  FailedCreateAssociationList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Failed.member",
                   (FailedCreateAssociationList.to_query v.failed)));
           Some
             (Query.Pair
                ("Successful.member",
                  (AssociationDescriptionList.to_query v.successful)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("failed", (FailedCreateAssociationList.to_json v.failed));
           Some
             ("successful",
               (AssociationDescriptionList.to_json v.successful))])
    let of_json j =
      {
        successful =
          (AssociationDescriptionList.of_json
             (Util.of_option_exn (Json.lookup j "successful")));
        failed =
          (FailedCreateAssociationList.of_json
             (Util.of_option_exn (Json.lookup j "failed")))
      }
  end
module CreateDocumentResult =
  struct
    type t = {
      document_description: DocumentDescription.t option }
    let make ?document_description  () = { document_description }
    let parse xml =
      Some
        {
          document_description =
            (Util.option_bind (Xml.member "DocumentDescription" xml)
               DocumentDescription.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.document_description
              (fun f ->
                 Query.Pair
                   ("DocumentDescription", (DocumentDescription.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.document_description
              (fun f ->
                 ("document_description", (DocumentDescription.to_json f)))])
    let of_json j =
      {
        document_description =
          (Util.option_map (Json.lookup j "document_description")
             DocumentDescription.of_json)
      }
  end
module DocumentLimitExceeded =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DeleteDocumentResult =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CreateAssociationBatchRequest =
  struct
    type t = {
      entries: CreateAssociationBatchRequestEntries.t }
    let make ~entries  () = { entries }
    let parse xml =
      Some
        {
          entries =
            (Xml.required "Entries"
               (Util.option_bind (Xml.member "Entries" xml)
                  CreateAssociationBatchRequestEntries.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Entries.member",
                   (CreateAssociationBatchRequestEntries.to_query v.entries)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("entries",
                (CreateAssociationBatchRequestEntries.to_json v.entries))])
    let of_json j =
      {
        entries =
          (CreateAssociationBatchRequestEntries.of_json
             (Util.of_option_exn (Json.lookup j "entries")))
      }
  end
module GetDocumentRequest =
  struct
    type t = {
      name: String.t }
    let make ~name  () = { name }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("name", (String.to_json v.name))])
    let of_json j =
      { name = (String.of_json (Util.of_option_exn (Json.lookup j "name"))) }
  end
module StatusUnchanged =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CreateAssociationResult =
  struct
    type t = {
      association_description: AssociationDescription.t option }
    let make ?association_description  () = { association_description }
    let parse xml =
      Some
        {
          association_description =
            (Util.option_bind (Xml.member "AssociationDescription" xml)
               AssociationDescription.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.association_description
              (fun f ->
                 Query.Pair
                   ("AssociationDescription",
                     (AssociationDescription.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.association_description
              (fun f ->
                 ("association_description",
                   (AssociationDescription.to_json f)))])
    let of_json j =
      {
        association_description =
          (Util.option_map (Json.lookup j "association_description")
             AssociationDescription.of_json)
      }
  end
module InvalidDocumentContent =
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
module DeleteAssociationRequest =
  struct
    type t = {
      name: String.t ;
      instance_id: String.t }
    let make ~name  ~instance_id  () = { name; instance_id }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          instance_id =
            (Xml.required "InstanceId"
               (Util.option_bind (Xml.member "InstanceId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("InstanceId", (String.to_query v.instance_id)));
           Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("instance_id", (String.to_json v.instance_id));
           Some ("name", (String.to_json v.name))])
    let of_json j =
      {
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        instance_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "instance_id")))
      }
  end
module DuplicateInstanceId =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end