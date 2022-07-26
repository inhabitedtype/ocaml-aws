open Aws.BaseTypes

type calendar = CalendarLib.Calendar.t

module DuplicateInstanceId = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteAssociationRequest = struct
  type t =
    { name : String.t
    ; instance_id : String.t
    }

  let make ~name ~instance_id () = { name; instance_id }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      ; instance_id =
          Aws.Xml.required
            "InstanceId"
            (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
         ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("InstanceId", String.to_json v.instance_id)
         ; Some ("Name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    ; instance_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
    }
end

module AssociationStatusName = struct
  type t =
    | Pending
    | Success
    | Failed

  let str_to_t = [ "Failed", Failed; "Success", Success; "Pending", Pending ]

  let t_to_str = [ Failed, "Failed"; Success, "Success"; Pending, "Pending" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module AssociationStatus = struct
  type t =
    { date : DateTime.t
    ; name : AssociationStatusName.t
    ; message : String.t
    ; additional_info : String.t option
    }

  let make ~date ~name ~message ?additional_info () =
    { date; name; message; additional_info }

  let parse xml =
    Some
      { date =
          Aws.Xml.required
            "Date"
            (Aws.Util.option_bind (Aws.Xml.member "Date" xml) DateTime.parse)
      ; name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) AssociationStatusName.parse)
      ; message =
          Aws.Xml.required
            "Message"
            (Aws.Util.option_bind (Aws.Xml.member "Message" xml) String.parse)
      ; additional_info =
          Aws.Util.option_bind (Aws.Xml.member "AdditionalInfo" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.additional_info (fun f ->
               Aws.Query.Pair ("AdditionalInfo", String.to_query f))
         ; Some (Aws.Query.Pair ("Message", String.to_query v.message))
         ; Some (Aws.Query.Pair ("Name", AssociationStatusName.to_query v.name))
         ; Some (Aws.Query.Pair ("Date", DateTime.to_query v.date))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.additional_info (fun f ->
               "AdditionalInfo", String.to_json f)
         ; Some ("Message", String.to_json v.message)
         ; Some ("Name", AssociationStatusName.to_json v.name)
         ; Some ("Date", DateTime.to_json v.date)
         ])

  let of_json j =
    { date = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Date"))
    ; name =
        AssociationStatusName.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    ; message = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Message"))
    ; additional_info =
        Aws.Util.option_map (Aws.Json.lookup j "AdditionalInfo") String.of_json
    }
end

module AssociationDescription = struct
  type t =
    { name : String.t option
    ; instance_id : String.t option
    ; date : DateTime.t option
    ; status : AssociationStatus.t option
    }

  let make ?name ?instance_id ?date ?status () = { name; instance_id; date; status }

  let parse xml =
    Some
      { name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
      ; instance_id = Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse
      ; date = Aws.Util.option_bind (Aws.Xml.member "Date" xml) DateTime.parse
      ; status =
          Aws.Util.option_bind (Aws.Xml.member "Status" xml) AssociationStatus.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.status (fun f ->
               Aws.Query.Pair ("Status", AssociationStatus.to_query f))
         ; Aws.Util.option_map v.date (fun f ->
               Aws.Query.Pair ("Date", DateTime.to_query f))
         ; Aws.Util.option_map v.instance_id (fun f ->
               Aws.Query.Pair ("InstanceId", String.to_query f))
         ; Aws.Util.option_map v.name (fun f ->
               Aws.Query.Pair ("Name", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.status (fun f -> "Status", AssociationStatus.to_json f)
         ; Aws.Util.option_map v.date (fun f -> "Date", DateTime.to_json f)
         ; Aws.Util.option_map v.instance_id (fun f -> "InstanceId", String.to_json f)
         ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
         ])

  let of_json j =
    { name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
    ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "InstanceId") String.of_json
    ; date = Aws.Util.option_map (Aws.Json.lookup j "Date") DateTime.of_json
    ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") AssociationStatus.of_json
    }
end

module AssociationDescriptionList = struct
  type t = AssociationDescription.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map
         AssociationDescription.parse
         (Aws.Xml.members "AssociationDescription" xml))

  let to_query v = Aws.Query.to_query_list AssociationDescription.to_query v

  let to_json v = `List (List.map AssociationDescription.to_json v)

  let of_json j = Aws.Json.to_list AssociationDescription.of_json j
end

module InvalidDocumentContent = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module DocumentIdentifier = struct
  type t = { name : String.t option }

  let make ?name () = { name }

  let parse xml =
    Some { name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.name (fun f ->
               Aws.Query.Pair ("Name", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.name (fun f -> "Name", String.to_json f) ])

  let of_json j = { name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json }
end

module DocumentIdentifierList = struct
  type t = DocumentIdentifier.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map DocumentIdentifier.parse (Aws.Xml.members "DocumentIdentifier" xml))

  let to_query v = Aws.Query.to_query_list DocumentIdentifier.to_query v

  let to_json v = `List (List.map DocumentIdentifier.to_json v)

  let of_json j = Aws.Json.to_list DocumentIdentifier.of_json j
end

module CreateAssociationResult = struct
  type t = { association_description : AssociationDescription.t option }

  let make ?association_description () = { association_description }

  let parse xml =
    Some
      { association_description =
          Aws.Util.option_bind
            (Aws.Xml.member "AssociationDescription" xml)
            AssociationDescription.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.association_description (fun f ->
               Aws.Query.Pair ("AssociationDescription", AssociationDescription.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.association_description (fun f ->
               "AssociationDescription", AssociationDescription.to_json f)
         ])

  let of_json j =
    { association_description =
        Aws.Util.option_map
          (Aws.Json.lookup j "AssociationDescription")
          AssociationDescription.of_json
    }
end

module StatusUnchanged = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module GetDocumentRequest = struct
  type t = { name : String.t }

  let make ~name () = { name }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Name", String.to_query v.name)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("Name", String.to_json v.name) ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name")) }
end

module CreateAssociationBatchRequestEntry = struct
  type t =
    { name : String.t option
    ; instance_id : String.t option
    }

  let make ?name ?instance_id () = { name; instance_id }

  let parse xml =
    Some
      { name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
      ; instance_id = Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.instance_id (fun f ->
               Aws.Query.Pair ("InstanceId", String.to_query f))
         ; Aws.Util.option_map v.name (fun f ->
               Aws.Query.Pair ("Name", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.instance_id (fun f -> "InstanceId", String.to_json f)
         ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
         ])

  let of_json j =
    { name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
    ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "InstanceId") String.of_json
    }
end

module CreateAssociationBatchRequestEntries = struct
  type t = CreateAssociationBatchRequestEntry.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map CreateAssociationBatchRequestEntry.parse (Aws.Xml.members "entries" xml))

  let to_query v = Aws.Query.to_query_list CreateAssociationBatchRequestEntry.to_query v

  let to_json v = `List (List.map CreateAssociationBatchRequestEntry.to_json v)

  let of_json j = Aws.Json.to_list CreateAssociationBatchRequestEntry.of_json j
end

module CreateAssociationBatchRequest = struct
  type t = { entries : CreateAssociationBatchRequestEntries.t }

  let make ~entries () = { entries }

  let parse xml =
    Some
      { entries =
          Aws.Xml.required
            "Entries"
            (Aws.Util.option_bind
               (Aws.Xml.member "Entries" xml)
               CreateAssociationBatchRequestEntries.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("Entries.member", CreateAssociationBatchRequestEntries.to_query v.entries))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Entries", CreateAssociationBatchRequestEntries.to_json v.entries) ])

  let of_json j =
    { entries =
        CreateAssociationBatchRequestEntries.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Entries"))
    }
end

module DeleteDocumentResult = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DocumentLimitExceeded = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DocumentStatus = struct
  type t =
    | Creating
    | Active
    | Deleting

  let str_to_t = [ "Deleting", Deleting; "Active", Active; "Creating", Creating ]

  let t_to_str = [ Deleting, "Deleting"; Active, "Active"; Creating, "Creating" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module DocumentDescription = struct
  type t =
    { sha1 : String.t option
    ; name : String.t option
    ; created_date : DateTime.t option
    ; status : DocumentStatus.t option
    }

  let make ?sha1 ?name ?created_date ?status () = { sha1; name; created_date; status }

  let parse xml =
    Some
      { sha1 = Aws.Util.option_bind (Aws.Xml.member "Sha1" xml) String.parse
      ; name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
      ; created_date =
          Aws.Util.option_bind (Aws.Xml.member "CreatedDate" xml) DateTime.parse
      ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) DocumentStatus.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.status (fun f ->
               Aws.Query.Pair ("Status", DocumentStatus.to_query f))
         ; Aws.Util.option_map v.created_date (fun f ->
               Aws.Query.Pair ("CreatedDate", DateTime.to_query f))
         ; Aws.Util.option_map v.name (fun f ->
               Aws.Query.Pair ("Name", String.to_query f))
         ; Aws.Util.option_map v.sha1 (fun f ->
               Aws.Query.Pair ("Sha1", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.status (fun f -> "Status", DocumentStatus.to_json f)
         ; Aws.Util.option_map v.created_date (fun f -> "CreatedDate", DateTime.to_json f)
         ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
         ; Aws.Util.option_map v.sha1 (fun f -> "Sha1", String.to_json f)
         ])

  let of_json j =
    { sha1 = Aws.Util.option_map (Aws.Json.lookup j "Sha1") String.of_json
    ; name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
    ; created_date =
        Aws.Util.option_map (Aws.Json.lookup j "CreatedDate") DateTime.of_json
    ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") DocumentStatus.of_json
    }
end

module CreateDocumentResult = struct
  type t = { document_description : DocumentDescription.t option }

  let make ?document_description () = { document_description }

  let parse xml =
    Some
      { document_description =
          Aws.Util.option_bind
            (Aws.Xml.member "DocumentDescription" xml)
            DocumentDescription.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.document_description (fun f ->
               Aws.Query.Pair ("DocumentDescription", DocumentDescription.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.document_description (fun f ->
               "DocumentDescription", DocumentDescription.to_json f)
         ])

  let of_json j =
    { document_description =
        Aws.Util.option_map
          (Aws.Json.lookup j "DocumentDescription")
          DocumentDescription.of_json
    }
end

module Fault = struct
  type t =
    | Client
    | Server
    | Unknown

  let str_to_t = [ "Unknown", Unknown; "Server", Server; "Client", Client ]

  let t_to_str = [ Unknown, "Unknown"; Server, "Server"; Client, "Client" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module FailedCreateAssociation = struct
  type t =
    { entry : CreateAssociationBatchRequestEntry.t option
    ; message : String.t option
    ; fault : Fault.t option
    }

  let make ?entry ?message ?fault () = { entry; message; fault }

  let parse xml =
    Some
      { entry =
          Aws.Util.option_bind
            (Aws.Xml.member "Entry" xml)
            CreateAssociationBatchRequestEntry.parse
      ; message = Aws.Util.option_bind (Aws.Xml.member "Message" xml) String.parse
      ; fault = Aws.Util.option_bind (Aws.Xml.member "Fault" xml) Fault.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.fault (fun f ->
               Aws.Query.Pair ("Fault", Fault.to_query f))
         ; Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("Message", String.to_query f))
         ; Aws.Util.option_map v.entry (fun f ->
               Aws.Query.Pair ("Entry", CreateAssociationBatchRequestEntry.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.fault (fun f -> "Fault", Fault.to_json f)
         ; Aws.Util.option_map v.message (fun f -> "Message", String.to_json f)
         ; Aws.Util.option_map v.entry (fun f ->
               "Entry", CreateAssociationBatchRequestEntry.to_json f)
         ])

  let of_json j =
    { entry =
        Aws.Util.option_map
          (Aws.Json.lookup j "Entry")
          CreateAssociationBatchRequestEntry.of_json
    ; message = Aws.Util.option_map (Aws.Json.lookup j "Message") String.of_json
    ; fault = Aws.Util.option_map (Aws.Json.lookup j "Fault") Fault.of_json
    }
end

module FailedCreateAssociationList = struct
  type t = FailedCreateAssociation.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map
         FailedCreateAssociation.parse
         (Aws.Xml.members "FailedCreateAssociationEntry" xml))

  let to_query v = Aws.Query.to_query_list FailedCreateAssociation.to_query v

  let to_json v = `List (List.map FailedCreateAssociation.to_json v)

  let of_json j = Aws.Json.to_list FailedCreateAssociation.of_json j
end

module CreateAssociationBatchResult = struct
  type t =
    { successful : AssociationDescriptionList.t
    ; failed : FailedCreateAssociationList.t
    }

  let make ?(successful = []) ?(failed = []) () = { successful; failed }

  let parse xml =
    Some
      { successful =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "Successful" xml)
               AssociationDescriptionList.parse)
      ; failed =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "Failed" xml)
               FailedCreateAssociationList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("Failed.member", FailedCreateAssociationList.to_query v.failed))
         ; Some
             (Aws.Query.Pair
                ("Successful.member", AssociationDescriptionList.to_query v.successful))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Failed", FailedCreateAssociationList.to_json v.failed)
         ; Some ("Successful", AssociationDescriptionList.to_json v.successful)
         ])

  let of_json j =
    { successful =
        AssociationDescriptionList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Successful"))
    ; failed =
        FailedCreateAssociationList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Failed"))
    }
end

module AssociationFilterKey = struct
  type t =
    | InstanceId
    | Name

  let str_to_t = [ "Name", Name; "InstanceId", InstanceId ]

  let t_to_str = [ Name, "Name"; InstanceId, "InstanceId" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module AssociationFilter = struct
  type t =
    { key : AssociationFilterKey.t
    ; value : String.t
    }

  let make ~key ~value () = { key; value }

  let parse xml =
    Some
      { key =
          Aws.Xml.required
            "key"
            (Aws.Util.option_bind (Aws.Xml.member "key" xml) AssociationFilterKey.parse)
      ; value =
          Aws.Xml.required
            "value"
            (Aws.Util.option_bind (Aws.Xml.member "value" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("value", String.to_query v.value))
         ; Some (Aws.Query.Pair ("key", AssociationFilterKey.to_query v.key))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("value", String.to_json v.value)
         ; Some ("key", AssociationFilterKey.to_json v.key)
         ])

  let of_json j =
    { key =
        AssociationFilterKey.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "key"))
    ; value = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "value"))
    }
end

module TooManyUpdates = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module GetDocumentResult = struct
  type t =
    { name : String.t option
    ; content : String.t option
    }

  let make ?name ?content () = { name; content }

  let parse xml =
    Some
      { name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
      ; content = Aws.Util.option_bind (Aws.Xml.member "Content" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.content (fun f ->
               Aws.Query.Pair ("Content", String.to_query f))
         ; Aws.Util.option_map v.name (fun f ->
               Aws.Query.Pair ("Name", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.content (fun f -> "Content", String.to_json f)
         ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
         ])

  let of_json j =
    { name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
    ; content = Aws.Util.option_map (Aws.Json.lookup j "Content") String.of_json
    }
end

module AssociationAlreadyExists = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidDocument = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidNextToken = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module AssociationFilterList = struct
  type t = AssociationFilter.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map AssociationFilter.parse (Aws.Xml.members "AssociationFilter" xml))

  let to_query v = Aws.Query.to_query_list AssociationFilter.to_query v

  let to_json v = `List (List.map AssociationFilter.to_json v)

  let of_json j = Aws.Json.to_list AssociationFilter.of_json j
end

module UpdateAssociationStatusRequest = struct
  type t =
    { name : String.t
    ; instance_id : String.t
    ; association_status : AssociationStatus.t
    }

  let make ~name ~instance_id ~association_status () =
    { name; instance_id; association_status }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      ; instance_id =
          Aws.Xml.required
            "InstanceId"
            (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
      ; association_status =
          Aws.Xml.required
            "AssociationStatus"
            (Aws.Util.option_bind
               (Aws.Xml.member "AssociationStatus" xml)
               AssociationStatus.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("AssociationStatus", AssociationStatus.to_query v.association_status))
         ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
         ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("AssociationStatus", AssociationStatus.to_json v.association_status)
         ; Some ("InstanceId", String.to_json v.instance_id)
         ; Some ("Name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    ; instance_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
    ; association_status =
        AssociationStatus.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AssociationStatus"))
    }
end

module DescribeDocumentResult = struct
  type t = { document : DocumentDescription.t option }

  let make ?document () = { document }

  let parse xml =
    Some
      { document =
          Aws.Util.option_bind (Aws.Xml.member "Document" xml) DocumentDescription.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.document (fun f ->
               Aws.Query.Pair ("Document", DocumentDescription.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.document (fun f ->
               "Document", DocumentDescription.to_json f)
         ])

  let of_json j =
    { document =
        Aws.Util.option_map (Aws.Json.lookup j "Document") DocumentDescription.of_json
    }
end

module AssociationLimitExceeded = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InternalServerError = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DocumentFilterKey = struct
  type t = Name

  let str_to_t = [ "Name", Name ]

  let t_to_str = [ Name, "Name" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module DocumentFilter = struct
  type t =
    { key : DocumentFilterKey.t
    ; value : String.t
    }

  let make ~key ~value () = { key; value }

  let parse xml =
    Some
      { key =
          Aws.Xml.required
            "key"
            (Aws.Util.option_bind (Aws.Xml.member "key" xml) DocumentFilterKey.parse)
      ; value =
          Aws.Xml.required
            "value"
            (Aws.Util.option_bind (Aws.Xml.member "value" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("value", String.to_query v.value))
         ; Some (Aws.Query.Pair ("key", DocumentFilterKey.to_query v.key))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("value", String.to_json v.value)
         ; Some ("key", DocumentFilterKey.to_json v.key)
         ])

  let of_json j =
    { key = DocumentFilterKey.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "key"))
    ; value = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "value"))
    }
end

module DocumentFilterList = struct
  type t = DocumentFilter.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map DocumentFilter.parse (Aws.Xml.members "DocumentFilter" xml))

  let to_query v = Aws.Query.to_query_list DocumentFilter.to_query v

  let to_json v = `List (List.map DocumentFilter.to_json v)

  let of_json j = Aws.Json.to_list DocumentFilter.of_json j
end

module ListDocumentsRequest = struct
  type t =
    { document_filter_list : DocumentFilterList.t
    ; max_results : Integer.t option
    ; next_token : String.t option
    }

  let make ?(document_filter_list = []) ?max_results ?next_token () =
    { document_filter_list; max_results; next_token }

  let parse xml =
    Some
      { document_filter_list =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "DocumentFilterList" xml)
               DocumentFilterList.parse)
      ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Aws.Util.option_map v.max_results (fun f ->
               Aws.Query.Pair ("MaxResults", Integer.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "DocumentFilterList.member"
                , DocumentFilterList.to_query v.document_filter_list ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
         ; Some ("DocumentFilterList", DocumentFilterList.to_json v.document_filter_list)
         ])

  let of_json j =
    { document_filter_list =
        DocumentFilterList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "DocumentFilterList"))
    ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module DeleteAssociationResult = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module CreateAssociationRequest = struct
  type t =
    { name : String.t
    ; instance_id : String.t
    }

  let make ~name ~instance_id () = { name; instance_id }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      ; instance_id =
          Aws.Xml.required
            "InstanceId"
            (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
         ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("InstanceId", String.to_json v.instance_id)
         ; Some ("Name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    ; instance_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
    }
end

module DescribeDocumentRequest = struct
  type t = { name : String.t }

  let make ~name () = { name }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Name", String.to_query v.name)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("Name", String.to_json v.name) ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name")) }
end

module AssociationDoesNotExist = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeAssociationRequest = struct
  type t =
    { name : String.t
    ; instance_id : String.t
    }

  let make ~name ~instance_id () = { name; instance_id }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      ; instance_id =
          Aws.Xml.required
            "InstanceId"
            (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
         ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("InstanceId", String.to_json v.instance_id)
         ; Some ("Name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    ; instance_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
    }
end

module ListDocumentsResult = struct
  type t =
    { document_identifiers : DocumentIdentifierList.t
    ; next_token : String.t option
    }

  let make ?(document_identifiers = []) ?next_token () =
    { document_identifiers; next_token }

  let parse xml =
    Some
      { document_identifiers =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "DocumentIdentifiers" xml)
               DocumentIdentifierList.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "DocumentIdentifiers.member"
                , DocumentIdentifierList.to_query v.document_identifiers ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some
             ("DocumentIdentifiers", DocumentIdentifierList.to_json v.document_identifiers)
         ])

  let of_json j =
    { document_identifiers =
        DocumentIdentifierList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "DocumentIdentifiers"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module DescribeAssociationResult = struct
  type t = { association_description : AssociationDescription.t option }

  let make ?association_description () = { association_description }

  let parse xml =
    Some
      { association_description =
          Aws.Util.option_bind
            (Aws.Xml.member "AssociationDescription" xml)
            AssociationDescription.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.association_description (fun f ->
               Aws.Query.Pair ("AssociationDescription", AssociationDescription.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.association_description (fun f ->
               "AssociationDescription", AssociationDescription.to_json f)
         ])

  let of_json j =
    { association_description =
        Aws.Util.option_map
          (Aws.Json.lookup j "AssociationDescription")
          AssociationDescription.of_json
    }
end

module DocumentAlreadyExists = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteDocumentRequest = struct
  type t = { name : String.t }

  let make ~name () = { name }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Name", String.to_query v.name)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("Name", String.to_json v.name) ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name")) }
end

module AssociatedInstances = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module Association = struct
  type t =
    { name : String.t option
    ; instance_id : String.t option
    }

  let make ?name ?instance_id () = { name; instance_id }

  let parse xml =
    Some
      { name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
      ; instance_id = Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.instance_id (fun f ->
               Aws.Query.Pair ("InstanceId", String.to_query f))
         ; Aws.Util.option_map v.name (fun f ->
               Aws.Query.Pair ("Name", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.instance_id (fun f -> "InstanceId", String.to_json f)
         ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
         ])

  let of_json j =
    { name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
    ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "InstanceId") String.of_json
    }
end

module AssociationList = struct
  type t = Association.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Association.parse (Aws.Xml.members "Association" xml))

  let to_query v = Aws.Query.to_query_list Association.to_query v

  let to_json v = `List (List.map Association.to_json v)

  let of_json j = Aws.Json.to_list Association.of_json j
end

module ListAssociationsResult = struct
  type t =
    { associations : AssociationList.t
    ; next_token : String.t option
    }

  let make ?(associations = []) ?next_token () = { associations; next_token }

  let parse xml =
    Some
      { associations =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "Associations" xml)
               AssociationList.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("Associations.member", AssociationList.to_query v.associations))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("Associations", AssociationList.to_json v.associations)
         ])

  let of_json j =
    { associations =
        AssociationList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Associations"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module InvalidInstanceId = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module UpdateAssociationStatusResult = struct
  type t = { association_description : AssociationDescription.t option }

  let make ?association_description () = { association_description }

  let parse xml =
    Some
      { association_description =
          Aws.Util.option_bind
            (Aws.Xml.member "AssociationDescription" xml)
            AssociationDescription.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.association_description (fun f ->
               Aws.Query.Pair ("AssociationDescription", AssociationDescription.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.association_description (fun f ->
               "AssociationDescription", AssociationDescription.to_json f)
         ])

  let of_json j =
    { association_description =
        Aws.Util.option_map
          (Aws.Json.lookup j "AssociationDescription")
          AssociationDescription.of_json
    }
end

module MaxDocumentSizeExceeded = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ListAssociationsRequest = struct
  type t =
    { association_filter_list : AssociationFilterList.t
    ; max_results : Integer.t option
    ; next_token : String.t option
    }

  let make ~association_filter_list ?max_results ?next_token () =
    { association_filter_list; max_results; next_token }

  let parse xml =
    Some
      { association_filter_list =
          Aws.Xml.required
            "AssociationFilterList"
            (Aws.Util.option_bind
               (Aws.Xml.member "AssociationFilterList" xml)
               AssociationFilterList.parse)
      ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Aws.Util.option_map v.max_results (fun f ->
               Aws.Query.Pair ("MaxResults", Integer.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "AssociationFilterList.member"
                , AssociationFilterList.to_query v.association_filter_list ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
         ; Some
             ( "AssociationFilterList"
             , AssociationFilterList.to_json v.association_filter_list )
         ])

  let of_json j =
    { association_filter_list =
        AssociationFilterList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AssociationFilterList"))
    ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module CreateDocumentRequest = struct
  type t =
    { content : String.t
    ; name : String.t
    }

  let make ~content ~name () = { content; name }

  let parse xml =
    Some
      { content =
          Aws.Xml.required
            "Content"
            (Aws.Util.option_bind (Aws.Xml.member "Content" xml) String.parse)
      ; name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Name", String.to_query v.name))
         ; Some (Aws.Query.Pair ("Content", String.to_query v.content))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Name", String.to_json v.name)
         ; Some ("Content", String.to_json v.content)
         ])

  let of_json j =
    { content = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Content"))
    ; name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    }
end
