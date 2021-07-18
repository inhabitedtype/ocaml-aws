open Aws
open Aws.BaseTypes
open CalendarLib
type calendar = Calendar.t
module Runtime =
  struct
    type t =
      | Nodejs 
      | Java8 
    let str_to_t = [("java8", Java8); ("nodejs", Nodejs)]
    let t_to_str = [(Java8, "java8"); (Nodejs, "nodejs")]
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
module FunctionConfiguration =
  struct
    type t =
      {
      function_name: String.t option ;
      function_arn: String.t option ;
      runtime: Runtime.t option ;
      role: String.t option ;
      handler: String.t option ;
      code_size: Long.t option ;
      description: String.t option ;
      timeout: Integer.t option ;
      memory_size: Integer.t option ;
      last_modified: String.t option }
    let make ?function_name  ?function_arn  ?runtime  ?role  ?handler 
      ?code_size  ?description  ?timeout  ?memory_size  ?last_modified  () =
      {
        function_name;
        function_arn;
        runtime;
        role;
        handler;
        code_size;
        description;
        timeout;
        memory_size;
        last_modified
      }
    let parse xml =
      Some
        {
          function_name =
            (Util.option_bind (Xml.member "FunctionName" xml) String.parse);
          function_arn =
            (Util.option_bind (Xml.member "FunctionArn" xml) String.parse);
          runtime =
            (Util.option_bind (Xml.member "Runtime" xml) Runtime.parse);
          role = (Util.option_bind (Xml.member "Role" xml) String.parse);
          handler =
            (Util.option_bind (Xml.member "Handler" xml) String.parse);
          code_size =
            (Util.option_bind (Xml.member "CodeSize" xml) Long.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          timeout =
            (Util.option_bind (Xml.member "Timeout" xml) Integer.parse);
          memory_size =
            (Util.option_bind (Xml.member "MemorySize" xml) Integer.parse);
          last_modified =
            (Util.option_bind (Xml.member "LastModified" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.last_modified
              (fun f -> Query.Pair ("LastModified", (String.to_query f)));
           Util.option_map v.memory_size
             (fun f -> Query.Pair ("MemorySize", (Integer.to_query f)));
           Util.option_map v.timeout
             (fun f -> Query.Pair ("Timeout", (Integer.to_query f)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.code_size
             (fun f -> Query.Pair ("CodeSize", (Long.to_query f)));
           Util.option_map v.handler
             (fun f -> Query.Pair ("Handler", (String.to_query f)));
           Util.option_map v.role
             (fun f -> Query.Pair ("Role", (String.to_query f)));
           Util.option_map v.runtime
             (fun f -> Query.Pair ("Runtime", (Runtime.to_query f)));
           Util.option_map v.function_arn
             (fun f -> Query.Pair ("FunctionArn", (String.to_query f)));
           Util.option_map v.function_name
             (fun f -> Query.Pair ("FunctionName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.last_modified
              (fun f -> ("last_modified", (String.to_json f)));
           Util.option_map v.memory_size
             (fun f -> ("memory_size", (Integer.to_json f)));
           Util.option_map v.timeout
             (fun f -> ("timeout", (Integer.to_json f)));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Util.option_map v.code_size
             (fun f -> ("code_size", (Long.to_json f)));
           Util.option_map v.handler
             (fun f -> ("handler", (String.to_json f)));
           Util.option_map v.role (fun f -> ("role", (String.to_json f)));
           Util.option_map v.runtime
             (fun f -> ("runtime", (Runtime.to_json f)));
           Util.option_map v.function_arn
             (fun f -> ("function_arn", (String.to_json f)));
           Util.option_map v.function_name
             (fun f -> ("function_name", (String.to_json f)))])
    let of_json j =
      {
        function_name =
          (Util.option_map (Json.lookup j "function_name") String.of_json);
        function_arn =
          (Util.option_map (Json.lookup j "function_arn") String.of_json);
        runtime = (Util.option_map (Json.lookup j "runtime") Runtime.of_json);
        role = (Util.option_map (Json.lookup j "role") String.of_json);
        handler = (Util.option_map (Json.lookup j "handler") String.of_json);
        code_size =
          (Util.option_map (Json.lookup j "code_size") Long.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        timeout = (Util.option_map (Json.lookup j "timeout") Integer.of_json);
        memory_size =
          (Util.option_map (Json.lookup j "memory_size") Integer.of_json);
        last_modified =
          (Util.option_map (Json.lookup j "last_modified") String.of_json)
      }
  end
module EventSourceMappingConfiguration =
  struct
    type t =
      {
      u_u_i_d: String.t option ;
      batch_size: Integer.t option ;
      event_source_arn: String.t option ;
      function_arn: String.t option ;
      last_modified: DateTime.t option ;
      last_processing_result: String.t option ;
      state: String.t option ;
      state_transition_reason: String.t option }
    let make ?u_u_i_d  ?batch_size  ?event_source_arn  ?function_arn 
      ?last_modified  ?last_processing_result  ?state 
      ?state_transition_reason  () =
      {
        u_u_i_d;
        batch_size;
        event_source_arn;
        function_arn;
        last_modified;
        last_processing_result;
        state;
        state_transition_reason
      }
    let parse xml =
      Some
        {
          u_u_i_d = (Util.option_bind (Xml.member "UUID" xml) String.parse);
          batch_size =
            (Util.option_bind (Xml.member "BatchSize" xml) Integer.parse);
          event_source_arn =
            (Util.option_bind (Xml.member "EventSourceArn" xml) String.parse);
          function_arn =
            (Util.option_bind (Xml.member "FunctionArn" xml) String.parse);
          last_modified =
            (Util.option_bind (Xml.member "LastModified" xml) DateTime.parse);
          last_processing_result =
            (Util.option_bind (Xml.member "LastProcessingResult" xml)
               String.parse);
          state = (Util.option_bind (Xml.member "State" xml) String.parse);
          state_transition_reason =
            (Util.option_bind (Xml.member "StateTransitionReason" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.state_transition_reason
              (fun f ->
                 Query.Pair ("StateTransitionReason", (String.to_query f)));
           Util.option_map v.state
             (fun f -> Query.Pair ("State", (String.to_query f)));
           Util.option_map v.last_processing_result
             (fun f ->
                Query.Pair ("LastProcessingResult", (String.to_query f)));
           Util.option_map v.last_modified
             (fun f -> Query.Pair ("LastModified", (DateTime.to_query f)));
           Util.option_map v.function_arn
             (fun f -> Query.Pair ("FunctionArn", (String.to_query f)));
           Util.option_map v.event_source_arn
             (fun f -> Query.Pair ("EventSourceArn", (String.to_query f)));
           Util.option_map v.batch_size
             (fun f -> Query.Pair ("BatchSize", (Integer.to_query f)));
           Util.option_map v.u_u_i_d
             (fun f -> Query.Pair ("UUID", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.state_transition_reason
              (fun f -> ("state_transition_reason", (String.to_json f)));
           Util.option_map v.state (fun f -> ("state", (String.to_json f)));
           Util.option_map v.last_processing_result
             (fun f -> ("last_processing_result", (String.to_json f)));
           Util.option_map v.last_modified
             (fun f -> ("last_modified", (DateTime.to_json f)));
           Util.option_map v.function_arn
             (fun f -> ("function_arn", (String.to_json f)));
           Util.option_map v.event_source_arn
             (fun f -> ("event_source_arn", (String.to_json f)));
           Util.option_map v.batch_size
             (fun f -> ("batch_size", (Integer.to_json f)));
           Util.option_map v.u_u_i_d
             (fun f -> ("u_u_i_d", (String.to_json f)))])
    let of_json j =
      {
        u_u_i_d = (Util.option_map (Json.lookup j "u_u_i_d") String.of_json);
        batch_size =
          (Util.option_map (Json.lookup j "batch_size") Integer.of_json);
        event_source_arn =
          (Util.option_map (Json.lookup j "event_source_arn") String.of_json);
        function_arn =
          (Util.option_map (Json.lookup j "function_arn") String.of_json);
        last_modified =
          (Util.option_map (Json.lookup j "last_modified") DateTime.of_json);
        last_processing_result =
          (Util.option_map (Json.lookup j "last_processing_result")
             String.of_json);
        state = (Util.option_map (Json.lookup j "state") String.of_json);
        state_transition_reason =
          (Util.option_map (Json.lookup j "state_transition_reason")
             String.of_json)
      }
  end
module EventSourcePosition =
  struct
    type t =
      | TRIM_HORIZON 
      | LATEST 
    let str_to_t = [("LATEST", LATEST); ("TRIM_HORIZON", TRIM_HORIZON)]
    let t_to_str = [(LATEST, "LATEST"); (TRIM_HORIZON, "TRIM_HORIZON")]
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
module FunctionCodeLocation =
  struct
    type t = {
      repository_type: String.t option ;
      location: String.t option }
    let make ?repository_type  ?location  () = { repository_type; location }
    let parse xml =
      Some
        {
          repository_type =
            (Util.option_bind (Xml.member "RepositoryType" xml) String.parse);
          location =
            (Util.option_bind (Xml.member "Location" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.location
              (fun f -> Query.Pair ("Location", (String.to_query f)));
           Util.option_map v.repository_type
             (fun f -> Query.Pair ("RepositoryType", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.location
              (fun f -> ("location", (String.to_json f)));
           Util.option_map v.repository_type
             (fun f -> ("repository_type", (String.to_json f)))])
    let of_json j =
      {
        repository_type =
          (Util.option_map (Json.lookup j "repository_type") String.of_json);
        location =
          (Util.option_map (Json.lookup j "location") String.of_json)
      }
  end
module FunctionList =
  struct
    type t = FunctionConfiguration.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map FunctionConfiguration.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list FunctionConfiguration.to_query v
    let to_json v = `List (List.map FunctionConfiguration.to_json v)
    let of_json j = Json.to_list FunctionConfiguration.of_json j
  end
module EventSourceMappingsList =
  struct
    type t = EventSourceMappingConfiguration.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map EventSourceMappingConfiguration.parse
           (Xml.members "member" xml))
    let to_query v =
      Query.to_query_list EventSourceMappingConfiguration.to_query v
    let to_json v =
      `List (List.map EventSourceMappingConfiguration.to_json v)
    let of_json j = Json.to_list EventSourceMappingConfiguration.of_json j
  end
module InvocationType =
  struct
    type t =
      | Event 
      | RequestResponse 
      | DryRun 
    let str_to_t =
      [("DryRun", DryRun);
      ("RequestResponse", RequestResponse);
      ("Event", Event)]
    let t_to_str =
      [(DryRun, "DryRun");
      (RequestResponse, "RequestResponse");
      (Event, "Event")]
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
module LogType =
  struct
    type t =
      | None 
      | Tail 
    let str_to_t = [("Tail", Tail); ("None", None)]
    let t_to_str = [(Tail, "Tail"); (None, "None")]
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
module FunctionCode =
  struct
    type t =
      {
      zip_file: Blob.t option ;
      s3_bucket: String.t option ;
      s3_key: String.t option ;
      s3_object_version: String.t option }
    let make ?zip_file  ?s3_bucket  ?s3_key  ?s3_object_version  () =
      { zip_file; s3_bucket; s3_key; s3_object_version }
    let parse xml =
      Some
        {
          zip_file = (Util.option_bind (Xml.member "ZipFile" xml) Blob.parse);
          s3_bucket =
            (Util.option_bind (Xml.member "S3Bucket" xml) String.parse);
          s3_key = (Util.option_bind (Xml.member "S3Key" xml) String.parse);
          s3_object_version =
            (Util.option_bind (Xml.member "S3ObjectVersion" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.s3_object_version
              (fun f -> Query.Pair ("S3ObjectVersion", (String.to_query f)));
           Util.option_map v.s3_key
             (fun f -> Query.Pair ("S3Key", (String.to_query f)));
           Util.option_map v.s3_bucket
             (fun f -> Query.Pair ("S3Bucket", (String.to_query f)));
           Util.option_map v.zip_file
             (fun f -> Query.Pair ("ZipFile", (Blob.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.s3_object_version
              (fun f -> ("s3_object_version", (String.to_json f)));
           Util.option_map v.s3_key (fun f -> ("s3_key", (String.to_json f)));
           Util.option_map v.s3_bucket
             (fun f -> ("s3_bucket", (String.to_json f)));
           Util.option_map v.zip_file
             (fun f -> ("zip_file", (Blob.to_json f)))])
    let of_json j =
      {
        zip_file = (Util.option_map (Json.lookup j "zip_file") Blob.of_json);
        s3_bucket =
          (Util.option_map (Json.lookup j "s3_bucket") String.of_json);
        s3_key = (Util.option_map (Json.lookup j "s3_key") String.of_json);
        s3_object_version =
          (Util.option_map (Json.lookup j "s3_object_version") String.of_json)
      }
  end
module GetFunctionRequest =
  struct
    type t = {
      function_name: String.t }
    let make ~function_name  () = { function_name }
    let parse xml =
      Some
        {
          function_name =
            (Xml.required "FunctionName"
               (Util.option_bind (Xml.member "FunctionName" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("FunctionName", (String.to_query v.function_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("function_name", (String.to_json v.function_name))])
    let of_json j =
      {
        function_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "function_name")))
      }
  end
module InvalidRequestContentException =
  struct
    type t = {
      type_: String.t option ;
      message: String.t option }
    let make ?type_  ?message  () = { type_; message }
    let parse xml =
      Some
        {
          type_ = (Util.option_bind (Xml.member "Type" xml) String.parse);
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)));
           Util.option_map v.type_
             (fun f -> Query.Pair ("Type", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)));
           Util.option_map v.type_ (fun f -> ("type_", (String.to_json f)))])
    let of_json j =
      {
        type_ = (Util.option_map (Json.lookup j "type_") String.of_json);
        message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ListEventSourceMappingsRequest =
  struct
    type t =
      {
      event_source_arn: String.t option ;
      function_name: String.t option ;
      marker: String.t option ;
      max_items: Integer.t option }
    let make ?event_source_arn  ?function_name  ?marker  ?max_items  () =
      { event_source_arn; function_name; marker; max_items }
    let parse xml =
      Some
        {
          event_source_arn =
            (Util.option_bind (Xml.member "EventSourceArn" xml) String.parse);
          function_name =
            (Util.option_bind (Xml.member "FunctionName" xml) String.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          max_items =
            (Util.option_bind (Xml.member "MaxItems" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> Query.Pair ("MaxItems", (Integer.to_query f)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.function_name
             (fun f -> Query.Pair ("FunctionName", (String.to_query f)));
           Util.option_map v.event_source_arn
             (fun f -> Query.Pair ("EventSourceArn", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> ("max_items", (Integer.to_json f)));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.function_name
             (fun f -> ("function_name", (String.to_json f)));
           Util.option_map v.event_source_arn
             (fun f -> ("event_source_arn", (String.to_json f)))])
    let of_json j =
      {
        event_source_arn =
          (Util.option_map (Json.lookup j "event_source_arn") String.of_json);
        function_name =
          (Util.option_map (Json.lookup j "function_name") String.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        max_items =
          (Util.option_map (Json.lookup j "max_items") Integer.of_json)
      }
  end
module AddPermissionResponse =
  struct
    type t = {
      statement: String.t option }
    let make ?statement  () = { statement }
    let parse xml =
      Some
        {
          statement =
            (Util.option_bind (Xml.member "Statement" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.statement
              (fun f -> Query.Pair ("Statement", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.statement
              (fun f -> ("statement", (String.to_json f)))])
    let of_json j =
      {
        statement =
          (Util.option_map (Json.lookup j "statement") String.of_json)
      }
  end
module CodeStorageExceededException =
  struct
    type t = {
      type_: String.t option ;
      message: String.t option }
    let make ?type_  ?message  () = { type_; message }
    let parse xml =
      Some
        {
          type_ = (Util.option_bind (Xml.member "Type" xml) String.parse);
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)));
           Util.option_map v.type_
             (fun f -> Query.Pair ("Type", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)));
           Util.option_map v.type_ (fun f -> ("type_", (String.to_json f)))])
    let of_json j =
      {
        type_ = (Util.option_map (Json.lookup j "type_") String.of_json);
        message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module UpdateEventSourceMappingRequest =
  struct
    type t =
      {
      u_u_i_d: String.t ;
      function_name: String.t option ;
      enabled: Boolean.t option ;
      batch_size: Integer.t option }
    let make ~u_u_i_d  ?function_name  ?enabled  ?batch_size  () =
      { u_u_i_d; function_name; enabled; batch_size }
    let parse xml =
      Some
        {
          u_u_i_d =
            (Xml.required "UUID"
               (Util.option_bind (Xml.member "UUID" xml) String.parse));
          function_name =
            (Util.option_bind (Xml.member "FunctionName" xml) String.parse);
          enabled =
            (Util.option_bind (Xml.member "Enabled" xml) Boolean.parse);
          batch_size =
            (Util.option_bind (Xml.member "BatchSize" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.batch_size
              (fun f -> Query.Pair ("BatchSize", (Integer.to_query f)));
           Util.option_map v.enabled
             (fun f -> Query.Pair ("Enabled", (Boolean.to_query f)));
           Util.option_map v.function_name
             (fun f -> Query.Pair ("FunctionName", (String.to_query f)));
           Some (Query.Pair ("UUID", (String.to_query v.u_u_i_d)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.batch_size
              (fun f -> ("batch_size", (Integer.to_json f)));
           Util.option_map v.enabled
             (fun f -> ("enabled", (Boolean.to_json f)));
           Util.option_map v.function_name
             (fun f -> ("function_name", (String.to_json f)));
           Some ("u_u_i_d", (String.to_json v.u_u_i_d))])
    let of_json j =
      {
        u_u_i_d =
          (String.of_json (Util.of_option_exn (Json.lookup j "u_u_i_d")));
        function_name =
          (Util.option_map (Json.lookup j "function_name") String.of_json);
        enabled = (Util.option_map (Json.lookup j "enabled") Boolean.of_json);
        batch_size =
          (Util.option_map (Json.lookup j "batch_size") Integer.of_json)
      }
  end
module InvokeAsyncResponse =
  struct
    type t = {
      status: Integer.t }
    let make ~status  () = { status }
    let parse xml =
      Some
        {
          status =
            (Xml.required "Status"
               (Util.option_bind (Xml.member "Status" xml) Integer.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Status", (Integer.to_query v.status)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("status", (Integer.to_json v.status))])
    let of_json j =
      {
        status =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "status")))
      }
  end
module ResourceConflictException =
  struct
    type t = {
      type_: String.t option ;
      message: String.t option }
    let make ?type_  ?message  () = { type_; message }
    let parse xml =
      Some
        {
          type_ = (Util.option_bind (Xml.member "Type" xml) String.parse);
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)));
           Util.option_map v.type_
             (fun f -> Query.Pair ("Type", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)));
           Util.option_map v.type_ (fun f -> ("type_", (String.to_json f)))])
    let of_json j =
      {
        type_ = (Util.option_map (Json.lookup j "type_") String.of_json);
        message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module UpdateFunctionCodeRequest =
  struct
    type t =
      {
      function_name: String.t ;
      zip_file: Blob.t option ;
      s3_bucket: String.t option ;
      s3_key: String.t option ;
      s3_object_version: String.t option }
    let make ~function_name  ?zip_file  ?s3_bucket  ?s3_key 
      ?s3_object_version  () =
      { function_name; zip_file; s3_bucket; s3_key; s3_object_version }
    let parse xml =
      Some
        {
          function_name =
            (Xml.required "FunctionName"
               (Util.option_bind (Xml.member "FunctionName" xml) String.parse));
          zip_file = (Util.option_bind (Xml.member "ZipFile" xml) Blob.parse);
          s3_bucket =
            (Util.option_bind (Xml.member "S3Bucket" xml) String.parse);
          s3_key = (Util.option_bind (Xml.member "S3Key" xml) String.parse);
          s3_object_version =
            (Util.option_bind (Xml.member "S3ObjectVersion" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.s3_object_version
              (fun f -> Query.Pair ("S3ObjectVersion", (String.to_query f)));
           Util.option_map v.s3_key
             (fun f -> Query.Pair ("S3Key", (String.to_query f)));
           Util.option_map v.s3_bucket
             (fun f -> Query.Pair ("S3Bucket", (String.to_query f)));
           Util.option_map v.zip_file
             (fun f -> Query.Pair ("ZipFile", (Blob.to_query f)));
           Some
             (Query.Pair ("FunctionName", (String.to_query v.function_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.s3_object_version
              (fun f -> ("s3_object_version", (String.to_json f)));
           Util.option_map v.s3_key (fun f -> ("s3_key", (String.to_json f)));
           Util.option_map v.s3_bucket
             (fun f -> ("s3_bucket", (String.to_json f)));
           Util.option_map v.zip_file
             (fun f -> ("zip_file", (Blob.to_json f)));
           Some ("function_name", (String.to_json v.function_name))])
    let of_json j =
      {
        function_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "function_name")));
        zip_file = (Util.option_map (Json.lookup j "zip_file") Blob.of_json);
        s3_bucket =
          (Util.option_map (Json.lookup j "s3_bucket") String.of_json);
        s3_key = (Util.option_map (Json.lookup j "s3_key") String.of_json);
        s3_object_version =
          (Util.option_map (Json.lookup j "s3_object_version") String.of_json)
      }
  end
module CreateEventSourceMappingRequest =
  struct
    type t =
      {
      event_source_arn: String.t ;
      function_name: String.t ;
      enabled: Boolean.t option ;
      batch_size: Integer.t option ;
      starting_position: EventSourcePosition.t }
    let make ~event_source_arn  ~function_name  ?enabled  ?batch_size 
      ~starting_position  () =
      {
        event_source_arn;
        function_name;
        enabled;
        batch_size;
        starting_position
      }
    let parse xml =
      Some
        {
          event_source_arn =
            (Xml.required "EventSourceArn"
               (Util.option_bind (Xml.member "EventSourceArn" xml)
                  String.parse));
          function_name =
            (Xml.required "FunctionName"
               (Util.option_bind (Xml.member "FunctionName" xml) String.parse));
          enabled =
            (Util.option_bind (Xml.member "Enabled" xml) Boolean.parse);
          batch_size =
            (Util.option_bind (Xml.member "BatchSize" xml) Integer.parse);
          starting_position =
            (Xml.required "StartingPosition"
               (Util.option_bind (Xml.member "StartingPosition" xml)
                  EventSourcePosition.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("StartingPosition",
                   (EventSourcePosition.to_query v.starting_position)));
           Util.option_map v.batch_size
             (fun f -> Query.Pair ("BatchSize", (Integer.to_query f)));
           Util.option_map v.enabled
             (fun f -> Query.Pair ("Enabled", (Boolean.to_query f)));
           Some
             (Query.Pair ("FunctionName", (String.to_query v.function_name)));
           Some
             (Query.Pair
                ("EventSourceArn", (String.to_query v.event_source_arn)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("starting_position",
                (EventSourcePosition.to_json v.starting_position));
           Util.option_map v.batch_size
             (fun f -> ("batch_size", (Integer.to_json f)));
           Util.option_map v.enabled
             (fun f -> ("enabled", (Boolean.to_json f)));
           Some ("function_name", (String.to_json v.function_name));
           Some ("event_source_arn", (String.to_json v.event_source_arn))])
    let of_json j =
      {
        event_source_arn =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "event_source_arn")));
        function_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "function_name")));
        enabled = (Util.option_map (Json.lookup j "enabled") Boolean.of_json);
        batch_size =
          (Util.option_map (Json.lookup j "batch_size") Integer.of_json);
        starting_position =
          (EventSourcePosition.of_json
             (Util.of_option_exn (Json.lookup j "starting_position")))
      }
  end
module UnsupportedMediaTypeException =
  struct
    type t = {
      type_: String.t option ;
      message: String.t option }
    let make ?type_  ?message  () = { type_; message }
    let parse xml =
      Some
        {
          type_ = (Util.option_bind (Xml.member "Type" xml) String.parse);
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)));
           Util.option_map v.type_
             (fun f -> Query.Pair ("Type", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)));
           Util.option_map v.type_ (fun f -> ("type_", (String.to_json f)))])
    let of_json j =
      {
        type_ = (Util.option_map (Json.lookup j "type_") String.of_json);
        message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module RemovePermissionRequest =
  struct
    type t = {
      function_name: String.t ;
      statement_id: String.t }
    let make ~function_name  ~statement_id  () =
      { function_name; statement_id }
    let parse xml =
      Some
        {
          function_name =
            (Xml.required "FunctionName"
               (Util.option_bind (Xml.member "FunctionName" xml) String.parse));
          statement_id =
            (Xml.required "StatementId"
               (Util.option_bind (Xml.member "StatementId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("StatementId", (String.to_query v.statement_id)));
           Some
             (Query.Pair ("FunctionName", (String.to_query v.function_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("statement_id", (String.to_json v.statement_id));
           Some ("function_name", (String.to_json v.function_name))])
    let of_json j =
      {
        function_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "function_name")));
        statement_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "statement_id")))
      }
  end
module GetFunctionResponse =
  struct
    type t =
      {
      configuration: FunctionConfiguration.t option ;
      code: FunctionCodeLocation.t option }
    let make ?configuration  ?code  () = { configuration; code }
    let parse xml =
      Some
        {
          configuration =
            (Util.option_bind (Xml.member "Configuration" xml)
               FunctionConfiguration.parse);
          code =
            (Util.option_bind (Xml.member "Code" xml)
               FunctionCodeLocation.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.code
              (fun f ->
                 Query.Pair ("Code", (FunctionCodeLocation.to_query f)));
           Util.option_map v.configuration
             (fun f ->
                Query.Pair
                  ("Configuration", (FunctionConfiguration.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.code
              (fun f -> ("code", (FunctionCodeLocation.to_json f)));
           Util.option_map v.configuration
             (fun f -> ("configuration", (FunctionConfiguration.to_json f)))])
    let of_json j =
      {
        configuration =
          (Util.option_map (Json.lookup j "configuration")
             FunctionConfiguration.of_json);
        code =
          (Util.option_map (Json.lookup j "code")
             FunctionCodeLocation.of_json)
      }
  end
module TooManyRequestsException =
  struct
    type t =
      {
      retry_after_seconds: String.t option ;
      type_: String.t option ;
      message: String.t option }
    let make ?retry_after_seconds  ?type_  ?message  () =
      { retry_after_seconds; type_; message }
    let parse xml =
      Some
        {
          retry_after_seconds =
            (Util.option_bind (Xml.member "Retry-After" xml) String.parse);
          type_ = (Util.option_bind (Xml.member "Type" xml) String.parse);
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)));
           Util.option_map v.type_
             (fun f -> Query.Pair ("Type", (String.to_query f)));
           Util.option_map v.retry_after_seconds
             (fun f -> Query.Pair ("Retry-After", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)));
           Util.option_map v.type_ (fun f -> ("type_", (String.to_json f)));
           Util.option_map v.retry_after_seconds
             (fun f -> ("retry_after_seconds", (String.to_json f)))])
    let of_json j =
      {
        retry_after_seconds =
          (Util.option_map (Json.lookup j "retry_after_seconds")
             String.of_json);
        type_ = (Util.option_map (Json.lookup j "type_") String.of_json);
        message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module AddPermissionRequest =
  struct
    type t =
      {
      function_name: String.t ;
      statement_id: String.t ;
      action: String.t ;
      principal: String.t ;
      source_arn: String.t option ;
      source_account: String.t option }
    let make ~function_name  ~statement_id  ~action  ~principal  ?source_arn 
      ?source_account  () =
      {
        function_name;
        statement_id;
        action;
        principal;
        source_arn;
        source_account
      }
    let parse xml =
      Some
        {
          function_name =
            (Xml.required "FunctionName"
               (Util.option_bind (Xml.member "FunctionName" xml) String.parse));
          statement_id =
            (Xml.required "StatementId"
               (Util.option_bind (Xml.member "StatementId" xml) String.parse));
          action =
            (Xml.required "Action"
               (Util.option_bind (Xml.member "Action" xml) String.parse));
          principal =
            (Xml.required "Principal"
               (Util.option_bind (Xml.member "Principal" xml) String.parse));
          source_arn =
            (Util.option_bind (Xml.member "SourceArn" xml) String.parse);
          source_account =
            (Util.option_bind (Xml.member "SourceAccount" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.source_account
              (fun f -> Query.Pair ("SourceAccount", (String.to_query f)));
           Util.option_map v.source_arn
             (fun f -> Query.Pair ("SourceArn", (String.to_query f)));
           Some (Query.Pair ("Principal", (String.to_query v.principal)));
           Some (Query.Pair ("Action", (String.to_query v.action)));
           Some
             (Query.Pair ("StatementId", (String.to_query v.statement_id)));
           Some
             (Query.Pair ("FunctionName", (String.to_query v.function_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.source_account
              (fun f -> ("source_account", (String.to_json f)));
           Util.option_map v.source_arn
             (fun f -> ("source_arn", (String.to_json f)));
           Some ("principal", (String.to_json v.principal));
           Some ("action", (String.to_json v.action));
           Some ("statement_id", (String.to_json v.statement_id));
           Some ("function_name", (String.to_json v.function_name))])
    let of_json j =
      {
        function_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "function_name")));
        statement_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "statement_id")));
        action =
          (String.of_json (Util.of_option_exn (Json.lookup j "action")));
        principal =
          (String.of_json (Util.of_option_exn (Json.lookup j "principal")));
        source_arn =
          (Util.option_map (Json.lookup j "source_arn") String.of_json);
        source_account =
          (Util.option_map (Json.lookup j "source_account") String.of_json)
      }
  end
module GetFunctionConfigurationRequest =
  struct
    type t = {
      function_name: String.t }
    let make ~function_name  () = { function_name }
    let parse xml =
      Some
        {
          function_name =
            (Xml.required "FunctionName"
               (Util.option_bind (Xml.member "FunctionName" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("FunctionName", (String.to_query v.function_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("function_name", (String.to_json v.function_name))])
    let of_json j =
      {
        function_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "function_name")))
      }
  end
module InvokeAsyncRequest =
  struct
    type t = {
      function_name: String.t ;
      invoke_args: Blob.t }
    let make ~function_name  ~invoke_args  () =
      { function_name; invoke_args }
    let parse xml =
      Some
        {
          function_name =
            (Xml.required "FunctionName"
               (Util.option_bind (Xml.member "FunctionName" xml) String.parse));
          invoke_args =
            (Xml.required "InvokeArgs"
               (Util.option_bind (Xml.member "InvokeArgs" xml) Blob.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("InvokeArgs", (Blob.to_query v.invoke_args)));
           Some
             (Query.Pair ("FunctionName", (String.to_query v.function_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("invoke_args", (Blob.to_json v.invoke_args));
           Some ("function_name", (String.to_json v.function_name))])
    let of_json j =
      {
        function_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "function_name")));
        invoke_args =
          (Blob.of_json (Util.of_option_exn (Json.lookup j "invoke_args")))
      }
  end
module InvalidParameterValueException =
  struct
    type t = {
      type_: String.t option ;
      message: String.t option }
    let make ?type_  ?message  () = { type_; message }
    let parse xml =
      Some
        {
          type_ = (Util.option_bind (Xml.member "Type" xml) String.parse);
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)));
           Util.option_map v.type_
             (fun f -> Query.Pair ("Type", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)));
           Util.option_map v.type_ (fun f -> ("type_", (String.to_json f)))])
    let of_json j =
      {
        type_ = (Util.option_map (Json.lookup j "type_") String.of_json);
        message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ListFunctionsResponse =
  struct
    type t = {
      next_marker: String.t option ;
      functions: FunctionList.t }
    let make ?next_marker  ?(functions= [])  () = { next_marker; functions }
    let parse xml =
      Some
        {
          next_marker =
            (Util.option_bind (Xml.member "NextMarker" xml) String.parse);
          functions =
            (Util.of_option []
               (Util.option_bind (Xml.member "Functions" xml)
                  FunctionList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Functions.member", (FunctionList.to_query v.functions)));
           Util.option_map v.next_marker
             (fun f -> Query.Pair ("NextMarker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("functions", (FunctionList.to_json v.functions));
           Util.option_map v.next_marker
             (fun f -> ("next_marker", (String.to_json f)))])
    let of_json j =
      {
        next_marker =
          (Util.option_map (Json.lookup j "next_marker") String.of_json);
        functions =
          (FunctionList.of_json
             (Util.of_option_exn (Json.lookup j "functions")))
      }
  end
module UpdateFunctionConfigurationRequest =
  struct
    type t =
      {
      function_name: String.t ;
      role: String.t option ;
      handler: String.t option ;
      description: String.t option ;
      timeout: Integer.t option ;
      memory_size: Integer.t option }
    let make ~function_name  ?role  ?handler  ?description  ?timeout 
      ?memory_size  () =
      { function_name; role; handler; description; timeout; memory_size }
    let parse xml =
      Some
        {
          function_name =
            (Xml.required "FunctionName"
               (Util.option_bind (Xml.member "FunctionName" xml) String.parse));
          role = (Util.option_bind (Xml.member "Role" xml) String.parse);
          handler =
            (Util.option_bind (Xml.member "Handler" xml) String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          timeout =
            (Util.option_bind (Xml.member "Timeout" xml) Integer.parse);
          memory_size =
            (Util.option_bind (Xml.member "MemorySize" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.memory_size
              (fun f -> Query.Pair ("MemorySize", (Integer.to_query f)));
           Util.option_map v.timeout
             (fun f -> Query.Pair ("Timeout", (Integer.to_query f)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.handler
             (fun f -> Query.Pair ("Handler", (String.to_query f)));
           Util.option_map v.role
             (fun f -> Query.Pair ("Role", (String.to_query f)));
           Some
             (Query.Pair ("FunctionName", (String.to_query v.function_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.memory_size
              (fun f -> ("memory_size", (Integer.to_json f)));
           Util.option_map v.timeout
             (fun f -> ("timeout", (Integer.to_json f)));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Util.option_map v.handler
             (fun f -> ("handler", (String.to_json f)));
           Util.option_map v.role (fun f -> ("role", (String.to_json f)));
           Some ("function_name", (String.to_json v.function_name))])
    let of_json j =
      {
        function_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "function_name")));
        role = (Util.option_map (Json.lookup j "role") String.of_json);
        handler = (Util.option_map (Json.lookup j "handler") String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        timeout = (Util.option_map (Json.lookup j "timeout") Integer.of_json);
        memory_size =
          (Util.option_map (Json.lookup j "memory_size") Integer.of_json)
      }
  end
module RequestTooLargeException =
  struct
    type t = {
      type_: String.t option ;
      message: String.t option }
    let make ?type_  ?message  () = { type_; message }
    let parse xml =
      Some
        {
          type_ = (Util.option_bind (Xml.member "Type" xml) String.parse);
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)));
           Util.option_map v.type_
             (fun f -> Query.Pair ("Type", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)));
           Util.option_map v.type_ (fun f -> ("type_", (String.to_json f)))])
    let of_json j =
      {
        type_ = (Util.option_map (Json.lookup j "type_") String.of_json);
        message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module GetPolicyRequest =
  struct
    type t = {
      function_name: String.t }
    let make ~function_name  () = { function_name }
    let parse xml =
      Some
        {
          function_name =
            (Xml.required "FunctionName"
               (Util.option_bind (Xml.member "FunctionName" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("FunctionName", (String.to_query v.function_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("function_name", (String.to_json v.function_name))])
    let of_json j =
      {
        function_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "function_name")))
      }
  end
module ListEventSourceMappingsResponse =
  struct
    type t =
      {
      next_marker: String.t option ;
      event_source_mappings: EventSourceMappingsList.t }
    let make ?next_marker  ?(event_source_mappings= [])  () =
      { next_marker; event_source_mappings }
    let parse xml =
      Some
        {
          next_marker =
            (Util.option_bind (Xml.member "NextMarker" xml) String.parse);
          event_source_mappings =
            (Util.of_option []
               (Util.option_bind (Xml.member "EventSourceMappings" xml)
                  EventSourceMappingsList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("EventSourceMappings.member",
                   (EventSourceMappingsList.to_query v.event_source_mappings)));
           Util.option_map v.next_marker
             (fun f -> Query.Pair ("NextMarker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("event_source_mappings",
                (EventSourceMappingsList.to_json v.event_source_mappings));
           Util.option_map v.next_marker
             (fun f -> ("next_marker", (String.to_json f)))])
    let of_json j =
      {
        next_marker =
          (Util.option_map (Json.lookup j "next_marker") String.of_json);
        event_source_mappings =
          (EventSourceMappingsList.of_json
             (Util.of_option_exn (Json.lookup j "event_source_mappings")))
      }
  end
module InvocationRequest =
  struct
    type t =
      {
      function_name: String.t ;
      invocation_type: InvocationType.t option ;
      log_type: LogType.t option ;
      client_context: String.t option ;
      payload: Blob.t option }
    let make ~function_name  ?invocation_type  ?log_type  ?client_context 
      ?payload  () =
      { function_name; invocation_type; log_type; client_context; payload }
    let parse xml =
      Some
        {
          function_name =
            (Xml.required "FunctionName"
               (Util.option_bind (Xml.member "FunctionName" xml) String.parse));
          invocation_type =
            (Util.option_bind (Xml.member "X-Amz-Invocation-Type" xml)
               InvocationType.parse);
          log_type =
            (Util.option_bind (Xml.member "X-Amz-Log-Type" xml) LogType.parse);
          client_context =
            (Util.option_bind (Xml.member "X-Amz-Client-Context" xml)
               String.parse);
          payload = (Util.option_bind (Xml.member "Payload" xml) Blob.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.payload
              (fun f -> Query.Pair ("Payload", (Blob.to_query f)));
           Util.option_map v.client_context
             (fun f ->
                Query.Pair ("X-Amz-Client-Context", (String.to_query f)));
           Util.option_map v.log_type
             (fun f -> Query.Pair ("X-Amz-Log-Type", (LogType.to_query f)));
           Util.option_map v.invocation_type
             (fun f ->
                Query.Pair
                  ("X-Amz-Invocation-Type", (InvocationType.to_query f)));
           Some
             (Query.Pair ("FunctionName", (String.to_query v.function_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.payload
              (fun f -> ("payload", (Blob.to_json f)));
           Util.option_map v.client_context
             (fun f -> ("client_context", (String.to_json f)));
           Util.option_map v.log_type
             (fun f -> ("log_type", (LogType.to_json f)));
           Util.option_map v.invocation_type
             (fun f -> ("invocation_type", (InvocationType.to_json f)));
           Some ("function_name", (String.to_json v.function_name))])
    let of_json j =
      {
        function_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "function_name")));
        invocation_type =
          (Util.option_map (Json.lookup j "invocation_type")
             InvocationType.of_json);
        log_type =
          (Util.option_map (Json.lookup j "log_type") LogType.of_json);
        client_context =
          (Util.option_map (Json.lookup j "client_context") String.of_json);
        payload = (Util.option_map (Json.lookup j "payload") Blob.of_json)
      }
  end
module CreateFunctionRequest =
  struct
    type t =
      {
      function_name: String.t ;
      runtime: Runtime.t ;
      role: String.t ;
      handler: String.t ;
      description: String.t option ;
      timeout: Integer.t option ;
      memory_size: Integer.t option ;
      code: FunctionCode.t }
    let make ~function_name  ~runtime  ~role  ~handler  ?description 
      ?timeout  ?memory_size  ~code  () =
      {
        function_name;
        runtime;
        role;
        handler;
        description;
        timeout;
        memory_size;
        code
      }
    let parse xml =
      Some
        {
          function_name =
            (Xml.required "FunctionName"
               (Util.option_bind (Xml.member "FunctionName" xml) String.parse));
          runtime =
            (Xml.required "Runtime"
               (Util.option_bind (Xml.member "Runtime" xml) Runtime.parse));
          role =
            (Xml.required "Role"
               (Util.option_bind (Xml.member "Role" xml) String.parse));
          handler =
            (Xml.required "Handler"
               (Util.option_bind (Xml.member "Handler" xml) String.parse));
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          timeout =
            (Util.option_bind (Xml.member "Timeout" xml) Integer.parse);
          memory_size =
            (Util.option_bind (Xml.member "MemorySize" xml) Integer.parse);
          code =
            (Xml.required "Code"
               (Util.option_bind (Xml.member "Code" xml) FunctionCode.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Code", (FunctionCode.to_query v.code)));
           Util.option_map v.memory_size
             (fun f -> Query.Pair ("MemorySize", (Integer.to_query f)));
           Util.option_map v.timeout
             (fun f -> Query.Pair ("Timeout", (Integer.to_query f)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Some (Query.Pair ("Handler", (String.to_query v.handler)));
           Some (Query.Pair ("Role", (String.to_query v.role)));
           Some (Query.Pair ("Runtime", (Runtime.to_query v.runtime)));
           Some
             (Query.Pair ("FunctionName", (String.to_query v.function_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("code", (FunctionCode.to_json v.code));
           Util.option_map v.memory_size
             (fun f -> ("memory_size", (Integer.to_json f)));
           Util.option_map v.timeout
             (fun f -> ("timeout", (Integer.to_json f)));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Some ("handler", (String.to_json v.handler));
           Some ("role", (String.to_json v.role));
           Some ("runtime", (Runtime.to_json v.runtime));
           Some ("function_name", (String.to_json v.function_name))])
    let of_json j =
      {
        function_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "function_name")));
        runtime =
          (Runtime.of_json (Util.of_option_exn (Json.lookup j "runtime")));
        role = (String.of_json (Util.of_option_exn (Json.lookup j "role")));
        handler =
          (String.of_json (Util.of_option_exn (Json.lookup j "handler")));
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        timeout = (Util.option_map (Json.lookup j "timeout") Integer.of_json);
        memory_size =
          (Util.option_map (Json.lookup j "memory_size") Integer.of_json);
        code =
          (FunctionCode.of_json (Util.of_option_exn (Json.lookup j "code")))
      }
  end
module ListFunctionsRequest =
  struct
    type t = {
      marker: String.t option ;
      max_items: Integer.t option }
    let make ?marker  ?max_items  () = { marker; max_items }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          max_items =
            (Util.option_bind (Xml.member "MaxItems" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> Query.Pair ("MaxItems", (Integer.to_query f)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> ("max_items", (Integer.to_json f)));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        max_items =
          (Util.option_map (Json.lookup j "max_items") Integer.of_json)
      }
  end
module ServiceException =
  struct
    type t = {
      type_: String.t option ;
      message: String.t option }
    let make ?type_  ?message  () = { type_; message }
    let parse xml =
      Some
        {
          type_ = (Util.option_bind (Xml.member "Type" xml) String.parse);
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)));
           Util.option_map v.type_
             (fun f -> Query.Pair ("Type", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)));
           Util.option_map v.type_ (fun f -> ("type_", (String.to_json f)))])
    let of_json j =
      {
        type_ = (Util.option_map (Json.lookup j "type_") String.of_json);
        message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module GetEventSourceMappingRequest =
  struct
    type t = {
      u_u_i_d: String.t }
    let make ~u_u_i_d  () = { u_u_i_d }
    let parse xml =
      Some
        {
          u_u_i_d =
            (Xml.required "UUID"
               (Util.option_bind (Xml.member "UUID" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("UUID", (String.to_query v.u_u_i_d)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("u_u_i_d", (String.to_json v.u_u_i_d))])
    let of_json j =
      {
        u_u_i_d =
          (String.of_json (Util.of_option_exn (Json.lookup j "u_u_i_d")))
      }
  end
module DeleteFunctionRequest =
  struct
    type t = {
      function_name: String.t }
    let make ~function_name  () = { function_name }
    let parse xml =
      Some
        {
          function_name =
            (Xml.required "FunctionName"
               (Util.option_bind (Xml.member "FunctionName" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("FunctionName", (String.to_query v.function_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("function_name", (String.to_json v.function_name))])
    let of_json j =
      {
        function_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "function_name")))
      }
  end
module ResourceNotFoundException =
  struct
    type t = {
      type_: String.t option ;
      message: String.t option }
    let make ?type_  ?message  () = { type_; message }
    let parse xml =
      Some
        {
          type_ = (Util.option_bind (Xml.member "Type" xml) String.parse);
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)));
           Util.option_map v.type_
             (fun f -> Query.Pair ("Type", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)));
           Util.option_map v.type_ (fun f -> ("type_", (String.to_json f)))])
    let of_json j =
      {
        type_ = (Util.option_map (Json.lookup j "type_") String.of_json);
        message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module PolicyLengthExceededException =
  struct
    type t = {
      type_: String.t option ;
      message: String.t option }
    let make ?type_  ?message  () = { type_; message }
    let parse xml =
      Some
        {
          type_ = (Util.option_bind (Xml.member "Type" xml) String.parse);
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)));
           Util.option_map v.type_
             (fun f -> Query.Pair ("Type", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)));
           Util.option_map v.type_ (fun f -> ("type_", (String.to_json f)))])
    let of_json j =
      {
        type_ = (Util.option_map (Json.lookup j "type_") String.of_json);
        message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module DeleteEventSourceMappingRequest =
  struct
    type t = {
      u_u_i_d: String.t }
    let make ~u_u_i_d  () = { u_u_i_d }
    let parse xml =
      Some
        {
          u_u_i_d =
            (Xml.required "UUID"
               (Util.option_bind (Xml.member "UUID" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("UUID", (String.to_query v.u_u_i_d)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("u_u_i_d", (String.to_json v.u_u_i_d))])
    let of_json j =
      {
        u_u_i_d =
          (String.of_json (Util.of_option_exn (Json.lookup j "u_u_i_d")))
      }
  end
module GetPolicyResponse =
  struct
    type t = {
      policy: String.t option }
    let make ?policy  () = { policy }
    let parse xml =
      Some
        { policy = (Util.option_bind (Xml.member "Policy" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.policy
              (fun f -> Query.Pair ("Policy", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.policy
              (fun f -> ("policy", (String.to_json f)))])
    let of_json j =
      { policy = (Util.option_map (Json.lookup j "policy") String.of_json) }
  end
module InvocationResponse =
  struct
    type t =
      {
      status_code: Integer.t option ;
      function_error: String.t option ;
      log_result: String.t option ;
      payload: Blob.t option }
    let make ?status_code  ?function_error  ?log_result  ?payload  () =
      { status_code; function_error; log_result; payload }
    let parse xml =
      Some
        {
          status_code =
            (Util.option_bind (Xml.member "StatusCode" xml) Integer.parse);
          function_error =
            (Util.option_bind (Xml.member "X-Amz-Function-Error" xml)
               String.parse);
          log_result =
            (Util.option_bind (Xml.member "X-Amz-Log-Result" xml)
               String.parse);
          payload = (Util.option_bind (Xml.member "Payload" xml) Blob.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.payload
              (fun f -> Query.Pair ("Payload", (Blob.to_query f)));
           Util.option_map v.log_result
             (fun f -> Query.Pair ("X-Amz-Log-Result", (String.to_query f)));
           Util.option_map v.function_error
             (fun f ->
                Query.Pair ("X-Amz-Function-Error", (String.to_query f)));
           Util.option_map v.status_code
             (fun f -> Query.Pair ("StatusCode", (Integer.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.payload
              (fun f -> ("payload", (Blob.to_json f)));
           Util.option_map v.log_result
             (fun f -> ("log_result", (String.to_json f)));
           Util.option_map v.function_error
             (fun f -> ("function_error", (String.to_json f)));
           Util.option_map v.status_code
             (fun f -> ("status_code", (Integer.to_json f)))])
    let of_json j =
      {
        status_code =
          (Util.option_map (Json.lookup j "status_code") Integer.of_json);
        function_error =
          (Util.option_map (Json.lookup j "function_error") String.of_json);
        log_result =
          (Util.option_map (Json.lookup j "log_result") String.of_json);
        payload = (Util.option_map (Json.lookup j "payload") Blob.of_json)
      }
  end