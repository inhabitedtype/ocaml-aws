open Aws
open Aws.BaseTypes
open CalendarLib
type calendar = Calendar.t
module KeyValue =
  struct
    type t = {
      key: String.t option ;
      value: String.t option }
    let make ?key  ?value  () = { key; value }
    let parse xml =
      Some
        {
          key = (Util.option_bind (Xml.member "Key" xml) String.parse);
          value = (Util.option_bind (Xml.member "Value" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.value
              (fun f -> Query.Pair ("Value", (String.to_query f)));
           Util.option_map v.key
             (fun f -> Query.Pair ("Key", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.value (fun f -> ("value", (String.to_json f)));
           Util.option_map v.key (fun f -> ("key", (String.to_json f)))])
    let of_json j =
      {
        key = (Util.option_map (Json.lookup j "key") String.of_json);
        value = (Util.option_map (Json.lookup j "value") String.of_json)
      }
  end
module XmlStringList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module KeyValueList =
  struct
    type t = KeyValue.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map KeyValue.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list KeyValue.to_query v
    let to_json v = `List (List.map KeyValue.to_json v)
    let of_json j = Json.to_list KeyValue.of_json j
  end
module ScriptBootstrapActionConfig =
  struct
    type t = {
      path: String.t ;
      args: XmlStringList.t }
    let make ~path  ?(args= [])  () = { path; args }
    let parse xml =
      Some
        {
          path =
            (Xml.required "Path"
               (Util.option_bind (Xml.member "Path" xml) String.parse));
          args =
            (Util.of_option []
               (Util.option_bind (Xml.member "Args" xml) XmlStringList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Args.member", (XmlStringList.to_query v.args)));
           Some (Query.Pair ("Path", (String.to_query v.path)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("args", (XmlStringList.to_json v.args));
           Some ("path", (String.to_json v.path))])
    let of_json j =
      {
        path = (String.of_json (Util.of_option_exn (Json.lookup j "path")));
        args =
          (XmlStringList.of_json (Util.of_option_exn (Json.lookup j "args")))
      }
  end
module InstanceGroupState =
  struct
    type t =
      | PROVISIONING
      | BOOTSTRAPPING
      | RUNNING
      | RESIZING
      | SUSPENDED
      | TERMINATING
      | TERMINATED
      | ARRESTED
      | SHUTTING_DOWN
      | ENDED
    let str_to_t =
      [("ENDED", ENDED);
      ("SHUTTING_DOWN", SHUTTING_DOWN);
      ("ARRESTED", ARRESTED);
      ("TERMINATED", TERMINATED);
      ("TERMINATING", TERMINATING);
      ("SUSPENDED", SUSPENDED);
      ("RESIZING", RESIZING);
      ("RUNNING", RUNNING);
      ("BOOTSTRAPPING", BOOTSTRAPPING);
      ("PROVISIONING", PROVISIONING)]
    let t_to_str =
      [(ENDED, "ENDED");
      (SHUTTING_DOWN, "SHUTTING_DOWN");
      (ARRESTED, "ARRESTED");
      (TERMINATED, "TERMINATED");
      (TERMINATING, "TERMINATING");
      (SUSPENDED, "SUSPENDED");
      (RESIZING, "RESIZING");
      (RUNNING, "RUNNING");
      (BOOTSTRAPPING, "BOOTSTRAPPING");
      (PROVISIONING, "PROVISIONING")]
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
module InstanceRoleType =
  struct
    type t =
      | MASTER
      | CORE
      | TASK
    let str_to_t = [("TASK", TASK); ("CORE", CORE); ("MASTER", MASTER)]
    let t_to_str = [(TASK, "TASK"); (CORE, "CORE"); (MASTER, "MASTER")]
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
module MarketType =
  struct
    type t =
      | ON_DEMAND
      | SPOT
    let str_to_t = [("SPOT", SPOT); ("ON_DEMAND", ON_DEMAND)]
    let t_to_str = [(SPOT, "SPOT"); (ON_DEMAND, "ON_DEMAND")]
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
module ActionOnFailure =
  struct
    type t =
      | TERMINATE_JOB_FLOW
      | TERMINATE_CLUSTER
      | CANCEL_AND_WAIT
      | CONTINUE
    let str_to_t =
      [("CONTINUE", CONTINUE);
      ("CANCEL_AND_WAIT", CANCEL_AND_WAIT);
      ("TERMINATE_CLUSTER", TERMINATE_CLUSTER);
      ("TERMINATE_JOB_FLOW", TERMINATE_JOB_FLOW)]
    let t_to_str =
      [(CONTINUE, "CONTINUE");
      (CANCEL_AND_WAIT, "CANCEL_AND_WAIT");
      (TERMINATE_CLUSTER, "TERMINATE_CLUSTER");
      (TERMINATE_JOB_FLOW, "TERMINATE_JOB_FLOW")]
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
module HadoopJarStepConfig =
  struct
    type t =
      {
      properties: KeyValueList.t ;
      jar: String.t ;
      main_class: String.t option ;
      args: XmlStringList.t }
    let make ?(properties= [])  ~jar  ?main_class  ?(args= [])  () =
      { properties; jar; main_class; args }
    let parse xml =
      Some
        {
          properties =
            (Util.of_option []
               (Util.option_bind (Xml.member "Properties" xml)
                  KeyValueList.parse));
          jar =
            (Xml.required "Jar"
               (Util.option_bind (Xml.member "Jar" xml) String.parse));
          main_class =
            (Util.option_bind (Xml.member "MainClass" xml) String.parse);
          args =
            (Util.of_option []
               (Util.option_bind (Xml.member "Args" xml) XmlStringList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Args.member", (XmlStringList.to_query v.args)));
           Util.option_map v.main_class
             (fun f -> Query.Pair ("MainClass", (String.to_query f)));
           Some (Query.Pair ("Jar", (String.to_query v.jar)));
           Some
             (Query.Pair
                ("Properties.member", (KeyValueList.to_query v.properties)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("args", (XmlStringList.to_json v.args));
           Util.option_map v.main_class
             (fun f -> ("main_class", (String.to_json f)));
           Some ("jar", (String.to_json v.jar));
           Some ("properties", (KeyValueList.to_json v.properties))])
    let of_json j =
      {
        properties =
          (KeyValueList.of_json
             (Util.of_option_exn (Json.lookup j "properties")));
        jar = (String.of_json (Util.of_option_exn (Json.lookup j "jar")));
        main_class =
          (Util.option_map (Json.lookup j "main_class") String.of_json);
        args =
          (XmlStringList.of_json (Util.of_option_exn (Json.lookup j "args")))
      }
  end
module StepExecutionState =
  struct
    type t =
      | PENDING
      | RUNNING
      | CONTINUE
      | COMPLETED
      | CANCELLED
      | FAILED
      | INTERRUPTED
    let str_to_t =
      [("INTERRUPTED", INTERRUPTED);
      ("FAILED", FAILED);
      ("CANCELLED", CANCELLED);
      ("COMPLETED", COMPLETED);
      ("CONTINUE", CONTINUE);
      ("RUNNING", RUNNING);
      ("PENDING", PENDING)]
    let t_to_str =
      [(INTERRUPTED, "INTERRUPTED");
      (FAILED, "FAILED");
      (CANCELLED, "CANCELLED");
      (COMPLETED, "COMPLETED");
      (CONTINUE, "CONTINUE");
      (RUNNING, "RUNNING");
      (PENDING, "PENDING")]
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
module StringMap =
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
module StepStateChangeReasonCode =
  struct
    type t =
      | NONE
    let str_to_t = [("NONE", NONE)]
    let t_to_str = [(NONE, "NONE")]
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
module InstanceGroupStateChangeReasonCode =
  struct
    type t =
      | INTERNAL_ERROR
      | VALIDATION_ERROR
      | INSTANCE_FAILURE
      | CLUSTER_TERMINATED
    let str_to_t =
      [("CLUSTER_TERMINATED", CLUSTER_TERMINATED);
      ("INSTANCE_FAILURE", INSTANCE_FAILURE);
      ("VALIDATION_ERROR", VALIDATION_ERROR);
      ("INTERNAL_ERROR", INTERNAL_ERROR)]
    let t_to_str =
      [(CLUSTER_TERMINATED, "CLUSTER_TERMINATED");
      (INSTANCE_FAILURE, "INSTANCE_FAILURE");
      (VALIDATION_ERROR, "VALIDATION_ERROR");
      (INTERNAL_ERROR, "INTERNAL_ERROR")]
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
module InstanceStateChangeReasonCode =
  struct
    type t =
      | INTERNAL_ERROR
      | VALIDATION_ERROR
      | INSTANCE_FAILURE
      | BOOTSTRAP_FAILURE
      | CLUSTER_TERMINATED
    let str_to_t =
      [("CLUSTER_TERMINATED", CLUSTER_TERMINATED);
      ("BOOTSTRAP_FAILURE", BOOTSTRAP_FAILURE);
      ("INSTANCE_FAILURE", INSTANCE_FAILURE);
      ("VALIDATION_ERROR", VALIDATION_ERROR);
      ("INTERNAL_ERROR", INTERNAL_ERROR)]
    let t_to_str =
      [(CLUSTER_TERMINATED, "CLUSTER_TERMINATED");
      (BOOTSTRAP_FAILURE, "BOOTSTRAP_FAILURE");
      (INSTANCE_FAILURE, "INSTANCE_FAILURE");
      (VALIDATION_ERROR, "VALIDATION_ERROR");
      (INTERNAL_ERROR, "INTERNAL_ERROR")]
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
module ClusterStateChangeReasonCode =
  struct
    type t =
      | INTERNAL_ERROR
      | VALIDATION_ERROR
      | INSTANCE_FAILURE
      | BOOTSTRAP_FAILURE
      | USER_REQUEST
      | STEP_FAILURE
      | ALL_STEPS_COMPLETED
    let str_to_t =
      [("ALL_STEPS_COMPLETED", ALL_STEPS_COMPLETED);
      ("STEP_FAILURE", STEP_FAILURE);
      ("USER_REQUEST", USER_REQUEST);
      ("BOOTSTRAP_FAILURE", BOOTSTRAP_FAILURE);
      ("INSTANCE_FAILURE", INSTANCE_FAILURE);
      ("VALIDATION_ERROR", VALIDATION_ERROR);
      ("INTERNAL_ERROR", INTERNAL_ERROR)]
    let t_to_str =
      [(ALL_STEPS_COMPLETED, "ALL_STEPS_COMPLETED");
      (STEP_FAILURE, "STEP_FAILURE");
      (USER_REQUEST, "USER_REQUEST");
      (BOOTSTRAP_FAILURE, "BOOTSTRAP_FAILURE");
      (INSTANCE_FAILURE, "INSTANCE_FAILURE");
      (VALIDATION_ERROR, "VALIDATION_ERROR");
      (INTERNAL_ERROR, "INTERNAL_ERROR")]
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
module BootstrapActionConfig =
  struct
    type t =
      {
      name: String.t ;
      script_bootstrap_action: ScriptBootstrapActionConfig.t }
    let make ~name  ~script_bootstrap_action  () =
      { name; script_bootstrap_action }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          script_bootstrap_action =
            (Xml.required "ScriptBootstrapAction"
               (Util.option_bind (Xml.member "ScriptBootstrapAction" xml)
                  ScriptBootstrapActionConfig.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ScriptBootstrapAction",
                   (ScriptBootstrapActionConfig.to_query
                      v.script_bootstrap_action)));
           Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("script_bootstrap_action",
                (ScriptBootstrapActionConfig.to_json
                   v.script_bootstrap_action));
           Some ("name", (String.to_json v.name))])
    let of_json j =
      {
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        script_bootstrap_action =
          (ScriptBootstrapActionConfig.of_json
             (Util.of_option_exn (Json.lookup j "script_bootstrap_action")))
      }
  end
module InstanceGroupDetail =
  struct
    type t =
      {
      instance_group_id: String.t option ;
      name: String.t option ;
      market: MarketType.t ;
      instance_role: InstanceRoleType.t ;
      bid_price: String.t option ;
      instance_type: String.t ;
      instance_request_count: Integer.t ;
      instance_running_count: Integer.t ;
      state: InstanceGroupState.t ;
      last_state_change_reason: String.t option ;
      creation_date_time: DateTime.t ;
      start_date_time: DateTime.t option ;
      ready_date_time: DateTime.t option ;
      end_date_time: DateTime.t option }
    let make ?instance_group_id  ?name  ~market  ~instance_role  ?bid_price
      ~instance_type  ~instance_request_count  ~instance_running_count
      ~state  ?last_state_change_reason  ~creation_date_time
      ?start_date_time  ?ready_date_time  ?end_date_time  () =
      {
        instance_group_id;
        name;
        market;
        instance_role;
        bid_price;
        instance_type;
        instance_request_count;
        instance_running_count;
        state;
        last_state_change_reason;
        creation_date_time;
        start_date_time;
        ready_date_time;
        end_date_time
      }
    let parse xml =
      Some
        {
          instance_group_id =
            (Util.option_bind (Xml.member "InstanceGroupId" xml) String.parse);
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          market =
            (Xml.required "Market"
               (Util.option_bind (Xml.member "Market" xml) MarketType.parse));
          instance_role =
            (Xml.required "InstanceRole"
               (Util.option_bind (Xml.member "InstanceRole" xml)
                  InstanceRoleType.parse));
          bid_price =
            (Util.option_bind (Xml.member "BidPrice" xml) String.parse);
          instance_type =
            (Xml.required "InstanceType"
               (Util.option_bind (Xml.member "InstanceType" xml) String.parse));
          instance_request_count =
            (Xml.required "InstanceRequestCount"
               (Util.option_bind (Xml.member "InstanceRequestCount" xml)
                  Integer.parse));
          instance_running_count =
            (Xml.required "InstanceRunningCount"
               (Util.option_bind (Xml.member "InstanceRunningCount" xml)
                  Integer.parse));
          state =
            (Xml.required "State"
               (Util.option_bind (Xml.member "State" xml)
                  InstanceGroupState.parse));
          last_state_change_reason =
            (Util.option_bind (Xml.member "LastStateChangeReason" xml)
               String.parse);
          creation_date_time =
            (Xml.required "CreationDateTime"
               (Util.option_bind (Xml.member "CreationDateTime" xml)
                  DateTime.parse));
          start_date_time =
            (Util.option_bind (Xml.member "StartDateTime" xml) DateTime.parse);
          ready_date_time =
            (Util.option_bind (Xml.member "ReadyDateTime" xml) DateTime.parse);
          end_date_time =
            (Util.option_bind (Xml.member "EndDateTime" xml) DateTime.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.end_date_time
              (fun f -> Query.Pair ("EndDateTime", (DateTime.to_query f)));
           Util.option_map v.ready_date_time
             (fun f -> Query.Pair ("ReadyDateTime", (DateTime.to_query f)));
           Util.option_map v.start_date_time
             (fun f -> Query.Pair ("StartDateTime", (DateTime.to_query f)));
           Some
             (Query.Pair
                ("CreationDateTime",
                  (DateTime.to_query v.creation_date_time)));
           Util.option_map v.last_state_change_reason
             (fun f ->
                Query.Pair ("LastStateChangeReason", (String.to_query f)));
           Some (Query.Pair ("State", (InstanceGroupState.to_query v.state)));
           Some
             (Query.Pair
                ("InstanceRunningCount",
                  (Integer.to_query v.instance_running_count)));
           Some
             (Query.Pair
                ("InstanceRequestCount",
                  (Integer.to_query v.instance_request_count)));
           Some
             (Query.Pair ("InstanceType", (String.to_query v.instance_type)));
           Util.option_map v.bid_price
             (fun f -> Query.Pair ("BidPrice", (String.to_query f)));
           Some
             (Query.Pair
                ("InstanceRole", (InstanceRoleType.to_query v.instance_role)));
           Some (Query.Pair ("Market", (MarketType.to_query v.market)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)));
           Util.option_map v.instance_group_id
             (fun f -> Query.Pair ("InstanceGroupId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.end_date_time
              (fun f -> ("end_date_time", (DateTime.to_json f)));
           Util.option_map v.ready_date_time
             (fun f -> ("ready_date_time", (DateTime.to_json f)));
           Util.option_map v.start_date_time
             (fun f -> ("start_date_time", (DateTime.to_json f)));
           Some
             ("creation_date_time", (DateTime.to_json v.creation_date_time));
           Util.option_map v.last_state_change_reason
             (fun f -> ("last_state_change_reason", (String.to_json f)));
           Some ("state", (InstanceGroupState.to_json v.state));
           Some
             ("instance_running_count",
               (Integer.to_json v.instance_running_count));
           Some
             ("instance_request_count",
               (Integer.to_json v.instance_request_count));
           Some ("instance_type", (String.to_json v.instance_type));
           Util.option_map v.bid_price
             (fun f -> ("bid_price", (String.to_json f)));
           Some ("instance_role", (InstanceRoleType.to_json v.instance_role));
           Some ("market", (MarketType.to_json v.market));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)));
           Util.option_map v.instance_group_id
             (fun f -> ("instance_group_id", (String.to_json f)))])
    let of_json j =
      {
        instance_group_id =
          (Util.option_map (Json.lookup j "instance_group_id") String.of_json);
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        market =
          (MarketType.of_json (Util.of_option_exn (Json.lookup j "market")));
        instance_role =
          (InstanceRoleType.of_json
             (Util.of_option_exn (Json.lookup j "instance_role")));
        bid_price =
          (Util.option_map (Json.lookup j "bid_price") String.of_json);
        instance_type =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "instance_type")));
        instance_request_count =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "instance_request_count")));
        instance_running_count =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "instance_running_count")));
        state =
          (InstanceGroupState.of_json
             (Util.of_option_exn (Json.lookup j "state")));
        last_state_change_reason =
          (Util.option_map (Json.lookup j "last_state_change_reason")
             String.of_json);
        creation_date_time =
          (DateTime.of_json
             (Util.of_option_exn (Json.lookup j "creation_date_time")));
        start_date_time =
          (Util.option_map (Json.lookup j "start_date_time") DateTime.of_json);
        ready_date_time =
          (Util.option_map (Json.lookup j "ready_date_time") DateTime.of_json);
        end_date_time =
          (Util.option_map (Json.lookup j "end_date_time") DateTime.of_json)
      }
  end
module StepConfig =
  struct
    type t =
      {
      name: String.t ;
      action_on_failure: ActionOnFailure.t option ;
      hadoop_jar_step: HadoopJarStepConfig.t }
    let make ~name  ?action_on_failure  ~hadoop_jar_step  () =
      { name; action_on_failure; hadoop_jar_step }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          action_on_failure =
            (Util.option_bind (Xml.member "ActionOnFailure" xml)
               ActionOnFailure.parse);
          hadoop_jar_step =
            (Xml.required "HadoopJarStep"
               (Util.option_bind (Xml.member "HadoopJarStep" xml)
                  HadoopJarStepConfig.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("HadoopJarStep",
                   (HadoopJarStepConfig.to_query v.hadoop_jar_step)));
           Util.option_map v.action_on_failure
             (fun f ->
                Query.Pair ("ActionOnFailure", (ActionOnFailure.to_query f)));
           Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("hadoop_jar_step",
                (HadoopJarStepConfig.to_json v.hadoop_jar_step));
           Util.option_map v.action_on_failure
             (fun f -> ("action_on_failure", (ActionOnFailure.to_json f)));
           Some ("name", (String.to_json v.name))])
    let of_json j =
      {
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        action_on_failure =
          (Util.option_map (Json.lookup j "action_on_failure")
             ActionOnFailure.of_json);
        hadoop_jar_step =
          (HadoopJarStepConfig.of_json
             (Util.of_option_exn (Json.lookup j "hadoop_jar_step")))
      }
  end
module StepExecutionStatusDetail =
  struct
    type t =
      {
      state: StepExecutionState.t ;
      creation_date_time: DateTime.t ;
      start_date_time: DateTime.t option ;
      end_date_time: DateTime.t option ;
      last_state_change_reason: String.t option }
    let make ~state  ~creation_date_time  ?start_date_time  ?end_date_time
      ?last_state_change_reason  () =
      {
        state;
        creation_date_time;
        start_date_time;
        end_date_time;
        last_state_change_reason
      }
    let parse xml =
      Some
        {
          state =
            (Xml.required "State"
               (Util.option_bind (Xml.member "State" xml)
                  StepExecutionState.parse));
          creation_date_time =
            (Xml.required "CreationDateTime"
               (Util.option_bind (Xml.member "CreationDateTime" xml)
                  DateTime.parse));
          start_date_time =
            (Util.option_bind (Xml.member "StartDateTime" xml) DateTime.parse);
          end_date_time =
            (Util.option_bind (Xml.member "EndDateTime" xml) DateTime.parse);
          last_state_change_reason =
            (Util.option_bind (Xml.member "LastStateChangeReason" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.last_state_change_reason
              (fun f ->
                 Query.Pair ("LastStateChangeReason", (String.to_query f)));
           Util.option_map v.end_date_time
             (fun f -> Query.Pair ("EndDateTime", (DateTime.to_query f)));
           Util.option_map v.start_date_time
             (fun f -> Query.Pair ("StartDateTime", (DateTime.to_query f)));
           Some
             (Query.Pair
                ("CreationDateTime",
                  (DateTime.to_query v.creation_date_time)));
           Some (Query.Pair ("State", (StepExecutionState.to_query v.state)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.last_state_change_reason
              (fun f -> ("last_state_change_reason", (String.to_json f)));
           Util.option_map v.end_date_time
             (fun f -> ("end_date_time", (DateTime.to_json f)));
           Util.option_map v.start_date_time
             (fun f -> ("start_date_time", (DateTime.to_json f)));
           Some
             ("creation_date_time", (DateTime.to_json v.creation_date_time));
           Some ("state", (StepExecutionState.to_json v.state))])
    let of_json j =
      {
        state =
          (StepExecutionState.of_json
             (Util.of_option_exn (Json.lookup j "state")));
        creation_date_time =
          (DateTime.of_json
             (Util.of_option_exn (Json.lookup j "creation_date_time")));
        start_date_time =
          (Util.option_map (Json.lookup j "start_date_time") DateTime.of_json);
        end_date_time =
          (Util.option_map (Json.lookup j "end_date_time") DateTime.of_json);
        last_state_change_reason =
          (Util.option_map (Json.lookup j "last_state_change_reason")
             String.of_json)
      }
  end
module StringList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module rec ConfigurationList : sig
         type t = Configuration.t list
         val parse : Ezxmlm.nodes -> Configuration.t list option
         val to_query : t -> Query.t
         val to_json : t -> Json.t
         val of_json : Json.t -> t
         val make : t -> unit -> t
end =
  struct
    type t = Configuration.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map Configuration.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Configuration.to_query v
    let to_json v = `List (List.map Configuration.to_json v)
    let of_json j = Json.to_list Configuration.of_json j
  end
   and Configuration : sig
     type t =
       {
         classification: String.t option ;
         configurations: ConfigurationList.t ;
         properties: StringMap.t option }
     val parse : Ezxmlm.nodes -> t option
     val to_query : t -> Query.t
     val to_json : t -> Json.t
     val of_json : Json.t -> t
     val make : ?classification:string -> ?configurations:ConfigurationList.t -> ?properties:StringMap.t -> unit -> t
   end  =
  struct
    type t =
      {
      classification: String.t option ;
      configurations: ConfigurationList.t ;
      properties: StringMap.t option }
    let make ?classification  ?(configurations= [])  ?properties  () =
      { classification; configurations; properties }
    let parse xml =
      Some
        {
          classification =
            (Util.option_bind (Xml.member "Classification" xml) String.parse);
          configurations =
            (Util.of_option []
               (Util.option_bind (Xml.member "Configurations" xml)
                  ConfigurationList.parse));
          properties =
            (Util.option_bind (Xml.member "Properties" xml) StringMap.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.properties
              (fun f -> Query.Pair ("Properties", (StringMap.to_query f)));
           Some
             (Query.Pair
                ("Configurations.member",
                  (ConfigurationList.to_query v.configurations)));
           Util.option_map v.classification
             (fun f -> Query.Pair ("Classification", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.properties
              (fun f -> ("properties", (StringMap.to_json f)));
           Some
             ("configurations", (ConfigurationList.to_json v.configurations));
           Util.option_map v.classification
             (fun f -> ("classification", (String.to_json f)))])
    let of_json j =
      {
        classification =
          (Util.option_map (Json.lookup j "classification") String.of_json);
        configurations =
          (ConfigurationList.of_json
             (Util.of_option_exn (Json.lookup j "configurations")));
        properties =
          (Util.option_map (Json.lookup j "properties") StringMap.of_json)
      }
  end
module StepState =
  struct
    type t =
      | PENDING
      | RUNNING
      | COMPLETED
      | CANCELLED
      | FAILED
      | INTERRUPTED
    let str_to_t =
      [("INTERRUPTED", INTERRUPTED);
      ("FAILED", FAILED);
      ("CANCELLED", CANCELLED);
      ("COMPLETED", COMPLETED);
      ("RUNNING", RUNNING);
      ("PENDING", PENDING)]
    let t_to_str =
      [(INTERRUPTED, "INTERRUPTED");
      (FAILED, "FAILED");
      (CANCELLED, "CANCELLED");
      (COMPLETED, "COMPLETED");
      (RUNNING, "RUNNING");
      (PENDING, "PENDING")]
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
module StepStateChangeReason =
  struct
    type t =
      {
      code: StepStateChangeReasonCode.t option ;
      message: String.t option }
    let make ?code  ?message  () = { code; message }
    let parse xml =
      Some
        {
          code =
            (Util.option_bind (Xml.member "Code" xml)
               StepStateChangeReasonCode.parse);
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)));
           Util.option_map v.code
             (fun f ->
                Query.Pair ("Code", (StepStateChangeReasonCode.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)));
           Util.option_map v.code
             (fun f -> ("code", (StepStateChangeReasonCode.to_json f)))])
    let of_json j =
      {
        code =
          (Util.option_map (Json.lookup j "code")
             StepStateChangeReasonCode.of_json);
        message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module StepTimeline =
  struct
    type t =
      {
      creation_date_time: DateTime.t option ;
      start_date_time: DateTime.t option ;
      end_date_time: DateTime.t option }
    let make ?creation_date_time  ?start_date_time  ?end_date_time  () =
      { creation_date_time; start_date_time; end_date_time }
    let parse xml =
      Some
        {
          creation_date_time =
            (Util.option_bind (Xml.member "CreationDateTime" xml)
               DateTime.parse);
          start_date_time =
            (Util.option_bind (Xml.member "StartDateTime" xml) DateTime.parse);
          end_date_time =
            (Util.option_bind (Xml.member "EndDateTime" xml) DateTime.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.end_date_time
              (fun f -> Query.Pair ("EndDateTime", (DateTime.to_query f)));
           Util.option_map v.start_date_time
             (fun f -> Query.Pair ("StartDateTime", (DateTime.to_query f)));
           Util.option_map v.creation_date_time
             (fun f -> Query.Pair ("CreationDateTime", (DateTime.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.end_date_time
              (fun f -> ("end_date_time", (DateTime.to_json f)));
           Util.option_map v.start_date_time
             (fun f -> ("start_date_time", (DateTime.to_json f)));
           Util.option_map v.creation_date_time
             (fun f -> ("creation_date_time", (DateTime.to_json f)))])
    let of_json j =
      {
        creation_date_time =
          (Util.option_map (Json.lookup j "creation_date_time")
             DateTime.of_json);
        start_date_time =
          (Util.option_map (Json.lookup j "start_date_time") DateTime.of_json);
        end_date_time =
          (Util.option_map (Json.lookup j "end_date_time") DateTime.of_json)
      }
  end
module InstanceGroupStateChangeReason =
  struct
    type t =
      {
      code: InstanceGroupStateChangeReasonCode.t option ;
      message: String.t option }
    let make ?code  ?message  () = { code; message }
    let parse xml =
      Some
        {
          code =
            (Util.option_bind (Xml.member "Code" xml)
               InstanceGroupStateChangeReasonCode.parse);
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)));
           Util.option_map v.code
             (fun f ->
                Query.Pair
                  ("Code", (InstanceGroupStateChangeReasonCode.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)));
           Util.option_map v.code
             (fun f ->
                ("code", (InstanceGroupStateChangeReasonCode.to_json f)))])
    let of_json j =
      {
        code =
          (Util.option_map (Json.lookup j "code")
             InstanceGroupStateChangeReasonCode.of_json);
        message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module InstanceGroupTimeline =
  struct
    type t =
      {
      creation_date_time: DateTime.t option ;
      ready_date_time: DateTime.t option ;
      end_date_time: DateTime.t option }
    let make ?creation_date_time  ?ready_date_time  ?end_date_time  () =
      { creation_date_time; ready_date_time; end_date_time }
    let parse xml =
      Some
        {
          creation_date_time =
            (Util.option_bind (Xml.member "CreationDateTime" xml)
               DateTime.parse);
          ready_date_time =
            (Util.option_bind (Xml.member "ReadyDateTime" xml) DateTime.parse);
          end_date_time =
            (Util.option_bind (Xml.member "EndDateTime" xml) DateTime.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.end_date_time
              (fun f -> Query.Pair ("EndDateTime", (DateTime.to_query f)));
           Util.option_map v.ready_date_time
             (fun f -> Query.Pair ("ReadyDateTime", (DateTime.to_query f)));
           Util.option_map v.creation_date_time
             (fun f -> Query.Pair ("CreationDateTime", (DateTime.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.end_date_time
              (fun f -> ("end_date_time", (DateTime.to_json f)));
           Util.option_map v.ready_date_time
             (fun f -> ("ready_date_time", (DateTime.to_json f)));
           Util.option_map v.creation_date_time
             (fun f -> ("creation_date_time", (DateTime.to_json f)))])
    let of_json j =
      {
        creation_date_time =
          (Util.option_map (Json.lookup j "creation_date_time")
             DateTime.of_json);
        ready_date_time =
          (Util.option_map (Json.lookup j "ready_date_time") DateTime.of_json);
        end_date_time =
          (Util.option_map (Json.lookup j "end_date_time") DateTime.of_json)
      }
  end
module InstanceState =
  struct
    type t =
      | AWAITING_FULFILLMENT
      | PROVISIONING
      | BOOTSTRAPPING
      | RUNNING
      | TERMINATED
    let str_to_t =
      [("TERMINATED", TERMINATED);
      ("RUNNING", RUNNING);
      ("BOOTSTRAPPING", BOOTSTRAPPING);
      ("PROVISIONING", PROVISIONING);
      ("AWAITING_FULFILLMENT", AWAITING_FULFILLMENT)]
    let t_to_str =
      [(TERMINATED, "TERMINATED");
      (RUNNING, "RUNNING");
      (BOOTSTRAPPING, "BOOTSTRAPPING");
      (PROVISIONING, "PROVISIONING");
      (AWAITING_FULFILLMENT, "AWAITING_FULFILLMENT")]
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
module InstanceStateChangeReason =
  struct
    type t =
      {
      code: InstanceStateChangeReasonCode.t option ;
      message: String.t option }
    let make ?code  ?message  () = { code; message }
    let parse xml =
      Some
        {
          code =
            (Util.option_bind (Xml.member "Code" xml)
               InstanceStateChangeReasonCode.parse);
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)));
           Util.option_map v.code
             (fun f ->
                Query.Pair
                  ("Code", (InstanceStateChangeReasonCode.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)));
           Util.option_map v.code
             (fun f -> ("code", (InstanceStateChangeReasonCode.to_json f)))])
    let of_json j =
      {
        code =
          (Util.option_map (Json.lookup j "code")
             InstanceStateChangeReasonCode.of_json);
        message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module InstanceTimeline =
  struct
    type t =
      {
      creation_date_time: DateTime.t option ;
      ready_date_time: DateTime.t option ;
      end_date_time: DateTime.t option }
    let make ?creation_date_time  ?ready_date_time  ?end_date_time  () =
      { creation_date_time; ready_date_time; end_date_time }
    let parse xml =
      Some
        {
          creation_date_time =
            (Util.option_bind (Xml.member "CreationDateTime" xml)
               DateTime.parse);
          ready_date_time =
            (Util.option_bind (Xml.member "ReadyDateTime" xml) DateTime.parse);
          end_date_time =
            (Util.option_bind (Xml.member "EndDateTime" xml) DateTime.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.end_date_time
              (fun f -> Query.Pair ("EndDateTime", (DateTime.to_query f)));
           Util.option_map v.ready_date_time
             (fun f -> Query.Pair ("ReadyDateTime", (DateTime.to_query f)));
           Util.option_map v.creation_date_time
             (fun f -> Query.Pair ("CreationDateTime", (DateTime.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.end_date_time
              (fun f -> ("end_date_time", (DateTime.to_json f)));
           Util.option_map v.ready_date_time
             (fun f -> ("ready_date_time", (DateTime.to_json f)));
           Util.option_map v.creation_date_time
             (fun f -> ("creation_date_time", (DateTime.to_json f)))])
    let of_json j =
      {
        creation_date_time =
          (Util.option_map (Json.lookup j "creation_date_time")
             DateTime.of_json);
        ready_date_time =
          (Util.option_map (Json.lookup j "ready_date_time") DateTime.of_json);
        end_date_time =
          (Util.option_map (Json.lookup j "end_date_time") DateTime.of_json)
      }
  end
module ClusterState =
  struct
    type t =
      | STARTING
      | BOOTSTRAPPING
      | RUNNING
      | WAITING
      | TERMINATING
      | TERMINATED
      | TERMINATED_WITH_ERRORS
    let str_to_t =
      [("TERMINATED_WITH_ERRORS", TERMINATED_WITH_ERRORS);
      ("TERMINATED", TERMINATED);
      ("TERMINATING", TERMINATING);
      ("WAITING", WAITING);
      ("RUNNING", RUNNING);
      ("BOOTSTRAPPING", BOOTSTRAPPING);
      ("STARTING", STARTING)]
    let t_to_str =
      [(TERMINATED_WITH_ERRORS, "TERMINATED_WITH_ERRORS");
      (TERMINATED, "TERMINATED");
      (TERMINATING, "TERMINATING");
      (WAITING, "WAITING");
      (RUNNING, "RUNNING");
      (BOOTSTRAPPING, "BOOTSTRAPPING");
      (STARTING, "STARTING")]
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
module ClusterStateChangeReason =
  struct
    type t =
      {
      code: ClusterStateChangeReasonCode.t option ;
      message: String.t option }
    let make ?code  ?message  () = { code; message }
    let parse xml =
      Some
        {
          code =
            (Util.option_bind (Xml.member "Code" xml)
               ClusterStateChangeReasonCode.parse);
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)));
           Util.option_map v.code
             (fun f ->
                Query.Pair
                  ("Code", (ClusterStateChangeReasonCode.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)));
           Util.option_map v.code
             (fun f -> ("code", (ClusterStateChangeReasonCode.to_json f)))])
    let of_json j =
      {
        code =
          (Util.option_map (Json.lookup j "code")
             ClusterStateChangeReasonCode.of_json);
        message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ClusterTimeline =
  struct
    type t =
      {
      creation_date_time: DateTime.t option ;
      ready_date_time: DateTime.t option ;
      end_date_time: DateTime.t option }
    let make ?creation_date_time  ?ready_date_time  ?end_date_time  () =
      { creation_date_time; ready_date_time; end_date_time }
    let parse xml =
      Some
        {
          creation_date_time =
            (Util.option_bind (Xml.member "CreationDateTime" xml)
               DateTime.parse);
          ready_date_time =
            (Util.option_bind (Xml.member "ReadyDateTime" xml) DateTime.parse);
          end_date_time =
            (Util.option_bind (Xml.member "EndDateTime" xml) DateTime.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.end_date_time
              (fun f -> Query.Pair ("EndDateTime", (DateTime.to_query f)));
           Util.option_map v.ready_date_time
             (fun f -> Query.Pair ("ReadyDateTime", (DateTime.to_query f)));
           Util.option_map v.creation_date_time
             (fun f -> Query.Pair ("CreationDateTime", (DateTime.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.end_date_time
              (fun f -> ("end_date_time", (DateTime.to_json f)));
           Util.option_map v.ready_date_time
             (fun f -> ("ready_date_time", (DateTime.to_json f)));
           Util.option_map v.creation_date_time
             (fun f -> ("creation_date_time", (DateTime.to_json f)))])
    let of_json j =
      {
        creation_date_time =
          (Util.option_map (Json.lookup j "creation_date_time")
             DateTime.of_json);
        ready_date_time =
          (Util.option_map (Json.lookup j "ready_date_time") DateTime.of_json);
        end_date_time =
          (Util.option_map (Json.lookup j "end_date_time") DateTime.of_json)
      }
  end
module BootstrapActionDetail =
  struct
    type t = {
      bootstrap_action_config: BootstrapActionConfig.t option }
    let make ?bootstrap_action_config  () = { bootstrap_action_config }
    let parse xml =
      Some
        {
          bootstrap_action_config =
            (Util.option_bind (Xml.member "BootstrapActionConfig" xml)
               BootstrapActionConfig.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.bootstrap_action_config
              (fun f ->
                 Query.Pair
                   ("BootstrapActionConfig",
                     (BootstrapActionConfig.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.bootstrap_action_config
              (fun f ->
                 ("bootstrap_action_config",
                   (BootstrapActionConfig.to_json f)))])
    let of_json j =
      {
        bootstrap_action_config =
          (Util.option_map (Json.lookup j "bootstrap_action_config")
             BootstrapActionConfig.of_json)
      }
  end
module JobFlowExecutionState =
  struct
    type t =
      | STARTING
      | BOOTSTRAPPING
      | RUNNING
      | WAITING
      | SHUTTING_DOWN
      | TERMINATED
      | COMPLETED
      | FAILED
    let str_to_t =
      [("FAILED", FAILED);
      ("COMPLETED", COMPLETED);
      ("TERMINATED", TERMINATED);
      ("SHUTTING_DOWN", SHUTTING_DOWN);
      ("WAITING", WAITING);
      ("RUNNING", RUNNING);
      ("BOOTSTRAPPING", BOOTSTRAPPING);
      ("STARTING", STARTING)]
    let t_to_str =
      [(FAILED, "FAILED");
      (COMPLETED, "COMPLETED");
      (TERMINATED, "TERMINATED");
      (SHUTTING_DOWN, "SHUTTING_DOWN");
      (WAITING, "WAITING");
      (RUNNING, "RUNNING");
      (BOOTSTRAPPING, "BOOTSTRAPPING");
      (STARTING, "STARTING")]
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
module InstanceGroupDetailList =
  struct
    type t = InstanceGroupDetail.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map InstanceGroupDetail.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list InstanceGroupDetail.to_query v
    let to_json v = `List (List.map InstanceGroupDetail.to_json v)
    let of_json j = Json.to_list InstanceGroupDetail.of_json j
  end
module PlacementType =
  struct
    type t = {
      availability_zone: String.t }
    let make ~availability_zone  () = { availability_zone }
    let parse xml =
      Some
        {
          availability_zone =
            (Xml.required "AvailabilityZone"
               (Util.option_bind (Xml.member "AvailabilityZone" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("AvailabilityZone", (String.to_query v.availability_zone)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("availability_zone", (String.to_json v.availability_zone))])
    let of_json j =
      {
        availability_zone =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "availability_zone")))
      }
  end
module StepDetail =
  struct
    type t =
      {
      step_config: StepConfig.t ;
      execution_status_detail: StepExecutionStatusDetail.t }
    let make ~step_config  ~execution_status_detail  () =
      { step_config; execution_status_detail }
    let parse xml =
      Some
        {
          step_config =
            (Xml.required "StepConfig"
               (Util.option_bind (Xml.member "StepConfig" xml)
                  StepConfig.parse));
          execution_status_detail =
            (Xml.required "ExecutionStatusDetail"
               (Util.option_bind (Xml.member "ExecutionStatusDetail" xml)
                  StepExecutionStatusDetail.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ExecutionStatusDetail",
                   (StepExecutionStatusDetail.to_query
                      v.execution_status_detail)));
           Some
             (Query.Pair ("StepConfig", (StepConfig.to_query v.step_config)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("execution_status_detail",
                (StepExecutionStatusDetail.to_json v.execution_status_detail));
           Some ("step_config", (StepConfig.to_json v.step_config))])
    let of_json j =
      {
        step_config =
          (StepConfig.of_json
             (Util.of_option_exn (Json.lookup j "step_config")));
        execution_status_detail =
          (StepExecutionStatusDetail.of_json
             (Util.of_option_exn (Json.lookup j "execution_status_detail")))
      }
  end
module Application =
  struct
    type t =
      {
      name: String.t option ;
      version: String.t option ;
      args: StringList.t ;
      additional_info: StringMap.t option }
    let make ?name  ?version  ?(args= [])  ?additional_info  () =
      { name; version; args; additional_info }
    let parse xml =
      Some
        {
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          version =
            (Util.option_bind (Xml.member "Version" xml) String.parse);
          args =
            (Util.of_option []
               (Util.option_bind (Xml.member "Args" xml) StringList.parse));
          additional_info =
            (Util.option_bind (Xml.member "AdditionalInfo" xml)
               StringMap.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.additional_info
              (fun f -> Query.Pair ("AdditionalInfo", (StringMap.to_query f)));
           Some (Query.Pair ("Args.member", (StringList.to_query v.args)));
           Util.option_map v.version
             (fun f -> Query.Pair ("Version", (String.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.additional_info
              (fun f -> ("additional_info", (StringMap.to_json f)));
           Some ("args", (StringList.to_json v.args));
           Util.option_map v.version
             (fun f -> ("version", (String.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)))])
    let of_json j =
      {
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        version = (Util.option_map (Json.lookup j "version") String.of_json);
        args =
          (StringList.of_json (Util.of_option_exn (Json.lookup j "args")));
        additional_info =
          (Util.option_map (Json.lookup j "additional_info")
             StringMap.of_json)
      }
  end
module Tag =
  struct
    type t = {
      key: String.t option ;
      value: String.t option }
    let make ?key  ?value  () = { key; value }
    let parse xml =
      Some
        {
          key = (Util.option_bind (Xml.member "Key" xml) String.parse);
          value = (Util.option_bind (Xml.member "Value" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.value
              (fun f -> Query.Pair ("Value", (String.to_query f)));
           Util.option_map v.key
             (fun f -> Query.Pair ("Key", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.value (fun f -> ("value", (String.to_json f)));
           Util.option_map v.key (fun f -> ("key", (String.to_json f)))])
    let of_json j =
      {
        key = (Util.option_map (Json.lookup j "key") String.of_json);
        value = (Util.option_map (Json.lookup j "value") String.of_json)
      }
  end
module InstanceGroupConfig =
  struct
    type t =
      {
      name: String.t option ;
      market: MarketType.t option ;
      instance_role: InstanceRoleType.t ;
      bid_price: String.t option ;
      instance_type: String.t ;
      instance_count: Integer.t ;
      configurations: ConfigurationList.t }
    let make ?name  ?market  ~instance_role  ?bid_price  ~instance_type
      ~instance_count  ?(configurations= [])  () =
      {
        name;
        market;
        instance_role;
        bid_price;
        instance_type;
        instance_count;
        configurations
      }
    let parse xml =
      Some
        {
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          market =
            (Util.option_bind (Xml.member "Market" xml) MarketType.parse);
          instance_role =
            (Xml.required "InstanceRole"
               (Util.option_bind (Xml.member "InstanceRole" xml)
                  InstanceRoleType.parse));
          bid_price =
            (Util.option_bind (Xml.member "BidPrice" xml) String.parse);
          instance_type =
            (Xml.required "InstanceType"
               (Util.option_bind (Xml.member "InstanceType" xml) String.parse));
          instance_count =
            (Xml.required "InstanceCount"
               (Util.option_bind (Xml.member "InstanceCount" xml)
                  Integer.parse));
          configurations =
            (Util.of_option []
               (Util.option_bind (Xml.member "Configurations" xml)
                  ConfigurationList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Configurations.member",
                   (ConfigurationList.to_query v.configurations)));
           Some
             (Query.Pair
                ("InstanceCount", (Integer.to_query v.instance_count)));
           Some
             (Query.Pair ("InstanceType", (String.to_query v.instance_type)));
           Util.option_map v.bid_price
             (fun f -> Query.Pair ("BidPrice", (String.to_query f)));
           Some
             (Query.Pair
                ("InstanceRole", (InstanceRoleType.to_query v.instance_role)));
           Util.option_map v.market
             (fun f -> Query.Pair ("Market", (MarketType.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("configurations",
                (ConfigurationList.to_json v.configurations));
           Some ("instance_count", (Integer.to_json v.instance_count));
           Some ("instance_type", (String.to_json v.instance_type));
           Util.option_map v.bid_price
             (fun f -> ("bid_price", (String.to_json f)));
           Some ("instance_role", (InstanceRoleType.to_json v.instance_role));
           Util.option_map v.market
             (fun f -> ("market", (MarketType.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)))])
    let of_json j =
      {
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        market =
          (Util.option_map (Json.lookup j "market") MarketType.of_json);
        instance_role =
          (InstanceRoleType.of_json
             (Util.of_option_exn (Json.lookup j "instance_role")));
        bid_price =
          (Util.option_map (Json.lookup j "bid_price") String.of_json);
        instance_type =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "instance_type")));
        instance_count =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "instance_count")));
        configurations =
          (ConfigurationList.of_json
             (Util.of_option_exn (Json.lookup j "configurations")))
      }
  end
module HadoopStepConfig =
  struct
    type t =
      {
      jar: String.t option ;
      properties: StringMap.t option ;
      main_class: String.t option ;
      args: StringList.t }
    let make ?jar  ?properties  ?main_class  ?(args= [])  () =
      { jar; properties; main_class; args }
    let parse xml =
      Some
        {
          jar = (Util.option_bind (Xml.member "Jar" xml) String.parse);
          properties =
            (Util.option_bind (Xml.member "Properties" xml) StringMap.parse);
          main_class =
            (Util.option_bind (Xml.member "MainClass" xml) String.parse);
          args =
            (Util.of_option []
               (Util.option_bind (Xml.member "Args" xml) StringList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Args.member", (StringList.to_query v.args)));
           Util.option_map v.main_class
             (fun f -> Query.Pair ("MainClass", (String.to_query f)));
           Util.option_map v.properties
             (fun f -> Query.Pair ("Properties", (StringMap.to_query f)));
           Util.option_map v.jar
             (fun f -> Query.Pair ("Jar", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("args", (StringList.to_json v.args));
           Util.option_map v.main_class
             (fun f -> ("main_class", (String.to_json f)));
           Util.option_map v.properties
             (fun f -> ("properties", (StringMap.to_json f)));
           Util.option_map v.jar (fun f -> ("jar", (String.to_json f)))])
    let of_json j =
      {
        jar = (Util.option_map (Json.lookup j "jar") String.of_json);
        properties =
          (Util.option_map (Json.lookup j "properties") StringMap.of_json);
        main_class =
          (Util.option_map (Json.lookup j "main_class") String.of_json);
        args =
          (StringList.of_json (Util.of_option_exn (Json.lookup j "args")))
      }
  end
module StepStatus =
  struct
    type t =
      {
      state: StepState.t option ;
      state_change_reason: StepStateChangeReason.t option ;
      timeline: StepTimeline.t option }
    let make ?state  ?state_change_reason  ?timeline  () =
      { state; state_change_reason; timeline }
    let parse xml =
      Some
        {
          state = (Util.option_bind (Xml.member "State" xml) StepState.parse);
          state_change_reason =
            (Util.option_bind (Xml.member "StateChangeReason" xml)
               StepStateChangeReason.parse);
          timeline =
            (Util.option_bind (Xml.member "Timeline" xml) StepTimeline.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.timeline
              (fun f -> Query.Pair ("Timeline", (StepTimeline.to_query f)));
           Util.option_map v.state_change_reason
             (fun f ->
                Query.Pair
                  ("StateChangeReason", (StepStateChangeReason.to_query f)));
           Util.option_map v.state
             (fun f -> Query.Pair ("State", (StepState.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.timeline
              (fun f -> ("timeline", (StepTimeline.to_json f)));
           Util.option_map v.state_change_reason
             (fun f ->
                ("state_change_reason", (StepStateChangeReason.to_json f)));
           Util.option_map v.state
             (fun f -> ("state", (StepState.to_json f)))])
    let of_json j =
      {
        state = (Util.option_map (Json.lookup j "state") StepState.of_json);
        state_change_reason =
          (Util.option_map (Json.lookup j "state_change_reason")
             StepStateChangeReason.of_json);
        timeline =
          (Util.option_map (Json.lookup j "timeline") StepTimeline.of_json)
      }
  end
module EC2InstanceIdsToTerminateList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module InstanceGroupStatus =
  struct
    type t =
      {
      state: InstanceGroupState.t option ;
      state_change_reason: InstanceGroupStateChangeReason.t option ;
      timeline: InstanceGroupTimeline.t option }
    let make ?state  ?state_change_reason  ?timeline  () =
      { state; state_change_reason; timeline }
    let parse xml =
      Some
        {
          state =
            (Util.option_bind (Xml.member "State" xml)
               InstanceGroupState.parse);
          state_change_reason =
            (Util.option_bind (Xml.member "StateChangeReason" xml)
               InstanceGroupStateChangeReason.parse);
          timeline =
            (Util.option_bind (Xml.member "Timeline" xml)
               InstanceGroupTimeline.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.timeline
              (fun f ->
                 Query.Pair ("Timeline", (InstanceGroupTimeline.to_query f)));
           Util.option_map v.state_change_reason
             (fun f ->
                Query.Pair
                  ("StateChangeReason",
                    (InstanceGroupStateChangeReason.to_query f)));
           Util.option_map v.state
             (fun f -> Query.Pair ("State", (InstanceGroupState.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.timeline
              (fun f -> ("timeline", (InstanceGroupTimeline.to_json f)));
           Util.option_map v.state_change_reason
             (fun f ->
                ("state_change_reason",
                  (InstanceGroupStateChangeReason.to_json f)));
           Util.option_map v.state
             (fun f -> ("state", (InstanceGroupState.to_json f)))])
    let of_json j =
      {
        state =
          (Util.option_map (Json.lookup j "state") InstanceGroupState.of_json);
        state_change_reason =
          (Util.option_map (Json.lookup j "state_change_reason")
             InstanceGroupStateChangeReason.of_json);
        timeline =
          (Util.option_map (Json.lookup j "timeline")
             InstanceGroupTimeline.of_json)
      }
  end
module InstanceGroupType =
  struct
    type t =
      | MASTER
      | CORE
      | TASK
    let str_to_t = [("TASK", TASK); ("CORE", CORE); ("MASTER", MASTER)]
    let t_to_str = [(TASK, "TASK"); (CORE, "CORE"); (MASTER, "MASTER")]
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
module InstanceStatus =
  struct
    type t =
      {
      state: InstanceState.t option ;
      state_change_reason: InstanceStateChangeReason.t option ;
      timeline: InstanceTimeline.t option }
    let make ?state  ?state_change_reason  ?timeline  () =
      { state; state_change_reason; timeline }
    let parse xml =
      Some
        {
          state =
            (Util.option_bind (Xml.member "State" xml) InstanceState.parse);
          state_change_reason =
            (Util.option_bind (Xml.member "StateChangeReason" xml)
               InstanceStateChangeReason.parse);
          timeline =
            (Util.option_bind (Xml.member "Timeline" xml)
               InstanceTimeline.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.timeline
              (fun f ->
                 Query.Pair ("Timeline", (InstanceTimeline.to_query f)));
           Util.option_map v.state_change_reason
             (fun f ->
                Query.Pair
                  ("StateChangeReason",
                    (InstanceStateChangeReason.to_query f)));
           Util.option_map v.state
             (fun f -> Query.Pair ("State", (InstanceState.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.timeline
              (fun f -> ("timeline", (InstanceTimeline.to_json f)));
           Util.option_map v.state_change_reason
             (fun f ->
                ("state_change_reason",
                  (InstanceStateChangeReason.to_json f)));
           Util.option_map v.state
             (fun f -> ("state", (InstanceState.to_json f)))])
    let of_json j =
      {
        state =
          (Util.option_map (Json.lookup j "state") InstanceState.of_json);
        state_change_reason =
          (Util.option_map (Json.lookup j "state_change_reason")
             InstanceStateChangeReason.of_json);
        timeline =
          (Util.option_map (Json.lookup j "timeline")
             InstanceTimeline.of_json)
      }
  end
module ClusterStatus =
  struct
    type t =
      {
      state: ClusterState.t option ;
      state_change_reason: ClusterStateChangeReason.t option ;
      timeline: ClusterTimeline.t option }
    let make ?state  ?state_change_reason  ?timeline  () =
      { state; state_change_reason; timeline }
    let parse xml =
      Some
        {
          state =
            (Util.option_bind (Xml.member "State" xml) ClusterState.parse);
          state_change_reason =
            (Util.option_bind (Xml.member "StateChangeReason" xml)
               ClusterStateChangeReason.parse);
          timeline =
            (Util.option_bind (Xml.member "Timeline" xml)
               ClusterTimeline.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.timeline
              (fun f -> Query.Pair ("Timeline", (ClusterTimeline.to_query f)));
           Util.option_map v.state_change_reason
             (fun f ->
                Query.Pair
                  ("StateChangeReason",
                    (ClusterStateChangeReason.to_query f)));
           Util.option_map v.state
             (fun f -> Query.Pair ("State", (ClusterState.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.timeline
              (fun f -> ("timeline", (ClusterTimeline.to_json f)));
           Util.option_map v.state_change_reason
             (fun f ->
                ("state_change_reason", (ClusterStateChangeReason.to_json f)));
           Util.option_map v.state
             (fun f -> ("state", (ClusterState.to_json f)))])
    let of_json j =
      {
        state =
          (Util.option_map (Json.lookup j "state") ClusterState.of_json);
        state_change_reason =
          (Util.option_map (Json.lookup j "state_change_reason")
             ClusterStateChangeReason.of_json);
        timeline =
          (Util.option_map (Json.lookup j "timeline") ClusterTimeline.of_json)
      }
  end
module BootstrapActionDetailList =
  struct
    type t = BootstrapActionDetail.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map BootstrapActionDetail.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list BootstrapActionDetail.to_query v
    let to_json v = `List (List.map BootstrapActionDetail.to_json v)
    let of_json j = Json.to_list BootstrapActionDetail.of_json j
  end
module JobFlowExecutionStatusDetail =
  struct
    type t =
      {
      state: JobFlowExecutionState.t ;
      creation_date_time: DateTime.t ;
      start_date_time: DateTime.t option ;
      ready_date_time: DateTime.t option ;
      end_date_time: DateTime.t option ;
      last_state_change_reason: String.t option }
    let make ~state  ~creation_date_time  ?start_date_time  ?ready_date_time
      ?end_date_time  ?last_state_change_reason  () =
      {
        state;
        creation_date_time;
        start_date_time;
        ready_date_time;
        end_date_time;
        last_state_change_reason
      }
    let parse xml =
      Some
        {
          state =
            (Xml.required "State"
               (Util.option_bind (Xml.member "State" xml)
                  JobFlowExecutionState.parse));
          creation_date_time =
            (Xml.required "CreationDateTime"
               (Util.option_bind (Xml.member "CreationDateTime" xml)
                  DateTime.parse));
          start_date_time =
            (Util.option_bind (Xml.member "StartDateTime" xml) DateTime.parse);
          ready_date_time =
            (Util.option_bind (Xml.member "ReadyDateTime" xml) DateTime.parse);
          end_date_time =
            (Util.option_bind (Xml.member "EndDateTime" xml) DateTime.parse);
          last_state_change_reason =
            (Util.option_bind (Xml.member "LastStateChangeReason" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.last_state_change_reason
              (fun f ->
                 Query.Pair ("LastStateChangeReason", (String.to_query f)));
           Util.option_map v.end_date_time
             (fun f -> Query.Pair ("EndDateTime", (DateTime.to_query f)));
           Util.option_map v.ready_date_time
             (fun f -> Query.Pair ("ReadyDateTime", (DateTime.to_query f)));
           Util.option_map v.start_date_time
             (fun f -> Query.Pair ("StartDateTime", (DateTime.to_query f)));
           Some
             (Query.Pair
                ("CreationDateTime",
                  (DateTime.to_query v.creation_date_time)));
           Some
             (Query.Pair ("State", (JobFlowExecutionState.to_query v.state)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.last_state_change_reason
              (fun f -> ("last_state_change_reason", (String.to_json f)));
           Util.option_map v.end_date_time
             (fun f -> ("end_date_time", (DateTime.to_json f)));
           Util.option_map v.ready_date_time
             (fun f -> ("ready_date_time", (DateTime.to_json f)));
           Util.option_map v.start_date_time
             (fun f -> ("start_date_time", (DateTime.to_json f)));
           Some
             ("creation_date_time", (DateTime.to_json v.creation_date_time));
           Some ("state", (JobFlowExecutionState.to_json v.state))])
    let of_json j =
      {
        state =
          (JobFlowExecutionState.of_json
             (Util.of_option_exn (Json.lookup j "state")));
        creation_date_time =
          (DateTime.of_json
             (Util.of_option_exn (Json.lookup j "creation_date_time")));
        start_date_time =
          (Util.option_map (Json.lookup j "start_date_time") DateTime.of_json);
        ready_date_time =
          (Util.option_map (Json.lookup j "ready_date_time") DateTime.of_json);
        end_date_time =
          (Util.option_map (Json.lookup j "end_date_time") DateTime.of_json);
        last_state_change_reason =
          (Util.option_map (Json.lookup j "last_state_change_reason")
             String.of_json)
      }
  end
module JobFlowInstancesDetail =
  struct
    type t =
      {
      master_instance_type: String.t ;
      master_public_dns_name: String.t option ;
      master_instance_id: String.t option ;
      slave_instance_type: String.t ;
      instance_count: Integer.t ;
      instance_groups: InstanceGroupDetailList.t ;
      normalized_instance_hours: Integer.t option ;
      ec2_key_name: String.t option ;
      ec2_subnet_id: String.t option ;
      placement: PlacementType.t option ;
      keep_job_flow_alive_when_no_steps: Boolean.t option ;
      termination_protected: Boolean.t option ;
      hadoop_version: String.t option }
    let make ~master_instance_type  ?master_public_dns_name
      ?master_instance_id  ~slave_instance_type  ~instance_count
      ?(instance_groups= [])  ?normalized_instance_hours  ?ec2_key_name
      ?ec2_subnet_id  ?placement  ?keep_job_flow_alive_when_no_steps
      ?termination_protected  ?hadoop_version  () =
      {
        master_instance_type;
        master_public_dns_name;
        master_instance_id;
        slave_instance_type;
        instance_count;
        instance_groups;
        normalized_instance_hours;
        ec2_key_name;
        ec2_subnet_id;
        placement;
        keep_job_flow_alive_when_no_steps;
        termination_protected;
        hadoop_version
      }
    let parse xml =
      Some
        {
          master_instance_type =
            (Xml.required "MasterInstanceType"
               (Util.option_bind (Xml.member "MasterInstanceType" xml)
                  String.parse));
          master_public_dns_name =
            (Util.option_bind (Xml.member "MasterPublicDnsName" xml)
               String.parse);
          master_instance_id =
            (Util.option_bind (Xml.member "MasterInstanceId" xml)
               String.parse);
          slave_instance_type =
            (Xml.required "SlaveInstanceType"
               (Util.option_bind (Xml.member "SlaveInstanceType" xml)
                  String.parse));
          instance_count =
            (Xml.required "InstanceCount"
               (Util.option_bind (Xml.member "InstanceCount" xml)
                  Integer.parse));
          instance_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "InstanceGroups" xml)
                  InstanceGroupDetailList.parse));
          normalized_instance_hours =
            (Util.option_bind (Xml.member "NormalizedInstanceHours" xml)
               Integer.parse);
          ec2_key_name =
            (Util.option_bind (Xml.member "Ec2KeyName" xml) String.parse);
          ec2_subnet_id =
            (Util.option_bind (Xml.member "Ec2SubnetId" xml) String.parse);
          placement =
            (Util.option_bind (Xml.member "Placement" xml)
               PlacementType.parse);
          keep_job_flow_alive_when_no_steps =
            (Util.option_bind (Xml.member "KeepJobFlowAliveWhenNoSteps" xml)
               Boolean.parse);
          termination_protected =
            (Util.option_bind (Xml.member "TerminationProtected" xml)
               Boolean.parse);
          hadoop_version =
            (Util.option_bind (Xml.member "HadoopVersion" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.hadoop_version
              (fun f -> Query.Pair ("HadoopVersion", (String.to_query f)));
           Util.option_map v.termination_protected
             (fun f ->
                Query.Pair ("TerminationProtected", (Boolean.to_query f)));
           Util.option_map v.keep_job_flow_alive_when_no_steps
             (fun f ->
                Query.Pair
                  ("KeepJobFlowAliveWhenNoSteps", (Boolean.to_query f)));
           Util.option_map v.placement
             (fun f -> Query.Pair ("Placement", (PlacementType.to_query f)));
           Util.option_map v.ec2_subnet_id
             (fun f -> Query.Pair ("Ec2SubnetId", (String.to_query f)));
           Util.option_map v.ec2_key_name
             (fun f -> Query.Pair ("Ec2KeyName", (String.to_query f)));
           Util.option_map v.normalized_instance_hours
             (fun f ->
                Query.Pair ("NormalizedInstanceHours", (Integer.to_query f)));
           Some
             (Query.Pair
                ("InstanceGroups.member",
                  (InstanceGroupDetailList.to_query v.instance_groups)));
           Some
             (Query.Pair
                ("InstanceCount", (Integer.to_query v.instance_count)));
           Some
             (Query.Pair
                ("SlaveInstanceType",
                  (String.to_query v.slave_instance_type)));
           Util.option_map v.master_instance_id
             (fun f -> Query.Pair ("MasterInstanceId", (String.to_query f)));
           Util.option_map v.master_public_dns_name
             (fun f ->
                Query.Pair ("MasterPublicDnsName", (String.to_query f)));
           Some
             (Query.Pair
                ("MasterInstanceType",
                  (String.to_query v.master_instance_type)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.hadoop_version
              (fun f -> ("hadoop_version", (String.to_json f)));
           Util.option_map v.termination_protected
             (fun f -> ("termination_protected", (Boolean.to_json f)));
           Util.option_map v.keep_job_flow_alive_when_no_steps
             (fun f ->
                ("keep_job_flow_alive_when_no_steps", (Boolean.to_json f)));
           Util.option_map v.placement
             (fun f -> ("placement", (PlacementType.to_json f)));
           Util.option_map v.ec2_subnet_id
             (fun f -> ("ec2_subnet_id", (String.to_json f)));
           Util.option_map v.ec2_key_name
             (fun f -> ("ec2_key_name", (String.to_json f)));
           Util.option_map v.normalized_instance_hours
             (fun f -> ("normalized_instance_hours", (Integer.to_json f)));
           Some
             ("instance_groups",
               (InstanceGroupDetailList.to_json v.instance_groups));
           Some ("instance_count", (Integer.to_json v.instance_count));
           Some
             ("slave_instance_type", (String.to_json v.slave_instance_type));
           Util.option_map v.master_instance_id
             (fun f -> ("master_instance_id", (String.to_json f)));
           Util.option_map v.master_public_dns_name
             (fun f -> ("master_public_dns_name", (String.to_json f)));
           Some
             ("master_instance_type",
               (String.to_json v.master_instance_type))])
    let of_json j =
      {
        master_instance_type =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "master_instance_type")));
        master_public_dns_name =
          (Util.option_map (Json.lookup j "master_public_dns_name")
             String.of_json);
        master_instance_id =
          (Util.option_map (Json.lookup j "master_instance_id")
             String.of_json);
        slave_instance_type =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "slave_instance_type")));
        instance_count =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "instance_count")));
        instance_groups =
          (InstanceGroupDetailList.of_json
             (Util.of_option_exn (Json.lookup j "instance_groups")));
        normalized_instance_hours =
          (Util.option_map (Json.lookup j "normalized_instance_hours")
             Integer.of_json);
        ec2_key_name =
          (Util.option_map (Json.lookup j "ec2_key_name") String.of_json);
        ec2_subnet_id =
          (Util.option_map (Json.lookup j "ec2_subnet_id") String.of_json);
        placement =
          (Util.option_map (Json.lookup j "placement") PlacementType.of_json);
        keep_job_flow_alive_when_no_steps =
          (Util.option_map
             (Json.lookup j "keep_job_flow_alive_when_no_steps")
             Boolean.of_json);
        termination_protected =
          (Util.option_map (Json.lookup j "termination_protected")
             Boolean.of_json);
        hadoop_version =
          (Util.option_map (Json.lookup j "hadoop_version") String.of_json)
      }
  end
module StepDetailList =
  struct
    type t = StepDetail.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map StepDetail.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list StepDetail.to_query v
    let to_json v = `List (List.map StepDetail.to_json v)
    let of_json j = Json.to_list StepDetail.of_json j
  end
module SupportedProductsList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module ApplicationList =
  struct
    type t = Application.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Application.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Application.to_query v
    let to_json v = `List (List.map Application.to_json v)
    let of_json j = Json.to_list Application.of_json j
  end
module Ec2InstanceAttributes =
  struct
    type t =
      {
      ec2_key_name: String.t option ;
      ec2_subnet_id: String.t option ;
      ec2_availability_zone: String.t option ;
      iam_instance_profile: String.t option ;
      emr_managed_master_security_group: String.t option ;
      emr_managed_slave_security_group: String.t option ;
      additional_master_security_groups: StringList.t ;
      additional_slave_security_groups: StringList.t }
    let make ?ec2_key_name  ?ec2_subnet_id  ?ec2_availability_zone
      ?iam_instance_profile  ?emr_managed_master_security_group
      ?emr_managed_slave_security_group  ?(additional_master_security_groups=
      [])  ?(additional_slave_security_groups= [])  () =
      {
        ec2_key_name;
        ec2_subnet_id;
        ec2_availability_zone;
        iam_instance_profile;
        emr_managed_master_security_group;
        emr_managed_slave_security_group;
        additional_master_security_groups;
        additional_slave_security_groups
      }
    let parse xml =
      Some
        {
          ec2_key_name =
            (Util.option_bind (Xml.member "Ec2KeyName" xml) String.parse);
          ec2_subnet_id =
            (Util.option_bind (Xml.member "Ec2SubnetId" xml) String.parse);
          ec2_availability_zone =
            (Util.option_bind (Xml.member "Ec2AvailabilityZone" xml)
               String.parse);
          iam_instance_profile =
            (Util.option_bind (Xml.member "IamInstanceProfile" xml)
               String.parse);
          emr_managed_master_security_group =
            (Util.option_bind
               (Xml.member "EmrManagedMasterSecurityGroup" xml) String.parse);
          emr_managed_slave_security_group =
            (Util.option_bind (Xml.member "EmrManagedSlaveSecurityGroup" xml)
               String.parse);
          additional_master_security_groups =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "AdditionalMasterSecurityGroups" xml)
                  StringList.parse));
          additional_slave_security_groups =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "AdditionalSlaveSecurityGroups" xml)
                  StringList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("AdditionalSlaveSecurityGroups.member",
                   (StringList.to_query v.additional_slave_security_groups)));
           Some
             (Query.Pair
                ("AdditionalMasterSecurityGroups.member",
                  (StringList.to_query v.additional_master_security_groups)));
           Util.option_map v.emr_managed_slave_security_group
             (fun f ->
                Query.Pair
                  ("EmrManagedSlaveSecurityGroup", (String.to_query f)));
           Util.option_map v.emr_managed_master_security_group
             (fun f ->
                Query.Pair
                  ("EmrManagedMasterSecurityGroup", (String.to_query f)));
           Util.option_map v.iam_instance_profile
             (fun f -> Query.Pair ("IamInstanceProfile", (String.to_query f)));
           Util.option_map v.ec2_availability_zone
             (fun f ->
                Query.Pair ("Ec2AvailabilityZone", (String.to_query f)));
           Util.option_map v.ec2_subnet_id
             (fun f -> Query.Pair ("Ec2SubnetId", (String.to_query f)));
           Util.option_map v.ec2_key_name
             (fun f -> Query.Pair ("Ec2KeyName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("additional_slave_security_groups",
                (StringList.to_json v.additional_slave_security_groups));
           Some
             ("additional_master_security_groups",
               (StringList.to_json v.additional_master_security_groups));
           Util.option_map v.emr_managed_slave_security_group
             (fun f ->
                ("emr_managed_slave_security_group", (String.to_json f)));
           Util.option_map v.emr_managed_master_security_group
             (fun f ->
                ("emr_managed_master_security_group", (String.to_json f)));
           Util.option_map v.iam_instance_profile
             (fun f -> ("iam_instance_profile", (String.to_json f)));
           Util.option_map v.ec2_availability_zone
             (fun f -> ("ec2_availability_zone", (String.to_json f)));
           Util.option_map v.ec2_subnet_id
             (fun f -> ("ec2_subnet_id", (String.to_json f)));
           Util.option_map v.ec2_key_name
             (fun f -> ("ec2_key_name", (String.to_json f)))])
    let of_json j =
      {
        ec2_key_name =
          (Util.option_map (Json.lookup j "ec2_key_name") String.of_json);
        ec2_subnet_id =
          (Util.option_map (Json.lookup j "ec2_subnet_id") String.of_json);
        ec2_availability_zone =
          (Util.option_map (Json.lookup j "ec2_availability_zone")
             String.of_json);
        iam_instance_profile =
          (Util.option_map (Json.lookup j "iam_instance_profile")
             String.of_json);
        emr_managed_master_security_group =
          (Util.option_map
             (Json.lookup j "emr_managed_master_security_group")
             String.of_json);
        emr_managed_slave_security_group =
          (Util.option_map (Json.lookup j "emr_managed_slave_security_group")
             String.of_json);
        additional_master_security_groups =
          (StringList.of_json
             (Util.of_option_exn
                (Json.lookup j "additional_master_security_groups")));
        additional_slave_security_groups =
          (StringList.of_json
             (Util.of_option_exn
                (Json.lookup j "additional_slave_security_groups")))
      }
  end
module TagList =
  struct
    type t = Tag.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Tag.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Tag.to_query v
    let to_json v = `List (List.map Tag.to_json v)
    let of_json j = Json.to_list Tag.of_json j
  end
module InstanceGroupConfigList =
  struct
    type t = InstanceGroupConfig.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map InstanceGroupConfig.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list InstanceGroupConfig.to_query v
    let to_json v = `List (List.map InstanceGroupConfig.to_json v)
    let of_json j = Json.to_list InstanceGroupConfig.of_json j
  end
module SecurityGroupsList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module SupportedProductConfig =
  struct
    type t = {
      name: String.t option ;
      args: XmlStringList.t }
    let make ?name  ?(args= [])  () = { name; args }
    let parse xml =
      Some
        {
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          args =
            (Util.of_option []
               (Util.option_bind (Xml.member "Args" xml) XmlStringList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Args.member", (XmlStringList.to_query v.args)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("args", (XmlStringList.to_json v.args));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)))])
    let of_json j =
      {
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        args =
          (XmlStringList.of_json (Util.of_option_exn (Json.lookup j "args")))
      }
  end
module StepSummary =
  struct
    type t =
      {
      id: String.t option ;
      name: String.t option ;
      config: HadoopStepConfig.t option ;
      action_on_failure: ActionOnFailure.t option ;
      status: StepStatus.t option }
    let make ?id  ?name  ?config  ?action_on_failure  ?status  () =
      { id; name; config; action_on_failure; status }
    let parse xml =
      Some
        {
          id = (Util.option_bind (Xml.member "Id" xml) String.parse);
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          config =
            (Util.option_bind (Xml.member "Config" xml)
               HadoopStepConfig.parse);
          action_on_failure =
            (Util.option_bind (Xml.member "ActionOnFailure" xml)
               ActionOnFailure.parse);
          status =
            (Util.option_bind (Xml.member "Status" xml) StepStatus.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> Query.Pair ("Status", (StepStatus.to_query f)));
           Util.option_map v.action_on_failure
             (fun f ->
                Query.Pair ("ActionOnFailure", (ActionOnFailure.to_query f)));
           Util.option_map v.config
             (fun f -> Query.Pair ("Config", (HadoopStepConfig.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)));
           Util.option_map v.id
             (fun f -> Query.Pair ("Id", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> ("status", (StepStatus.to_json f)));
           Util.option_map v.action_on_failure
             (fun f -> ("action_on_failure", (ActionOnFailure.to_json f)));
           Util.option_map v.config
             (fun f -> ("config", (HadoopStepConfig.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)));
           Util.option_map v.id (fun f -> ("id", (String.to_json f)))])
    let of_json j =
      {
        id = (Util.option_map (Json.lookup j "id") String.of_json);
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        config =
          (Util.option_map (Json.lookup j "config") HadoopStepConfig.of_json);
        action_on_failure =
          (Util.option_map (Json.lookup j "action_on_failure")
             ActionOnFailure.of_json);
        status =
          (Util.option_map (Json.lookup j "status") StepStatus.of_json)
      }
  end
module InstanceGroupModifyConfig =
  struct
    type t =
      {
      instance_group_id: String.t ;
      instance_count: Integer.t option ;
      e_c2_instance_ids_to_terminate: EC2InstanceIdsToTerminateList.t }
    let make ~instance_group_id  ?instance_count
      ?(e_c2_instance_ids_to_terminate= [])  () =
      { instance_group_id; instance_count; e_c2_instance_ids_to_terminate }
    let parse xml =
      Some
        {
          instance_group_id =
            (Xml.required "InstanceGroupId"
               (Util.option_bind (Xml.member "InstanceGroupId" xml)
                  String.parse));
          instance_count =
            (Util.option_bind (Xml.member "InstanceCount" xml) Integer.parse);
          e_c2_instance_ids_to_terminate =
            (Util.of_option []
               (Util.option_bind (Xml.member "EC2InstanceIdsToTerminate" xml)
                  EC2InstanceIdsToTerminateList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("EC2InstanceIdsToTerminate.member",
                   (EC2InstanceIdsToTerminateList.to_query
                      v.e_c2_instance_ids_to_terminate)));
           Util.option_map v.instance_count
             (fun f -> Query.Pair ("InstanceCount", (Integer.to_query f)));
           Some
             (Query.Pair
                ("InstanceGroupId", (String.to_query v.instance_group_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("e_c2_instance_ids_to_terminate",
                (EC2InstanceIdsToTerminateList.to_json
                   v.e_c2_instance_ids_to_terminate));
           Util.option_map v.instance_count
             (fun f -> ("instance_count", (Integer.to_json f)));
           Some ("instance_group_id", (String.to_json v.instance_group_id))])
    let of_json j =
      {
        instance_group_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "instance_group_id")));
        instance_count =
          (Util.option_map (Json.lookup j "instance_count") Integer.of_json);
        e_c2_instance_ids_to_terminate =
          (EC2InstanceIdsToTerminateList.of_json
             (Util.of_option_exn
                (Json.lookup j "e_c2_instance_ids_to_terminate")))
      }
  end
module InstanceGroup =
  struct
    type t =
      {
      id: String.t option ;
      name: String.t option ;
      market: MarketType.t option ;
      instance_group_type: InstanceGroupType.t option ;
      bid_price: String.t option ;
      instance_type: String.t option ;
      requested_instance_count: Integer.t option ;
      running_instance_count: Integer.t option ;
      status: InstanceGroupStatus.t option ;
      configurations: ConfigurationList.t }
    let make ?id  ?name  ?market  ?instance_group_type  ?bid_price
      ?instance_type  ?requested_instance_count  ?running_instance_count
      ?status  ?(configurations= [])  () =
      {
        id;
        name;
        market;
        instance_group_type;
        bid_price;
        instance_type;
        requested_instance_count;
        running_instance_count;
        status;
        configurations
      }
    let parse xml =
      Some
        {
          id = (Util.option_bind (Xml.member "Id" xml) String.parse);
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          market =
            (Util.option_bind (Xml.member "Market" xml) MarketType.parse);
          instance_group_type =
            (Util.option_bind (Xml.member "InstanceGroupType" xml)
               InstanceGroupType.parse);
          bid_price =
            (Util.option_bind (Xml.member "BidPrice" xml) String.parse);
          instance_type =
            (Util.option_bind (Xml.member "InstanceType" xml) String.parse);
          requested_instance_count =
            (Util.option_bind (Xml.member "RequestedInstanceCount" xml)
               Integer.parse);
          running_instance_count =
            (Util.option_bind (Xml.member "RunningInstanceCount" xml)
               Integer.parse);
          status =
            (Util.option_bind (Xml.member "Status" xml)
               InstanceGroupStatus.parse);
          configurations =
            (Util.of_option []
               (Util.option_bind (Xml.member "Configurations" xml)
                  ConfigurationList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Configurations.member",
                   (ConfigurationList.to_query v.configurations)));
           Util.option_map v.status
             (fun f ->
                Query.Pair ("Status", (InstanceGroupStatus.to_query f)));
           Util.option_map v.running_instance_count
             (fun f ->
                Query.Pair ("RunningInstanceCount", (Integer.to_query f)));
           Util.option_map v.requested_instance_count
             (fun f ->
                Query.Pair ("RequestedInstanceCount", (Integer.to_query f)));
           Util.option_map v.instance_type
             (fun f -> Query.Pair ("InstanceType", (String.to_query f)));
           Util.option_map v.bid_price
             (fun f -> Query.Pair ("BidPrice", (String.to_query f)));
           Util.option_map v.instance_group_type
             (fun f ->
                Query.Pair
                  ("InstanceGroupType", (InstanceGroupType.to_query f)));
           Util.option_map v.market
             (fun f -> Query.Pair ("Market", (MarketType.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)));
           Util.option_map v.id
             (fun f -> Query.Pair ("Id", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("configurations",
                (ConfigurationList.to_json v.configurations));
           Util.option_map v.status
             (fun f -> ("status", (InstanceGroupStatus.to_json f)));
           Util.option_map v.running_instance_count
             (fun f -> ("running_instance_count", (Integer.to_json f)));
           Util.option_map v.requested_instance_count
             (fun f -> ("requested_instance_count", (Integer.to_json f)));
           Util.option_map v.instance_type
             (fun f -> ("instance_type", (String.to_json f)));
           Util.option_map v.bid_price
             (fun f -> ("bid_price", (String.to_json f)));
           Util.option_map v.instance_group_type
             (fun f -> ("instance_group_type", (InstanceGroupType.to_json f)));
           Util.option_map v.market
             (fun f -> ("market", (MarketType.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)));
           Util.option_map v.id (fun f -> ("id", (String.to_json f)))])
    let of_json j =
      {
        id = (Util.option_map (Json.lookup j "id") String.of_json);
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        market =
          (Util.option_map (Json.lookup j "market") MarketType.of_json);
        instance_group_type =
          (Util.option_map (Json.lookup j "instance_group_type")
             InstanceGroupType.of_json);
        bid_price =
          (Util.option_map (Json.lookup j "bid_price") String.of_json);
        instance_type =
          (Util.option_map (Json.lookup j "instance_type") String.of_json);
        requested_instance_count =
          (Util.option_map (Json.lookup j "requested_instance_count")
             Integer.of_json);
        running_instance_count =
          (Util.option_map (Json.lookup j "running_instance_count")
             Integer.of_json);
        status =
          (Util.option_map (Json.lookup j "status")
             InstanceGroupStatus.of_json);
        configurations =
          (ConfigurationList.of_json
             (Util.of_option_exn (Json.lookup j "configurations")))
      }
  end
module Instance =
  struct
    type t =
      {
      id: String.t option ;
      ec2_instance_id: String.t option ;
      public_dns_name: String.t option ;
      public_ip_address: String.t option ;
      private_dns_name: String.t option ;
      private_ip_address: String.t option ;
      status: InstanceStatus.t option }
    let make ?id  ?ec2_instance_id  ?public_dns_name  ?public_ip_address
      ?private_dns_name  ?private_ip_address  ?status  () =
      {
        id;
        ec2_instance_id;
        public_dns_name;
        public_ip_address;
        private_dns_name;
        private_ip_address;
        status
      }
    let parse xml =
      Some
        {
          id = (Util.option_bind (Xml.member "Id" xml) String.parse);
          ec2_instance_id =
            (Util.option_bind (Xml.member "Ec2InstanceId" xml) String.parse);
          public_dns_name =
            (Util.option_bind (Xml.member "PublicDnsName" xml) String.parse);
          public_ip_address =
            (Util.option_bind (Xml.member "PublicIpAddress" xml) String.parse);
          private_dns_name =
            (Util.option_bind (Xml.member "PrivateDnsName" xml) String.parse);
          private_ip_address =
            (Util.option_bind (Xml.member "PrivateIpAddress" xml)
               String.parse);
          status =
            (Util.option_bind (Xml.member "Status" xml) InstanceStatus.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> Query.Pair ("Status", (InstanceStatus.to_query f)));
           Util.option_map v.private_ip_address
             (fun f -> Query.Pair ("PrivateIpAddress", (String.to_query f)));
           Util.option_map v.private_dns_name
             (fun f -> Query.Pair ("PrivateDnsName", (String.to_query f)));
           Util.option_map v.public_ip_address
             (fun f -> Query.Pair ("PublicIpAddress", (String.to_query f)));
           Util.option_map v.public_dns_name
             (fun f -> Query.Pair ("PublicDnsName", (String.to_query f)));
           Util.option_map v.ec2_instance_id
             (fun f -> Query.Pair ("Ec2InstanceId", (String.to_query f)));
           Util.option_map v.id
             (fun f -> Query.Pair ("Id", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> ("status", (InstanceStatus.to_json f)));
           Util.option_map v.private_ip_address
             (fun f -> ("private_ip_address", (String.to_json f)));
           Util.option_map v.private_dns_name
             (fun f -> ("private_dns_name", (String.to_json f)));
           Util.option_map v.public_ip_address
             (fun f -> ("public_ip_address", (String.to_json f)));
           Util.option_map v.public_dns_name
             (fun f -> ("public_dns_name", (String.to_json f)));
           Util.option_map v.ec2_instance_id
             (fun f -> ("ec2_instance_id", (String.to_json f)));
           Util.option_map v.id (fun f -> ("id", (String.to_json f)))])
    let of_json j =
      {
        id = (Util.option_map (Json.lookup j "id") String.of_json);
        ec2_instance_id =
          (Util.option_map (Json.lookup j "ec2_instance_id") String.of_json);
        public_dns_name =
          (Util.option_map (Json.lookup j "public_dns_name") String.of_json);
        public_ip_address =
          (Util.option_map (Json.lookup j "public_ip_address") String.of_json);
        private_dns_name =
          (Util.option_map (Json.lookup j "private_dns_name") String.of_json);
        private_ip_address =
          (Util.option_map (Json.lookup j "private_ip_address")
             String.of_json);
        status =
          (Util.option_map (Json.lookup j "status") InstanceStatus.of_json)
      }
  end
module ClusterSummary =
  struct
    type t =
      {
      id: String.t option ;
      name: String.t option ;
      status: ClusterStatus.t option ;
      normalized_instance_hours: Integer.t option }
    let make ?id  ?name  ?status  ?normalized_instance_hours  () =
      { id; name; status; normalized_instance_hours }
    let parse xml =
      Some
        {
          id = (Util.option_bind (Xml.member "Id" xml) String.parse);
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          status =
            (Util.option_bind (Xml.member "Status" xml) ClusterStatus.parse);
          normalized_instance_hours =
            (Util.option_bind (Xml.member "NormalizedInstanceHours" xml)
               Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.normalized_instance_hours
              (fun f ->
                 Query.Pair ("NormalizedInstanceHours", (Integer.to_query f)));
           Util.option_map v.status
             (fun f -> Query.Pair ("Status", (ClusterStatus.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)));
           Util.option_map v.id
             (fun f -> Query.Pair ("Id", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.normalized_instance_hours
              (fun f -> ("normalized_instance_hours", (Integer.to_json f)));
           Util.option_map v.status
             (fun f -> ("status", (ClusterStatus.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)));
           Util.option_map v.id (fun f -> ("id", (String.to_json f)))])
    let of_json j =
      {
        id = (Util.option_map (Json.lookup j "id") String.of_json);
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        status =
          (Util.option_map (Json.lookup j "status") ClusterStatus.of_json);
        normalized_instance_hours =
          (Util.option_map (Json.lookup j "normalized_instance_hours")
             Integer.of_json)
      }
  end
module JobFlowDetail =
  struct
    type t =
      {
      job_flow_id: String.t ;
      name: String.t ;
      log_uri: String.t option ;
      ami_version: String.t option ;
      execution_status_detail: JobFlowExecutionStatusDetail.t ;
      instances: JobFlowInstancesDetail.t ;
      steps: StepDetailList.t ;
      bootstrap_actions: BootstrapActionDetailList.t ;
      supported_products: SupportedProductsList.t ;
      visible_to_all_users: Boolean.t option ;
      job_flow_role: String.t option ;
      service_role: String.t option }
    let make ~job_flow_id  ~name  ?log_uri  ?ami_version
      ~execution_status_detail  ~instances  ?(steps= [])
      ?(bootstrap_actions= [])  ?(supported_products= [])
      ?visible_to_all_users  ?job_flow_role  ?service_role  () =
      {
        job_flow_id;
        name;
        log_uri;
        ami_version;
        execution_status_detail;
        instances;
        steps;
        bootstrap_actions;
        supported_products;
        visible_to_all_users;
        job_flow_role;
        service_role
      }
    let parse xml =
      Some
        {
          job_flow_id =
            (Xml.required "JobFlowId"
               (Util.option_bind (Xml.member "JobFlowId" xml) String.parse));
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          log_uri = (Util.option_bind (Xml.member "LogUri" xml) String.parse);
          ami_version =
            (Util.option_bind (Xml.member "AmiVersion" xml) String.parse);
          execution_status_detail =
            (Xml.required "ExecutionStatusDetail"
               (Util.option_bind (Xml.member "ExecutionStatusDetail" xml)
                  JobFlowExecutionStatusDetail.parse));
          instances =
            (Xml.required "Instances"
               (Util.option_bind (Xml.member "Instances" xml)
                  JobFlowInstancesDetail.parse));
          steps =
            (Util.of_option []
               (Util.option_bind (Xml.member "Steps" xml)
                  StepDetailList.parse));
          bootstrap_actions =
            (Util.of_option []
               (Util.option_bind (Xml.member "BootstrapActions" xml)
                  BootstrapActionDetailList.parse));
          supported_products =
            (Util.of_option []
               (Util.option_bind (Xml.member "SupportedProducts" xml)
                  SupportedProductsList.parse));
          visible_to_all_users =
            (Util.option_bind (Xml.member "VisibleToAllUsers" xml)
               Boolean.parse);
          job_flow_role =
            (Util.option_bind (Xml.member "JobFlowRole" xml) String.parse);
          service_role =
            (Util.option_bind (Xml.member "ServiceRole" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.service_role
              (fun f -> Query.Pair ("ServiceRole", (String.to_query f)));
           Util.option_map v.job_flow_role
             (fun f -> Query.Pair ("JobFlowRole", (String.to_query f)));
           Util.option_map v.visible_to_all_users
             (fun f -> Query.Pair ("VisibleToAllUsers", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("SupportedProducts.member",
                  (SupportedProductsList.to_query v.supported_products)));
           Some
             (Query.Pair
                ("BootstrapActions.member",
                  (BootstrapActionDetailList.to_query v.bootstrap_actions)));
           Some
             (Query.Pair ("Steps.member", (StepDetailList.to_query v.steps)));
           Some
             (Query.Pair
                ("Instances", (JobFlowInstancesDetail.to_query v.instances)));
           Some
             (Query.Pair
                ("ExecutionStatusDetail",
                  (JobFlowExecutionStatusDetail.to_query
                     v.execution_status_detail)));
           Util.option_map v.ami_version
             (fun f -> Query.Pair ("AmiVersion", (String.to_query f)));
           Util.option_map v.log_uri
             (fun f -> Query.Pair ("LogUri", (String.to_query f)));
           Some (Query.Pair ("Name", (String.to_query v.name)));
           Some (Query.Pair ("JobFlowId", (String.to_query v.job_flow_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.service_role
              (fun f -> ("service_role", (String.to_json f)));
           Util.option_map v.job_flow_role
             (fun f -> ("job_flow_role", (String.to_json f)));
           Util.option_map v.visible_to_all_users
             (fun f -> ("visible_to_all_users", (Boolean.to_json f)));
           Some
             ("supported_products",
               (SupportedProductsList.to_json v.supported_products));
           Some
             ("bootstrap_actions",
               (BootstrapActionDetailList.to_json v.bootstrap_actions));
           Some ("steps", (StepDetailList.to_json v.steps));
           Some ("instances", (JobFlowInstancesDetail.to_json v.instances));
           Some
             ("execution_status_detail",
               (JobFlowExecutionStatusDetail.to_json
                  v.execution_status_detail));
           Util.option_map v.ami_version
             (fun f -> ("ami_version", (String.to_json f)));
           Util.option_map v.log_uri
             (fun f -> ("log_uri", (String.to_json f)));
           Some ("name", (String.to_json v.name));
           Some ("job_flow_id", (String.to_json v.job_flow_id))])
    let of_json j =
      {
        job_flow_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "job_flow_id")));
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        log_uri = (Util.option_map (Json.lookup j "log_uri") String.of_json);
        ami_version =
          (Util.option_map (Json.lookup j "ami_version") String.of_json);
        execution_status_detail =
          (JobFlowExecutionStatusDetail.of_json
             (Util.of_option_exn (Json.lookup j "execution_status_detail")));
        instances =
          (JobFlowInstancesDetail.of_json
             (Util.of_option_exn (Json.lookup j "instances")));
        steps =
          (StepDetailList.of_json
             (Util.of_option_exn (Json.lookup j "steps")));
        bootstrap_actions =
          (BootstrapActionDetailList.of_json
             (Util.of_option_exn (Json.lookup j "bootstrap_actions")));
        supported_products =
          (SupportedProductsList.of_json
             (Util.of_option_exn (Json.lookup j "supported_products")));
        visible_to_all_users =
          (Util.option_map (Json.lookup j "visible_to_all_users")
             Boolean.of_json);
        job_flow_role =
          (Util.option_map (Json.lookup j "job_flow_role") String.of_json);
        service_role =
          (Util.option_map (Json.lookup j "service_role") String.of_json)
      }
  end
module Command =
  struct
    type t =
      {
      name: String.t option ;
      script_path: String.t option ;
      args: StringList.t }
    let make ?name  ?script_path  ?(args= [])  () =
      { name; script_path; args }
    let parse xml =
      Some
        {
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          script_path =
            (Util.option_bind (Xml.member "ScriptPath" xml) String.parse);
          args =
            (Util.of_option []
               (Util.option_bind (Xml.member "Args" xml) StringList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Args.member", (StringList.to_query v.args)));
           Util.option_map v.script_path
             (fun f -> Query.Pair ("ScriptPath", (String.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("args", (StringList.to_json v.args));
           Util.option_map v.script_path
             (fun f -> ("script_path", (String.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)))])
    let of_json j =
      {
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        script_path =
          (Util.option_map (Json.lookup j "script_path") String.of_json);
        args =
          (StringList.of_json (Util.of_option_exn (Json.lookup j "args")))
      }
  end
module ClusterStateList =
  struct
    type t = ClusterState.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ClusterState.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list ClusterState.to_query v
    let to_json v = `List (List.map ClusterState.to_json v)
    let of_json j = Json.to_list ClusterState.of_json j
  end
module Step =
  struct
    type t =
      {
      id: String.t option ;
      name: String.t option ;
      config: HadoopStepConfig.t option ;
      action_on_failure: ActionOnFailure.t option ;
      status: StepStatus.t option }
    let make ?id  ?name  ?config  ?action_on_failure  ?status  () =
      { id; name; config; action_on_failure; status }
    let parse xml =
      Some
        {
          id = (Util.option_bind (Xml.member "Id" xml) String.parse);
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          config =
            (Util.option_bind (Xml.member "Config" xml)
               HadoopStepConfig.parse);
          action_on_failure =
            (Util.option_bind (Xml.member "ActionOnFailure" xml)
               ActionOnFailure.parse);
          status =
            (Util.option_bind (Xml.member "Status" xml) StepStatus.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> Query.Pair ("Status", (StepStatus.to_query f)));
           Util.option_map v.action_on_failure
             (fun f ->
                Query.Pair ("ActionOnFailure", (ActionOnFailure.to_query f)));
           Util.option_map v.config
             (fun f -> Query.Pair ("Config", (HadoopStepConfig.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)));
           Util.option_map v.id
             (fun f -> Query.Pair ("Id", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> ("status", (StepStatus.to_json f)));
           Util.option_map v.action_on_failure
             (fun f -> ("action_on_failure", (ActionOnFailure.to_json f)));
           Util.option_map v.config
             (fun f -> ("config", (HadoopStepConfig.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)));
           Util.option_map v.id (fun f -> ("id", (String.to_json f)))])
    let of_json j =
      {
        id = (Util.option_map (Json.lookup j "id") String.of_json);
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        config =
          (Util.option_map (Json.lookup j "config") HadoopStepConfig.of_json);
        action_on_failure =
          (Util.option_map (Json.lookup j "action_on_failure")
             ActionOnFailure.of_json);
        status =
          (Util.option_map (Json.lookup j "status") StepStatus.of_json)
      }
  end
module Cluster =
  struct
    type t =
      {
      id: String.t ;
      name: String.t ;
      status: ClusterStatus.t ;
      ec2_instance_attributes: Ec2InstanceAttributes.t option ;
      log_uri: String.t option ;
      requested_ami_version: String.t option ;
      running_ami_version: String.t option ;
      release_label: String.t option ;
      auto_terminate: Boolean.t option ;
      termination_protected: Boolean.t option ;
      visible_to_all_users: Boolean.t option ;
      applications: ApplicationList.t ;
      tags: TagList.t ;
      service_role: String.t option ;
      normalized_instance_hours: Integer.t option ;
      master_public_dns_name: String.t option ;
      configurations: ConfigurationList.t }
    let make ~id  ~name  ~status  ?ec2_instance_attributes  ?log_uri
      ?requested_ami_version  ?running_ami_version  ?release_label
      ?auto_terminate  ?termination_protected  ?visible_to_all_users
      ?(applications= [])  ?(tags= [])  ?service_role
      ?normalized_instance_hours  ?master_public_dns_name  ?(configurations=
      [])  () =
      {
        id;
        name;
        status;
        ec2_instance_attributes;
        log_uri;
        requested_ami_version;
        running_ami_version;
        release_label;
        auto_terminate;
        termination_protected;
        visible_to_all_users;
        applications;
        tags;
        service_role;
        normalized_instance_hours;
        master_public_dns_name;
        configurations
      }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          status =
            (Xml.required "Status"
               (Util.option_bind (Xml.member "Status" xml)
                  ClusterStatus.parse));
          ec2_instance_attributes =
            (Util.option_bind (Xml.member "Ec2InstanceAttributes" xml)
               Ec2InstanceAttributes.parse);
          log_uri = (Util.option_bind (Xml.member "LogUri" xml) String.parse);
          requested_ami_version =
            (Util.option_bind (Xml.member "RequestedAmiVersion" xml)
               String.parse);
          running_ami_version =
            (Util.option_bind (Xml.member "RunningAmiVersion" xml)
               String.parse);
          release_label =
            (Util.option_bind (Xml.member "ReleaseLabel" xml) String.parse);
          auto_terminate =
            (Util.option_bind (Xml.member "AutoTerminate" xml) Boolean.parse);
          termination_protected =
            (Util.option_bind (Xml.member "TerminationProtected" xml)
               Boolean.parse);
          visible_to_all_users =
            (Util.option_bind (Xml.member "VisibleToAllUsers" xml)
               Boolean.parse);
          applications =
            (Util.of_option []
               (Util.option_bind (Xml.member "Applications" xml)
                  ApplicationList.parse));
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse));
          service_role =
            (Util.option_bind (Xml.member "ServiceRole" xml) String.parse);
          normalized_instance_hours =
            (Util.option_bind (Xml.member "NormalizedInstanceHours" xml)
               Integer.parse);
          master_public_dns_name =
            (Util.option_bind (Xml.member "MasterPublicDnsName" xml)
               String.parse);
          configurations =
            (Util.of_option []
               (Util.option_bind (Xml.member "Configurations" xml)
                  ConfigurationList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Configurations.member",
                   (ConfigurationList.to_query v.configurations)));
           Util.option_map v.master_public_dns_name
             (fun f ->
                Query.Pair ("MasterPublicDnsName", (String.to_query f)));
           Util.option_map v.normalized_instance_hours
             (fun f ->
                Query.Pair ("NormalizedInstanceHours", (Integer.to_query f)));
           Util.option_map v.service_role
             (fun f -> Query.Pair ("ServiceRole", (String.to_query f)));
           Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Some
             (Query.Pair
                ("Applications.member",
                  (ApplicationList.to_query v.applications)));
           Util.option_map v.visible_to_all_users
             (fun f -> Query.Pair ("VisibleToAllUsers", (Boolean.to_query f)));
           Util.option_map v.termination_protected
             (fun f ->
                Query.Pair ("TerminationProtected", (Boolean.to_query f)));
           Util.option_map v.auto_terminate
             (fun f -> Query.Pair ("AutoTerminate", (Boolean.to_query f)));
           Util.option_map v.release_label
             (fun f -> Query.Pair ("ReleaseLabel", (String.to_query f)));
           Util.option_map v.running_ami_version
             (fun f -> Query.Pair ("RunningAmiVersion", (String.to_query f)));
           Util.option_map v.requested_ami_version
             (fun f ->
                Query.Pair ("RequestedAmiVersion", (String.to_query f)));
           Util.option_map v.log_uri
             (fun f -> Query.Pair ("LogUri", (String.to_query f)));
           Util.option_map v.ec2_instance_attributes
             (fun f ->
                Query.Pair
                  ("Ec2InstanceAttributes",
                    (Ec2InstanceAttributes.to_query f)));
           Some (Query.Pair ("Status", (ClusterStatus.to_query v.status)));
           Some (Query.Pair ("Name", (String.to_query v.name)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("configurations",
                (ConfigurationList.to_json v.configurations));
           Util.option_map v.master_public_dns_name
             (fun f -> ("master_public_dns_name", (String.to_json f)));
           Util.option_map v.normalized_instance_hours
             (fun f -> ("normalized_instance_hours", (Integer.to_json f)));
           Util.option_map v.service_role
             (fun f -> ("service_role", (String.to_json f)));
           Some ("tags", (TagList.to_json v.tags));
           Some ("applications", (ApplicationList.to_json v.applications));
           Util.option_map v.visible_to_all_users
             (fun f -> ("visible_to_all_users", (Boolean.to_json f)));
           Util.option_map v.termination_protected
             (fun f -> ("termination_protected", (Boolean.to_json f)));
           Util.option_map v.auto_terminate
             (fun f -> ("auto_terminate", (Boolean.to_json f)));
           Util.option_map v.release_label
             (fun f -> ("release_label", (String.to_json f)));
           Util.option_map v.running_ami_version
             (fun f -> ("running_ami_version", (String.to_json f)));
           Util.option_map v.requested_ami_version
             (fun f -> ("requested_ami_version", (String.to_json f)));
           Util.option_map v.log_uri
             (fun f -> ("log_uri", (String.to_json f)));
           Util.option_map v.ec2_instance_attributes
             (fun f ->
                ("ec2_instance_attributes",
                  (Ec2InstanceAttributes.to_json f)));
           Some ("status", (ClusterStatus.to_json v.status));
           Some ("name", (String.to_json v.name));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        status =
          (ClusterStatus.of_json
             (Util.of_option_exn (Json.lookup j "status")));
        ec2_instance_attributes =
          (Util.option_map (Json.lookup j "ec2_instance_attributes")
             Ec2InstanceAttributes.of_json);
        log_uri = (Util.option_map (Json.lookup j "log_uri") String.of_json);
        requested_ami_version =
          (Util.option_map (Json.lookup j "requested_ami_version")
             String.of_json);
        running_ami_version =
          (Util.option_map (Json.lookup j "running_ami_version")
             String.of_json);
        release_label =
          (Util.option_map (Json.lookup j "release_label") String.of_json);
        auto_terminate =
          (Util.option_map (Json.lookup j "auto_terminate") Boolean.of_json);
        termination_protected =
          (Util.option_map (Json.lookup j "termination_protected")
             Boolean.of_json);
        visible_to_all_users =
          (Util.option_map (Json.lookup j "visible_to_all_users")
             Boolean.of_json);
        applications =
          (ApplicationList.of_json
             (Util.of_option_exn (Json.lookup j "applications")));
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")));
        service_role =
          (Util.option_map (Json.lookup j "service_role") String.of_json);
        normalized_instance_hours =
          (Util.option_map (Json.lookup j "normalized_instance_hours")
             Integer.of_json);
        master_public_dns_name =
          (Util.option_map (Json.lookup j "master_public_dns_name")
             String.of_json);
        configurations =
          (ConfigurationList.of_json
             (Util.of_option_exn (Json.lookup j "configurations")))
      }
  end
module BootstrapActionConfigList =
  struct
    type t = BootstrapActionConfig.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map BootstrapActionConfig.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list BootstrapActionConfig.to_query v
    let to_json v = `List (List.map BootstrapActionConfig.to_json v)
    let of_json j = Json.to_list BootstrapActionConfig.of_json j
  end
module JobFlowInstancesConfig =
  struct
    type t =
      {
      master_instance_type: String.t option ;
      slave_instance_type: String.t option ;
      instance_count: Integer.t option ;
      instance_groups: InstanceGroupConfigList.t ;
      ec2_key_name: String.t option ;
      placement: PlacementType.t option ;
      keep_job_flow_alive_when_no_steps: Boolean.t option ;
      termination_protected: Boolean.t option ;
      hadoop_version: String.t option ;
      ec2_subnet_id: String.t option ;
      emr_managed_master_security_group: String.t option ;
      emr_managed_slave_security_group: String.t option ;
      additional_master_security_groups: SecurityGroupsList.t ;
      additional_slave_security_groups: SecurityGroupsList.t }
    let make ?master_instance_type  ?slave_instance_type  ?instance_count
      ?(instance_groups= [])  ?ec2_key_name  ?placement
      ?keep_job_flow_alive_when_no_steps  ?termination_protected
      ?hadoop_version  ?ec2_subnet_id  ?emr_managed_master_security_group
      ?emr_managed_slave_security_group  ?(additional_master_security_groups=
      [])  ?(additional_slave_security_groups= [])  () =
      {
        master_instance_type;
        slave_instance_type;
        instance_count;
        instance_groups;
        ec2_key_name;
        placement;
        keep_job_flow_alive_when_no_steps;
        termination_protected;
        hadoop_version;
        ec2_subnet_id;
        emr_managed_master_security_group;
        emr_managed_slave_security_group;
        additional_master_security_groups;
        additional_slave_security_groups
      }
    let parse xml =
      Some
        {
          master_instance_type =
            (Util.option_bind (Xml.member "MasterInstanceType" xml)
               String.parse);
          slave_instance_type =
            (Util.option_bind (Xml.member "SlaveInstanceType" xml)
               String.parse);
          instance_count =
            (Util.option_bind (Xml.member "InstanceCount" xml) Integer.parse);
          instance_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "InstanceGroups" xml)
                  InstanceGroupConfigList.parse));
          ec2_key_name =
            (Util.option_bind (Xml.member "Ec2KeyName" xml) String.parse);
          placement =
            (Util.option_bind (Xml.member "Placement" xml)
               PlacementType.parse);
          keep_job_flow_alive_when_no_steps =
            (Util.option_bind (Xml.member "KeepJobFlowAliveWhenNoSteps" xml)
               Boolean.parse);
          termination_protected =
            (Util.option_bind (Xml.member "TerminationProtected" xml)
               Boolean.parse);
          hadoop_version =
            (Util.option_bind (Xml.member "HadoopVersion" xml) String.parse);
          ec2_subnet_id =
            (Util.option_bind (Xml.member "Ec2SubnetId" xml) String.parse);
          emr_managed_master_security_group =
            (Util.option_bind
               (Xml.member "EmrManagedMasterSecurityGroup" xml) String.parse);
          emr_managed_slave_security_group =
            (Util.option_bind (Xml.member "EmrManagedSlaveSecurityGroup" xml)
               String.parse);
          additional_master_security_groups =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "AdditionalMasterSecurityGroups" xml)
                  SecurityGroupsList.parse));
          additional_slave_security_groups =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "AdditionalSlaveSecurityGroups" xml)
                  SecurityGroupsList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("AdditionalSlaveSecurityGroups.member",
                   (SecurityGroupsList.to_query
                      v.additional_slave_security_groups)));
           Some
             (Query.Pair
                ("AdditionalMasterSecurityGroups.member",
                  (SecurityGroupsList.to_query
                     v.additional_master_security_groups)));
           Util.option_map v.emr_managed_slave_security_group
             (fun f ->
                Query.Pair
                  ("EmrManagedSlaveSecurityGroup", (String.to_query f)));
           Util.option_map v.emr_managed_master_security_group
             (fun f ->
                Query.Pair
                  ("EmrManagedMasterSecurityGroup", (String.to_query f)));
           Util.option_map v.ec2_subnet_id
             (fun f -> Query.Pair ("Ec2SubnetId", (String.to_query f)));
           Util.option_map v.hadoop_version
             (fun f -> Query.Pair ("HadoopVersion", (String.to_query f)));
           Util.option_map v.termination_protected
             (fun f ->
                Query.Pair ("TerminationProtected", (Boolean.to_query f)));
           Util.option_map v.keep_job_flow_alive_when_no_steps
             (fun f ->
                Query.Pair
                  ("KeepJobFlowAliveWhenNoSteps", (Boolean.to_query f)));
           Util.option_map v.placement
             (fun f -> Query.Pair ("Placement", (PlacementType.to_query f)));
           Util.option_map v.ec2_key_name
             (fun f -> Query.Pair ("Ec2KeyName", (String.to_query f)));
           Some
             (Query.Pair
                ("InstanceGroups.member",
                  (InstanceGroupConfigList.to_query v.instance_groups)));
           Util.option_map v.instance_count
             (fun f -> Query.Pair ("InstanceCount", (Integer.to_query f)));
           Util.option_map v.slave_instance_type
             (fun f -> Query.Pair ("SlaveInstanceType", (String.to_query f)));
           Util.option_map v.master_instance_type
             (fun f -> Query.Pair ("MasterInstanceType", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("additional_slave_security_groups",
                (SecurityGroupsList.to_json
                   v.additional_slave_security_groups));
           Some
             ("additional_master_security_groups",
               (SecurityGroupsList.to_json
                  v.additional_master_security_groups));
           Util.option_map v.emr_managed_slave_security_group
             (fun f ->
                ("emr_managed_slave_security_group", (String.to_json f)));
           Util.option_map v.emr_managed_master_security_group
             (fun f ->
                ("emr_managed_master_security_group", (String.to_json f)));
           Util.option_map v.ec2_subnet_id
             (fun f -> ("ec2_subnet_id", (String.to_json f)));
           Util.option_map v.hadoop_version
             (fun f -> ("hadoop_version", (String.to_json f)));
           Util.option_map v.termination_protected
             (fun f -> ("termination_protected", (Boolean.to_json f)));
           Util.option_map v.keep_job_flow_alive_when_no_steps
             (fun f ->
                ("keep_job_flow_alive_when_no_steps", (Boolean.to_json f)));
           Util.option_map v.placement
             (fun f -> ("placement", (PlacementType.to_json f)));
           Util.option_map v.ec2_key_name
             (fun f -> ("ec2_key_name", (String.to_json f)));
           Some
             ("instance_groups",
               (InstanceGroupConfigList.to_json v.instance_groups));
           Util.option_map v.instance_count
             (fun f -> ("instance_count", (Integer.to_json f)));
           Util.option_map v.slave_instance_type
             (fun f -> ("slave_instance_type", (String.to_json f)));
           Util.option_map v.master_instance_type
             (fun f -> ("master_instance_type", (String.to_json f)))])
    let of_json j =
      {
        master_instance_type =
          (Util.option_map (Json.lookup j "master_instance_type")
             String.of_json);
        slave_instance_type =
          (Util.option_map (Json.lookup j "slave_instance_type")
             String.of_json);
        instance_count =
          (Util.option_map (Json.lookup j "instance_count") Integer.of_json);
        instance_groups =
          (InstanceGroupConfigList.of_json
             (Util.of_option_exn (Json.lookup j "instance_groups")));
        ec2_key_name =
          (Util.option_map (Json.lookup j "ec2_key_name") String.of_json);
        placement =
          (Util.option_map (Json.lookup j "placement") PlacementType.of_json);
        keep_job_flow_alive_when_no_steps =
          (Util.option_map
             (Json.lookup j "keep_job_flow_alive_when_no_steps")
             Boolean.of_json);
        termination_protected =
          (Util.option_map (Json.lookup j "termination_protected")
             Boolean.of_json);
        hadoop_version =
          (Util.option_map (Json.lookup j "hadoop_version") String.of_json);
        ec2_subnet_id =
          (Util.option_map (Json.lookup j "ec2_subnet_id") String.of_json);
        emr_managed_master_security_group =
          (Util.option_map
             (Json.lookup j "emr_managed_master_security_group")
             String.of_json);
        emr_managed_slave_security_group =
          (Util.option_map (Json.lookup j "emr_managed_slave_security_group")
             String.of_json);
        additional_master_security_groups =
          (SecurityGroupsList.of_json
             (Util.of_option_exn
                (Json.lookup j "additional_master_security_groups")));
        additional_slave_security_groups =
          (SecurityGroupsList.of_json
             (Util.of_option_exn
                (Json.lookup j "additional_slave_security_groups")))
      }
  end
module NewSupportedProductsList =
  struct
    type t = SupportedProductConfig.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map SupportedProductConfig.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list SupportedProductConfig.to_query v
    let to_json v = `List (List.map SupportedProductConfig.to_json v)
    let of_json j = Json.to_list SupportedProductConfig.of_json j
  end
module StepConfigList =
  struct
    type t = StepConfig.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map StepConfig.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list StepConfig.to_query v
    let to_json v = `List (List.map StepConfig.to_json v)
    let of_json j = Json.to_list StepConfig.of_json j
  end
module StepSummaryList =
  struct
    type t = StepSummary.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map StepSummary.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list StepSummary.to_query v
    let to_json v = `List (List.map StepSummary.to_json v)
    let of_json j = Json.to_list StepSummary.of_json j
  end
module InstanceGroupModifyConfigList =
  struct
    type t = InstanceGroupModifyConfig.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map InstanceGroupModifyConfig.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list InstanceGroupModifyConfig.to_query v
    let to_json v = `List (List.map InstanceGroupModifyConfig.to_json v)
    let of_json j = Json.to_list InstanceGroupModifyConfig.of_json j
  end
module InstanceGroupList =
  struct
    type t = InstanceGroup.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map InstanceGroup.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list InstanceGroup.to_query v
    let to_json v = `List (List.map InstanceGroup.to_json v)
    let of_json j = Json.to_list InstanceGroup.of_json j
  end
module StepIdsList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module InstanceList =
  struct
    type t = Instance.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Instance.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Instance.to_query v
    let to_json v = `List (List.map Instance.to_json v)
    let of_json j = Json.to_list Instance.of_json j
  end
module ClusterSummaryList =
  struct
    type t = ClusterSummary.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ClusterSummary.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list ClusterSummary.to_query v
    let to_json v = `List (List.map ClusterSummary.to_json v)
    let of_json j = Json.to_list ClusterSummary.of_json j
  end
module StepStateList =
  struct
    type t = StepState.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map StepState.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list StepState.to_query v
    let to_json v = `List (List.map StepState.to_json v)
    let of_json j = Json.to_list StepState.of_json j
  end
module JobFlowExecutionStateList =
  struct
    type t = JobFlowExecutionState.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map JobFlowExecutionState.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list JobFlowExecutionState.to_query v
    let to_json v = `List (List.map JobFlowExecutionState.to_json v)
    let of_json j = Json.to_list JobFlowExecutionState.of_json j
  end
module JobFlowDetailList =
  struct
    type t = JobFlowDetail.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map JobFlowDetail.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list JobFlowDetail.to_query v
    let to_json v = `List (List.map JobFlowDetail.to_json v)
    let of_json j = Json.to_list JobFlowDetail.of_json j
  end
module CommandList =
  struct
    type t = Command.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Command.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Command.to_query v
    let to_json v = `List (List.map Command.to_json v)
    let of_json j = Json.to_list Command.of_json j
  end
module InstanceGroupIdsList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module InstanceGroupTypeList =
  struct
    type t = InstanceGroupType.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map InstanceGroupType.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list InstanceGroupType.to_query v
    let to_json v = `List (List.map InstanceGroupType.to_json v)
    let of_json j = Json.to_list InstanceGroupType.of_json j
  end
module ListInstanceGroupsInput =
  struct
    type t = {
      cluster_id: String.t ;
      marker: String.t option }
    let make ~cluster_id  ?marker  () = { cluster_id; marker }
    let parse xml =
      Some
        {
          cluster_id =
            (Xml.required "ClusterId"
               (Util.option_bind (Xml.member "ClusterId" xml) String.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some (Query.Pair ("ClusterId", (String.to_query v.cluster_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Some ("cluster_id", (String.to_json v.cluster_id))])
    let of_json j =
      {
        cluster_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "cluster_id")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module AddTagsInput =
  struct
    type t = {
      resource_id: String.t ;
      tags: TagList.t }
    let make ~resource_id  ~tags  () = { resource_id; tags }
    let parse xml =
      Some
        {
          resource_id =
            (Xml.required "ResourceId"
               (Util.option_bind (Xml.member "ResourceId" xml) String.parse));
          tags =
            (Xml.required "Tags"
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Some (Query.Pair ("ResourceId", (String.to_query v.resource_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Some ("resource_id", (String.to_json v.resource_id))])
    let of_json j =
      {
        resource_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "resource_id")));
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module ListClustersInput =
  struct
    type t =
      {
      created_after: DateTime.t option ;
      created_before: DateTime.t option ;
      cluster_states: ClusterStateList.t ;
      marker: String.t option }
    let make ?created_after  ?created_before  ?(cluster_states= [])  ?marker
      () = { created_after; created_before; cluster_states; marker }
    let parse xml =
      Some
        {
          created_after =
            (Util.option_bind (Xml.member "CreatedAfter" xml) DateTime.parse);
          created_before =
            (Util.option_bind (Xml.member "CreatedBefore" xml) DateTime.parse);
          cluster_states =
            (Util.of_option []
               (Util.option_bind (Xml.member "ClusterStates" xml)
                  ClusterStateList.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some
             (Query.Pair
                ("ClusterStates.member",
                  (ClusterStateList.to_query v.cluster_states)));
           Util.option_map v.created_before
             (fun f -> Query.Pair ("CreatedBefore", (DateTime.to_query f)));
           Util.option_map v.created_after
             (fun f -> Query.Pair ("CreatedAfter", (DateTime.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Some
             ("cluster_states", (ClusterStateList.to_json v.cluster_states));
           Util.option_map v.created_before
             (fun f -> ("created_before", (DateTime.to_json f)));
           Util.option_map v.created_after
             (fun f -> ("created_after", (DateTime.to_json f)))])
    let of_json j =
      {
        created_after =
          (Util.option_map (Json.lookup j "created_after") DateTime.of_json);
        created_before =
          (Util.option_map (Json.lookup j "created_before") DateTime.of_json);
        cluster_states =
          (ClusterStateList.of_json
             (Util.of_option_exn (Json.lookup j "cluster_states")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module DescribeStepOutput =
  struct
    type t = {
      step: Step.t option }
    let make ?step  () = { step }
    let parse xml =
      Some { step = (Util.option_bind (Xml.member "Step" xml) Step.parse) }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.step
              (fun f -> Query.Pair ("Step", (Step.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.step (fun f -> ("step", (Step.to_json f)))])
    let of_json j =
      { step = (Util.option_map (Json.lookup j "step") Step.of_json) }
  end
module AddInstanceGroupsInput =
  struct
    type t =
      {
      instance_groups: InstanceGroupConfigList.t ;
      job_flow_id: String.t }
    let make ~instance_groups  ~job_flow_id  () =
      { instance_groups; job_flow_id }
    let parse xml =
      Some
        {
          instance_groups =
            (Xml.required "InstanceGroups"
               (Util.option_bind (Xml.member "InstanceGroups" xml)
                  InstanceGroupConfigList.parse));
          job_flow_id =
            (Xml.required "JobFlowId"
               (Util.option_bind (Xml.member "JobFlowId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("JobFlowId", (String.to_query v.job_flow_id)));
           Some
             (Query.Pair
                ("InstanceGroups.member",
                  (InstanceGroupConfigList.to_query v.instance_groups)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("job_flow_id", (String.to_json v.job_flow_id));
           Some
             ("instance_groups",
               (InstanceGroupConfigList.to_json v.instance_groups))])
    let of_json j =
      {
        instance_groups =
          (InstanceGroupConfigList.of_json
             (Util.of_option_exn (Json.lookup j "instance_groups")));
        job_flow_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "job_flow_id")))
      }
  end
module SetVisibleToAllUsersInput =
  struct
    type t =
      {
      job_flow_ids: XmlStringList.t ;
      visible_to_all_users: Boolean.t }
    let make ~job_flow_ids  ~visible_to_all_users  () =
      { job_flow_ids; visible_to_all_users }
    let parse xml =
      Some
        {
          job_flow_ids =
            (Xml.required "JobFlowIds"
               (Util.option_bind (Xml.member "JobFlowIds" xml)
                  XmlStringList.parse));
          visible_to_all_users =
            (Xml.required "VisibleToAllUsers"
               (Util.option_bind (Xml.member "VisibleToAllUsers" xml)
                  Boolean.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("VisibleToAllUsers",
                   (Boolean.to_query v.visible_to_all_users)));
           Some
             (Query.Pair
                ("JobFlowIds.member",
                  (XmlStringList.to_query v.job_flow_ids)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("visible_to_all_users",
                (Boolean.to_json v.visible_to_all_users));
           Some ("job_flow_ids", (XmlStringList.to_json v.job_flow_ids))])
    let of_json j =
      {
        job_flow_ids =
          (XmlStringList.of_json
             (Util.of_option_exn (Json.lookup j "job_flow_ids")));
        visible_to_all_users =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "visible_to_all_users")))
      }
  end
module ListBootstrapActionsInput =
  struct
    type t = {
      cluster_id: String.t ;
      marker: String.t option }
    let make ~cluster_id  ?marker  () = { cluster_id; marker }
    let parse xml =
      Some
        {
          cluster_id =
            (Xml.required "ClusterId"
               (Util.option_bind (Xml.member "ClusterId" xml) String.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some (Query.Pair ("ClusterId", (String.to_query v.cluster_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Some ("cluster_id", (String.to_json v.cluster_id))])
    let of_json j =
      {
        cluster_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "cluster_id")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module DescribeClusterOutput =
  struct
    type t = {
      cluster: Cluster.t }
    let make ~cluster  () = { cluster }
    let parse xml =
      Some
        {
          cluster =
            (Xml.required "Cluster"
               (Util.option_bind (Xml.member "Cluster" xml) Cluster.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Cluster", (Cluster.to_query v.cluster)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("cluster", (Cluster.to_json v.cluster))])
    let of_json j =
      {
        cluster =
          (Cluster.of_json (Util.of_option_exn (Json.lookup j "cluster")))
      }
  end
module DescribeStepInput =
  struct
    type t = {
      cluster_id: String.t ;
      step_id: String.t }
    let make ~cluster_id  ~step_id  () = { cluster_id; step_id }
    let parse xml =
      Some
        {
          cluster_id =
            (Xml.required "ClusterId"
               (Util.option_bind (Xml.member "ClusterId" xml) String.parse));
          step_id =
            (Xml.required "StepId"
               (Util.option_bind (Xml.member "StepId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("StepId", (String.to_query v.step_id)));
           Some (Query.Pair ("ClusterId", (String.to_query v.cluster_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("step_id", (String.to_json v.step_id));
           Some ("cluster_id", (String.to_json v.cluster_id))])
    let of_json j =
      {
        cluster_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "cluster_id")));
        step_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "step_id")))
      }
  end
module RunJobFlowInput =
  struct
    type t =
      {
      name: String.t ;
      log_uri: String.t option ;
      additional_info: String.t option ;
      ami_version: String.t option ;
      release_label: String.t option ;
      instances: JobFlowInstancesConfig.t ;
      steps: StepConfigList.t ;
      bootstrap_actions: BootstrapActionConfigList.t ;
      supported_products: SupportedProductsList.t ;
      new_supported_products: NewSupportedProductsList.t ;
      applications: ApplicationList.t ;
      configurations: ConfigurationList.t ;
      visible_to_all_users: Boolean.t option ;
      job_flow_role: String.t option ;
      service_role: String.t option ;
      tags: TagList.t }
    let make ~name  ?log_uri  ?additional_info  ?ami_version  ?release_label
      ~instances  ?(steps= [])  ?(bootstrap_actions= [])
      ?(supported_products= [])  ?(new_supported_products= [])
      ?(applications= [])  ?(configurations= [])  ?visible_to_all_users
      ?job_flow_role  ?service_role  ?(tags= [])  () =
      {
        name;
        log_uri;
        additional_info;
        ami_version;
        release_label;
        instances;
        steps;
        bootstrap_actions;
        supported_products;
        new_supported_products;
        applications;
        configurations;
        visible_to_all_users;
        job_flow_role;
        service_role;
        tags
      }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          log_uri = (Util.option_bind (Xml.member "LogUri" xml) String.parse);
          additional_info =
            (Util.option_bind (Xml.member "AdditionalInfo" xml) String.parse);
          ami_version =
            (Util.option_bind (Xml.member "AmiVersion" xml) String.parse);
          release_label =
            (Util.option_bind (Xml.member "ReleaseLabel" xml) String.parse);
          instances =
            (Xml.required "Instances"
               (Util.option_bind (Xml.member "Instances" xml)
                  JobFlowInstancesConfig.parse));
          steps =
            (Util.of_option []
               (Util.option_bind (Xml.member "Steps" xml)
                  StepConfigList.parse));
          bootstrap_actions =
            (Util.of_option []
               (Util.option_bind (Xml.member "BootstrapActions" xml)
                  BootstrapActionConfigList.parse));
          supported_products =
            (Util.of_option []
               (Util.option_bind (Xml.member "SupportedProducts" xml)
                  SupportedProductsList.parse));
          new_supported_products =
            (Util.of_option []
               (Util.option_bind (Xml.member "NewSupportedProducts" xml)
                  NewSupportedProductsList.parse));
          applications =
            (Util.of_option []
               (Util.option_bind (Xml.member "Applications" xml)
                  ApplicationList.parse));
          configurations =
            (Util.of_option []
               (Util.option_bind (Xml.member "Configurations" xml)
                  ConfigurationList.parse));
          visible_to_all_users =
            (Util.option_bind (Xml.member "VisibleToAllUsers" xml)
               Boolean.parse);
          job_flow_role =
            (Util.option_bind (Xml.member "JobFlowRole" xml) String.parse);
          service_role =
            (Util.option_bind (Xml.member "ServiceRole" xml) String.parse);
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Util.option_map v.service_role
             (fun f -> Query.Pair ("ServiceRole", (String.to_query f)));
           Util.option_map v.job_flow_role
             (fun f -> Query.Pair ("JobFlowRole", (String.to_query f)));
           Util.option_map v.visible_to_all_users
             (fun f -> Query.Pair ("VisibleToAllUsers", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("Configurations.member",
                  (ConfigurationList.to_query v.configurations)));
           Some
             (Query.Pair
                ("Applications.member",
                  (ApplicationList.to_query v.applications)));
           Some
             (Query.Pair
                ("NewSupportedProducts.member",
                  (NewSupportedProductsList.to_query v.new_supported_products)));
           Some
             (Query.Pair
                ("SupportedProducts.member",
                  (SupportedProductsList.to_query v.supported_products)));
           Some
             (Query.Pair
                ("BootstrapActions.member",
                  (BootstrapActionConfigList.to_query v.bootstrap_actions)));
           Some
             (Query.Pair ("Steps.member", (StepConfigList.to_query v.steps)));
           Some
             (Query.Pair
                ("Instances", (JobFlowInstancesConfig.to_query v.instances)));
           Util.option_map v.release_label
             (fun f -> Query.Pair ("ReleaseLabel", (String.to_query f)));
           Util.option_map v.ami_version
             (fun f -> Query.Pair ("AmiVersion", (String.to_query f)));
           Util.option_map v.additional_info
             (fun f -> Query.Pair ("AdditionalInfo", (String.to_query f)));
           Util.option_map v.log_uri
             (fun f -> Query.Pair ("LogUri", (String.to_query f)));
           Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Util.option_map v.service_role
             (fun f -> ("service_role", (String.to_json f)));
           Util.option_map v.job_flow_role
             (fun f -> ("job_flow_role", (String.to_json f)));
           Util.option_map v.visible_to_all_users
             (fun f -> ("visible_to_all_users", (Boolean.to_json f)));
           Some
             ("configurations", (ConfigurationList.to_json v.configurations));
           Some ("applications", (ApplicationList.to_json v.applications));
           Some
             ("new_supported_products",
               (NewSupportedProductsList.to_json v.new_supported_products));
           Some
             ("supported_products",
               (SupportedProductsList.to_json v.supported_products));
           Some
             ("bootstrap_actions",
               (BootstrapActionConfigList.to_json v.bootstrap_actions));
           Some ("steps", (StepConfigList.to_json v.steps));
           Some ("instances", (JobFlowInstancesConfig.to_json v.instances));
           Util.option_map v.release_label
             (fun f -> ("release_label", (String.to_json f)));
           Util.option_map v.ami_version
             (fun f -> ("ami_version", (String.to_json f)));
           Util.option_map v.additional_info
             (fun f -> ("additional_info", (String.to_json f)));
           Util.option_map v.log_uri
             (fun f -> ("log_uri", (String.to_json f)));
           Some ("name", (String.to_json v.name))])
    let of_json j =
      {
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        log_uri = (Util.option_map (Json.lookup j "log_uri") String.of_json);
        additional_info =
          (Util.option_map (Json.lookup j "additional_info") String.of_json);
        ami_version =
          (Util.option_map (Json.lookup j "ami_version") String.of_json);
        release_label =
          (Util.option_map (Json.lookup j "release_label") String.of_json);
        instances =
          (JobFlowInstancesConfig.of_json
             (Util.of_option_exn (Json.lookup j "instances")));
        steps =
          (StepConfigList.of_json
             (Util.of_option_exn (Json.lookup j "steps")));
        bootstrap_actions =
          (BootstrapActionConfigList.of_json
             (Util.of_option_exn (Json.lookup j "bootstrap_actions")));
        supported_products =
          (SupportedProductsList.of_json
             (Util.of_option_exn (Json.lookup j "supported_products")));
        new_supported_products =
          (NewSupportedProductsList.of_json
             (Util.of_option_exn (Json.lookup j "new_supported_products")));
        applications =
          (ApplicationList.of_json
             (Util.of_option_exn (Json.lookup j "applications")));
        configurations =
          (ConfigurationList.of_json
             (Util.of_option_exn (Json.lookup j "configurations")));
        visible_to_all_users =
          (Util.option_map (Json.lookup j "visible_to_all_users")
             Boolean.of_json);
        job_flow_role =
          (Util.option_map (Json.lookup j "job_flow_role") String.of_json);
        service_role =
          (Util.option_map (Json.lookup j "service_role") String.of_json);
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module RemoveTagsInput =
  struct
    type t = {
      resource_id: String.t ;
      tag_keys: StringList.t }
    let make ~resource_id  ~tag_keys  () = { resource_id; tag_keys }
    let parse xml =
      Some
        {
          resource_id =
            (Xml.required "ResourceId"
               (Util.option_bind (Xml.member "ResourceId" xml) String.parse));
          tag_keys =
            (Xml.required "TagKeys"
               (Util.option_bind (Xml.member "TagKeys" xml) StringList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("TagKeys.member", (StringList.to_query v.tag_keys)));
           Some (Query.Pair ("ResourceId", (String.to_query v.resource_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tag_keys", (StringList.to_json v.tag_keys));
           Some ("resource_id", (String.to_json v.resource_id))])
    let of_json j =
      {
        resource_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "resource_id")));
        tag_keys =
          (StringList.of_json (Util.of_option_exn (Json.lookup j "tag_keys")))
      }
  end
module ListStepsOutput =
  struct
    type t = {
      steps: StepSummaryList.t ;
      marker: String.t option }
    let make ?(steps= [])  ?marker  () = { steps; marker }
    let parse xml =
      Some
        {
          steps =
            (Util.of_option []
               (Util.option_bind (Xml.member "Steps" xml)
                  StepSummaryList.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some
             (Query.Pair ("Steps.member", (StepSummaryList.to_query v.steps)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Some ("steps", (StepSummaryList.to_json v.steps))])
    let of_json j =
      {
        steps =
          (StepSummaryList.of_json
             (Util.of_option_exn (Json.lookup j "steps")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module ModifyInstanceGroupsInput =
  struct
    type t = {
      instance_groups: InstanceGroupModifyConfigList.t }
    let make ?(instance_groups= [])  () = { instance_groups }
    let parse xml =
      Some
        {
          instance_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "InstanceGroups" xml)
                  InstanceGroupModifyConfigList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("InstanceGroups.member",
                   (InstanceGroupModifyConfigList.to_query v.instance_groups)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("instance_groups",
                (InstanceGroupModifyConfigList.to_json v.instance_groups))])
    let of_json j =
      {
        instance_groups =
          (InstanceGroupModifyConfigList.of_json
             (Util.of_option_exn (Json.lookup j "instance_groups")))
      }
  end
module AddJobFlowStepsInput =
  struct
    type t = {
      job_flow_id: String.t ;
      steps: StepConfigList.t }
    let make ~job_flow_id  ~steps  () = { job_flow_id; steps }
    let parse xml =
      Some
        {
          job_flow_id =
            (Xml.required "JobFlowId"
               (Util.option_bind (Xml.member "JobFlowId" xml) String.parse));
          steps =
            (Xml.required "Steps"
               (Util.option_bind (Xml.member "Steps" xml)
                  StepConfigList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Steps.member", (StepConfigList.to_query v.steps)));
           Some (Query.Pair ("JobFlowId", (String.to_query v.job_flow_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("steps", (StepConfigList.to_json v.steps));
           Some ("job_flow_id", (String.to_json v.job_flow_id))])
    let of_json j =
      {
        job_flow_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "job_flow_id")));
        steps =
          (StepConfigList.of_json
             (Util.of_option_exn (Json.lookup j "steps")))
      }
  end
module ListInstanceGroupsOutput =
  struct
    type t = {
      instance_groups: InstanceGroupList.t ;
      marker: String.t option }
    let make ?(instance_groups= [])  ?marker  () =
      { instance_groups; marker }
    let parse xml =
      Some
        {
          instance_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "InstanceGroups" xml)
                  InstanceGroupList.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some
             (Query.Pair
                ("InstanceGroups.member",
                  (InstanceGroupList.to_query v.instance_groups)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Some
             ("instance_groups",
               (InstanceGroupList.to_json v.instance_groups))])
    let of_json j =
      {
        instance_groups =
          (InstanceGroupList.of_json
             (Util.of_option_exn (Json.lookup j "instance_groups")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module SetTerminationProtectionInput =
  struct
    type t =
      {
      job_flow_ids: XmlStringList.t ;
      termination_protected: Boolean.t }
    let make ~job_flow_ids  ~termination_protected  () =
      { job_flow_ids; termination_protected }
    let parse xml =
      Some
        {
          job_flow_ids =
            (Xml.required "JobFlowIds"
               (Util.option_bind (Xml.member "JobFlowIds" xml)
                  XmlStringList.parse));
          termination_protected =
            (Xml.required "TerminationProtected"
               (Util.option_bind (Xml.member "TerminationProtected" xml)
                  Boolean.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("TerminationProtected",
                   (Boolean.to_query v.termination_protected)));
           Some
             (Query.Pair
                ("JobFlowIds.member",
                  (XmlStringList.to_query v.job_flow_ids)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("termination_protected",
                (Boolean.to_json v.termination_protected));
           Some ("job_flow_ids", (XmlStringList.to_json v.job_flow_ids))])
    let of_json j =
      {
        job_flow_ids =
          (XmlStringList.of_json
             (Util.of_option_exn (Json.lookup j "job_flow_ids")));
        termination_protected =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "termination_protected")))
      }
  end
module AddJobFlowStepsOutput =
  struct
    type t = {
      step_ids: StepIdsList.t }
    let make ?(step_ids= [])  () = { step_ids }
    let parse xml =
      Some
        {
          step_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "StepIds" xml) StepIdsList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("StepIds.member", (StepIdsList.to_query v.step_ids)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("step_ids", (StepIdsList.to_json v.step_ids))])
    let of_json j =
      {
        step_ids =
          (StepIdsList.of_json
             (Util.of_option_exn (Json.lookup j "step_ids")))
      }
  end
module ListInstancesOutput =
  struct
    type t = {
      instances: InstanceList.t ;
      marker: String.t option }
    let make ?(instances= [])  ?marker  () = { instances; marker }
    let parse xml =
      Some
        {
          instances =
            (Util.of_option []
               (Util.option_bind (Xml.member "Instances" xml)
                  InstanceList.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some
             (Query.Pair
                ("Instances.member", (InstanceList.to_query v.instances)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Some ("instances", (InstanceList.to_json v.instances))])
    let of_json j =
      {
        instances =
          (InstanceList.of_json
             (Util.of_option_exn (Json.lookup j "instances")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module InternalServerException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ListClustersOutput =
  struct
    type t = {
      clusters: ClusterSummaryList.t ;
      marker: String.t option }
    let make ?(clusters= [])  ?marker  () = { clusters; marker }
    let parse xml =
      Some
        {
          clusters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Clusters" xml)
                  ClusterSummaryList.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some
             (Query.Pair
                ("Clusters.member", (ClusterSummaryList.to_query v.clusters)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Some ("clusters", (ClusterSummaryList.to_json v.clusters))])
    let of_json j =
      {
        clusters =
          (ClusterSummaryList.of_json
             (Util.of_option_exn (Json.lookup j "clusters")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module InvalidRequestException =
  struct
    type t = {
      error_code: String.t option ;
      message: String.t option }
    let make ?error_code  ?message  () = { error_code; message }
    let parse xml =
      Some
        {
          error_code =
            (Util.option_bind (Xml.member "ErrorCode" xml) String.parse);
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)));
           Util.option_map v.error_code
             (fun f -> Query.Pair ("ErrorCode", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)));
           Util.option_map v.error_code
             (fun f -> ("error_code", (String.to_json f)))])
    let of_json j =
      {
        error_code =
          (Util.option_map (Json.lookup j "error_code") String.of_json);
        message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ListStepsInput =
  struct
    type t =
      {
      cluster_id: String.t ;
      step_states: StepStateList.t ;
      step_ids: XmlStringList.t ;
      marker: String.t option }
    let make ~cluster_id  ?(step_states= [])  ?(step_ids= [])  ?marker  () =
      { cluster_id; step_states; step_ids; marker }
    let parse xml =
      Some
        {
          cluster_id =
            (Xml.required "ClusterId"
               (Util.option_bind (Xml.member "ClusterId" xml) String.parse));
          step_states =
            (Util.of_option []
               (Util.option_bind (Xml.member "StepStates" xml)
                  StepStateList.parse));
          step_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "StepIds" xml)
                  XmlStringList.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some
             (Query.Pair
                ("StepIds.member", (XmlStringList.to_query v.step_ids)));
           Some
             (Query.Pair
                ("StepStates.member", (StepStateList.to_query v.step_states)));
           Some (Query.Pair ("ClusterId", (String.to_query v.cluster_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Some ("step_ids", (XmlStringList.to_json v.step_ids));
           Some ("step_states", (StepStateList.to_json v.step_states));
           Some ("cluster_id", (String.to_json v.cluster_id))])
    let of_json j =
      {
        cluster_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "cluster_id")));
        step_states =
          (StepStateList.of_json
             (Util.of_option_exn (Json.lookup j "step_states")));
        step_ids =
          (XmlStringList.of_json
             (Util.of_option_exn (Json.lookup j "step_ids")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
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
module DescribeJobFlowsInput =
  struct
    type t =
      {
      created_after: DateTime.t option ;
      created_before: DateTime.t option ;
      job_flow_ids: XmlStringList.t ;
      job_flow_states: JobFlowExecutionStateList.t }
    let make ?created_after  ?created_before  ?(job_flow_ids= [])
      ?(job_flow_states= [])  () =
      { created_after; created_before; job_flow_ids; job_flow_states }
    let parse xml =
      Some
        {
          created_after =
            (Util.option_bind (Xml.member "CreatedAfter" xml) DateTime.parse);
          created_before =
            (Util.option_bind (Xml.member "CreatedBefore" xml) DateTime.parse);
          job_flow_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "JobFlowIds" xml)
                  XmlStringList.parse));
          job_flow_states =
            (Util.of_option []
               (Util.option_bind (Xml.member "JobFlowStates" xml)
                  JobFlowExecutionStateList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("JobFlowStates.member",
                   (JobFlowExecutionStateList.to_query v.job_flow_states)));
           Some
             (Query.Pair
                ("JobFlowIds.member",
                  (XmlStringList.to_query v.job_flow_ids)));
           Util.option_map v.created_before
             (fun f -> Query.Pair ("CreatedBefore", (DateTime.to_query f)));
           Util.option_map v.created_after
             (fun f -> Query.Pair ("CreatedAfter", (DateTime.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("job_flow_states",
                (JobFlowExecutionStateList.to_json v.job_flow_states));
           Some ("job_flow_ids", (XmlStringList.to_json v.job_flow_ids));
           Util.option_map v.created_before
             (fun f -> ("created_before", (DateTime.to_json f)));
           Util.option_map v.created_after
             (fun f -> ("created_after", (DateTime.to_json f)))])
    let of_json j =
      {
        created_after =
          (Util.option_map (Json.lookup j "created_after") DateTime.of_json);
        created_before =
          (Util.option_map (Json.lookup j "created_before") DateTime.of_json);
        job_flow_ids =
          (XmlStringList.of_json
             (Util.of_option_exn (Json.lookup j "job_flow_ids")));
        job_flow_states =
          (JobFlowExecutionStateList.of_json
             (Util.of_option_exn (Json.lookup j "job_flow_states")))
      }
  end
module AddTagsOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module RemoveTagsOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeJobFlowsOutput =
  struct
    type t = {
      job_flows: JobFlowDetailList.t }
    let make ?(job_flows= [])  () = { job_flows }
    let parse xml =
      Some
        {
          job_flows =
            (Util.of_option []
               (Util.option_bind (Xml.member "JobFlows" xml)
                  JobFlowDetailList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("JobFlows.member",
                   (JobFlowDetailList.to_query v.job_flows)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("job_flows", (JobFlowDetailList.to_json v.job_flows))])
    let of_json j =
      {
        job_flows =
          (JobFlowDetailList.of_json
             (Util.of_option_exn (Json.lookup j "job_flows")))
      }
  end
module ListBootstrapActionsOutput =
  struct
    type t = {
      bootstrap_actions: CommandList.t ;
      marker: String.t option }
    let make ?(bootstrap_actions= [])  ?marker  () =
      { bootstrap_actions; marker }
    let parse xml =
      Some
        {
          bootstrap_actions =
            (Util.of_option []
               (Util.option_bind (Xml.member "BootstrapActions" xml)
                  CommandList.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some
             (Query.Pair
                ("BootstrapActions.member",
                  (CommandList.to_query v.bootstrap_actions)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Some
             ("bootstrap_actions", (CommandList.to_json v.bootstrap_actions))])
    let of_json j =
      {
        bootstrap_actions =
          (CommandList.of_json
             (Util.of_option_exn (Json.lookup j "bootstrap_actions")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module RunJobFlowOutput =
  struct
    type t = {
      job_flow_id: String.t option }
    let make ?job_flow_id  () = { job_flow_id }
    let parse xml =
      Some
        {
          job_flow_id =
            (Util.option_bind (Xml.member "JobFlowId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.job_flow_id
              (fun f -> Query.Pair ("JobFlowId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.job_flow_id
              (fun f -> ("job_flow_id", (String.to_json f)))])
    let of_json j =
      {
        job_flow_id =
          (Util.option_map (Json.lookup j "job_flow_id") String.of_json)
      }
  end
module AddInstanceGroupsOutput =
  struct
    type t =
      {
      job_flow_id: String.t option ;
      instance_group_ids: InstanceGroupIdsList.t }
    let make ?job_flow_id  ?(instance_group_ids= [])  () =
      { job_flow_id; instance_group_ids }
    let parse xml =
      Some
        {
          job_flow_id =
            (Util.option_bind (Xml.member "JobFlowId" xml) String.parse);
          instance_group_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "InstanceGroupIds" xml)
                  InstanceGroupIdsList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("InstanceGroupIds.member",
                   (InstanceGroupIdsList.to_query v.instance_group_ids)));
           Util.option_map v.job_flow_id
             (fun f -> Query.Pair ("JobFlowId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("instance_group_ids",
                (InstanceGroupIdsList.to_json v.instance_group_ids));
           Util.option_map v.job_flow_id
             (fun f -> ("job_flow_id", (String.to_json f)))])
    let of_json j =
      {
        job_flow_id =
          (Util.option_map (Json.lookup j "job_flow_id") String.of_json);
        instance_group_ids =
          (InstanceGroupIdsList.of_json
             (Util.of_option_exn (Json.lookup j "instance_group_ids")))
      }
  end
module TerminateJobFlowsInput =
  struct
    type t = {
      job_flow_ids: XmlStringList.t }
    let make ~job_flow_ids  () = { job_flow_ids }
    let parse xml =
      Some
        {
          job_flow_ids =
            (Xml.required "JobFlowIds"
               (Util.option_bind (Xml.member "JobFlowIds" xml)
                  XmlStringList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("JobFlowIds.member",
                   (XmlStringList.to_query v.job_flow_ids)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("job_flow_ids", (XmlStringList.to_json v.job_flow_ids))])
    let of_json j =
      {
        job_flow_ids =
          (XmlStringList.of_json
             (Util.of_option_exn (Json.lookup j "job_flow_ids")))
      }
  end
module DescribeClusterInput =
  struct
    type t = {
      cluster_id: String.t }
    let make ~cluster_id  () = { cluster_id }
    let parse xml =
      Some
        {
          cluster_id =
            (Xml.required "ClusterId"
               (Util.option_bind (Xml.member "ClusterId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("ClusterId", (String.to_query v.cluster_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("cluster_id", (String.to_json v.cluster_id))])
    let of_json j =
      {
        cluster_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "cluster_id")))
      }
  end
module ListInstancesInput =
  struct
    type t =
      {
      cluster_id: String.t ;
      instance_group_id: String.t option ;
      instance_group_types: InstanceGroupTypeList.t ;
      marker: String.t option }
    let make ~cluster_id  ?instance_group_id  ?(instance_group_types= [])
      ?marker  () =
      { cluster_id; instance_group_id; instance_group_types; marker }
    let parse xml =
      Some
        {
          cluster_id =
            (Xml.required "ClusterId"
               (Util.option_bind (Xml.member "ClusterId" xml) String.parse));
          instance_group_id =
            (Util.option_bind (Xml.member "InstanceGroupId" xml) String.parse);
          instance_group_types =
            (Util.of_option []
               (Util.option_bind (Xml.member "InstanceGroupTypes" xml)
                  InstanceGroupTypeList.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some
             (Query.Pair
                ("InstanceGroupTypes.member",
                  (InstanceGroupTypeList.to_query v.instance_group_types)));
           Util.option_map v.instance_group_id
             (fun f -> Query.Pair ("InstanceGroupId", (String.to_query f)));
           Some (Query.Pair ("ClusterId", (String.to_query v.cluster_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Some
             ("instance_group_types",
               (InstanceGroupTypeList.to_json v.instance_group_types));
           Util.option_map v.instance_group_id
             (fun f -> ("instance_group_id", (String.to_json f)));
           Some ("cluster_id", (String.to_json v.cluster_id))])
    let of_json j =
      {
        cluster_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "cluster_id")));
        instance_group_id =
          (Util.option_map (Json.lookup j "instance_group_id") String.of_json);
        instance_group_types =
          (InstanceGroupTypeList.of_json
             (Util.of_option_exn (Json.lookup j "instance_group_types")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
