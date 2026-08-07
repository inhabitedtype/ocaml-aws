open Aws.BaseTypes

type t =
  { max_connections_percent : Integer.t option
  ; max_idle_connections_percent : Integer.t option
  ; connection_borrow_timeout : Integer.t option
  ; session_pinning_filters : StringList.t
  ; init_query : String.t option
  }

let make
    ?max_connections_percent
    ?max_idle_connections_percent
    ?connection_borrow_timeout
    ?(session_pinning_filters = [])
    ?init_query
    () =
  { max_connections_percent
  ; max_idle_connections_percent
  ; connection_borrow_timeout
  ; session_pinning_filters
  ; init_query
  }

let parse xml =
  Some
    { max_connections_percent =
        Aws.Util.option_bind (Aws.Xml.member "MaxConnectionsPercent" xml) Integer.parse
    ; max_idle_connections_percent =
        Aws.Util.option_bind
          (Aws.Xml.member "MaxIdleConnectionsPercent" xml)
          Integer.parse
    ; connection_borrow_timeout =
        Aws.Util.option_bind (Aws.Xml.member "ConnectionBorrowTimeout" xml) Integer.parse
    ; session_pinning_filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SessionPinningFilters" xml)
             StringList.parse)
    ; init_query = Aws.Util.option_bind (Aws.Xml.member "InitQuery" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.init_query (fun f ->
             Aws.Query.Pair ("InitQuery", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SessionPinningFilters.member"
              , StringList.to_query v.session_pinning_filters ))
       ; Aws.Util.option_map v.connection_borrow_timeout (fun f ->
             Aws.Query.Pair ("ConnectionBorrowTimeout", Integer.to_query f))
       ; Aws.Util.option_map v.max_idle_connections_percent (fun f ->
             Aws.Query.Pair ("MaxIdleConnectionsPercent", Integer.to_query f))
       ; Aws.Util.option_map v.max_connections_percent (fun f ->
             Aws.Query.Pair ("MaxConnectionsPercent", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.init_query (fun f -> "InitQuery", String.to_json f)
       ; Some ("SessionPinningFilters", StringList.to_json v.session_pinning_filters)
       ; Aws.Util.option_map v.connection_borrow_timeout (fun f ->
             "ConnectionBorrowTimeout", Integer.to_json f)
       ; Aws.Util.option_map v.max_idle_connections_percent (fun f ->
             "MaxIdleConnectionsPercent", Integer.to_json f)
       ; Aws.Util.option_map v.max_connections_percent (fun f ->
             "MaxConnectionsPercent", Integer.to_json f)
       ])

let of_json j =
  { max_connections_percent =
      Aws.Util.option_map (Aws.Json.lookup j "MaxConnectionsPercent") Integer.of_json
  ; max_idle_connections_percent =
      Aws.Util.option_map (Aws.Json.lookup j "MaxIdleConnectionsPercent") Integer.of_json
  ; connection_borrow_timeout =
      Aws.Util.option_map (Aws.Json.lookup j "ConnectionBorrowTimeout") Integer.of_json
  ; session_pinning_filters =
      StringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SessionPinningFilters"))
  ; init_query = Aws.Util.option_map (Aws.Json.lookup j "InitQuery") String.of_json
  }
