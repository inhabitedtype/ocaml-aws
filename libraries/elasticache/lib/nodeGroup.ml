open Aws.BaseTypes

type t =
  { node_group_id : String.t option
  ; status : String.t option
  ; primary_endpoint : Endpoint.t option
  ; reader_endpoint : Endpoint.t option
  ; slots : String.t option
  ; node_group_members : NodeGroupMemberList.t
  }

let make
    ?node_group_id
    ?status
    ?primary_endpoint
    ?reader_endpoint
    ?slots
    ?(node_group_members = [])
    () =
  { node_group_id; status; primary_endpoint; reader_endpoint; slots; node_group_members }

let parse xml =
  Some
    { node_group_id = Aws.Util.option_bind (Aws.Xml.member "NodeGroupId" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; primary_endpoint =
        Aws.Util.option_bind (Aws.Xml.member "PrimaryEndpoint" xml) Endpoint.parse
    ; reader_endpoint =
        Aws.Util.option_bind (Aws.Xml.member "ReaderEndpoint" xml) Endpoint.parse
    ; slots = Aws.Util.option_bind (Aws.Xml.member "Slots" xml) String.parse
    ; node_group_members =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "NodeGroupMembers" xml)
             NodeGroupMemberList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "NodeGroupMembers.member"
              , NodeGroupMemberList.to_query v.node_group_members ))
       ; Aws.Util.option_map v.slots (fun f ->
             Aws.Query.Pair ("Slots", String.to_query f))
       ; Aws.Util.option_map v.reader_endpoint (fun f ->
             Aws.Query.Pair ("ReaderEndpoint", Endpoint.to_query f))
       ; Aws.Util.option_map v.primary_endpoint (fun f ->
             Aws.Query.Pair ("PrimaryEndpoint", Endpoint.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.node_group_id (fun f ->
             Aws.Query.Pair ("NodeGroupId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("NodeGroupMembers", NodeGroupMemberList.to_json v.node_group_members)
       ; Aws.Util.option_map v.slots (fun f -> "Slots", String.to_json f)
       ; Aws.Util.option_map v.reader_endpoint (fun f ->
             "ReaderEndpoint", Endpoint.to_json f)
       ; Aws.Util.option_map v.primary_endpoint (fun f ->
             "PrimaryEndpoint", Endpoint.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.node_group_id (fun f -> "NodeGroupId", String.to_json f)
       ])

let of_json j =
  { node_group_id = Aws.Util.option_map (Aws.Json.lookup j "NodeGroupId") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; primary_endpoint =
      Aws.Util.option_map (Aws.Json.lookup j "PrimaryEndpoint") Endpoint.of_json
  ; reader_endpoint =
      Aws.Util.option_map (Aws.Json.lookup j "ReaderEndpoint") Endpoint.of_json
  ; slots = Aws.Util.option_map (Aws.Json.lookup j "Slots") String.of_json
  ; node_group_members =
      NodeGroupMemberList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeGroupMembers"))
  }
