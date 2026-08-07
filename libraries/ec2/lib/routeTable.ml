open Aws.BaseTypes

type t =
  { associations : RouteTableAssociationList.t
  ; propagating_vgws : PropagatingVgwList.t
  ; route_table_id : String.t option
  ; routes : RouteList.t
  ; tags : TagList.t
  ; vpc_id : String.t option
  ; owner_id : String.t option
  }

let make
    ?(associations = [])
    ?(propagating_vgws = [])
    ?route_table_id
    ?(routes = [])
    ?(tags = [])
    ?vpc_id
    ?owner_id
    () =
  { associations; propagating_vgws; route_table_id; routes; tags; vpc_id; owner_id }

let parse xml =
  Some
    { associations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "associationSet" xml)
             RouteTableAssociationList.parse)
    ; propagating_vgws =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "propagatingVgwSet" xml)
             PropagatingVgwList.parse)
    ; route_table_id =
        Aws.Util.option_bind (Aws.Xml.member "routeTableId" xml) String.parse
    ; routes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "routeSet" xml) RouteList.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Some (Aws.Query.Pair ("RouteSet", RouteList.to_query v.routes))
       ; Aws.Util.option_map v.route_table_id (fun f ->
             Aws.Query.Pair ("RouteTableId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("PropagatingVgwSet", PropagatingVgwList.to_query v.propagating_vgws))
       ; Some
           (Aws.Query.Pair
              ("AssociationSet", RouteTableAssociationList.to_query v.associations))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Some ("routeSet", RouteList.to_json v.routes)
       ; Aws.Util.option_map v.route_table_id (fun f -> "routeTableId", String.to_json f)
       ; Some ("propagatingVgwSet", PropagatingVgwList.to_json v.propagating_vgws)
       ; Some ("associationSet", RouteTableAssociationList.to_json v.associations)
       ])

let of_json j =
  { associations =
      RouteTableAssociationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "associationSet"))
  ; propagating_vgws =
      PropagatingVgwList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "propagatingVgwSet"))
  ; route_table_id = Aws.Util.option_map (Aws.Json.lookup j "routeTableId") String.of_json
  ; routes = RouteList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "routeSet"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  }
