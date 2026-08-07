open Aws.BaseTypes

type t =
  { route_tables : RouteTableList.t
  ; next_token : String.t option
  }

let make ?(route_tables = []) ?next_token () = { route_tables; next_token }

let parse xml =
  Some
    { route_tables =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "routeTableSet" xml) RouteTableList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("RouteTableSet", RouteTableList.to_query v.route_tables))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("routeTableSet", RouteTableList.to_json v.route_tables)
       ])

let of_json j =
  { route_tables =
      RouteTableList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "routeTableSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
