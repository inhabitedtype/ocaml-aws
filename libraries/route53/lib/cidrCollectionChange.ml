open Aws.BaseTypes

type t =
  { location_name : String.t
  ; action : CidrCollectionChangeAction.t
  ; cidr_list : CidrList.t
  }

let make ~location_name ~action ~cidr_list () = { location_name; action; cidr_list }

let parse xml =
  Some
    { location_name =
        Aws.Xml.required
          "LocationName"
          (Aws.Util.option_bind (Aws.Xml.member "LocationName" xml) String.parse)
    ; action =
        Aws.Xml.required
          "Action"
          (Aws.Util.option_bind
             (Aws.Xml.member "Action" xml)
             CidrCollectionChangeAction.parse)
    ; cidr_list =
        Aws.Xml.required
          "CidrList"
          (Aws.Util.option_bind (Aws.Xml.member "CidrList" xml) CidrList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("CidrList.member", CidrList.to_query v.cidr_list))
       ; Some (Aws.Query.Pair ("Action", CidrCollectionChangeAction.to_query v.action))
       ; Some (Aws.Query.Pair ("LocationName", String.to_query v.location_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("CidrList", CidrList.to_json v.cidr_list)
       ; Some ("Action", CidrCollectionChangeAction.to_json v.action)
       ; Some ("LocationName", String.to_json v.location_name)
       ])

let of_json j =
  { location_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LocationName"))
  ; action =
      CidrCollectionChangeAction.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Action"))
  ; cidr_list = CidrList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CidrList"))
  }
