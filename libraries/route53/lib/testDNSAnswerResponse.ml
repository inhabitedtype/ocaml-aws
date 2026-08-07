open Aws.BaseTypes

type t =
  { nameserver : String.t
  ; record_name : String.t
  ; record_type : RRType.t
  ; record_data : RecordData.t
  ; response_code : String.t
  ; protocol : String.t
  }

let make ~nameserver ~record_name ~record_type ~record_data ~response_code ~protocol () =
  { nameserver; record_name; record_type; record_data; response_code; protocol }

let parse xml =
  Some
    { nameserver =
        Aws.Xml.required
          "Nameserver"
          (Aws.Util.option_bind (Aws.Xml.member "Nameserver" xml) String.parse)
    ; record_name =
        Aws.Xml.required
          "RecordName"
          (Aws.Util.option_bind (Aws.Xml.member "RecordName" xml) String.parse)
    ; record_type =
        Aws.Xml.required
          "RecordType"
          (Aws.Util.option_bind (Aws.Xml.member "RecordType" xml) RRType.parse)
    ; record_data =
        Aws.Xml.required
          "RecordData"
          (Aws.Util.option_bind (Aws.Xml.member "RecordData" xml) RecordData.parse)
    ; response_code =
        Aws.Xml.required
          "ResponseCode"
          (Aws.Util.option_bind (Aws.Xml.member "ResponseCode" xml) String.parse)
    ; protocol =
        Aws.Xml.required
          "Protocol"
          (Aws.Util.option_bind (Aws.Xml.member "Protocol" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Protocol", String.to_query v.protocol))
       ; Some (Aws.Query.Pair ("ResponseCode", String.to_query v.response_code))
       ; Some (Aws.Query.Pair ("RecordData.member", RecordData.to_query v.record_data))
       ; Some (Aws.Query.Pair ("RecordType", RRType.to_query v.record_type))
       ; Some (Aws.Query.Pair ("RecordName", String.to_query v.record_name))
       ; Some (Aws.Query.Pair ("Nameserver", String.to_query v.nameserver))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Protocol", String.to_json v.protocol)
       ; Some ("ResponseCode", String.to_json v.response_code)
       ; Some ("RecordData", RecordData.to_json v.record_data)
       ; Some ("RecordType", RRType.to_json v.record_type)
       ; Some ("RecordName", String.to_json v.record_name)
       ; Some ("Nameserver", String.to_json v.nameserver)
       ])

let of_json j =
  { nameserver = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Nameserver"))
  ; record_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RecordName"))
  ; record_type = RRType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RecordType"))
  ; record_data =
      RecordData.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RecordData"))
  ; response_code =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResponseCode"))
  ; protocol = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Protocol"))
  }
