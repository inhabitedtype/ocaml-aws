module C = CalendarLib.Calendar
module P = CalendarLib.Printer.Calendar

let date_yymmdd = P.sprint "%Y%m%d"

let date_time_iso8601 = P.sprint "%Y-%m-%dT%H:%M:%S"

let date_time = P.sprint "%Y%m%dT%H%M%SZ"

let now_utc () = C.(now () |> to_gmt)

(* (tmcgilchrist) This function is expecting datetimes like
    - "2021-03-17T21:43:32.000Z" from EC2 or
    - "2021-03-18T09:38:33Z" from STS
   We regex off the trailing ".000" and parse them. If there are other
   datetime formats in xml / json there will be trouble and the parser
   will fail with xml node not present or json attribute not present.
*)
let parse s =
  P.from_fstring
    "%Y-%m-%dT%TZ"
    (Str.replace_first (Str.regexp "\\.\\([0-9][0-9][0-9]\\)") "" s)

let format t = P.sprint "%Y-%m-%dT%T.000Z" t
