open Aws_s3_test

module T = TestSuite (struct
  type 'a m = 'a Lwt.t

  let access_key = Unix.getenv "AWS_ACCESS_KEY_ID"

  let secret_key = Unix.getenv "AWS_SECRET_ACCESS_KEY"

  let token = None

  let run_request = Aws_lwt.Runtime.run_request ~access_key ~secret_key ?token

  let un_m = Lwt_main.run
end)
