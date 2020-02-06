open Aws_sqs_test

module T = TestSuite(struct
    type 'a m = 'a Lwt.t

    let access_key = Unix.getenv "AWS_ACCESS_KEY"
    let secret_key = Unix.getenv "AWS_SECRET_KEY"
    (* Get region out of AWS_DEFAULT_REGION and pass into Runtime*)


    let run_request = Aws_lwt.Runtime.run_request ~access_key ~secret_key
    let un_m = Lwt_main.run
  end)
