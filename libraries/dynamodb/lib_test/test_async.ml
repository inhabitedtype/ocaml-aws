open Aws_dynamodb_test

module T = TestSuite(struct
    type 'a m = 'a Async.Deferred.t

    let access_key = Unix.getenv "AWS_ACCESS_KEY"
    let secret_key = Unix.getenv "AWS_SECRET_KEY"
    let region = Unix.getenv "AWS_DEFAULT_REGION"

    let run_request x = Aws_async.Runtime.run_request ~region ~access_key ~secret_key x
    let un_m v = Async.Thread_safe.block_on_async_exn (fun () -> v)
  end)
