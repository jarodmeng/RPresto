# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

context(paste(Sys.getenv("PRESTO_TYPE", "Presto"), "dbSendQuery"))

test_that("dbSendQuery works with live database", {
  conn <- setup_live_connection()

  expect_error(
    {
      result <- dbSendQuery(conn, "INVALID SQL")
      while (!dbHasCompleted(result)) {
        dbFetch(result)
      }
    },
    "Query.*failed:.*(no viable alternative at|mismatched) input 'INVALID'"
  )

  result <- dbSendQuery(conn, "SELECT * FROM __non_existent_table__")
  expect_is(result, "PrestoResult")
  expect_equal(result@statement, "SELECT * FROM __non_existent_table__")
  expect_equal(result@query$fetchedRowCount(), 0)
  expect_false(result@query$hasCompleted())
  expect_true(dbClearResult(result))

  result <- dbSendQuery(conn, "SELECT 1")
  expect_is(result, "PrestoResult")
  expect_equal(result@statement, "SELECT 1")
  expect_equal(result@query$fetchedRowCount(), 0)
  expect_false(result@query$hasCompleted())
  expect_true(dbClearResult(result))

  result <- dbSendQuery(conn, "SELECT 1")
  expect_is(result, "PrestoResult")
  expect_equal(result@bigint, "integer")

  result <- dbSendQuery(conn, "SELECT 1", bigint = "integer64")
  expect_is(result, "PrestoResult")
  expect_equal(result@bigint, "integer64")

  result <- dbSendQuery(conn, "SELECT 1", bigint = "numeric")
  expect_is(result, "PrestoResult")
  expect_equal(result@bigint, "numeric")

  result <- dbSendQuery(conn, "SELECT 1", bigint = "character")
  expect_is(result, "PrestoResult")
  expect_equal(result@bigint, "character")
})

test_that("dbSendQuery works with mock - status code 404", {
  conn <- setup_mock_connection()
  with_mocked_bindings(
    {
      assign("request.count", 0, envir = environment(httr_POST))
      result <- dbSendQuery(conn, "SELECT 1 AS n")
      expect_true(dbIsValid(result))

      broken_conn <- conn
      broken_conn@host <- "http://broken_host"
      expect_error(
        dbSendQuery(broken_conn, "SELECT 1 AS n"),
        "Received error response \\(HTTP 404\\).*"
      )
    },
    httr_POST = function(url, body, ...) {
      response_404 <- mock_httr_response(
        url,
        status_code = 404,
        request_body = "SELECT 1 AS n",
        state = "RETRYING"
      )[["response"]]

      if (grepl("^http://broken_host", url)) {
        return(response_404)
      }
      if (url == "http://localhost:8000/v1/statement" &&
        request.count < 2) {
        request.count <<- request.count + 1
        return(response_404)
      }
      return(mock_httr_response(
        "http://localhost:8000/v1/statement",
        status_code = 200,
        state = "RUNNING",
        request_body = "SELECT 1 AS n",
        next_uri = "http://localhost:8000/query_1/1"
      )[["response"]])
    }
  )
})

test_that("dbSendQuery works with mock - status code 503", {
  conn <- setup_mock_connection()
  with_mocked_bindings(
    {
      assign("request.count", 0, envir = environment(httr_POST))
      result <- dbSendQuery(conn, "SELECT 1 AS n")
      expect_true(dbIsValid(result))
    },
    httr_POST = function(url, body, ...) {
      if (request.count == 0) {
        request.count <<- 1
        return(mock_httr_replies(
          mock_httr_response(
            "http://localhost:8000/v1/statement",
            status_code = 503,
            request_body = "SELECT 1 AS n",
            state = "RETRYING"
          )
        )(url, body = body, ...))
      }
      return(mock_httr_replies(
        mock_httr_response(
          "http://localhost:8000/v1/statement",
          status_code = 200,
          state = "RUNNING",
          request_body = "SELECT 1 AS n",
          next_uri = "http://localhost:8000/query_1/1"
        )
      )(url, body = body, ...))
    }
  )
})

test_that("dbSendQuery works with mock - status code 400", {
  conn <- setup_mock_connection()
  with_mocked_bindings(
    {
      expect_error(dbSendQuery(conn, "SELECT 1"), '"Servers down"')
    },
    httr_POST = mock_httr_replies(
      mock_httr_response(
        "http://localhost:8000/v1/statement",
        status_code = 400,
        request_body = "SELECT 1",
        extra_content = list(error = list(message = "Servers down")),
        state = ""
      )
    )
  )
})

test_that("dbSendQuery works with mock - status code 200, FAILED", {
  conn <- setup_mock_connection()
  with_mocked_bindings(
    {
      expect_error(dbSendQuery(conn, "SELECT 1"), "Query .* failed:")
    },
    httr_POST = mock_httr_replies(
      mock_httr_response(
        "http://localhost:8000/v1/statement",
        status_code = 200,
        request_body = "SELECT 1",
        state = "FAILED"
      )
    )
  )
})

test_that("dbSendQuery works with mock - status code 500", {
  conn <- setup_mock_connection()
  with_mocked_bindings(
    {
      expect_error(
        dbSendQuery(conn, "SELECT 1"),
        "Received error response \\(HTTP 500\\):.*"
      )
    },
    httr_POST = mock_httr_replies(
      mock_httr_response(
        "http://localhost:8000/v1/statement",
        status_code = 500,
        request_body = "SELECT 1",
        state = ""
      )
    )
  )
})

test_that("dbSendQuery works with mock - regular", {
  conn <- setup_mock_connection()
  with_mocked_bindings(
    {
      result <- dbSendQuery(conn, "SELECT 1")
      expect_is(result, "PrestoResult")
      expect_equal(result@statement, "SELECT 1")
      expect_equal(result@query$fetchedRowCount(), 0)
      expect_false(result@query$hasCompleted())
    },
    httr_POST = mock_httr_replies(
      mock_httr_response(
        "http://localhost:8000/v1/statement",
        status_code = 200,
        request_body = "SELECT 1",
        state = "RUNNING",
        next_uri = "http://localhost:8000/query_1/1"
      )
    )
  )
})

test_that("dbSendQuery works with mock - POST data", {
  conn <- setup_mock_connection()
  with_mocked_bindings(
    {
      result <- dbSendQuery(conn, "SELECT 1 AS x")
      expect_is(result, "PrestoResult")
      expect_equal(result@statement, "SELECT 1 AS x")
      expect_equal(result@query$fetchedRowCount(), 0)
      expect_false(result@query$hasCompleted())
      expect_equal_data_frame(
        dbFetch(result),
        tibble::tibble(x = 1)
      )
      expect_false(result@query$hasCompleted())
      expect_equal(result@query$fetchedRowCount(), 1)
      expect_equal_data_frame(
        dbFetch(result),
        tibble::tibble()
      )
      expect_true(result@query$hasCompleted())
      expect_equal(result@query$fetchedRowCount(), 1)
      expect_true(dbHasCompleted(result))
    },
    httr_POST = mock_httr_replies(
      mock_httr_response(
        "http://localhost:8000/v1/statement",
        status_code = 200,
        request_body = "SELECT 1 AS x",
        state = "RUNNING",
        data = data.frame(x = 1),
        next_uri = "http://localhost:8000/query_1/1"
      )
    ),
    httr_GET = mock_httr_replies(
      mock_httr_response(
        "http://localhost:8000/query_1/1",
        status_code = 200,
        state = "FINISHED",
        data = data.frame()
      )
    )
  )
})
