# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

context('PrestoQuery2')

source('utilities.R')

test_that('PrestoQuery2 initialization works', {
  conn <- setup_live_connection()
  statement <- 'SELECT * FROM iris'
  query <- RPresto:::PrestoQuery2$new(conn, statement)
  expect_is(query$conn(), 'PrestoConnection')
  expect_equal(query$statement(), statement)
  expect_equal(query$next.uri(), NA_character_)
  expect_equal(query$state(), NA_character_)
  expect_equal(query$request.results(), list())
})
