# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

context(paste(Sys.getenv("PRESTO_TYPE", "Presto"), "dbExistsTable and db_has_table"))

test_that("dbExistsTable works with live database", {
  conn <- setup_live_connection()
  expect_false(dbExistsTable(conn, "_non_existent_table_"))
  expect_false(db_has_table(conn, "_non_existent_table_"))
  expect_true(dbExistsTable(conn, "iris"))
  expect_true(db_has_table(conn, "iris"))
  expect_true(dbExistsTable(conn, dbQuoteIdentifier(conn, "iris")))
  expect_true(db_has_table(conn, dbQuoteIdentifier(conn, "iris")))
  expect_true(dbExistsTable(conn, dbplyr::in_schema(conn@schema, "iris")))
  expect_true(db_has_table(conn, dbplyr::in_schema(conn@schema, "iris")))
  expect_true(dbExistsTable(conn, DBI::Id(table = "iris")))
  expect_true(db_has_table(conn, DBI::Id(table = "iris")))
})
