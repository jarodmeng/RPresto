% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PrestoConnection.R, R/dbAppendTable.R,
%   R/dbCreateTable.R, R/dbCreateTableAs.R, R/dbExistsTable.R,
%   R/dbGetQuery.R, R/dbListFields.R, R/dbListTables.R, R/dbQuoteIdentifier.R,
%   R/dbQuoteLiteral.R, R/dbReadTable.R, R/dbRemoveTable.R, R/dbRenameTable.R,
%   R/dbSendQuery.R, R/dbWriteTable.R, R/sqlCreateTable.R, R/sqlCreateTableAs.R
\docType{class}
\name{PrestoConnection-class}
\alias{PrestoConnection-class}
\alias{show,PrestoConnection-method}
\alias{.dbAppendTable}
\alias{dbAppendTable,PrestoConnection,ANY,data.frame-method}
\alias{.dbCreateTable}
\alias{dbCreateTable,PrestoConnection-method}
\alias{.dbCreateTableAs}
\alias{dbCreateTableAs,PrestoConnection-method}
\alias{dbExistsTable,PrestoConnection,ANY-method}
\alias{dbGetQuery,PrestoConnection,character-method}
\alias{.dbListFields_PrestoConnection}
\alias{dbListFields,PrestoConnection,ANY-method}
\alias{dbListFields,PrestoConnection,character-method}
\alias{dbListFields,PrestoConnection,dbplyr_schema-method}
\alias{dbListFields,PrestoConnection,Id-method}
\alias{dbListFields,PrestoConnection,SQL-method}
\alias{dbListTables,PrestoConnection-method}
\alias{.dbQuoteIdentifier_PrestoConnection_dbplyr_schema}
\alias{dbQuoteIdentifier,PrestoConnection,dbplyr_schema-method}
\alias{.dbQuoteIdentifier_PrestoConnection_dbplyr_table_path}
\alias{dbQuoteIdentifier,PrestoConnection,dbplyr_table_path-method}
\alias{.dbQuoteIdentifier_PrestoConnection_AsIs}
\alias{dbQuoteIdentifier,PrestoConnection,AsIs-method}
\alias{.dbQuoteLiteral}
\alias{dbQuoteLiteral,PrestoConnection-method}
\alias{.dbReadTable}
\alias{dbReadTable,PrestoConnection,ANY-method}
\alias{dbReadTable,PrestoConnection,character-method}
\alias{dbReadTable,PrestoConnection,dbplyr_schema-method}
\alias{dbReadTable,PrestoConnection,Id-method}
\alias{dbReadTable,PrestoConnection,SQL-method}
\alias{.dbRemoveTable}
\alias{dbRemoveTable,PrestoConnection,ANY-method}
\alias{.dbRenameTable}
\alias{dbRenameTable,PrestoConnection-method}
\alias{dbSendQuery,PrestoConnection,character-method}
\alias{.dbWriteTable}
\alias{dbWriteTable,PrestoConnection,ANY,data.frame-method}
\alias{.sqlCreateTable}
\alias{sqlCreateTable,PrestoConnection-method}
\alias{.sqlCreateTableAs}
\alias{sqlCreateTableAs,PrestoConnection-method}
\title{S4 implementation of \code{DBIConnection} for Presto.}
\usage{
\S4method{show}{PrestoConnection}(object)

\S4method{dbAppendTable}{PrestoConnection,ANY,data.frame}(conn, name, value, ..., chunk.fields = NULL, row.names = NULL)

\S4method{dbCreateTable}{PrestoConnection}(
  conn,
  name,
  fields,
  with = NULL,
  ...,
  row.names = NULL,
  temporary = FALSE
)

\S4method{dbCreateTableAs}{PrestoConnection}(conn, name, sql, overwrite = FALSE, with = NULL, ...)

\S4method{dbExistsTable}{PrestoConnection,ANY}(conn, name, ...)

\S4method{dbGetQuery}{PrestoConnection,character}(conn, statement, ..., quiet = getOption("rpresto.quiet"))

\S4method{dbListFields}{PrestoConnection,ANY}(conn, name, ...)

\S4method{dbListFields}{PrestoConnection,character}(conn, name, ...)

\S4method{dbListFields}{PrestoConnection,dbplyr_schema}(conn, name, ...)

\S4method{dbListFields}{PrestoConnection,Id}(conn, name, ...)

\S4method{dbListFields}{PrestoConnection,SQL}(conn, name, ...)

\S4method{dbListTables}{PrestoConnection}(conn, pattern, ...)

\S4method{dbQuoteIdentifier}{PrestoConnection,dbplyr_schema}(conn, x, ...)

\S4method{dbQuoteIdentifier}{PrestoConnection,dbplyr_table_path}(conn, x, ...)

\S4method{dbQuoteIdentifier}{PrestoConnection,AsIs}(conn, x, ...)

\S4method{dbQuoteLiteral}{PrestoConnection}(conn, x, ...)

\S4method{dbReadTable}{PrestoConnection,ANY}(conn, name, ...)

\S4method{dbReadTable}{PrestoConnection,character}(conn, name, ...)

\S4method{dbReadTable}{PrestoConnection,dbplyr_schema}(conn, name, ...)

\S4method{dbReadTable}{PrestoConnection,Id}(conn, name, ...)

\S4method{dbReadTable}{PrestoConnection,SQL}(conn, name, ...)

\S4method{dbRemoveTable}{PrestoConnection,ANY}(conn, name, ..., fail_if_missing = TRUE)

\S4method{dbRenameTable}{PrestoConnection}(conn, name, new_name, ...)

\S4method{dbSendQuery}{PrestoConnection,character}(conn, statement, ..., quiet = getOption("rpresto.quiet"))

\S4method{dbWriteTable}{PrestoConnection,ANY,data.frame}(
  conn,
  name,
  value,
  overwrite = FALSE,
  ...,
  append = FALSE,
  field.types = NULL,
  temporary = FALSE,
  row.names = FALSE,
  with = NULL,
  chunk.fields = NULL,
  use.one.query = FALSE
)

\S4method{sqlCreateTable}{PrestoConnection}(
  con,
  table,
  fields,
  row.names = NA,
  temporary = FALSE,
  with = NULL,
  ...
)

\S4method{sqlCreateTableAs}{PrestoConnection}(con, name, sql, with = NULL, ...)
}
\arguments{
\item{conn}{a \code{PrestoConnection} object, as returned by \code{\link[DBI:dbConnect]{DBI::dbConnect()}}.}

\item{name}{The table name, passed on to \code{\link[DBI:dbQuoteIdentifier]{dbQuoteIdentifier()}}. Options are:
\itemize{
\item a character string with the unquoted DBMS table name,
e.g. \code{"table_name"},
\item a call to \code{\link[DBI:Id]{Id()}} with components to the fully qualified table name,
e.g. \code{Id(schema = "my_schema", table = "table_name")}
\item a call to \code{\link[DBI:SQL]{SQL()}} with the quoted and fully qualified table name
given verbatim, e.g. \code{SQL('"my_schema"."table_name"')}
}}

\item{value}{A \link{data.frame} (or coercible to data.frame).}

\item{...}{Other parameters passed on to methods.}

\item{chunk.fields}{A character vector of names of the fields that should
be used to slice the value data frame into chunks for batch append. This is
necessary when the data frame is too big to be uploaded at once in one
single INSERT INTO statement. Default to NULL which inserts the entire
value data frame.}

\item{row.names}{Must be \code{NULL}.}

\item{fields}{Either a character vector or a data frame.

A named character vector: Names are column names, values are types.
Names are escaped with \code{\link[DBI:dbQuoteIdentifier]{dbQuoteIdentifier()}}.
Field types are unescaped.

A data frame: field types are generated using
\code{\link[DBI:dbDataType]{dbDataType()}}.}

\item{with}{An optional WITH clause for the CREATE TABLE statement.}

\item{temporary}{If \code{TRUE}, will generate a temporary table.}

\item{quiet}{If a progress bar should be shown for long queries (which run
for more than 2 seconds. Default to \code{getOption("rpresto.quiet")} which
if not set, defaults to \code{NA} which turns on the progress bar for
interactive queries.}

\item{pattern}{optional SQL pattern for filtering table names, e.g. '\%test\%'}

\item{x}{A character vector, \link[DBI]{SQL} or \link[DBI]{Id} object to quote as identifier.}

\item{fail_if_missing}{If \code{FALSE}, \code{dbRemoveTable()} succeeds if the
table doesn't exist.}

\item{use.one.query}{A boolean to indicate if to use a single CREATE TABLE AS
statement rather than the default implementation of using
separate CREATE TABLE and INSERT INTO statements. Some Presto backends
might have different requirements between the two approaches. e.g.
INSERT INTO might not be allowed to mutate an unpartitioned table created
by CREATE TABLE. If set to TRUE, chunk.fields cannot be used.}

\item{con}{A database connection.}

\item{table}{The table name, passed on to \code{\link[DBI:dbQuoteIdentifier]{dbQuoteIdentifier()}}. Options are:
\itemize{
\item a character string with the unquoted DBMS table name,
e.g. \code{"table_name"},
\item a call to \code{\link[DBI:Id]{Id()}} with components to the fully qualified table name,
e.g. \code{Id(schema = "my_schema", table = "table_name")}
\item a call to \code{\link[DBI:SQL]{SQL()}} with the quoted and fully qualified table name
given verbatim, e.g. \code{SQL('"my_schema"."table_name"')}
}}
}
\description{
S4 implementation of \code{DBIConnection} for Presto.
}
\keyword{internal}
