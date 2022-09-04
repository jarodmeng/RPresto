# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

#' Class to encapsulate a Presto query
#'
#' @keywords internal
PrestoQuery2 <- setRefClass('PrestoQuery2',
  fields = c(
    '.conn',
    '.statement',
    '.next.uri',
    '.state',
    '.request.results',
    '.timestamp'
  ),
  methods = list(
    initialize = function(conn, statement) {
      initFields(
        .conn = conn,
        .statement = statement,
        .next.uri = NA_character_,
        .state = NA_character_,
        .request.results = list()
      )
    },
    # Access functions
    conn = function() {
      return(.conn)
    },
    statement = function() {
      return(.statement)
    },
    next.uri = function(value) {
      if (!missing(value)) {
        .next.uri <<- value
      }
      invisible(.next.uri)
    },
    state = function(value) {
      if (!missing(value)) {
        .state <<- value
      }
      invisible(.state)
    },
    request.results = function(request_result) {
      if (!missing(request_result)) {
        .request.results <<- c(.request.results, list(request_result))
      }
      invisible(.request.results)
    },
    # Auxiliary functions
    post_uri = function() {
      return(paste0(.conn@host, ':', .conn@port, '/v1/statement'))
    },
    request_headers = function() {
      return(.request_headers(.conn))
    },
    update_session = function(content, response) {
      if (!is.null(content$updateType)) {
        if (content$updateType == 'PREPARE') {
          properties <- httr::headers(response)[['x-presto-added-prepare']]
          if (!is.null(properties)) {
            for (pair in strsplit(properties, ',', fixed = TRUE)) {
              pair <- unlist(strsplit(pair, '=', fixed = TRUE))
              .conn@session$setPreparedStatement(pair[1], pair[2])
            }
          }
        }
        if (content$updateType == 'DEALLOCATE') {
          properties <-
            httr::headers(response)[['x-presto-deallocated-prepare']]
          if (!is.null(properties)) {
            for (name in strsplit(properties, ',', fixed = TRUE)) {
              .conn@session$unsetPreparedStatement(name)
            }
          }
        }
      }
    },
    extract_data = function(content) {
      df <- extract.data(
        content,
        session.timezone = .conn@session.timezone,
        output.timezone = .conn@output.timezone,
        timestamp = .timestamp
      )
      return(df)
    },
    # Main functions
    request = function(type, statement = NULL) {
      headers <- request_headers()
      ## Retry if 503
      if (type == 'POST') {
        if (!is.na(.next.uri)) {
          stop(
            'The POST request should be the first HTTP request', call. = FALSE
          )
        }
        if (is.null(statement)) {
          stop('POST cannot have NULL statement', call. = FALSE)
        }
        uri <- post_uri()
        response <- httr::POST(
          url = uri,
          body = enc2utf8(statement),
          config = headers
        )
        .timestamp <<-
          lubridate::with_tz(response$date, tz = .conn@session.timezone)
      } else if (type == 'GET') {
        uri <- .next.uri
        response <- httr::GET(
          url = uri,
          config = headers
        )
      }
      .check.response.status(response)
      content <- .response.to.content(response)
      content_state = state(.get.content.state(content))
      if (content_state == 'FAILED') {
        .stop.with.error.message(content)
      } else {
        next.uri(content$nextUri)
        response$content <- NULL
        update_session(content, response)
        data <- extract_data(content)
        request_result <- list(
          type = type,
          uri = uri,
          state = content_state,
          response_sans_content = response,
          ## redundancy when storing both content and data
          content = content,
          data = data
        )
        request.results(request_result)
      }
      invisible(TRUE)
    },
    execute = function() {
      request('POST', .statement)
      while (!is.null(.next.uri)) {
        request('GET')
      }
      invisible(TRUE)
    },
    fetch_all = function() {
      data_list <- purrr::map(.request.results, ~.$data)
      df <- .combine_results(data_list)
      return(df)
    }
  )
)
