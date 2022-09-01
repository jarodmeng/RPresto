# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

#' Class to encapsulate a Presto query
#'
#' @keywords internal
PrestoQuery2 <- setRefClass('PrestoQuery2',
  fields=c(
    '.conn',
    '.statement',
    '.next.uri',
    '.state',
    '.request.results'
  ),
  methods=list(
    initialize = function(conn, statement) {
      initFields(
        .conn = conn,
        .statement = statement,
        .next.uri = NA_character_,
        .state = NA_character_,
        .request.results = list()
      )
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
    append.request.result = function(request_result) {
      .request.results <<- c(.request.results, list(request_result))
    },
    post_uri = function() {
      return(paste0(.conn@host, ':', .conn@port, '/v1/statement'))
    },
    request_headers = function() {
      return(.request_headers(.conn))
    },
    request = function(type, statement = NULL) {
      headers <- request_headers()
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
        request_result <- list(
          type = type,
          uri = uri,
          state = content_state,
          response_sans_content = response,
          content = content
        )
        append.request.result(request_result)
      }
      invisible(TRUE)
    },
    execute = function() {
      request('POST', .statement)
      while (!is.null(.next.uri)) {
        request('GET')
      }
      invisible(TRUE)
    }
  )
)
