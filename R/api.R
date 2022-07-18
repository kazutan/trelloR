###
# API lower level functions
###

# auth --------------------------------------------------------------------

#' Set Trello's auth-infos
#'
#' This is a function that sets Trello's credentials (key / token).
#' The first time you call this function, it will use the keying package
#' to register the key / token information on your local machine.
#' Once the key / token information is registered in the local machine,
#' the functions in this package will automatically acquire
#' and use the authentication information in the local machine.
#' If you want to execute batch processing etc. on the virtual machine,
#' set key / token in the argument of each API functions
#' instead of registering the authentication information with this function.
#'
#' @param force_update Boolean. Specify whether to forcibly update the authentication information.
#' @return This function does not return the object, but records the credentials directly locally.
#'
#' @export
set_trello_auth_infos <- function(force_update = FALSE) {
  # update flag
  update_api_key <- FALSE
  update_api_token <- FALSE
  # confirm
  if (!force_update) {
    # check keyring
    auth_infos <- check_trello_auth_infos(null_return = TRUE)
    # confirm update
    if (!is.null(auth_infos$api_key)) {
      ans_api_key <- select.list(c("update", "keep"),
                                 title = "trello_api_key has already exist. Do you want to update?")
      if (ans_api_key == "update") {
        update_api_key <- TRUE
      }
    }
    if (!is.null(auth_infos$api_token)) {
      ans_api_token <- select.list(c("update", "keep"),
                                   title = "trello_api_token has already exist. Do you want to update?")
      if(ans_api_token == "update") {
        update_api_token <- TRUE
      }
    }
  }

  # set keys
  if (any(force_update, update_api_key)) {
    keyring::key_set("trello_api_key")
  }
  if (any(force_update, update_api_token)) {
    keyring::key_set("trello_api_token")
  }

}


#' Check Trello credentials
#'
#' @description
#' This function checks if Trello's credentials exist on that local machine.
#'
#' @details
#' If a character string is given to the arguments `key` and `token`, that character string is returned as a list.
#' If these arguments are `NULL`, check if they are registered on the local machine.
#' If you check and the credentials do not exist, it will return an error and stop.
#' At this time, if the argument `null_return` is `TRUE`, no error is returned and it returns as `NULL`.
#'
#' @return A list including `api_key` and `api_token`.
check_trello_auth_infos <- function(key = NULL, token = NULL, null_return = FALSE) {
  # check auth infos
  if (is.null(key)) {
    # Check if it is in the keyring
    api_key <- try(keyring::key_get("trello_api_key"), silent = TRUE)
    # What to do if it fails
    if (class(api_key) == "try-error") {
      if (null_return) {
        api_key <- NULL
      } else {
        stop('Please execute `set_trello_auth_infos()`')
      }
    }
  } else {
    api_key <- key
  }

  if (is.null(token)) {
    # Check if it is in the keyring
    api_token <- try(keyring::key_get("trello_api_token"), silent = TRUE)
    # What to do if it fails
    if (class(api_token) == "try-error") {
      if (null_return) {
        api_token <- NULL
      } else {
        stop('Please execute `set_trello_auth_infos()`')
      }
    }
  } else {
    api_token <- token
  }

  # return
  res <- list(api_key = api_key,
              api_token = api_token)
  return(res)
}


# get_trello_api ----------------------------------------------------------

#' GET/PUT/POST/DELETE trello api
#'
#' @description
#' These are low-level functions that deal with CRUD.
#' The first word of the function name corresponds to each API method.
#' If the arguments key / token is NULL, it looks for Trello credentials on that local machine.
#' If the credentials are not registered on the running local machine, you need to run `set_trello_auth_infos ()` first.
#'
#' Also, the key / token information is embedded in the query and API requested.
#' Be careful when handling the output of these functions.
#'
#' @param base_url string. the URL of the page to retrieve
#' @param path string. This string are passed on to `httr::modify_url()`.
#' @param query list. This list are passed on to `httr::modify_url()` with authentication information.
#' @param body,encode These argument is passed `httr::POST()` or `httr::PUT()`. See these documents for more details.
#' @param verbose Boolean. Specify whether to give verbose output.
#' @param content Boolean. IF `TRUE`, Output of this function is passed to `httr::content()`.
#' @param key,token string. The authentication information. See description.
#'
#' @returns
#' A list of API Responses.
#' If `content` is TRUE, Output is passed to `httr::content()`.
#'
#' @export
get_trello_api <- function(base_url, path, verbose = FALSE, content = TRUE, query = list(), key = NULL, token = NULL) {

  # check auth infos
  auth_infos <- check_trello_auth_infos(key, token)

  # build query
  query <- c(query,
             key = auth_infos$api_key,
             token = auth_infos$api_token)

  # execute GET
  res <- GET(
    url = base_url,
    accept_json(),
    path = path,
    query = query,
    if (verbose) {
      verbose()
    }
  )

  # status check
  stop_for_status(res, "There was a problem with your API request and an error was returned.\n Please check the details.")

  # return
  if (content) {
    return(content(res))
  } else {
    return(res)
  }
}


# put_trello_api ----------------------------------------------------------

#' @rdname get_trello_api
#' @export
put_trello_api <- function(base_url, path, verbose = FALSE, content = TRUE, query = list(), body = NULL, encode = c("multipart", "form", "json", "raw"), key = NULL, token = NULL) {

  # check auth infos
  auth_infos <- check_trello_auth_infos(key, token)

  # build query
  query <- c(query,
             key = auth_infos$api_key,
             token = auth_infos$api_token)

  # execute PUT
  res <- PUT(
    url = base_url,
    accept_json(),
    path = path,
    query = query,
    body = body,
    encode = encode,
    handle = handle,
    if (verbose) {
      verbose()
    }
  )

  # status check
  stop_for_status(res, "There was a problem with your API request and an error was returned.\n Please check the details.")

  # return
  if (content) {
    return(content(res))
  } else {
    return(res)
  }
}


# post_trello_api ----------------------------------------------------------

#' @rdname get_trello_api
#' @export
post_trello_api <- function(base_url, path, verbose = FALSE, content = TRUE, query = list(), body = NULL, encode = c("multipart", "form", "json", "raw"), key = NULL, token = NULL) {

  # check auth infos
  auth_infos <- check_trello_auth_infos(key, token)

  # build query
  query <- c(query,
             key = auth_infos$api_key,
             token = auth_infos$api_token)

  # execute POST
  res <- POST(
    url = base_url,
    accept_json(),
    path = path,
    query = query,
    body = body,
    encode = encode,
    handle = handle,
    if (verbose) {
      verbose()
    }
  )

  # status check
  stop_for_status(res, "There was a problem with your API request and an error was returned.\n Please check the details.")

  # return
  if (content) {
    return(content(res))
  } else {
    return(res)
  }
}


# remove_trello_api -------------------------------------------------------

#' @rdname get_trello_api
#' @export
delete_trello_api <- function(base_url, path, verbose = TRUE, key = NULL, token = NULL) {
  # check auth infos
  auth_infos <- check_trello_auth_infos(key, token)

  # build query
  query <- list(key = auth_infos$api_key,
                token = auth_infos$api_token)

  # execute DELETE
  res <- DELETE(
    url = base_url,
    path = path,
    query = query,
    if (verbose) {
      verbose()
    }
  )

  # status check
  stop_for_status(res, "There was a problem with your API request and an error was returned.\n Please check the details.")

  return(res)
}
