###
# API high level functions - get
###


# get_member_infos --------------------------------------------------------

#' GET member's infos
#'
#' WIP.
#'
#' @export
get_member_infos <- function(base_url,
                             member = "me",
                             post_path = NULL,
                             query = list(),
                             verbose = FALSE,
                             content = TRUE,
                             key = NULL,
                             token = NULL) {
  # create path
  path_member <- str_c(sep = "/",
                       "1", "member", member, post_path)

  # get
  if (length(path_member) == 1) {
    res <- get_trello_api(base_url = base_url,
                          path = path_member,
                          verbose = verbose,
                          content = content,
                          query = query,
                          key = key,
                          token = token)
  } else {
    res <- map(path_member, get_trello_api,
               base_url = base_url,
               verbose = verbose,
               content = content,
               query = query,
               key = key,
               token = token)
  }

  # return
  return(res)
}


# get_board_infos ---------------------------------------------------------

#' GET board's infos
#'
#' WIP.
#'
#' @export
get_board_infos <- function(base_url,
                            board_id,
                            post_path = NULL,
                            query = list(),
                            verbose = FALSE,
                            content = TRUE,
                            key = NULL,
                            token = NULL) {
  # create path
  path_board <- str_c(sep = "/",
                      "1", "boards", board_id, post_path)

  # get
  if (length(path_board) == 1) {
    res <- get_trello_api(base_url = base_url,
                          path = path_board,
                          verbose = verbose,
                          content = content,
                          query = query,
                          key = key,
                          token = token)
  } else {
    res <- map(path_board, get_trello_api,
               base_url = base_url,
               verbose = verbose,
               content = content,
               query = query,
               key = key,
               token = token)
  }

  # return
  return(res)
}


# get card infos ----------------------------------------------------------

#' GET card's infos
#'
#' WIP.
#'
#' @export
get_card_infos <- function(base_url,
                           card_id,
                           post_path = NULL,
                           query = list(),
                           verbose = FALSE,
                           content = TRUE,
                           key = NULL,
                           token = NULL) {
  # create path
  path_card <- str_c(sep = "/",
                     "1", "cards", card_id, post_path)

  # get
  if (length(path_card) == 1) {
    res <- get_trello_api(base_url = base_url,
                          path = path_card,
                          verbose = verbose,
                          content = content,
                          query = query,
                          key = key,
                          token = token)
  } else {
    res <- map(path_card, get_trello_api,
               base_url = base_url,
               verbose = verbose,
               content = content,
               query = query,
               key = key,
               token = token)
  }

  # return
  return(res)
}



