###
# API high level functions - delete
###

# remove card -------------------------------------------------------------

#' remove card.
#'
#' WIP.
#'
#' @export
remove_card <- function(base_url, card_id, verbose = FALSE, key = NULL, token = NULL) {
  # check length of card_id
  if (length(card_id) == 0) {
    message("card_id has 0length. No cards are removed.")
  } else {
    # create path
    path_card <- str_c(sep = "/",
                       "1", "cards", card_id)
    # delete
    if (length(path_card) == 1) {
      res <- delete_trello_api(base_url = base_url,
                               path = path_card,
                               verbose = verbose,
                               key = key,
                               token = token)
    } else {
      res <- map(path_card,
                 delete_trello_api,
                 base_url = base_url,
                 verbose = verbose,
                 key = key,
                 token = token)
    }

    # return
    return(res)
  }

}
