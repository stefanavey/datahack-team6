#' Calculate network co-occurence matrix
#'
#' Takes in a `data.frame` and calculates the pairwise co-occurences
#' of two indicators across a grouping variable.
#'
#' @param x `data.frame`
#' @param group the grouping variable
#' @param indicator the variable containing individual instances for which
#'   to find the pairwise co-occurence of, with respect to the grouping variable
#'
#' @return Long matrix of co-occurence, separated into columns `X1` and `X2`
#'
#' @examples
#' x <- data.frame(crid = c("A", "A", "B", "B", "C", "C"),
#'                 officer_id = c(1, 2, 2, 3, 1, 3))
#'
#' calc_network(x, group = "crid", indicator = "officer_id")
#'
#' ## Result:
#' ##   X1    X2 co_occurence
#' ##    1     2            1
#' ##    1     3            1
#' ##    2     3            1

calc_network <- function(x, group, indicator) {
    .comat <- function(x) {
        ## Creates all pairwise combinations of a vector
        ## coming from a `data.frame` column
        ## and return as data.frame
        uni <- x %>% unlist %>% sort %>% unique
        self <- data.frame(X1 = uni, X2 = uni)        

        if (nrow(self) == 1) {
            return(self)
        }

        combs <- uni %>% combn(2) %>%
            t %>% data.frame
        all <- bind_rows(self, combs)
        return(all)
    }

    y <- x %>%
        select_(group, indicator) %>%
        group_by_(group) %>%
        nest %>%
        mutate(co = map(data, .comat)) %>%
        unnest(co) %>%
        group_by(X1, X2) %>%
        count

    return(y)
}



