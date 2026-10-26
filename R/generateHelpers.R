#' @title genResponses
#'
#' @description Generates response vectors for `n` cases to `i` items given
#'   known item parameters, person abilities, and (optionally) other inputs.
#'
#'   Generation runs as a sequence of steps. Each step is also available as its
#'   own function so it can be checked in isolation:
#'
#'   1. [genRespInputs()] validates and standardises the inputs.
#'   2. [parsePerturbations()] validates `perturbR` and puts each perturbation
#'      into a standard form (one row per perturbation).
#'   3. [perturbItem()] builds the perturbed item parameters for each perturbation.
#'   4. [buildPerturbMap()] decides which perturbation (if any) applies to each
#'      case-item pair, based on `groups`.
#'   5. Category probabilities are calculated and responses drawn (in C++).
#'   6. [genMissingMCAR()] sets responses to missing completely at random.
#'
#'   Use `debug = TRUE` to return the output of every step, and
#'   [genResponsesProbs()] to tabulate the response probabilities used for each
#'   case, with and without perturbation.
#'
#' @param abilities A person by latent-dimension matrix of abilities.
#'   One column per dimension.
#' @param groups A vector of factors of the same length as `abilities` that
#'   allocates each case to a group. Used in `perturbR`. Defaults to NULL such that
#'   all cases are in the one group.
#' @param itemParams A list of item parameters of the structure used in `simplef`
#'   (a matrix of k categories by four (category score, delta dot, tau, discrimination)).
#'   See conquestr::makeItemList for a helper to generate this list.
#' @param BMatrix A simplified B-matrix mapping dimensions (columns) to items (rows).
#'   Or the integer "1" if the abilities are uni-dimensional.
#' @param mcarP A double indicating the proportion of missing data
#'   under the MCAR assumption.
#' @param perturbR A list of perturbations used to model misfit, or NULL (default)
#'  for none. Each element is a list describing one perturbation of one item.
#'  Elements can be given by name (recommended) or, when unnamed, in this order:
#'
#'  * `item` (integer). Which item in `itemParams` is affected.
#'  * `type` (string). One of
#'     * "discrimination" - multiplies the item's discrimination (column 4 of
#'       the item matrix) by `factor`, and adds `pivot` to the delta dot.
#'     * "shift" - adds `pivot` to the delta dot, creating a uniform shift in
#'       the category characteristic curves (e.g., DIF for a group).
#'  * `factor` (double). The multiplier applied to the discrimination when the
#'      type is "discrimination". For example, if the item has discrimination of
#'      1 and `factor` is 1.2, responses are generated assuming a discrimination
#'      of 1.2. Ignored when the type is "shift".
#'  * `pivot` (double). For "shift", the value added to the delta dot (column
#'      2, rows 2 to k, of the item matrix), i.e., the size of the shift. For
#'      "discrimination", its meaning depends on `pivotAt`.
#'  * `group` (character vector, optional). The group(s) in `groups` whose
#'      responses are perturbed, for example `"bfit"` or `c("g1", "g2")`. Omit
#'      it, or use `NULL` or `NA`, to perturb all cases. Ignored (all cases are
#'      perturbed) when `groups` is NULL. When elements are unnamed, every
#'      element from the fifth onwards is treated as a group.
#'  * `pivotAt` (string, optional, by name only). Used by "discrimination":
#'     * "theta" (default) - `pivot` is added to the delta dot. The perturbed
#'       and unperturbed curves of a dichotomous item then cross at
#'       theta = pivot / (a * (factor - 1)), where `a` is the item's
#'       discrimination. That is, the crossing point is on the theta scale,
#'       does not depend on the item's location, and the item location moves
#'       unless `pivot` is 0 and the item is centred at theta = 0.
#'     * "item" - the discrimination is changed around a point `pivot` logits
#'       from the item's location (its delta dot divided by its
#'       discrimination). At that point the category probabilities of the
#'       perturbed and unperturbed item are equal, for dichotomous and partial
#'       credit items, so the item's location does not move. Use `pivot = 0` to
#'       pivot at the item location.
#'  * `shift` (double, optional, by name only, default 0). Used by
#'      "discrimination": a value added to the delta dot after the
#'      discrimination is changed, moving the item location without moving
#'      the discrimination pivot relative to the original item. Use the
#'      "shift" type to shift an item without changing its discrimination.
#'
#'  An item can appear in more than one perturbation, for example to perturb it
#'  differently for different groups. If two perturbations apply to the same
#'  case and item, the later one is used, with a warning.
#' @param debug A Boolean. If `TRUE`, returns a list holding the responses and
#'   the intermediate data products of each step (see Value). Responses are
#'   identical to those from `debug = FALSE` for the same seed.
#'
#' @references  Adams, R. J., & Wright, B. D. (1994). When does misfit make a difference.
#'   In Objective Measurement Theory and Practice (Vol. 2). Ablex Publishing Corporation.
#' @return If `debug = FALSE` (default), a matrix, `n` cases by `i` items, of
#'   scored item responses. If `debug = TRUE`, a list with elements:
#'
#'   * `responses`: the matrix returned when `debug = FALSE`.
#'   * `responsesComplete`: responses before MCAR missingness was applied.
#'   * `missing`: a logical cases by items matrix, TRUE where [genMissingMCAR()]
#'     selected the case-item pair to be missing.
#'   * `inputs`: the standardised inputs returned by [genRespInputs()].
#'   * `perturbations`: a data frame, one row per perturbation, returned by
#'     [parsePerturbations()].
#'   * `perturbedItems`: a list of perturbed item parameter matrices, one per row
#'     of `perturbations`.
#'   * `perturbMap`: an integer cases by items matrix returned by
#'     [buildPerturbMap()]. 0 where the case received the unperturbed item,
#'     otherwise the row of `perturbations` that was applied.
#'   * `draws`: the uniform deviates used to draw each response
#'     (NA where the ability is NA).
#'   * `probs`: a list, one cases by categories matrix per item, of the category
#'     probabilities used to draw each response (perturbed where applicable).
#'   * `probsBase`: as `probs`, but under the unperturbed item parameters.
#' @seealso [conquestr::simplef()], [genResponsesProbs()], `browseVignettes("conquestr")`
#' @examples
#'   myItem <- matrix(c(0, 0, 0, 0, 1, 1, 0, 1), ncol = 4, byrow = TRUE)
#'   myItems <- list(myItem, myItem)
#'   myItems[[2]][2, 2] <- -1 # make the second item delta equal to -1
#'   myResponses <- genResponses(abilities = rnorm(100), itemParams = myItems)
#'
#'   # perturb item 2 for group "b" only, and keep the intermediate data
#'   myGroups <- factor(sample(c("a", "b"), 100, replace = TRUE))
#'   myPerturbations <- list(
#'     list(item = 2L, type = "shift", pivot = 1, group = "b")
#'   )
#'   myDebug <- genResponses(
#'     abilities = rnorm(100), groups = myGroups, itemParams = myItems,
#'     perturbR = myPerturbations, debug = TRUE
#'   )
#'   head(genResponsesProbs(myDebug, items = 2))
#' @importFrom stats runif
genResponses <- function(
    abilities, groups = NULL, itemParams,
    BMatrix = 1, mcarP = 0, perturbR = NULL, debug = FALSE
  ) {

  if (!is.logical(debug) || length(debug) != 1 || is.na(debug)) {
    stop("`debug` must be TRUE or FALSE")
  }

  inputs <- genRespInputs(abilities, groups, itemParams, BMatrix, mcarP)
  perturbations <- parsePerturbations(perturbR, inputs$nItems, inputs$groups)
  perturbedItems <- lapply(seq_len(nrow(perturbations)), function(i) {
    perturbItem(
      inputs$itemParams[[perturbations$item[i]]],
      type = perturbations$type[i],
      factor = perturbations$factor[i],
      pivot = perturbations$pivot[i],
      pivotAt = perturbations$pivotAt[i],
      shift = perturbations$shift[i]
    )
  })
  perturbMap <- buildPerturbMap(perturbations, inputs$groups, inputs$nCases, inputs$nItems)

  generated <- genResponsesCpp(
    inputs$abilities, inputs$itemParams, inputs$itemDim,
    perturbedItems, perturbMap, debug
  )

  # drawn after the responses, so MCAR does not change the responses drawn
  missing <- genMissingMCAR(inputs$nCases, inputs$nItems, mcarP)
  responses <- generated$responses
  responses[missing] <- NA

  if (!debug) return(responses)

  list(
    responses = responses,
    responsesComplete = generated$responses,
    missing = missing,
    inputs = inputs,
    perturbations = perturbations,
    perturbedItems = perturbedItems,
    perturbMap = perturbMap,
    draws = generated$draws,
    probs = generated$probs,
    probsBase = generated$probsBase
  )
}

#' @title genRespInputs
#'
#' @description Step 1 of [genResponses()]. Validates and standardises the
#'   inputs used to generate responses.
#'
#' @inheritParams genResponses
#' @return A list with elements `abilities` (a cases by dimensions double
#'   matrix), `groups` (a factor, or NULL), `itemParams` (a list of double
#'   matrices), `itemDim` (an integer vector, the ability column used by each
#'   item), `nCases`, and `nItems`.
#' @keywords internal
#' @seealso [genResponses()]
genRespInputs <- function(abilities, groups = NULL, itemParams, BMatrix = 1, mcarP = 0) {
  if (!is.list(itemParams) || length(itemParams) == 0) {
    stop(
      "`itemParams` should be a list of item params of the structure used in `simplef`"
    )
  }
  for (j in seq_along(itemParams)) {
    item <- as.matrix(itemParams[[j]])
    if (!is.numeric(item) || ncol(item) < 3 || nrow(item) < 2) {
      stop(
        "item ", j, " in `itemParams` must be a numeric matrix with at least ",
        "2 rows (categories) and 3 columns (score, delta dot, tau)"
      )
    }
    storage.mode(item) <- "double"
    itemParams[[j]] <- item
  }
  nItems <- length(itemParams)

  # in the unidimensional case abilities can be a vector, so coerce to matrix
  abilities <- as.matrix(abilities)
  storage.mode(abilities) <- "double"
  nCases <- nrow(abilities) # can include NAs - responses will all be NA

  if (is.matrix(BMatrix) && ncol(BMatrix) > 1) {
    if (nrow(BMatrix) != nItems) {
      stop("`BMatrix` must have one row per item in `itemParams`")
    }
    if (!all(rowSums(BMatrix) == 1)) stop("only between-item designs currently allowed")
    itemDim <- as.integer(apply(BMatrix, 1, function(r) which(r == 1)))
  } else {
    itemDim <- rep(1L, nItems)
  }

  if (!is.null(groups)) {
    if (!is.factor(groups)) groups <- as.factor(groups)
    if (length(groups) != nCases) {
      stop("`groups` must be the same length as the number of cases in `abilities`")
    }
  }

  if (!is.numeric(mcarP) || length(mcarP) != 1 || is.na(mcarP) || mcarP < 0 || mcarP > 1) {
    stop("`mcarP` must be a single number between 0 and 1")
  }

  list(
    abilities = abilities,
    groups = groups,
    itemParams = itemParams,
    itemDim = itemDim,
    nCases = nCases,
    nItems = nItems
  )
}

#' @title parsePerturbations
#'
#' @description Step 2 of [genResponses()]. Validates a list of perturbations
#'   and returns it in a standard form.
#'
#' @param perturbR A list of perturbations, as described in [genResponses()].
#' @param nItems An integer, the number of items in `itemParams`.
#' @param groups A factor allocating each case to a group, or NULL. Used to
#'   check that the groups named in `perturbR` exist.
#' @return A data frame with one row per perturbation and columns `item`,
#'   `type`, `factor` (NA for "shift"), `pivot`, `pivotAt` (NA for "shift"),
#'   `shift` (0 for "shift"), and `group` (a list column; NA means all cases).
#' @keywords internal
#' @seealso [genResponses()]
parsePerturbations <- function(perturbR, nItems, groups = NULL) {
  fields <- c("item", "type", "factor", "pivot", "group")
  out <- data.frame(
    item = integer(), type = character(), factor = numeric(), pivot = numeric(),
    pivotAt = character(), shift = numeric()
  )
  out$group <- I(list())
  if (is.null(perturbR) || length(perturbR) == 0) return(out)
  if (!is.list(perturbR)) stop("`perturbR` must be a list of perturbations")

  isNumber <- function(x) {
    x <- suppressWarnings(as.numeric(unlist(x)))
    length(x) == 1 && is.finite(x)
  }

  rows <- vector("list", length(perturbR))
  for (i in seq_along(perturbR)) {
    p <- as.list(perturbR[[i]])
    where <- paste0(" (element ", i, " of `perturbR`)")
    nms <- names(p)
    byName <- !is.null(nms) && any(nms %in% fields)
    getField <- function(name) {
      if (byName) return(p[[name]]) # NULL if absent
      pos <- match(name, fields)
      if (length(p) < pos) return(NULL)
      if (name == "group") return(unlist(p[pos:length(p)]))
      p[[pos]]
    }

    item <- suppressWarnings(as.integer(unlist(getField("item"))))
    if (length(item) != 1 || is.na(item) || !item %in% seq_len(nItems)) {
      stop("`item` must be a single item number between 1 and ", nItems, where)
    }

    type <- as.character(unlist(getField("type")))
    if (length(type) != 1 || !type %in% c("discrimination", "shift")) {
      stop("`type` must be one of 'discrimination' or 'shift'", where)
    }

    factor <- NA_real_
    if (type == "discrimination") {
      if (!isNumber(getField("factor"))) stop("`factor` must be a single number", where)
      factor <- as.numeric(unlist(getField("factor")))
    }

    if (!isNumber(getField("pivot"))) stop("`pivot` must be a single number", where)
    pivot <- as.numeric(unlist(getField("pivot")))

    # pivotAt and shift are only available by name
    pivotAt <- NA_character_
    shift <- 0
    if (byName) {
      if (type == "shift" && (!is.null(p$pivotAt) || !is.null(p$shift))) {
        stop("`pivotAt` and `shift` are only used by type 'discrimination'", where)
      }
      if (!is.null(p$shift)) {
        if (!isNumber(p$shift)) stop("`shift` must be a single number", where)
        shift <- as.numeric(unlist(p$shift))
      }
    }
    if (type == "discrimination") {
      pivotAt <- if (byName && !is.null(p$pivotAt)) as.character(unlist(p$pivotAt)) else "theta"
      if (length(pivotAt) != 1 || !pivotAt %in% c("theta", "item")) {
        stop("`pivotAt` must be one of 'theta' or 'item'", where)
      }
    }

    group <- as.character(unlist(getField("group")))
    if (length(group) == 0 || all(is.na(group))) {
      group <- NA_character_
    } else {
      if (anyNA(group)) stop("`group` cannot mix NA with group names", where)
      group <- unique(group)
      if (!is.null(groups)) {
        unknown <- setdiff(group, levels(groups))
        if (length(unknown) > 0) {
          stop(
            "`group` refers to group(s) not found in `groups`: ",
            paste0("'", unknown, "'", collapse = ", "), where
          )
        }
      }
    }

    rows[[i]] <- list(
      item = item, type = type, factor = factor, pivot = pivot,
      pivotAt = pivotAt, shift = shift, group = group
    )
  }

  out <- data.frame(
    item = vapply(rows, function(r) r$item, integer(1)),
    type = vapply(rows, function(r) r$type, character(1)),
    factor = vapply(rows, function(r) r$factor, numeric(1)),
    pivot = vapply(rows, function(r) r$pivot, numeric(1)),
    pivotAt = vapply(rows, function(r) r$pivotAt, character(1)),
    shift = vapply(rows, function(r) r$shift, numeric(1))
  )
  out$group <- I(lapply(rows, function(r) r$group))
  out
}

#' @title perturbItem
#'
#' @description Step 3 of [genResponses()]. Returns the item parameters after
#'   applying one perturbation.
#'
#' @param item An item matrix of the structure used in `simplef`
#'   (k categories by score, delta dot, tau, and discrimination).
#' @param type A string, "discrimination" or "shift".
#' @param factor A double, the multiplier applied to the discrimination
#'   (column 4) when `type` is "discrimination". Ignored for "shift".
#' @param pivot A double. For "shift", and for "discrimination" with
#'   `pivotAt = "theta"`, it is added to the delta dot (column 2, rows 2 to k).
#'   For "discrimination" with `pivotAt = "item"`, it is the distance, in
#'   logits, from the item location to the point around which the
#'   discrimination is changed.
#' @param pivotAt A string, "theta" (default) or "item". See [genResponses()].
#'   Ignored for "shift".
#' @param shift A double, added to the delta dot after the discrimination is
#'   changed. Ignored for "shift" (use `pivot`).
#' @return The perturbed item matrix.
#' @keywords internal
#' @seealso [genResponses()]
#' @examples
#' myItem <- matrix(c(0, 0, 0, 1, 1, 0.5, 0, 1), ncol = 4, byrow = TRUE)
#' perturbItem(myItem, type = "discrimination", factor = 2, pivot = 0)
#' # pivot at the item location (0.5), so the location does not move
#' perturbItem(myItem, type = "discrimination", factor = 2, pivot = 0, pivotAt = "item")
perturbItem <- function(item, type, factor = 1, pivot = 0, pivotAt = "theta", shift = 0) {
  item <- as.matrix(item)
  nCats <- nrow(item)
  if (nCats < 2) stop("`item` must have at least 2 categories")
  steps <- 2:nCats # the first category's delta is a placeholder (always 0)
  if (type == "shift") {
    item[steps, 2] <- item[steps, 2] + pivot
    return(item)
  }
  if (type != "discrimination") stop("`type` must be one of 'discrimination' or 'shift'")
  if (!pivotAt %in% c("theta", "item")) stop("`pivotAt` must be one of 'theta' or 'item'")
  if (ncol(item) < 4) item <- cbind(item, 1)

  if (pivotAt == "theta") {
    deltaChange <- rep(pivot, nCats - 1)
  } else {
    # The category logit is a_k * x_k * theta - sum_{j <= k} (delta_j + tau_j).
    # Multiplying a by `factor` and adding (factor - 1) * a_k * x_k * theta0 to
    # the cumulative deltas leaves every category logit, and so every category
    # probability, unchanged at theta0.
    location <- mean(item[steps, 2] / item[steps, 4])
    theta0 <- location + pivot
    cumChange <- (factor - 1) * item[, 4] * item[, 1] * theta0
    deltaChange <- diff(cumChange)
  }
  item[, 4] <- item[, 4] * factor
  item[steps, 2] <- item[steps, 2] + deltaChange + shift
  item
}

#' @title buildPerturbMap
#'
#' @description Step 4 of [genResponses()]. Decides which perturbation (if any)
#'   applies to each case-item pair.
#'
#' @param perturbations A data frame returned by [parsePerturbations()].
#' @param groups A factor allocating each case to a group, or NULL. When NULL,
#'   every perturbation applies to all cases.
#' @param nCases An integer, the number of cases.
#' @param nItems An integer, the number of items.
#' @return An integer cases by items matrix: 0 where the case receives the
#'   unperturbed item, otherwise the row of `perturbations` that applies.
#'   Where perturbations overlap, the later one is used, with a warning.
#' @keywords internal
#' @seealso [genResponses()]
buildPerturbMap <- function(perturbations, groups, nCases, nItems) {
  perturbMap <- matrix(0L, nCases, nItems)
  for (i in seq_len(nrow(perturbations))) {
    group <- perturbations$group[[i]]
    if (is.null(groups) || all(is.na(group))) {
      cases <- rep(TRUE, nCases)
    } else {
      cases <- as.character(groups) %in% group
    }
    j <- perturbations$item[i]
    overlap <- cases & perturbMap[, j] > 0L
    if (any(overlap)) {
      warning(
        "perturbation(s) ", paste(unique(perturbMap[overlap, j]), collapse = ", "),
        " and ", i, " both apply to item ", j, " for ", sum(overlap),
        " case(s); perturbation ", i, " is used",
        call. = FALSE
      )
    }
    perturbMap[cases, j] <- i
  }
  perturbMap
}

#' @title genMissingMCAR
#'
#' @description Step 6 of [genResponses()]. Selects case-item pairs to set to
#'   missing, completely at random.
#'
#' @param nCases An integer, the number of cases.
#' @param nItems An integer, the number of items.
#' @param mcarP A double, the probability that each case-item pair is missing.
#' @return A logical cases by items matrix, TRUE where the response is missing.
#' @keywords internal
#' @seealso [genResponses()]
#' @importFrom stats runif
genMissingMCAR <- function(nCases, nItems, mcarP = 0) {
  if (mcarP <= 0) return(matrix(FALSE, nCases, nItems))
  matrix(runif(nCases * nItems), nCases, nItems) > (1 - mcarP)
}

#' @title genResponsesProbs
#'
#' @description Tabulates the response probabilities used by [genResponses()]
#'   for each case, item, and category, alongside the probabilities under the
#'   unperturbed item. Use it to check that perturbations were applied to the
#'   intended cases, and by the intended amount.
#'
#' @param x The list returned by `genResponses(..., debug = TRUE)`.
#' @param items An integer vector of the items to include. Defaults to all items.
#' @return A data frame with one row per case, item, and category, and columns:
#'   `case`, `item`, `group`, `theta`, `perturbation` (0 if unperturbed,
#'   otherwise the row of `x$perturbations` applied), `category`, `score`,
#'   `prob` (used to draw the response), `probBase` (under the unperturbed
#'   item), `draw` (the uniform deviate), and `response` (before MCAR
#'   missingness).
#' @seealso [genResponses()]
#' @examples
#' myItem <- matrix(c(0, 0, 0, 1, 1, 0.5, 0, 1), ncol = 4, byrow = TRUE)
#' myDebug <- genResponses(
#'   abilities = rnorm(10), itemParams = list(myItem),
#'   perturbR = list(list(item = 1L, type = "shift", pivot = 1)), debug = TRUE
#' )
#' genResponsesProbs(myDebug)
genResponsesProbs <- function(x, items = NULL) {
  need <- c("inputs", "perturbMap", "probs", "probsBase", "draws", "responsesComplete")
  if (!is.list(x) || !all(need %in% names(x))) {
    stop("`x` must be the list returned by `genResponses(..., debug = TRUE)`")
  }
  inputs <- x$inputs
  if (is.null(items)) items <- seq_len(inputs$nItems)
  if (!all(items %in% seq_len(inputs$nItems))) {
    stop("`items` must be item numbers between 1 and ", inputs$nItems)
  }

  out <- lapply(items, function(j) {
    nCats <- ncol(x$probs[[j]])
    expandCase <- function(v) rep(v, times = nCats)
    data.frame(
      case = expandCase(seq_len(inputs$nCases)),
      item = j,
      group = if (is.null(inputs$groups)) NA else expandCase(inputs$groups),
      theta = expandCase(inputs$abilities[, inputs$itemDim[j]]),
      perturbation = expandCase(x$perturbMap[, j]),
      category = rep(seq_len(nCats), each = inputs$nCases),
      score = rep(inputs$itemParams[[j]][, 1], each = inputs$nCases),
      prob = as.vector(x$probs[[j]]),
      probBase = as.vector(x$probsBase[[j]]),
      draw = expandCase(x$draws[, j]),
      response = expandCase(x$responsesComplete[, j])
    )
  })
  out <- do.call(rbind, out)
  out <- out[order(out$item, out$case, out$category), ]
  rownames(out) <- NULL
  out
}

#' @title perturbResp
#'
#' @description Perturbs a response given specified (item) misfit to implied model.
#'
#' @param ability a double - the location of the person on the measure.
#' @param item a matrix - the item parameters in the format required by
#'   `conquestr::simplef`
#' @param group a string - the group this case belongs to. If NULL, the case is
#'   always in this group.
#' @param perturbation A list - the description of the way this item should be
#'   perturbed. In the (unnamed) format described in `conquestr::genResponses`
#' @return A double - the observed response.
#' @keywords internal
#' @seealso [conquestr::simplef()], [conquestr::genResponses()],
#'   [perturbItem()], `browseVignettes("conquestr")`
#' @examples
#' myItem <- matrix(c(
#'   0, 0, 0   , 1,
#'   1, 1, -0.2, 1,
#'   2, 1, 0.2 , 1
#' ), ncol = 4, byrow = TRUE)
#' perturbResp(0, myItem, "bfit", list(1L, "discrimination", 0.5, 0, "bfit"))
#' @importFrom stats runif
perturbResp <- function(ability, item, group, perturbation) {
  perturbation <- as.list(perturbation)
  pGroup <- NA_character_
  if (length(perturbation) >= 5) pGroup <- unlist(perturbation[5:length(perturbation)])

  # Is this case in a group to be perturbed?
  caseIsInGroup <- is.null(group) || all(is.na(pGroup)) || group %in% pGroup
  if (!caseIsInGroup) return(itemResp(ability, item))

  tmpI <- perturbItem(
    item,
    type = perturbation[[2]],
    factor = as.numeric(perturbation[[3]]),
    pivot = as.numeric(perturbation[[4]])
  )
  itemResp(ability, tmpI)
}


#' @title itemResp
#'
#' @description Returns a probabilistic item response, given one ability and one
#'   set of item parameters.
#'
#' @param myAblty A numeric ability.
#' @param myItem A single item (a matrix k categories by four
#'   (category score, delta dot, tau, discrimination).
#' @return A numeric score, usually an integer in the range
#'   0, 1, 2, ..., k-1. This is taken from the first column of the matrix of
#'   item parameters so may also contain a numeric score.
#' @keywords internal
#' @examples
#' myItem <- matrix(c(0, 0, 0, 0, 1, 1, 0, 1), ncol = 4, byrow = TRUE)
#' itemResp(0, myItem)
itemResp <- function(myAblty, myItem) {
  myRand <- runif(1)
  myRspP <- simplep(myAblty, myItem)
  myCdf <- cumsum(myRspP)
  myCatScore <- min(which(myRand < myCdf))
  return(myItem[myCatScore, 1])
}

#' @title rawScore
#'
#' @description returns the adjusted (for zero and perfects) and unadjusted raw scores as well as the maximum score.
#'
#' @param x a vector of scored item responses. Order of item responses is the same as the order of `itemParams`,
#'   missing item responses are scored NA.
#' @param itemParams A list of item params of the structure used in `simplef` (a matrix of k categories by three
#'   (category score, delta dot, tau)). See conquestr::makeItemList for a helper to generate this list.
#' @param perfAdjust the correction factor for zeros and perfects to be add/subtracted from the raw score.
#' @return a list containing three elements: (1) raw score (unadjusted), (2) raw score (adjusted), (3) maximum score.
#' @keywords internal
rawScore <- function(x, itemParams, perfAdjust = 0.3) {
  myResults <- list()
  if (perfAdjust < 0 || !inherits(perfAdjust, "numeric")) stop("perfAdjust must be numeric and greater than 0")

  # calc raw score
  myRaw <- sum(x, na.rm = TRUE)

  # calc max score
  tmpItemParams <- itemParams[!is.na(x)] # keep the item params with a valid response
  countItems <- length(x[!is.na(x)]) # how many items were responded to?
  if (length(countItems) > 0) {
    maxScore <- 0
    for (i in seq(countItems))
    {
      # this is the max category for this item - NOTE: this may include scores
      tmp <- max(tmpItemParams[[i]][ , 1])
      maxScore <- maxScore + tmp
    }
  } else {
    maxScore <- NA # no items responded to
  }

  # calc adjusted raw score
  myRawA <- myRaw
  if (perfAdjust > 0) {
    if (myRaw == 0) myRawA <- myRaw + perfAdjust
    if (myRaw == maxScore) myRawA <- myRaw - perfAdjust
  }

  myResults[["raw score unadjusted"]] <- myRaw
  myResults[["raw score adjusted"]] <- myRawA
  myResults[["highest score possible"]] <- maxScore

  return(myResults)
}

#' @title thetaScore
#'
#' @description Return score (O-E) at theta. Used in estimation of theta by minimising
#'   _raw score - expected score_ at theta
#'
#' @param theta a scalar value of theta.
#' @param responses a vector of item responses for one case (can contain NA). Responses are in the same order as `items`
#' @param itemParams a list of item designs (each used in a call to `simplef`). Must be of same length as `responses`.
#' @param perfAdjust the correction factor for zeros and perfects to be add/subtracted from the raw score.
#'
#' @return a double, the score (O-E) at theta.
#' @keywords internal
thetaScore <- function(theta, responses, itemParams, perfAdjust = 0.3) {
  tmpRaw <- rawScore(responses, itemParams, perfAdjust = perfAdjust)[[2]] # observed
  tmpExp <- list()
  for (i in seq(itemParams)) {
    if (is.na(responses[i])) {
      tmpExp[[i]] <- NA
      next
    }
    tmpExp[[i]] <- simplef(theta, itemParams[[i]])
  }
  tmpExpSum <- sum(unlist(tmpExp), na.rm = TRUE) # expected
  return(tmpRaw - tmpExpSum)
}

#' @title cnvrtItemParam
#'
#' @description takes an item in one model's parameterisation and
#'   returns it in another parameterisation.
#'
#' @param item an item design matrix that is of size response categories (m) by four:
#'   * column one is category values, usually from 0 to m. Sometimes referred to as 'x',
#'     and in this case, this value times the discrimination is the category score.
#'   * column two is the delta dot parameter repeated m times (the average difficulty of the item)
#'   * column three is the tau (step) parameter where for the first response category (x = 0) tau = 0,
#'     and for m >= 2, entries are deviations from delta dot. In the dichotomous case, all items
#'     in this column are zero.
#'   * column four is the discrimination parameter ("a")
#' @param from a string, either "muraki"  or "conquest" (default) (see 10.1177/0146621697211001).
#'   Describing the parameterisation of _item_
#' @param to a string, either "muraki"  or "conquest" (default) (see 10.1177/0146621697211001).
#'   Describing the output parameterisation of the returned _item_ parameter matrix
#'   Note that "muraki" assumes the scaling constant D = 1.7 (to give the normal ogive metric)
#' @param DFrom a number, giving the scaling constant for the `from` parameterisation. Default is 1 (logistic metric).
#'   Other common values are D = 1.7 (to give the normal ogive metric)
#' @param DTo a number, giving the scaling constant for the `to` parameterisation. Default is 1 (logistic metric).
#'   Other common values are D = 1.7 (to give the normal ogive metric)
#'
#' @return an m x 4 matrix of item parameters. The same dimensions as the input, _item_
#' @examples
#' myTheta <- 0
#' myDelta <- 1.5
#' a <- 1.5
#' m <- 3
#' itemParamX <- seq(0, m-1, 1)
#' itemParamD <- rep(myDelta, m)
#' itemParamT <- c(0, -0.5, 0.5)
#' itemParamA <- rep(a, m)
#' itemParam <- cbind(itemParamX, itemParamD, itemParamT, itemParamA)
#' colnames(itemParam)<- c("x", "d", "t", "a")
#' myItem <- cnvrtItemParam(itemParam, from = "conquest", to = "muraki", DFrom = 1, DTo = 1.7)
cnvrtItemParam <- function(item, from = "muraki", to = "conquest", DFrom = 1, DTo = 1) {
  # check item, check from and to
  # TODO
  myItem_t <- item
  if (DFrom <= 0 || DTo <= 0) stop("DFrom and DTo must be greater than 0")
  if (DFrom == DTo) {
    D <- 1
  } else {
    # what to do when DFrom < DTo
    D <- DFrom / DTo
  }
  
  if (from == "muraki") {
    # Muraki -> conquest
    myItem_t[, 4] <- item[, 4] * D # item score
    myItem_t[, 2] <- item[, 2] * (D * item[, 4]) # item locations ("conquest delta dots", in delta plus tau notation)
    myItem_t[, 3] <- -1 * item[, 3] * (D * item[, 4]) # item steps ("conquest taus", in delta plus tau notation) # -1 takes us from easiness to difficulty parameterisation
  } else if (from == "conquest") {
    # conquest -> Muraki
    myItem_t[, 4] <- item[, 4] * D # a 
    myItem_t[, 2] <- item[, 2] / item[, 4] # b 
    myItem_t[, 3] <- -1 * item[, 3] / item[, 4] # d
  }
  return(myItem_t)
}

#' @title simplep
#'
#' @description returns response probabilities for each response category of an item at a given value of theta.
#'
#' @param theta a scalar value of theta.
#' @param item an item design matrix that is of size response categories (m) by four:
#'   * column one is category values, usually from 0 to m. Sometimes referred to as 'x',
#'     and in this case, this value times the discrimination is the category score.
#'   * column two is the delta dot parameter repeated m times (the average difficulty of the item)
#'   * column three is the tau (step) parameter where for the first response category (x = 0) tau = 0,
#'     and for m >= 2, entries are deviations from delta dot. In the dichotomous case, all items
#'     in this column are zero.
#'   * column four is the discrimination parameter ("a")
#' @param model a string, either "muraki"  or "conquest" (default) (see 10.1177/0146621697211001).
#'   This tells downstream functions what parameterisation has been used for the model and helps with
#'   plotting and other outputs.
#' @param D a number, giving the scaling constant. Default is 1 (logistic metric).
#'   Other common values are D = 1.7 (to give the normal ogive metric)
#'
#' @return a k x 1 matrix of response probabilities evaluated at theta.
#' @keywords internal
#' @examples
#' myTheta <- 0
#' myDelta <- 1.5
#' a <- 1.5
#' k <- 3
#' itemParamX <- seq(0, k-1, 1)
#' itemParamD <- c(0, rep(myDelta, k-1))
#' itemParamT <- c(0, -0.5, 0.5)
#' itemParamA <- rep(a, k)
#' itemParam <- cbind(itemParamX, itemParamD, itemParamT, itemParamA)
#' colnames(itemParam)<- c("x", "d", "t", "a")
#' myProbs <- simplep(myTheta, itemParam)
simplep <- function(theta, item, model = "conquest", D = 1) {
  
  if (missing(model)) model <- "conquest"
  if (!model %in% c("muraki", "conquest")) stop("model must be muraki or conquest")
  if (!is.numeric(theta) && length(theta) != 1) stop("theta must be a scalar value")
  if (!is.matrix(item) && !ncol(item) %in% c(3, 4) && nrow(item) > 1) {
    stop("item must be a matrix with at least scores, delta dots, and taus")
  }

  if (ncol(item) == 3) {
    item <- cbind(item, rep(1, length(item[ , 1])))
    message("assuming item discrimination is constant and equal to 1")
  }

  # Probabilities are computed in C++ (src/generateCq.cpp, simplepCpp). The
  # (unnormalised) log-probability for category i is:
  #
  # "muraki": Muraki's (1992) GPCM. For category k (k = 0, ..., m), the model gives
  #   the cumulative logit
  #     sum_{v=1}^{k} D*a_i*(theta - b_iv)
  #   with b_i0 := 0 so the k = 0 term contributes 0. Following Muraki's
  #   (1992) reparameterisation, b_iv = b_i - d_iv, where b_i (item[, 2]) is
  #   constant across categories (the overall item location) and d_iv
  #   (item[, 3]) is the step deviation for category v (d_i1 := 0). Expanding
  #   and factoring out D*a_i:
  #     sum_{v=1}^{k} D*a_i*(theta - (b_i - d_iv))
  #       = D*a_i*(k*theta - k*b_i + sum_{v=1}^{k} d_iv)
  #       = D*a_i*(x*theta - sum_{v=1}^{k} b_i + sum_{v=1}^{k} d_iv) # since x = k here
  #   i.e. cumulative sums of b (item[, 2]) and d (item[, 3]) over
  #   categories 1:i, exactly mirroring the cumulative delta+tau sum used in
  #   the "conquest" branch below. Verified numerically against
  #   cnvrtItemParam()-converted items: this cumulative form reproduces
  #   identical response probabilities to the "conquest" branch (see
  #   tests/testthat/test-genHelpers.R).
  #     D * item[i, 4] * (item[i, 1] * theta - sum(item[1:i, 2]) + sum(item[1:i, 3]))
  #
  # "conquest": Adams' MMCLM, D*((a*b*theta_a) - sum(delta_a+tau_a))
  #     D * ((item[i, 4] * item[i, 1] * theta) - (sum(item[1:i, 2:3])))
  probs <- matrix(simplepCpp(theta, as.matrix(item), model == "muraki", D), ncol = 1)
  return(probs)
}

#' @title simplef
#'
#' @description returns expected score at a given value of theta.
#'
#' @param theta a scalar value of theta.
#' @param item an item design matrix that is of size response categories (k) by three. The three columns are:
#'   * column one is scoring values, usually from 0 to k.
#'   * column two is the delta dot parameter repeated k times (the average difficulty of the item)
#'   * column three is the tau (step) parameter where for k = 1, tau = 0, and for k >= 2,
#'     subsequent entries are deviations from delta dot.
#'   * column four is the discrimination paramter ("a")
#' @param model a string, either "muraki"  or "conquest" (default) (see 10.1177/0146621697211001).
#'   This tells downstream functions what parameterisation has been used for the model and helps with
#'   plotting and other outputs.
#' @param D a number, giving the scaling constant. Default is 1 (logistic metric).
#'   Other common values are D = 1.7 (to give the normal ogive metric)
#'
#' @return a double - the expected score at theta.
#' @keywords internal
#' @examples
#' myTheta <- 0
#' myDelta <- 1.5
#' k <- 3
#' a <- 1
#' itemParamX <- seq(0, k-1, 1)
#' itemParamD <- rep(myDelta, k)
#' itemParamT <- c(0, -0.5, 0.5)
#' itemParamA <- rep(a, k)
#' itemParam <- cbind(itemParamX, itemParamD, itemParamT, itemParamA)
#' colnames(itemParam)<- c("x", "d", "t", "a")
#' myExpect <- simplef(myTheta, itemParam)
simplef <- function(theta, item, model, D) {
  if (missing(model)) model <- "conquest"
  if (missing(D)) D <- 1
  probs <- simplep(theta, item, model, D)
  # Expected score is defined as E[X | theta] = sum_k x_k * P(X = x_k | theta),
  # i.e. a probability-weighted average of the category *scores* x_k (item[, 1]),
  # under both the "conquest" and "muraki" parameterisations. The discrimination
  # parameter a_i (item[, 4]) does not belong in this sum: it already enters the
  # calculation indirectly, via its effect on the category probabilities
  # returned by simplep() (which does scale the logits by a_i - see simplep()'s
  # derivation comment above). Multiplying by a_i again here would rescale
  # scores by discrimination, which conflates two distinct quantities: the
  # score metric (x, e.g. 0, 1, 2, ...) and the item's slope on the theta
  # metric (a). These have different units and should not be multiplied
  # together when computing an expected score.
  expt <- sum(item[, 1] * probs[, 1])
  return(expt)
}

#' @title pX
#'
#' @description returns response probabilities for given response _x_ to an item.
#'
#' @param x a scalar integer - a response to an item (usually in the range 0, k-1,
#'     where k is the number of response categories).
#' @param probs a matrix returned from simplef.
#' @return a 1 x 1 matrix giving the response probability of _x_.
#' @keywords internal
#' @examples
#' myTheta <- 0
#' myDelta <- 1.5
#' k <- 3
#' a <- 1.25
#' itemParamX <- seq(0, k-1, 1)
#' itemParamD <- rep(myDelta, k)
#' itemParamT <- c(0, -0.5, 0.5)
#' itemParamA <- rep(a, k)
#' itemParam <- cbind(itemParamX, itemParamD, itemParamT, itemParamA)
#' colnames(itemParam)<- c("x", "d", "t", "a")
#' myProbs <- simplef(myTheta, itemParam)
#' myProb <- pX(0, myProbs)
pX <- function(x, probs) {
  return(probs[x+1])
}

#' @title theta_ll
#'
#' @description returns the log of the likelihood of theta, given a vector of 
#'   item responses, item parameters.
#'   Note that this is the simple ll - it is the continuing product of the 
#'   response probabilities. It includes a hack, that will nudge the raw 
#'   response to the first item to adjust for zeros and perfects.
#'   This should be updated to work with adjusted raw scores instead...
#'
#' @param theta  a scalar value of theta.
#' @param responses a vector of item responses (used in a call to `pX`).
#' @param itemParams a list of item designs (each used in a call to `simplef`). 
#'   Must be of same length as `responses`.
#'
#' @return a double, the log of the likelihood at theta..
#' @keywords internal
theta_ll <- function(theta, responses, itemParams) {
  if (length(responses) != length(itemParams)) {
    stop(
      "you must provide a vector of responses and a list with an item design for each response"
    )
  }
  # drop items where response in NA
  itemParamsT <- list()
  if (any(is.na(responses))) {
    itemParamsT <- itemParams[[!is.na(responses)]]
    itemParams <- itemParamsT
  }
  results <- list()
  tmpProbs <- list()

  # deal with all NA, zero or perfects
  if (all(is.na(responses))) responses[1] <- 0.3 # this feels SUUUUUPER hacky
  tmpRaw <- rawScore(x = responses, itemParams = itemParams)
  if (as.numeric(tmpRaw[1]) == 0)
  {
    responses[!is.na(responses)][1] <-0.3

  } else if (as.numeric(tmpRaw[1]) == as.numeric(tmpRaw[3]))
  {
    responses[!is.na(responses)][1] <- (responses[!is.na(responses)][1]-0.3)
  }

  for (i in seq(length(responses))) {
    if (is.na(responses[i]))
    {
      results[[i]] <-0
    } else
    {
      p <- simplep(theta, itemParams[[i]])
      px <- pX(responses[i], p)
      results[[i]] <- log(px)
    }
  }
  ll <- sum(unlist(results))
  return(ll)
}



#' @title makeItemList
#'
#' @description creates a list of item matrices.
#'    Each matrix represent one item's set of item parameters. The structure of the
#'    matrix is the same as used in `conquestr::simplef`
#'    (a matrix of k categories by four (category score, delta dot, tau, discrimination)).
#'
#' @param scores a data frame or matrix containing category scores for each item.
#'    If NULL, it is assumed increasing integer scoring starting at 0 is used for all items (that is, the
#'    first category is scored 0, the second category is scored 1, the \eqn{k^{th}}{kth} category is scored k-1).
#'
#'    If a data frame, column labels should be "id", "itemid", "step", "score".
#'    If a matrix, the column order should be: "id", a unique item ID for each item matched with values in `deltaDot`;
#'    "itemid", item labels for each item (or NA); "step", an indicator of which step/item category this score
#'    represents and "score" the value for the scoring parameter associated with this category.
#'    There must be one score for each category (i.e. 2 for dichotomies and one for each of k categories for
#'    polytomies).
#'
#'    If a data frame, or a matrix:
#'    * "id" is an integer
#'    * "itemid" is a character string
#'    * "step" is an integer
#'    * "score" is numeric
#'    * The original category scores (i.e., increasing integer scoring) is preserved in the rownames of the matrix.
#'
#' @param deltaDot a data frame or matrix of delta dots (average item location/difficulty for each item).
#'
#'    If a data frame, column labels should be: "id", "itemid", "delta".
#'    "itemid" should be populated with an item label or be missing for all values.
#'    If a matrix, column order should be: "id", a unique item ID for each row; "itemid", item labels for each item
#'    (or NA); "delta", a delta dot.
#'
#'    If a data frame, or a matrix:
#'    * "id" is an integer
#'    * "itemid" is a character string
#'    * "delta" is numeric
#'
#' @param tau NULL if all items are dichotomies.
#'    A data frame or matrix of taus for polytomous items. Only polytomous items should be in this file.
#'    If an item ID in `deltaDot` in not in `tau` it is assumed that the item is dichotomous.
#'    The tau parameters represent the deviation from the delta dot to give the item parameters for adjacent category
#'    boundaries (e.g., delta one (
#'    \eqn{\delta_{1} = \dot{\delta} + \tau_{1}}{delta_1 = delta_dot + tau_1}
#'    ) is the boundary between \eqn{k_{1}}{k_1} and \eqn{k_{2}}{k_2},
#'    delta two (
#'    \eqn{\delta_{2} = \dot{\delta} + \tau_{2}}{delta_2 = delta_dot + tau_2}
#'    ) is the category boundary between \eqn{k_{1}}{k_1} and \eqn{k_{2}}{k_2}).
#'
#'    Where a polytomous item has k categories, there should be k-2 rows for that item in `tau`. For example,
#'    a 3-category item has categories \eqn{k_{1}}{k_1}, \eqn{k_{2}}{k_2} and \eqn{k_{3}}{k_3}.
#'    There will be one value in `tau` for this item.
#'    The value in `tau` represents the the first category boundary.
#'    (e.g., between \eqn{k_{1}}{k_1} and \eqn{k_{2}}{k_2}).
#'    The last (second in this case) category boundary is constrained to be the negative sum of the other tau values
#'    within this item (and is therefore not required in the file).
#'
#'    If a data frame, column labels should be "id", "itemid", "step", "tau".
#'    If a matrix, the column order should be: "id", a unique item ID for each item matched with values in `deltaDot`;
#'    "itemid", item labels for each item (or NA); "step", an indicator of which step/item category this threshold
#'    represents (minimum value should be 1 and maximum value should be k-1);
#'    "tau" the value for the tau parameter associated with this step.
#'
#'    If a data frame, or a matrix:
#'    * "id" is an integer
#'    * "itemid" is a character string
#'    * "step" is an integer
#'    * "tau" is numeric
#'
#' @param discrim a double, a data frame, or a matrix of item (or category) discrimination parameters.
#'    When a double is provided, the value is applied to all discrimination parameters.
#'    The default is 1. Setting the value to 1.7 is one approach to re-scale to the normal ogive metric.
#'    Otherwise a data.frame or matrix defining the discrimination parameter for each response category.
#'    If a data frame, column labels should be "id", "itemid", "step", "discrim".
#'    If step is NA and there is only one entry for an item "itemid", the discrimination is
#'    assumed to be constant for all response categories with the item.
#'    This is the case for named models like the GPCM and 2PL models, and can be a short
#'    hand way of defining the discrimination without specifying all categories.
#'    When discrimination varies across scoring categories, the Bock-nominal model is implied.
#'    In the case of discrimination varying across scoring categories, all categories must be
#'    defined.
#'
#'    If a data frame, or a matrix:
#'    * "id" is an integer
#'    * "itemid" is a character string
#'    * "step" is an integer
#'    * "discrim" is numeric
#'
#' @return a list.
#' @examples
#' nItems <- 10
#' myItemsDeltaDot <- data.frame(
#'   id= seq(nItems),
#'   itemid= NA,
#'   delta = runif (nItems, -4, 1) # nItems items in range -4,1
#' )
#' myItemsList <- conquestr::makeItemList(deltaDot = myItemsDeltaDot)
makeItemList <- function(scores = NULL, deltaDot, tau = NULL, discrim = 1) {
  # used for internal debugging
  isDebug <- FALSE

  # return object
  myItems <- list()

  # set up
  if (is.null(tau)) {
  tau <- data.frame(
      id = NA_integer_,
      itemid = NA_character_,
      step = NA_integer_,
      tau = NA_real_
    )
  }
  isScored <- FALSE
  hasDiscrims <- FALSE
  if (!is.null(scores)) isScored <- TRUE
  myScr <- scores
  myDlt <- deltaDot
  myTau <- tau
  if (!inherits(discrim, "numeric")) hasDiscrims <- TRUE
  myDiscim <- discrim

  itemCounter <- 1

  # coerce matrices to dfs
  if (inherits(myScr, "matrix")) {
    myScr <- data.frame(
      id = scores[, 1],
      itemid = scores[, 2],
      step = scores[, 3],
      score = scores[, 4]
    )
  }
  if (inherits(myDlt, "matrix")) {
    myDlt <- data.frame(
      id = deltaDot[, 1],
      itemid = deltaDot[, 2],
      delta = deltaDot[, 3]
    )
  }
  if (inherits(myTau, "matrix")) {
    myTau <- data.frame(
      id = tau[, 1],
      itemid = tau[, 2],
      step = tau[, 3],
      tau = tau[, 4]
    )
  }
  if (inherits(myDiscim, "matrix")) {
    myDiscim <- data.frame(
      id = discrim[, 1],
      itemid = discrim[, 2],
      step = discrim[, 3],
      score = discrim[, 4]
    )
  }

  # error checking
  if (!inherits(myDlt, "data.frame") || !inherits(myTau, "data.frame")) {
    stop("deltaDot and tau must be data frames or matrices")
  }
  if (any(!c("id", "delta") %in% names(myDlt))) {
    stop("'deltaDot' must contain the column labels 'id' and 'delta'")
  }
  if (any(!c("id", "step", "tau") %in% names(myTau))) {
    stop("'tau' must contain the column labels 'id', 'step', and 'tau'")
  }
  if (inherits(myDlt, "data.frame")) {
    if (!is.numeric(myDlt$delta)) {
      myDlt$delta <- as.numeric(myDlt$delta)
      if (!is.numeric(myDlt$delta)) stop("in `deltaDot`, 'delta' must be numeric")
    }
    if (!is.integer(myDlt$id)) {
      myDlt$id <- as.integer(myDlt$id)
      if (!is.integer(myDlt$id)) stop("in `deltaDot`, 'id' must be an integer")
    } 
  }
  if (inherits(myTau, "data.frame")) {
    if (!is.numeric(myTau$tau)) {
      myTau$tau <- as.numeric(myTau$tau)
      if (!is.numeric(myTau$tau)) stop("in `tau`, 'tau' must be numeric")
    }
    if (!is.integer(myTau$id)) {
      myTau$id <- as.integer(myTau$id)
      if (!is.integer(myTau$id)) stop("in `tau`, 'id' must be an integer")
    } 
    if (!is.integer(myTau$step)) {
      myTau$step <- as.integer(myTau$step)
      if (!is.integer(myTau$step)) stop("in `tau`, 'step' must be an integer")
    }
    # remember that tau can be all NA, so double check this too
    if (!all(tau$id %in% myDlt$id) && !all(is.na(tau$id))) stop(
      paste0(
          "all items ids in `tau` must also exist in `deltaDot`",
          " problem with item id ",
          tau$id[which(!tau$id %in% myDlt$id)],
          " in `tau` but not in `deltaDot`"
        )
    )
  }
  if (inherits(myScr, "data.frame")) {
    if (!is.numeric(myScr$score)) stop("in `score`, 'myScr' must be numeric")
    if (!is.integer(myScr$id)) stop("in `myScr`, 'id' must be an integer")
    if (!all(myScr$id %in% myDlt$id)) stop(
      paste0(
          "all items ids in `scores` must also exist in `deltaDot`",
          " problem with item id ",
          myScr$id[which(!myScr$id %in% myDlt$id)],
          " in `scores` but not in `deltaDot`"
        )
    )
  }
  if (inherits(myDiscim, "data.frame")) {
    if (!is.numeric(myDiscim$discrim)) stop("in `myDiscim`, 'discrim' must be numeric")
    if (!is.integer(myDiscim$id)) stop("in `myDiscim`, 'id' must be an integer")
    if (!all(myDiscim$id %in% myDlt$id)) stop(
      paste0(
          "all items ids in `discrim` must also exist in `deltaDot`",
          " problem with item id ",
          myDiscim$id[which(!myDiscim$id %in% myDlt$id)],
          " in `discrim` but not in `deltaDot`"
        )
    )
  } else {
    if (!inherits(myDiscim, "numeric")) stop("Discrim must be numeric")
  }

  # need to check id is unique!
  # need to check that tau$id in deltaDot$id

  # use scores, deltas, taus, and discrims to make conquestr items list
  for (item in unique(myDlt$id)) {
    if (isDebug) print(paste0("Begin working on item: ", item))
    tName <- myDlt$itemid[myDlt$id == item]
    tmpMyDlt <- myDlt[myDlt$id == item, ]
    if (!item %in% myTau$id) { # if item is dichotomous
      if (isDebug) print(paste0("item: ", item, " is dichotomous"))
      tItm <- matrix(c(0, 0, 0, 1, 1, tmpMyDlt$delta, 0, 1), ncol = 4, byrow = TRUE)
      colnames(tItm) <- c("k", "d", "t", "a")
    } else { # item is polytomous
      tmpMyTau <- myTau[myTau$id == item , ]
      tStps <- max(tmpMyTau$step) + 2
      if (isDebug) print(paste0("item: ", item, " is polytomous and has ", tStps, " steps"))
      tIdCnstr <- -1 * sum(tmpMyTau$tau) # negative sum of taus
      tItm <- matrix(rep(c(NA, tmpMyDlt$delta, NA, 1), tStps), ncol = 4, byrow = TRUE)
      colnames(tItm) <- c("k", "d", "t", "a")
      for (i in seq(tStps)) {
        if (i == 1) {
          tItm[i, 1] <- 0
          tItm[i, 2] <- 0
          tItm[i, 3] <- 0
        } else if (i < max(tStps)) {
          tItm[i, 1] <- (i - 1)
          tItm[i, 3] <- tmpMyTau$tau[tmpMyTau$step == (i - 1)]
        } else {
          tItm[i, 1] <- (i - 1)
          tItm[i, 3] <- tIdCnstr
        }
      }
    }
    # Discrimination
    if (is.numeric(myDiscim) && myDiscim != 1) {
       tItm[, 4] <- tItm[, 4] * myDiscim
    }
    if (hasDiscrims) {
      # work out if there is one entry per item
      tmpMyDiscrim <- myDiscim[myDiscim$id == item , ]
      if (length(tmpMyDiscrim$step) == 1 && is.na(tmpMyDiscrim$step)) {
        tItm[, 4] <- tmpMyDiscrim$discrim
      } else {
        # or if there is many entries per item
        if (length(tmpMyDiscrim$discrim) != length(tItm[, 4])) {
          stop(
            paste0(
              "problem with item ", item, ": you must provide",
              " a discrimination value for each response category",
              " or provide one value for each item and ensure",
              " the step value is NA"
            )
          )
          tItm[, 4] <- tmpMyDiscrim$discrim
        }
      }
    }
    # Scores
    if (isScored) {
      # save original categories as rownames
      rownames(tItm) <-  tItm[, 1]
      # work out if there is one entry per item
      tmpMyScr <- myScr[myScr$id == item, ]
      if (isDebug) {
        print(
          paste0("applying scores to item: ", item, " the following scores are provided by the user:\n")
        )
        print(tmpMyScr)
      }
      if (length(tmpMyScr$score) == length(tItm[, 1])) {
        tItm[, 1] <- tmpMyScr$score
      } else {
        stop(
          paste0(
            "problem with item ", item, ": you must provide",
            " a score value for each response category",
          )
        )
      }
    }

    # build return object
    myItems[[itemCounter]] <- tItm
    if (!is.na(tName)) names(myItems)[itemCounter] <- tName
    itemCounter <- itemCounter + 1
  }
  return(myItems)
}

#' @title makeItemDfs
#'
#' @description takes in a list of item matrices and returns a list of data frames
#'    each representing the parameters given in the matrices. The return object is 
#'    suitable to pass into `conquestr::makeItemList` to construct a lit of matrices
#'    where each matrix represent one item's set of item parameters. The structure of the
#'    matrix is the same as used in `conquestr::simplef`
#'    (a matrix of k categories by four (category score, delta dot, tau, discrimination)).
#'    A common use for this function is turn a list of item matrices into a flat data 
#'    structure.
#'
#' @param itemList a list of item matrices. The structure of each
#'    matrix is the same as used in `conquestr::simplef`
#'    (a matrix of k categories by four (category score, delta dot, tau, discrimination)).
#' 
#'
#' @return a list.
#' @examples
#' nItems <- 10
#' myItemsDeltaDot <- data.frame(
#'   id= seq(nItems),
#'   itemid= NA,
#'   delta = runif (nItems, -4, 1) # nItems items in range -4,1
#' )
#' myItemsList <- conquestr::makeItemList(deltaDot = myItemsDeltaDot)
makeItemDfs <- function(itemList) {
  # used for internal debugging
  isDebug <- FALSE

  # return object
  myItems <- list()

  # set up
  anyPolyItems <- FALSE
  firstPolyItem <- 0

  if (missing(itemList) || !is.list(itemList)) {
   stop("'itemList' must be a list of item matrices")
  }

  for (i in seq(itemList)) {
    if (!is.matrix(itemList[[i]])) stop("elements of 'itemList' must be matrices")
    if (nrow(itemList[[i]]) < 2) stop("elements of 'itemList' must have at least 2 rows")
    if (ncol(itemList[[i]]) != 4) stop(
      "elements of 'itemList' must have 4 columns: score, delta dot, tau, discrimination"
    )
    if (nrow(itemList[[i]]) > 2) {
      anyPolyItems <- TRUE
      if (firstPolyItem == 0) firstPolyItem <- i
      # print(paste0("setting `anyPolyItems` to true, and `firstPolyItem` is: ", firstPolyItem))
    }
  }

  # do the elements of itemList have names? If not, use counter for "itemid"
  if(is.null(names(itemList))) {
    isNamed <- FALSE
  } else {
    isNamed <- TRUE
  }

  for (i in seq(itemList)) {
    
    thisItemName <- as.character(i)
    if (isNamed) thisItemName<- names(itemList)[i]

    thisItemPoly <- FALSE
    if (anyPolyItems && nrow(itemList[[i]]) > 2) thisItemPoly <- TRUE
    
    thisItemConstDiscrim <- TRUE
    if (length(unique((itemList[[i]][2:(nrow(itemList[[i]])),4]))) > 1) {
      thisItemConstDiscrim <- FALSE
    }

    #scores
      # * "id" is an integer
      # * "itemid" is a character string
      # * "step" is an integer
      # * "score" is numeric
      # * The original category scores (i.e., increasing integer scoring) is preserved in the rownames of the matrix.
    tScores <- data.frame(
      id = as.integer(i),
      itemid = thisItemName,
      step = 1:nrow(itemList[[i]]),
      score = itemList[[i]][ , 1]
    )

    # deltadots
      # "id" is an integer
      # "itemid" is a character string
      # "delta" is numeric
    tDeltaDot <- data.frame(
      id = as.integer(i),
      itemid = thisItemName,
      delta = itemList[[i]][nrow(itemList[[i]]), 2]
    )

    # tau
      # "id" is an integer
      # "itemid" is a character string
      # "step" is an integer
      # "tau" is numeric
    # print(paste0("is this item poly? ", thisItemPoly, " nrows(itemList[[i]]): ", nrow(itemList[[i]])))
    if (thisItemPoly) {
      #print(
      #  paste0(
      #    "thisItemPoly: ", as.integer(i),
      #    " id: ", thisItemName, 
      #    " itemid: ", thisItemName, 
      #    " step: ", 1:(nrow(itemList[[i]])-2), 
      #    " tau: ", itemList[[i]][2:(nrow(itemList[[i]])-1) , 3]
      #  )
      #)
       tTau <- data.frame(
        id = as.integer(i),
        itemid = thisItemName,
        step = 1:(nrow(itemList[[i]])-2),
        tau = itemList[[i]][2:(nrow(itemList[[i]])-1) , 3]
       )
    }

    # discrim
      # when 1 row in DF and step = NA, discrim is for all cats
      # otherwise all steps must be defined 
      # "id" is an integer
      # "itemid" is a character string
      # "step" is an integer
      # "discrim" is numeric
    tDiscrim <- data.frame(
      id = as.integer(i),
      itemid = thisItemName,
      step = if (thisItemConstDiscrim) {
        NA
      } else {
        1:nrow(itemList[[i]])
      },
      discrim = if (thisItemConstDiscrim) {
        itemList[[i]][2, 4]
      } else {
        itemList[[i]][ , 4]
      }
    )

    if (i == 1) {
      scoresDf <- tScores
      deltaDotDf <- tDeltaDot
      discrimDf <- tDiscrim
    } else {
      scoresDf <- rbind(scoresDf, tScores)
      deltaDotDf <- rbind(deltaDotDf, tDeltaDot)
      discrimDf <- rbind(discrimDf, tDiscrim)
    }
    if (anyPolyItems && i >= firstPolyItem) {
      if (i == firstPolyItem) {
      tauDf <- tTau
      } else {
      tauDf <- rbind(tauDf, tTau)
      }
    }
  }

  myItems[["scores"]] <- scoresDf
  myItems[["deltaDot"]] <- deltaDotDf
  if (anyPolyItems) {
    myItems[["tau"]] <- tauDf
  } else {
    myItems[["tau"]] <- NULL
  }
  myItems[["discrim"]] <- discrimDf

  
  return(myItems)
}

#' @title mleCalc
#'
#' @description returns the MLE for each case and asymptotic variance given a _fixed_ set of item parameters.
#'    This is done by finding the root of the distance between the observed and expected score at theta.
#'    That is, the location where O-E = 0.
#'
#' @param responses a data frame made up of rows of vectors of item responses, one for each case.
#' @param itemParams a list of item designs (each used in a call to `simplef`). Must be of same length as `responses`.
#' @param ... optional arguments, e.g., perfAdj.
#'
#' @return a data frame.
#' @keywords internal
#' @importFrom stats optim
mleCalc <- function(responses, itemParams, ...) {
  myMle <- list()
  for (i in seq(length(responses[ , 1]))) {
    myMle[[i]] <- list()
    myScores <- rawScore(x = responses[i , ], itemParams = itemParams)
    tmpMle <- optim(
      0, theta_ll, method = "BFGS", control = list(fnscale = -1),
      responses = responses[i , ], itemParams = itemParams, hessian = TRUE
    )
    myMle[[i]][["rawScore"]] <- myScores[[1]]
    myMle[[i]][["maxScore"]] <- myScores[[3]]
    myMle[[i]][["est"]] <- tmpMle$par
    # variance of the MLE given by Newton R is inverse of the negative Hessian
      #(i.e. the inverse of the negative of the matrix)
    #print(paste0("inv hessian for case: ", i))
    myMle[[i]]["se"] <- sqrt(solve(-tmpMle$hessian))
  }
  myMles <- matrix(unlist(myMle), ncol = 4, byrow = TRUE)
  myMles <- as.data.frame(myMles)
  names(myMles) <- c("raw_score", "max_score", "est", "se")
  return(myMles)
}

#' @title getItemMatrix
#'
#' @description generates an item matrix. Useful for
#'
#' @param responses a data frame made up of rows of vectors of item responses, one for each case.
#' @param itemParams a list of item designs (each used in a call to `simplef`). Must be of same length as `responses`.
#' @param ... optional arguments, e.g., perfAdj.
#'
#' @return a data frame.
#' @keywords internal
#' @importFrom stats optim
getItemMatrix <- function(responses, itemParams, ...) {
  myMle <- list()
  for (i in seq(length(responses[ , 1]))) {
    myMle[[i]] <- list()
    myScores <- rawScore(x = responses[i , ], itemParams = itemParams)
    tmpMle <- optim(
      0, theta_ll, method = "BFGS", control = list(fnscale = -1),
      responses = responses[i , ], itemParams = itemParams, hessian = TRUE
    )
    myMle[[i]][["rawScore"]] <- myScores[[1]]
    myMle[[i]][["maxScore"]] <- myScores[[3]]
    myMle[[i]][["est"]] <- tmpMle$par
    # variance of the MLE given by Newton R is inverse of the negative Hessian
      #(i.e. the inverse of the negative of the matrix)
    #print(paste0("inv hessian for case: ", i))
    myMle[[i]]["se"] <- sqrt(solve(-tmpMle$hessian))
  }
  myMles <- matrix(unlist(myMle), ncol = 4, byrow = TRUE)
  myMles <- as.data.frame(myMles)
  names(myMles) <- c("raw_score", "max_score", "est", "se")
  return(myMles)
}

#' @title writeImportXsi
#'
#' @description Writes a fixed width text file in the format required for the 
#'   ACER ConQuest command and argument `import anchor_xsi`. Can also be used
#'   for initial values, though caution should be used with the interpretation 
#'   of the argument `lconstraint` which should relate to the model of interest 
#'   ACER ConQuest
#' 
#'   Currently only works with implicit variables. Explicit variables may be added
#'   in the future. 
#'
#' @param items a list of item matrices
#' @param bmatix either the integer 1L for a unidimensional model, or
#'   a matrix, items by dimensions with 1L representing that the item in on this
#'   dimension, and a 0 otherwise.
#' @param lconstraint the identification constraint in use, one of "none", "items",
#'   or "cases".
#' @param file a path and filename to write file to disk.
#' #' @return invisibly returns path of file written to disk)
#' @seealso [conquestr::simplef()], [conquestr::genResponses()], `browseVignettes("conquestr")`
#' @examples
#'   myItem <- matrix(c(0, 0, 0, 0, 1, 1, 0, 1), ncol = 4, byrow = TRUE)
#'   myItems <- list(myItem, myItem)
#'   myItems[[2]][2, 2] <- -1 # make the second item delta equal to -1
#'   myResponses <- genResponses(abilities = rnorm(100), itemParams = myItems)
writeImportXsi <- function(items, bmatix = 1L, lconstraint = "none", file) {
  # TODO: check inputs are valid
  if(!is.list(items)) stop("'items' must be of class list")
  if(missing((file))) stop("'file' must be delcared")

  myXsiFile <- list()
  myXsiFile[["deltaDots"]] <- list()
  myXsiFile[["steps"]] <- list()
  
  for (i in length(items)) {
    # colate delta dots
    myXsiFile[["deltaDots"]][[i]] <- items[[i]][ncol(items[[i]]) , 2]
  }
  

  return(NULL)
}
