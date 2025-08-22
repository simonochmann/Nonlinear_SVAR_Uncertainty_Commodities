# functions/tvar/identify/build_contemporaneous_A.R
# Compatibility wrapper: supports BOTH call styles:
# (A) build_contemporaneous_A(model, identification = list(method="chol", ordering=...))
# (B) build_contemporaneous_A(method="chol", Sigma=<matrix>, ordering=<char vec>)

build_contemporaneous_A <- function(...) {
  `%||%` <- function(x,y) if (is.null(x)) y else x
  args <- list(...)
  nms  <- names(args)
  
  looks_like_model <- function(x) is.list(x) && (!is.null(x$variables) || !is.null(x$regimes))
  
  # ------------------ MODEL + IDENTIFICATION FORM ------------------
  if (length(args) >= 1 && (("identification" %in% nms) || looks_like_model(args[[1]]))) {
    # Dependencies: canonicalize_ordering() + get_resid_cov()
    if (!exists("canonicalize_ordering", mode = "function")) {
      source(here::here("functions","tvar","identify","canonicalize_ordering.R"))
    }
    if (!exists("get_resid_cov", mode = "function")) {
      source(here::here("functions","tvar","utils","get_resid_cov.R"))
    }
    
    model <- args[[1]]
    id    <- args$identification %||% list()
    method    <- (id$method %||% id$type %||% "chol")
    ordering  <- canonicalize_ordering(id$ordering, model$variables)
    Sigma     <- get_resid_cov(model, var_names = model$variables, verbose = FALSE)
    
    # Recurse into the matrix form to avoid duplicating logic
    return(build_contemporaneous_A(method = method, Sigma = Sigma, ordering = ordering))
  }
  
  # ------------------ METHOD + SIGMA + ORDERING FORM ------------------
  # Accept positional or named args
  method   <- if ("method"   %in% nms) args$method   else (if (length(args) >= 1) args[[1]] else "chol")
  Sigma    <- if ("Sigma"    %in% nms) args$Sigma    else (if (length(args) >= 2) args[[2]] else stop("Sigma missing"))
  ordering <- if ("ordering" %in% nms) args$ordering else (if (length(args) >= 3) args[[3]] else NULL)
  
  method <- match.arg(as.character(method), c("chol","identity"))
  
  S <- as.matrix(Sigma)
  # If named and ordering provided, align
  if (!is.null(ordering) && !is.null(colnames(S))) {
    keep <- ordering[ordering %in% colnames(S)]
    S <- S[keep, keep, drop = FALSE]
  }
  
  if (identical(method, "chol")) {
    A <- t(chol(S))                        # lower-tri so that A A' = S
  } else {
    A <- diag(ncol(S)); rownames(A) <- colnames(A) <- colnames(S)
  }
  A
}
