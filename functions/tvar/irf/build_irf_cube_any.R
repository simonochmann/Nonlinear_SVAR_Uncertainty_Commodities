# functions/tvar/irf/build_irf_cube_any.R
build_irf_cube_any <- function(obj, vars, H) {
  `%||%` <- function(x,y) if (is.null(x)) y else x
  vars <- as.character(vars); k <- length(vars)
  IRF <- array(0, dim = c(k,k,H), dimnames = list(resp = vars, imp = vars, h = seq_len(H)))
  
  translate <- (function(vars) {
    function(nm) {
      if (is.null(nm)) return(character(0))
      nm <- as.character(nm); out <- character(length(nm))
      for (i in seq_along(nm)) {
        x <- nm[i]
        if (is.na(x) || !nzchar(x)) { out[i] <- NA_character_; next }
        if (x %in% vars)           { out[i] <- x;          next }
        d  <- utils::adist(tolower(x), tolower(vars))
        j  <- which.min(d)
        out[i] <- if (length(j) && d[j] <= 2) vars[j] else NA_character_
      }
      out
    }
  })(vars)
  
  pull_num <- function(x) {
    if (is.numeric(x)) return(as.numeric(x))
    if (is.list(x)) {
      for (nm in c("50%","p50","median","mean","point","central","irf","IRF","value","y"))
        if (!is.null(x[[nm]]) && is.numeric(x[[nm]])) return(as.numeric(x[[nm]]))
    }
    NULL
  }
  
  as_cube <- function() array(0, dim = c(k,k,H), dimnames = list(resp = vars, imp = vars, h = seq_len(H)))
  
  # 0) Long df path
  if (is.data.frame(obj) && nrow(obj)) {
    nm <- tolower(names(obj))
    col_resp <- which(nm %in% c("response","resp","variable","var","y"))
    col_imp  <- which(nm %in% c("impulse","imp","shock","x"))
    col_h    <- which(nm %in% c("h","horizon","lag","step"))
    col_val  <- which(nm %in% c("value","irf","median","mean","p50","point","central"))
    col_stat <- which(nm %in% c("stat","quantile","percentile","q","band"))
    if (length(col_resp) && length(col_imp) && length(col_h) && length(col_val)) {
      df <- obj
      if (length(col_stat)) {
        s <- tolower(df[[col_stat[1]]])
        pick <- which(s %in% c("50%","p50","median","mean","point","central"))
        if (length(pick)) df <- df[pick, , drop = FALSE]
      }
      rmap <- translate(df[[col_resp[1]]]); imap <- translate(df[[col_imp[1]]])
      keep <- !is.na(rmap) & !is.na(imap)
      if (any(keep)) {
        df <- df[keep, , drop = FALSE]; rmap <- rmap[keep]; imap <- imap[keep]
        hidx <- pmax(1L, pmin(H, as.integer(df[[col_h[1]]])))
        for (j in seq_len(nrow(df))) IRF[rmap[j], imap[j], hidx[j]] <- as.numeric(df[[col_val[1]]][j])
        return(IRF)
      }
    }
  }
  
  # 1) 4D [stat,h,resp,imp] or unnamed variant
  if (is.array(obj) && length(dim(obj)) == 4) {
    dn <- dimnames(obj)
    if (!is.null(dn)) {
      stat_names <- tolower(dn[[1]] %||% character())
      pref <- c("50%","p50","median","mean","point","central"); sidx <- if (length(stat_names)) { hit <- which(stat_names %in% pref); if (length(hit)) hit[1] else 1 } else 1
      rnames <- translate(dn[[3]] %||% character()); inames <- translate(dn[[4]] %||% character())
      ok_r <- which(!is.na(rnames)); ok_i <- which(!is.na(inames))
      if (length(ok_r) && length(ok_i)) {
        for (ri in ok_r) for (ii in ok_i) {
          vec <- obj[sidx, , dn[[3]][ri], dn[[4]][ii], drop = TRUE]
          if (is.numeric(vec) && length(vec)) {
            L <- min(H, length(vec)); IRF[rnames[ri], inames[ii], seq_len(L)] <- as.numeric(vec[seq_len(L)])
          }
        }
        if (any(IRF != 0)) return(IRF)
      }
    }
    # unnamed → try [stat,h,k,k] reshapes
    d <- dim(obj); k_dims <- which(d == length(vars))
    if (length(k_dims) >= 2) {
      stat_dim <- setdiff(1:4, k_dims)[1]; h_dim <- setdiff(1:4, c(k_dims[1], k_dims[2], stat_dim))[1]
      x <- aperm(obj, c(stat_dim, h_dim, k_dims[1], k_dims[2])) # [stat,h,k,k]
      sidx <- 1
      for (r in seq_along(vars)) for (i in seq_along(vars)) {
        vec <- x[sidx, , r, i, drop = TRUE]
        if (is.numeric(vec) && length(vec)) {
          L <- min(H, length(vec)); IRF[vars[r], vars[i], seq_len(L)] <- as.numeric(vec[seq_len(L)])
        }
      }
      if (any(IRF != 0)) return(IRF)
    }
  }
  
  # 2) 4D draws [h,resp,imp,draw] (names optional)
  if (is.array(obj) && length(dim(obj)) == 4) {
    dn <- dimnames(obj); d <- dim(obj)
    if (!is.null(dn)) {
      rnames <- translate(dn[[2]] %||% character()); inames <- translate(dn[[3]] %||% character())
      ok_r <- which(!is.na(rnames)); ok_i <- which(!is.na(inames))
      if (length(ok_r) && length(ok_i)) {
        m <- apply(obj, c(1,2,3), mean, na.rm = TRUE)
        for (ri in ok_r) for (ii in ok_i) {
          vec <- m[, dn[[2]][ri], dn[[3]][ii], drop = TRUE]
          if (is.numeric(vec) && length(vec)) {
            L <- min(H, length(vec)); IRF[rnames[ri], inames[ii], seq_len(L)] <- as.numeric(vec[seq_len(L)])
          }
        }
        if (any(IRF != 0)) return(IRF)
      }
    }
    k_dims <- which(d == length(vars))
    if (length(k_dims) >= 2) {
      rest <- setdiff(1:4, k_dims)
      h_dim <- rest[1]; draw_dim <- rest[2]
      x <- aperm(obj, c(h_dim, k_dims[1], k_dims[2], draw_dim)) # [h,k,k,draw]
      m <- apply(x, c(1,2,3), mean, na.rm = TRUE)               # [h,k,k]
      L <- min(H, dim(m)[1]); IRF[, , seq_len(L)] <- aperm(m[seq_len(L), , , drop = FALSE], c(2,3,1))
      dimnames(IRF) <- list(resp = vars, imp = vars, h = seq_len(H))
      if (any(IRF != 0)) return(IRF)
    }
  }
  
  # 3) 3D [resp,imp,h] (names optional)
  if (is.array(obj) && length(dim(obj)) == 3) {
    dn <- dimnames(obj); d <- dim(obj)
    if (!is.null(dn)) {
      rnames <- translate(dn[[1]] %||% character()); inames <- translate(dn[[2]] %||% character())
      ok_r <- which(!is.na(rnames)); ok_i <- which(!is.na(inames))
      if (length(ok_r) && length(ok_i)) {
        for (ri in ok_r) for (ii in ok_i) {
          vec <- obj[dn[[1]][ri], dn[[2]][ii], , drop = TRUE]
          if (is.numeric(vec) && length(vec)) {
            L <- min(H, length(vec)); IRF[rnames[ri], inames[ii], seq_len(L)] <- as.numeric(vec[seq_len(L)])
          }
        }
        if (any(IRF != 0)) return(IRF)
      }
    }
    k_dims <- which(d == length(vars))
    if (length(k_dims) >= 2) {
      h_dim <- setdiff(1:3, k_dims)[1]
      x <- aperm(obj, c(k_dims[1], k_dims[2], h_dim)) # [k,k,h]
      L <- min(H, dim(x)[3])
      for (r in seq_along(vars)) for (i in seq_along(vars))
        IRF[vars[r], vars[i], seq_len(L)] <- as.numeric(x[r, i, seq_len(L)])
      if (any(IRF != 0)) return(IRF)
    }
  }
  
  # 4) Per‑impulse lists: names like "irf_low_<impulse>" / "irf_high_<impulse>"
  if (is.list(obj) && length(names(obj))) {
    irf_nodes <- grep("^irf_", names(obj), value = TRUE)
    if (length(irf_nodes)) {
      get_impulse <- function(node_name) {
        m <- regmatches(node_name, regexec("^irf_(?:low|high|combined|[A-Za-z0-9]+)_([A-Za-z0-9_]+)$", node_name))[[1]]
        if (length(m) >= 2) m[2] else sub("^irf_","", node_name)
      }
      for (node in irf_nodes) {
        imp_raw <- get_impulse(node); imp <- translate(imp_raw)
        if (is.na(imp)) next
        x <- obj[[node]]
        if (is.list(x)) {
          for (el_name in names(x)) {
            v <- x[[el_name]]
            # named numeric vector case: "<resp>_<h>"
            if (is.numeric(v) && length(names(v))) {
              nms <- names(v)
              for (j in seq_along(v)) {
                mm <- regmatches(nms[j], regexec("^(.+?)[_\\- ]?(\\d+)$", nms[j]))[[1]]
                if (length(mm) >= 3) {
                  resp <- translate(mm[2]); h <- as.integer(mm[3])
                  if (!is.na(resp) && h >= 1L && h <= H) IRF[resp, imp, h] <- as.numeric(v[j])
                }
              }
            } else {
              # plain numeric vector of length H under the response name
              resp <- translate(el_name)
              if (!is.na(resp) && is.numeric(v)) {
                L <- min(H, length(v)); IRF[resp, imp, seq_len(L)] <- as.numeric(v[seq_len(L)])
              }
            }
          }
        }
      }
      if (any(IRF != 0)) return(IRF)
    }
    # recurse into typical children
    kids <- intersect(names(obj) %||% character(), c("regimes","combined","low","high","summary","draws",
                                                     "median","mean","p50","point","central","irf","IRF","data","df","tbl","envelope","central_tendency"))
    for (kk in kids) {
      got <- build_irf_cube_any(obj[[kk]], vars, H)
      if (any(got != 0)) return(got)
    }
  }
  IRF
}
