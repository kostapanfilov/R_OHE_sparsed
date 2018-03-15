get.sparse.scheme <- function(X, formula){
  nms <- intersect(names(X), .get.cat.cols())
  X[nms] <- lapply(X = X[nms], FUN = as.factor)

  smm <- sparse.model.matrix(object = as.formula(formula),
                             data = X,
                             contrasts.arg = sapply(X = X[nms], FUN = contrasts, contrasts = F),drop.unused.levels = T)
  out <- colnames(smm)
  out[out != "(Intercept)"]
}

get.sparse.matrix <- function(sparse.scheme, data, add.intercept = T){
  N <- nrow(data)
  data.scheme <- unique(sapply(X = sparse.scheme, FUN = function(x){
    gsub(pattern = "-?[[:digit:]]", replacement = "", x = x)
  }, USE.NAMES = F, simplify = T))

  ss <- sapply(X = seq(N), FUN = function(i.index){
    row.value <- sapply(X = data.scheme, FUN = function(colname){
      paste(sapply(X = strsplit(colname,":")[[1]],FUN = function(str){
        paste0(str, data[[str]][i.index])
      }),collapse = ":")
    },USE.NAMES = F, simplify = T)

    tmp.j <- match(x = row.value, table = sparse.scheme)
    tmp.j <- tmp.j[!is.na(tmp.j)]
    cbind(rep(i.index, length(tmp.j)),tmp.j)
  },simplify = F, USE.NAMES = F)
  ss <- do.call(rbind, ss)
  ssm0 <- cbind(seq(N), rep(length(sparse.scheme) + 1, N))

  sparseMatrix(j = c(ss[,2],ssm0[,2]), i = c(ss[,1],ssm0[,1]),  x = 1, dims = c(N,length(sparse.scheme) + 1),dimnames = list(NULL, c(sparse.scheme,"M0")))
}
