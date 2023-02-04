#' generate random social interaction data
#'
#' @param no number of interactions to be created
#' @param ni number of individuals
#' @param beh character, \code{beh} determines behaviours for the "positive" interactions of \code{datagen}
#' @param behw numeric, weights of occurences of behaviours in \code{datagen}
#' @param behdur logical, whether behaviours have durations
#' @param biaswithin bias parameter WTIHIN dyads: if 1: roughly 66\% of directionality (biased towards the ID that comes first in alphabet); if 0: no bias, i.e. 50\% bias
#' @param friendprop proportion of dyads with preferential (more frequent/longer) interactions
#' @param nadd numeric, additional number of non-focal IDs
#' @param presence numeric of length = 2, (1) proportion of IDs affected by absence, (2) proportion of days (relative to date range) for absence
#'
#' @return list with six items: (1) data.frame with interaction data, (2) data.frame with observation time data, (3) data.frame with presence data, (4) character with "friend" dyads, (5) character with focal individuals, (6) non-focal individuals
#' @details The process of inventing interactions within these functions starts with creating a data collection protocol. This protocol is modelled on focal animal sampling, in which the \code{ni} individuals serve as focal animals, and it ends up with a list of "protocols" for a given focal ID on several dates associated with pseudo-randomized observation time (i.e. protocol duration). See examples...
#'
#' The argument \code{friendprop} introduces higher rates or longer durations of interaction in this proportion of dyads as compared to other dyads. So far, this applies only to focal-focal dyads and won't introduce focal-nonfocal "friends" even if \code{nadd} is not zero.
#' @export
#' @importFrom stats rexp rnorm rnorm runif
#' @importFrom utils combn
#'
#' @examples
#' x <- datagen(no=20, ni=3)
#' # observation time table
#' x$ot
#' # interactions
#' x$dataseq


datagen <- function(no=50, ni=5, beh=c("appr", "gro", "supp", "prox"), behw=c(0.2, 0.3, 0.1, 0.4), behdur = c(F, T, F, T), biaswithin=1, friendprop=0.1, nadd=0, presence=NULL) {
  # create ID vector
  if(ni+nadd <= 26) ids <- sort(sample(letters, ni+nadd))
  if(ni+nadd > 26 & ni+nadd <= 324) {
    pos <- apply(combn(letters, 2), 2, function(z)paste(c(z[1], z[2]), collapse=""))
    # just make sure that "ot" or "in" dont show up in the ids ("id" doesnt show up because the pairs of letters are sorted anyway)
    pos <- pos[-c(177, 264)]
    ids <- sort(sample(pos, ni+nadd))
  }
  # and focals & non-focals
  focals <- sort(sample(ids, ni))
  nonfocals <- ids[!ids %in% focals]

  # create presence matrix
  # if not otherwise specified it is assumed that all IDs were present at all times
  if(is.null(presence)) pmat <- createnullpresence(IDs=ids, from="2000-01-01", to=as.Date("2000-01-01")+no-1)
  # and modify it if desired
  if(!is.null(presence)) {
    if(length(presence)==2 & max(presence)<=1 & min (presence)>0) {
      pmat <- createnullpresence(IDs=ids, from="2000-01-01", to=as.Date("2000-01-01")+no-1, propID=presence[1], propabs=presence[2])
    }else{
      pmat <- createnullpresence(IDs=ids, from="2000-01-01", to=as.Date("2000-01-01")+no-1)
      warning("input for presence doesnt make sense, therefore full presence matrix used...", call.=FALSE)
    }
  }

  dts <- sort(sample(1:no,no*0.6))
  ppmat <- pmat[dts, ]
  # possible follows...
  posfol <- apply(ppmat[, focals], 1, function(z) colnames(ppmat[,focals])[z==1])
  if (inherits(posfol, "matrix")) posfol <- lapply(apply(posfol, 2, list), unlist)
  # limit to three follows per day
  posfoln <- unlist(lapply(posfol, length)); posfoln[posfoln>3] <- 3

  # create OT table for focals
  # pool of observation times to sample from (based on 60min protocols with some jitter below that number)
  ots <- sort(round(rnorm(500, 60, 10))); ots <- ots[ots<=60]; ots <- c(ots, rep(60, length(ots)))
  ot <- matrix(ncol=3, nrow=0, ""); ct <- 1
  i=names(posfol)[1]
  for(i in names(posfol)) {
    p <- sample(posfol[[as.character(i)]], posfoln[ct])
    for(k in p) if(k %in% focals) ot <- rbind(ot, c(k, sample(ots, 1), i))
    ct <- ct + 1
  }
  ot <- data.frame(focal=ot[,1], date=pmat$date[as.numeric(ot[,3])], OT=as.numeric(ot[,2]))

  # create list with focals' preferred partners (here would also be the place to introduce directionality...)
  # number of dyads with focals as one ID
  pt <- matrix(nrow=length(focals), ncol=length(ids)-1, ""); rownames(pt) <- focals
  ptw <- matrix(nrow=length(focals), ncol=length(ids)-1, 1); rownames(ptw) <- focals
  for(f in focals) { pt[f, ] <- ids[ids!=f] }
  #  if(friendprop>0) {
  ndyads <- length(pt)/2;
  fdyads <- round(ndyads * friendprop)
  if(fdyads>=1) {
    p <- sample(1:ndyads, fdyads); cords <- expand.grid( 1:nrow(pt), 1:ncol(pt))
    m=p[1]
    prefdyads <- c()
    for(m in p) {
      ptw[m] <- 3
      fs <- rownames(pt)[cords[m, 1]]
      ps <- pt[m]
      if(ps %in% focals) ptw[ps, pt[ps, ]==fs] <- 3 # this is the directionality step...
      prefdyads <- c(prefdyads, paste(sort(c(fs, ps)), collapse="@"))
    }
  } else {
    prefdyads <- NA
  }
  #  }


  #   # add non-focals if existing
  #   if(length(nonfocals)>0) {
  #     for(i in 1:length(nonfocals)) {
  #       pt <- cbind(pt, nonfocals[i])
  #       ptw <- cbind(ptw, 1)
  #     }
  #   }
  #
  # generate the interactions...
  res <- data.frame(date=rep(pmat[1,1], no), focal=factor(no, focals), actor=factor(no, ids), receiver=factor(no, ids), beh=factor(no, beh), dur=numeric(no), pref=FALSE, ct=NA)
  ct=0

  while(sum(!is.na(res$ct)) < no) {
    i <- min(which(is.na(res$ct)))
    # select a random protocol
    prot <- ot[sample(1:nrow(ot), 1), ]; foc <- as.character(prot[1,1]); res$date[i] <- prot$date

    # select a partner for the focal (with preference for preferred partners)
    # accounting for presence and also for the possibility that non-focal ids are considered...
    p <- pmat[pmat$date==prot$date, ids[-c(which(ids==as.character(prot$focal)))]]
    (p <- names(p)[p==1])
    if(length(p)>=1) {
      part <- sample(pt[foc, pt[foc,] %in% p], 1, prob=ptw[foc, pt[foc,] %in% p])

      # introduce the bias concerning actor/receiver
      d <- sort(c(as.character(foc), part)); if(rnorm(1) > biaswithin) {d <- rev(d)}
      # choose behaviour
      b <- sample(1:length(beh), 1, prob=behw)
      # and assign duration
      dur <- ceiling(rexp(1, 0.02))
      # for preferred dyads, increase the duration
      if(ptw[as.character(foc), pt[as.character(foc),]==part] > 1) { dur <- round(dur * runif(1, 2.5, 4.5)); pref <- T }else{pref<-F}
      # depending on behaviour make duration NA
      if(!behdur[b]) dur <- NA


      res$focal[i] <- foc
      res$actor[i] <- d[1]
      res$receiver[i] <- d[2]
      res$beh[i] <- beh[b]
      res$dur[i] <- dur
      res$pref[i] <- pref
      res$ct[i] <- 9999
      ct <- ct + 1
    }


  }

  # just checking whether at least one of the interactors was a focal (should be TRUE)
  # sum(apply(res, 1, function(z) z[3] %in% focals | z[4] %in% focals))==no
  # check whether sometimes both ids were among focal ids (should be 1s AND 2s)
  # apply(res, 1, function(z) sum(c(z[3] %in% focals, z[4] %in% focals)))

  # sort interactions by date...
  res <- res[order(res$date), -c(which(colnames(res)=="ct"))]; rownames(res) <- NULL


  # and finally split the presence for focals and nonfocals
  # NO, don't! single presence enough
  #if(nadd>0) { presence2 <- pmat[, c("date", nonfocals)];presence <- pmat[, c("date", focals)]
  #} else { presence <- pmat; presence2 <- NULL }

  return(list(dataseq=res, ot=ot, presence=pmat, prefdyads=prefdyads, focals=focals, nonfocs=nonfocals))

  #return(list(dataseq=res, ot=ot, presence=presence, presence2=presence2, prefdyads=prefdyads, focals=focals, nonfocs=nonfocals))
}
