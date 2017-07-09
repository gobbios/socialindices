#' find variable name in source data set
#'
#' detect variable/column names in source data set
#' @param type character, one of \code{"date"}, \code{"time"}, \code{"focal"}, \code{"behaviour"}, \code{"actor"}, \code{"receiver"}, \code{"duration"}, \code{"obstime"} (observation time), \code{"partner"}, \code{"obsnumber"}, \code{"observer"} or \code{"IDs"}
#' @param src usually a data.frame
#' @param return.na by default \code{FALSE}, which results in the function stopping (returning an error) if a column is not found. If \code{TRUE} function will return \code{NA} in such a case (without producing an error). See also \code{Details}
#' @param possibilities logical. If \code{TRUE} returns the possible matches for the given \code{type} that are considered by the function
#'
#' @return character of variable name in the source data
#' @details the case of "duration" is special, such a column is not mandatory and therefore might not be found either because it does not exist or because its column name is not among the templates provided in the function. In case no duration column is found, it is returned as \code{NA} and a message is returned to inform the user about the possible ambiguity.
#' If \code{type="IDs"} an attempt is made to figure out whether there is one of the two possibilities of combinations of ID columns, i.e. (1) a focal and a partner column or (2) a focal, actor and receiver column
#' @export
#'
#' @examples
#' x <- datagen()
#' varname("date", x[[1]])
#' varname("focal", x[[1]])
#'
#' \dontrun{
#'   varname("observer", x[[1]]) # error
#'   varname("observer", x[[1]], return.na=TRUE) # works and returns NA
#' }
#'
#' # return possible matches
#' varname("observer", x[[1]], possibilities=TRUE)

varname <- function(type, src, return.na=FALSE, possibilities=FALSE) {
  xdata <- list(date=c("Date", "date", "DATE", "dte", "Dte"),
                focal=c("focal", "Focal", "FOCAL", "foc", "Foc", "ID", "FocalID", "focalID"),
                behaviour=c("behaviour", "Behaviour", "behav", "action", "Action", "behavior", "Behavior", "beh", "Beh", "BEH"),
                actor=c("actor", "Actor", "act", "Act", "giver", "Giver", "giv", "id1", "ID1"),
                receiver=c("receiver", "Receiver", "rec", "Rec", "recipient", "Recipient", "id2", "ID2"),
                partner=c("partner", "Partner", "Part", "part"),
                duration=c("duration", "Duration", "dur", "Dur", "durat", "Durat"),
                obstime=c("OT", "Ot", "ot", "obst", "Obst", "obstime", "Obstime"),
                time=c("time", "Time"),
                obsnumber=c("obsnb", "obsno"),
                observer=c("obs", "Obs", "Observer", "observer"))

  if(possibilities) {
    if(type=="date") return(xdata$date)
    if(type=="behaviour") return(xdata$behaviour)
    if(type=="duration") return(xdata$duration)
    if(type=="obstime") return(xdata$obstime)
    if(type=="time") return(xdata$time)
    if(type=="obsnumber") return(xdata$obsnumber)
    if(type=="observer") return(xdata$observer)
    if(type=="focal") return(xdata$focal)
    if(type=="actor") return(xdata$actor)
    if(type=="receiver") return(xdata$receiver)
    if(type=="partner") return(xdata$partner)

  }



  if(type=="IDs") {
    f <- which(xdata$focal %in% colnames(src))
    if(length(f)==1) { f <- xdata$focal[f]; names(f) <- "focal" } else {
      if(length(f)>1) message("multiple matches for 'focal'")}
    a <- which(xdata$actor %in% colnames(src))
    if(length(a)==1) { a <- xdata$actor[a]; names(a) <- "actor" } else {
      if(length(a)>1) message("multiple matches for 'actor'")}
    r <- which(xdata$receiver %in% colnames(src))
    if(length(r)==1) { r <- xdata$receiver[r]; names(r) <- "receiver" } else {
      if(length(r)>1) message("multiple matches for 'receiver'")}
    p <- which(xdata$partner %in% colnames(src))
    if(length(p)==1) { p <- xdata$partner[p]; names(p) <- "partner" } else {
      if(length(p)>1) message("multiple matches for 'partner'") }

    all <- c(f, a, r, p)
    if(length(f)==1 & length(all)==3) return(all)
    if(length(f)==1 & length(all)==2) return(all)

  }




  # duration is a special case, as it might not be among the column names on purpose...
  # if it is not found, NA is returned...
  if(type == "duration") {
    x <- which(xdata$duration %in% colnames(src))
    if(length(x)==1) { return(xdata$duration[x]) } else { message("duration column not found, NA assumed"); return(NA);  }
  }

  if(type == "date") {
    x <- which(xdata$date %in% colnames(src))
    if(length(x)==1) { return(xdata$date[x]) } else {
      if(return.na) {
        return(NA)
      }else{
        stop("date column not found", call.=FALSE)
      }
    }
  }

  if(type == "observer") {
    x <- which(xdata$observer %in% colnames(src))
    if(length(x)==1) { return(xdata$observer[x]) } else {
      if(return.na) {
        return(NA)
      } else {
        stop("observer column not found", call.=FALSE)
      }
    }
  }

  if(type == "time") {
    x <- which(xdata$time %in% colnames(src))
    if(length(x)==1) { return(xdata$time[x]) } else { stop("time column not found", call.=FALSE) }
  }

  if(type == "obsnumber") {
    x <- which(xdata$obsnumber %in% colnames(src))
    if(length(x)==1) { return(xdata$obsnumber[x]) } else { stop("observation number column not found", call.=FALSE) }
  }

  if(type == "partner") {
    x <- which(xdata$partner %in% colnames(src))
    if(length(x)==1) { return(xdata$partner[x]) } else { stop("partner column not found", call.=FALSE) }
  }

  if(type == "focal") {
    x <- which(xdata$focal %in% colnames(src))
    if(length(x)==1) { return(xdata$focal[x]) } else {
      if(return.na) {
        return(NA)
      } else {
        stop("focal column not found", call.=FALSE)
      }
    }
  }

  if(type == "behaviour") {
    x <- which(xdata$behaviour %in% colnames(src))
    if(length(x)==1) { return(xdata$behaviour[x]) } else { stop("behaviour column not found", call.=FALSE) }
  }

  if(type == "actor") {
    x <- which(xdata$actor %in% colnames(src))
    if(length(x)==1) { return(xdata$actor[x]) } else { stop("actor column not found", call.=FALSE) }
  }

  if(type == "receiver") {
    x <- which(xdata$receiver %in% colnames(src))
    if(length(x)==1) { return(xdata$receiver[x]) } else { stop("receiver column not found", call.=FALSE) }
  }

  if(type == "obstime") {
    x <- which(xdata$obstime %in% colnames(src))
    if(length(x)==1) { return(xdata$obstime[x]) } else { stop("observation time column not found", call.=FALSE) }
  }
}

