#' Sends the prepared ascii to the server
#'
#' @param message The message to send
#' @param host The hostname of the server computer
#' @param port The tcp port to connect to
sendMessage <- function(message, host = "127.0.0.1", port = 8080) {

  # Open connection to server
  con = socketConnection(host = host, port = port, server = FALSE, blocking = TRUE)

  # Put stuff to the server
  #writeChar(sentmsg, con, eos = "\r\n");
  writeLines(message, con = con)

  # Receive the ack-message from server
  ackMsg = readLines(con);
  #cat(ackMsg);

  # Close the connection
  close(con);

  return(ackMsg)
}

#' Prepares an int ascii string for the network message
intMessagePart <- function(ints) {
  asStr = sprintf("%10d", ints)

  return(asStr)
}

#' Prepares a float ascii string for the network message
floatMessagePart <- function(floats) {
  # first determine negative and positive components
  negIndex = floats < 0
  posIndex = !negIndex

  # Add a space to the front of the positive items to
  # get the same message length for both
  # all floats are 11 characters in length this way
  negs = sprintf("%.04E", floats[negIndex])
  pos = sprintf(" %.04E", floats[posIndex])

  stringVector = rep("", length(floats))
  stringVector[negIndex] = negs
  stringVector[posIndex] = pos

  return(stringVector)
}

#' Prepares string to be sent over the network
#' Puts string length to the forefront of the message part
stringMessagePart <- function(msgString)
{
  stringLength = nchar(msgString)
  lenOfStr = intMessagePart(stringLength);
  message = ccatmessageparts(c(lenOfStr, msgString))

  return(message)
}

#' Prepares a named string message for the network message
stringMessage <- function(name, msgString)
{
  namePart <- stringMessagePart(name)
  stringPart <- stringMessagePart(msgString)
  message = ccatmessageparts(namePart, stringPart);
  return(message)
}

#' Prepare a named vector of ints to be sent over the network
#' Puts length of the int message to the forefront of the message part
intVectorMessage <- function(name, intVector) {

  vecNamePart <- stringMessagePart(name)

  # Length part
  lengthPart = intMessagePart(length(intVector))

  # float vector part
  intVectorPart = intMessagePart(intVector)

  # combine vector length part and float vector part
  stringVector<-c(vecNamePart, lengthPart, intVectorPart)
  message<-ccatmessageparts(stringVector)
  #cat(message)

  return(message)
}

#' Prepare a named vector of floats to be sent over the network
#' Puts length of the int message to the forefront of the message part
floatVectorMessage <- function(name, floatVector) {

  # Vector name
  vecNamePart <- stringMessagePart(name)

  # Length part
  lengthPart <- intMessagePart(length(floatVector))

  # float vector part
  floatVectorPart <- floatMessagePart(floatVector)

  # combine vector length part and float vector part
  stringVector <- c(vecNamePart, lengthPart, floatVectorPart)
  message<-ccatmessageparts(stringVector)
  #cat(message)

  return(message)
}

#' concatenates message parts for the ascii message
ccatmessageparts <- function(...) {
  return(paste(c(...), collapse = ""))
}

#' Prepares the network header of a command
#'
#' @param plotTarget Plot name to target the command to
#' @param command command type
#' @param floatVectorCount number of float vectors in the message
#' @param intVectorCount number of float vectors in the message
#' @param stringCount The number of strings in the message
prepareHeader <- function(plotTarget, command, floatVectorCount, intVectorCount, stringCount ) {
  ptTargetInMsg <- stringMessagePart(plotTarget)
  commandInMsg <- intMessagePart(command)
  fltVCountInMsg <- intMessagePart(floatVectorCount)
  intVCountInMsg <- intMessagePart(intVectorCount)
  strCountInMsg <- intMessagePart(stringCount)
  message<-ccatmessageparts(ptTargetInMsg, commandInMsg, fltVCountInMsg, intVCountInMsg, strCountInMsg)
  return(message)
}

#' Calculates the explation percentages for each principal component in pr.out
#'
#' @param host pr.out
selast <- function(pr.out) {
  pr.var = pr.out$sdev^2
  pr.var
  pve = pr.var / sum(pr.var)
  pves = sprintf("PK%i %.02f ", 1:3, pve[1:3])

  return(pves)
}

#' Plot a set of data points using color indices of a predefined palette
#'
#' Prepares an axis title network message sends it to the vr plotter server
#'
#' @param plotTarget Plot name to target the command to
#' @param colorIndices Array of color indices the plotting server uses
#' @param x A float vector of data point x coordinates
#' @param Y A float vector of data point x coordinates
#' @param Z A float vector of data point x coordinates
#' @param type Data point geometry "sphere" or "cube"
#' @param size Data point object scale factor
#' @param host The hostname of the plotting server computer
#' @export
vrplot <- function(plotTarget, x, y, z, colorIndices, type = "sphere", size = 1.0, host = "localhost") {

  # Header structure:
  # plot target
  # cmd type
  # float vector counts
  # int vector counts
  # string counts

  hdr <- prepareHeader(plotTarget = plotTarget, command = 1, floatVectorCount = 4, intVectorCount = 1, stringCount = 1)

  # x-coordinate float vector
  xVecPart = floatVectorMessage("x", x)

  # x-coordinate float vector
  yVecPart = floatVectorMessage("y", y)

  # x-coordinate float vector
  zVecPart = floatVectorMessage("z", z)

  # plot size
  plotSizePart = floatVectorMessage("size", size)

  # color index vector (type int)
  colorIndicesPart = intVectorMessage("colors", colorIndices)

  # plot item type
  plotTypePart = stringMessage("type", type)

  ackMessage = sendMessage(ccatmessageparts(hdr, xVecPart, yVecPart, zVecPart, plotSizePart,  colorIndicesPart, plotTypePart), host = host)
  cat(ackMessage)
}

#' Set labels of a vr plot
#'
#' Prepares a main title network message and sends it to the vr plotter server
#'
#' @param plotTarget Plotname to target the command to
#' @export
vrtitle <- function(plotTarget, title, host = "localhost") {

  # Header structure:
  # plot target
  # cmd type
  # float vector counts
  # int vector counts
  # string counts

  hdr <- prepareHeader(plotTarget = plotTarget, command = 2, floatVectorCount = 0, intVectorCount = 0, stringCount = 1)

  # main title string
  mainTitleMessage = stringMessage("mainTitle", title)

  ackMessage = sendMessage(ccatmessageparts(hdr, mainTitleMessage), host = host)
  cat(ackMessage)
}

#' Set labels of a vr plot
#'
#' Prepares an axis title network message and sends it to the vr plotter server
#'
#' @param plotTarget Plotname to target the command to
#' @param xTitle X axis title
#' @param yTitle Y axis title
#' @param zTitle Z axis title
#' @param host The hostname of the plotting server computer
#' @export
vrlabels <- function(plotTarget, xTitle, yTitle, zTitle, host = "localhost") {

  # Header structure:
  # plot target
  # cmd type
  # float vector counts
  # int vector counts
  # string counts

  hdr <- prepareHeader(plotTarget = plotTarget, command = 3, floatVectorCount = 0, intVectorCount = 0, stringCount = 3)

  # x-axis title string
  xAxisTitlePart = stringMessage("xAxisTitle", xTitle)

  # y-axis title string
  yAxisTitlePart = stringMessage("yAxisTitle", yTitle)

  # z-axis title string
  zAxisTitlePart = stringMessage("zAxisTitle", zTitle)

  ackMessage = sendMessage(ccatmessageparts(hdr, xAxisTitlePart, yAxisTitlePart, zAxisTitlePart), host = host)
  cat(ackMessage)
}

#' Sends pca results from princomp command to vr plotting server
#'
#' @param plotTarget Plotname to target the command to
#' @param host The hostname of the plotting server computer
#' @param title The main title of the plot
#' @param pr.out Result object from princomp
#' @param color A vector of color indices to use for plotting
#' @param type Plot object type spheres or cubes
#' @export
vrshowpca <- function(plotTarget, title, pr.out, colors, type="spheres" ) {
  pves = selast(pr.out)

  if(length(colors) == 1) {
    colors = rep(0, nrow(pr.out$x))
    cat("Vareja vain 1")
  }
  else {
    cat("On vareja")
  }
  vrtitle(plotName, title)
  Sys.sleep(1)

  vrlabels(plotName, pves[1], pves[2], pves[3])
  Sys.sleep(1)

  vrplot(plotTarget, pr.out$x[,1], pr.out$x[,2], pr.out$x[,3], colorIndices = colors, type = type)
}

#' Clears a specified vr plot from the server
#'
#' Prepares a clear network message to be sent to the vr plotter server
#' and sends it.
#' @param plotTarget Plotname to target the command to
#' @param host The hostname of the plotting server computer
#' @export
vrclear <- function(plotTarget, host = "localhost") {

  # Header structure:
  # plot target
  # cmd type
  # float vector counts
  # int vector counts
  # string counts

  hdr <- prepareHeader(plotTarget = plotTarget, command = 4, floatVectorCount = 0, intVectorCount = 0, stringCount = 0)

  ackMessage = sendMessage(hdr, host = host)
  cat(ackMessage)
}

#' @export
vrdbg <- function(plotTarget, host = "localhost") {
  hdr <- prepareHeader(plotTarget = plotTarget, command = 5, floatVectorCount = 0, intVectorCount = 0, stringCount = 0)

  ackMessage = sendMessage(hdr, host = host)
  cat(ackMessage)
}



