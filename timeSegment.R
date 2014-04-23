# timeSegment
#
#   This function returns to categorized time from 24h.
#   arg: time: chr or factor
#
timeSegment <- function(time) {
  time_chr <- as.character(time)
  if("00:00" <= time_chr && time_chr <  "06:00") { return("midnight") }
  if("06:00" <= time_chr && time_chr <  "12:00") { return("morning") }
  if("12:00" <= time_chr && time_chr <  "18:00") { return("daytime") }
  return("evening")
}