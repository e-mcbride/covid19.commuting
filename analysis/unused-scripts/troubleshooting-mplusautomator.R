templatefile <- here("analysis/03_Mplus/trav-beh/time/LPA-time-template.txt")
!file.exists(templatefile)

readfile <- scan(templatefile, what="character", sep="\n", strip.white=FALSE, blank.lines.skip=FALSE, quiet=TRUE)
readfile

startinit <- grep("[[init]]", readfile, fixed=T)
endinit <- grep("[[/init]]", readfile, fixed=T)
startinit
endinit
length(startinit)
length(endinit)

length(startinit) != 1 || length(endinit) != 1

initSection <- readfile[(startinit+1):(endinit-1)]
initSection

bodySection <- readfile[(endinit+1):length(readfile)]

initCollection <- processInit(initSection)

templateTags <- parseTags(bodySection, initCollection)
templateTags <- lookupSimpleTags(templateTags, initCollection)

length(initCollection$iterators) > 0

recurseReplace(templateTags, initCollection)
