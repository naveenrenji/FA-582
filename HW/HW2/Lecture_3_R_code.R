#Lecture 3

library(XML)
ubase = "http://www.cherryblossom.org/"
url = paste(ubase, "results/2012/2012cucb10m-m.htm", sep = "")
doc = htmlParse(url)

ubase = "https://personal.stevens.edu/~dbozdog/"
url = paste(ubase, "results/2012/2012cucb10m-m.htm", sep = "")
doc = htmlParse(url)

#NEW
library(httr)
h <- handle(url)
res <- GET(handle = h)
#doc <- htmlParse(content(res, as = "text"))
#doc <- htmlParse(content(res))
doc = htmlParse(res)

preNode = getNodeSet(doc, "//pre")

txt = xmlValue(preNode[[1]])

nchar(txt)

substr(txt, 1, 200)

substr(txt, nchar(txt) - 200, nchar(txt))

els = strsplit(txt, "\\r\\n")[[1]]

class(els)

length(els)

els[1:30]

els[ length(els) ]

# Retrieve data from web site, find preformatted text,
# return as a character vector.

extractResTable = function(url)
{
  h <- handle(url)
  res <- GET(handle = h)
  #doc <- htmlParse(content(res, as = "text"))
  doc <- htmlParse(content(res))
  #doc = htmlParse(res)
  preNode = getNodeSet(doc, "//pre")
  txt = xmlValue(preNode[[1]])
  els = strsplit(txt, "\r\n")[[1]]   
  return(els)
}

m2012 = extractResTable(url)

identical(m2012, els)

#REMOVED
#ubase = "http://www.cherryblossom.org/"
#urls = paste(ubase, "results/", 1999:2012, "/", 1999:2012, "cucb10m-m.htm", sep = "")
#menTables = lapply(urls, extractResTable)
#options(error = recover)
#menTables = lapply(urls, extractResTable)
#Browse[1]> ls()
#Browse[1]> url
#Browse[1]> length(preNode)

menURLs = 
  c("results/2001/oof_m.html",
    "results/2002/oofm.htm", "results/2003/CB03-M.HTM",
    "results/2004/men.htm", "results/2005/CB05-M.htm", 
    "results/2006/men.htm", "results/2007/men.htm", 
    "results/2008/men.htm", "results/2009/09cucb-M.htm",
    "results/2010/2010cucb10m-m.htm", 
    "results/2011/2011cucb10m-m.htm",
    "results/2012/2012cucb10m-m.htm")

class(menURLs)

urls = paste(ubase, menURLs, sep = "")

urls[1:3]

menTables = lapply(urls[c(1:10,12)], extractResTable)
lapply(urls[11:11], extractResTable)
menTables[1]
names(menTables)

class(menTables)

names(menTables) = c(2001:2010,2012)
menTables[["2001"]]

sapply(menTables, length)

# Retrieve data from web site, 
# find the preformatted text,
# and write lines or return as a character vector.

extractResTable =  function(url = "https://personal.stevens.edu/~dbozdog/results/2001/oof_m.html",
           year = 2001){
    
    if (year == 2009 ) {
      print("2009")
      h <- handle(url)
      res <- GET(handle = h)
      doc <- htmlParse(content(res))
      
      div1 = getNodeSet(doc, "//div[@class='Section1']")
      pres = getNodeSet(div1[[1]], "//pre")
      els = sapply(pres, xmlValue)
      }
    else if (year == 2011){
    print("2011")
      doc = htmlParse(url)
     }  
    else {
      # Get preformatted text from <pre> elements
      h <- handle(url)
      res <- GET(handle = h)
      doc <- htmlParse(content(res))
      
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\r\n")[[1]]   
       } 
    
    return(els)
  }


years = 2001:2012
menTables={}
menTables = mapply(extractResTable, url = urls[c(-9,-11)], year = years[c(-9,-11)])
names(menTables) = years[c(-9,-11)]
sapply(menTables, length)

#save(menTables, file = "CBMenTextTables.rda")

load("CBMenTextTables.rda")

length(menTables)
sapply(menTables, length)

#2001
menTables[[1]]
menTables[[1]][[4]]
menTables[[1]][1:4]

els2011 = menTables[[11]]
els2011[1:10]

els2012 = menTables[[12]]
els2012[1:10]

eqIndex = grep("^===", els2012)
eqIndex

first3 = substr(els2012, 1, 3)
first3[1:4]
which(first3 == "===")

spacerRow = els2012[eqIndex]
headerRow = els2012[eqIndex - 1]
body = els2012[ -(1:eqIndex) ]

headerRow = tolower(headerRow)
headerRow

ageStart = regexpr("ag", headerRow)
ageStart

age = substr(body, start = ageStart, stop = ageStart + 1)
head(age)

summary(as.numeric(age))

blankLocs = gregexpr(" ", spacerRow)
blankLocs

blankLocs

#searchLocs = c(0, blankLocs)

searchLocs = c(0, blankLocs[[1]])

Values = mapply(substr, list(body), 
                start = searchLocs[ -length(searchLocs)] + 1, 
                stop = searchLocs[ -1 ] - 1)

dim(Values)
Values[1:3,]

findColLocs = function(spacerRow) {
  
  spaceLocs = gregexpr(" ", spacerRow)[[1]]
  rowLength = nchar(spacerRow)
  
  if (substring(spacerRow, rowLength, rowLength) != " ")
    return( c(0, spaceLocs, rowLength + 1))
  else return(c(0, spaceLocs))
}

selectCols = function(colNames, headerRow, searchLocs) 
  {
    sapply(colNames, 
           function(name, headerRow, searchLocs)
           {
             startPos = regexpr(name, headerRow)[[1]]
             if (startPos == -1) 
               return( c(NA, NA) )
             
             index = sum(startPos >= searchLocs)
             c(searchLocs[index] + 1, searchLocs[index + 1] - 1)
           },
           headerRow = headerRow, searchLocs = searchLocs )
  }

searchLocs = findColLocs(spacerRow)
searchLocs
ageLoc = selectCols("ag", headerRow, searchLocs) 
ageLocages = mapply(substr, list(body), start = ageLoc[1,], stop = ageLoc[2, ])


summary(as.numeric(ageLocages))

shortColNames = c("name", "home", "ag", "gun", "net", "time")

locCols = selectCols(shortColNames, headerRow, searchLocs)
locCols

Values = mapply(substr, list(body), start = locCols[1, ], stop = locCols[2, ])

dim(Values)
Values[1:4,]

class(Values)

colnames(Values) = shortColNames
head(Values)

tail(Values)[ , 1:3]

extractVariables = function(file, varNames =c("name", "home", "ag", "gun",
                             "net", "time"))
  {
    # Find the index of the row with =s
    eqIndex = grep("^===", file)
    # Extract the two key rows and the data
    spacerRow = file[eqIndex] 
    headerRow = tolower(file[ eqIndex - 1 ])
    body = file[ -(1 : eqIndex) ]
    
    # Obtain the starting and ending positions of variables
    searchLocs = findColLocs(spacerRow)
    locCols = selectCols(varNames, headerRow, searchLocs)
    
    Values = mapply(substr, list(body), start = locCols[1, ], 
                    stop = locCols[2, ])
    colnames(Values) = varNames
    
    invisible(Values)
  }


data=sapply(menTables, length)
data

#
#menTables[[1]][1:4]

#els2011 = menTables[[13]]
#els2011[1:10]

#els2012 = menTables[[14]]
#els2012[1:10]


menResMat = lapply(menTables, extractVariables)
length(menResMat)

sapply(menResMat, nrow)



age = as.numeric(menResMat[['2012']][ , 'ag'])
age

tail(age)

age = sapply(menResMat,function(x) as.numeric(x[ , 'ag']))
boxplot(age, ylab = "Age", xlab = "Year")

#for 2003 run
head(menTables[[1]])
head(menTables[[2]])
head(menTables[[3]])

#head(menFiles[['2003']])

#for 2006 selestion
menTables[[6]][2200:2205]

#menFiles[['2006']][2200:2205]#

# Need to modify selectCols() by changing the index for end of each variable when we perform the extraction


selectCols = function(shortColNames, headerRow, searchLocs) {
  sapply(shortColNames, function(shortName, headerRow, searchLocs){
    startPos = regexpr(shortName, headerRow)[[1]]
    if (startPos == -1) return( c(NA, NA) )
    index = sum(startPos >= searchLocs)
    c(searchLocs[index] + 1, searchLocs[index + 1])
  }, headerRow = headerRow, searchLocs = searchLocs )
}

menResMat = lapply(menTables, extractVariables)

age = sapply(menResMat, function(x) as.numeric(x[ , 'ag']))

boxplot(age, ylab = "Age", xlab = "Year")

#problem with 2009
sapply(menResMat, nrow)
menTables[[9]][1:10]
gsub("[^\\x{00}-\\x{7f}]","",menTables[[9]][1:10],perl=TRUE)


sapply(age,  function(x) sum(is.na(x)))

age2001 = age[["2001"]]

grep("^===", menTables[[1]])

menTables[[1]][1:10]
menTables[['2001']][1:10]

badAgeIndex = which(is.na(age2001)) + 5
menTables[['2001']][ badAgeIndex ]

badAgeIndex

# Blank lines are scattered throughout the file. We can modify the extraction by checking for 
# blank rows and removing them
blanks = grep("^[[:blank:]]*$", menTables[['2001']])
blanks

extractVariables = function(file, varNames =c("name", "home", "ag", "gun",
                             "net", "time"))
  {
    
    # Find the index of the row with =s
    eqIndex = grep("^===", file)
    # Extract the two key rows and the data 
    spacerRow = file[eqIndex] 
    headerRow = tolower(file[ eqIndex - 1 ])
    body = file[ -(1 : eqIndex) ]
    # Remove footnotes and blank rows
    footnotes = grep("^[[:blank:]]*(\\*|\\#)", body)
    if ( length(footnotes) > 0 ) body = body[ -footnotes ]
    blanks = grep("^[[:blank:]]*$", body)
    if (length(blanks) > 0 ) body = body[ -blanks ]
    
    
    # Obtain the starting and ending positions of variables   
    searchLocs = findColLocs(spacerRow)
    locCols = selectCols(varNames, headerRow, searchLocs)
    
    Values = mapply(substr, list(body), start = locCols[1, ], 
                    stop = locCols[2, ])
    colnames(Values) = varNames
    
    return(Values)
  }

menResMat = lapply(menTables, extractVariables)


which(age2001 < 5)

menTables[['2001']][ which(age2001 < 5) + 5 ]

charTime = menResMat[['2012']][, 'time']
head(charTime, 5)

tail(charTime, 5)

timePieces = strsplit(charTime, ":")

timePieces[[1]]

tail(timePieces, 1)

timePieces = sapply(timePieces, as.numeric)

runTime = sapply(timePieces, 
                 function(x) {
                   if (length(x) == 2) x[1] + x[2]/60
                   else 60*x[1] + x[2] + x[3]/60
                 })

summary(runTime)

convertTime = function(time) {
  timePieces = strsplit(time, ":")
  timePieces = sapply(timePieces, as.numeric)
  sapply(timePieces, function(x) {
    if (length(x) == 2) x[1] + x[2]/60
    else 60*x[1] + x[2] + x[3]/60
  })
}

createDF = function(Res, year, sex) {
    # Determine which time to use
    useTime = if( !is.na(Res[1, 'net']) )  
      Res[ , 'net']
    else if( !is.na(Res[1, 'gun']) ) 
      Res[ , 'gun']
    else 
      Res[ , 'time']
    
    runTime = convertTime(useTime)
    
    Results = data.frame(year = rep(year, nrow(Res)),
                         sex = rep(sex, nrow(Res)),
                         name = Res[ , 'name'],
                         home = Res[ , 'home'],
                         age = as.numeric(Res[, 'ag']), 
                         runTime = runTime,
                         stringsAsFactors = FALSE)
    invisible(Results)
  }

menDF = mapply(createDF, menResMat, year = 2001:2012,sex = rep("M", 14), SIMPLIFY = FALSE)

warnings()[ c(1:2, 49:50) ]

sapply(menDF, function(x) sum(is.na(x$runTime)))

## 

createDF = function(Res, year, sex) 
{
  # Determine which time to use
  if ( !is.na(Res[1, 'net']) ) useTime = Res[ , 'net']
  else if ( !is.na(Res[1, 'gun']) ) useTime = Res[ , 'gun']
  else useTime = Res[ , 'time']
  
  # Remove # and * and blanks from time
  useTime = gsub("[#\\*[:blank:]]", "", useTime)
  runTime = convertTime(useTime[ useTime != "" ])
  
  # Drop rows with no time
  Res = Res[ useTime != "", ]
  
  Results = data.frame(year = rep(year, nrow(Res)),
                       sex = rep(sex, nrow(Res)),
                       name = Res[ , 'name'], home = Res[ , 'home'],
                       age = as.numeric(Res[, 'ag']), 
                       runTime = runTime,
                       stringsAsFactors = FALSE)
  invisible(Results)
}

menDF = mapply(createDF, menResMat, year = 2001:2012,
               sex = rep("M", 14), SIMPLIFY = FALSE)

sapply(menDF, function(x) sum(is.na(x$runTime)))

separatorIdx = grep("^===", menTables[["2006"]])
separatorRow = menTables[['2006']][separatorIdx]
separatorRowX = paste(substring(separatorRow, 1, 63), " ", 
                      substring(separatorRow, 65, nchar(separatorRow)), 
                      sep = "")
menTables[['2006']][separatorIdx] = separatorRowX

menResMat = sapply(menTables, extractVariables)
menDF = mapply(createDF, menResMat, year = 2001:2012,
               sex = rep("M", 14), SIMPLIFY = FALSE)

sapply(menDF, function(x) sum(is.na(x$runTime)))

boxplot(sapply(menDF, function(x) x$runTime), 
        xlab = "Year", ylab = "Run Time (min)")


cbMen = do.call(rbind, menDF)
#save(cbMen, file = "cbMen.rda")
load("cbMen.rda")

dim(cbMen)

#load("cbMen.rda")


plot(runTime ~ age, data = cbMen, ylim = c(40, 180), xlab = "Age (years)", ylab = "Run Time (minutes)")


library(RColorBrewer)
ls("package:RColorBrewer")

display.brewer.all()

Purples8 = brewer.pal(9, "Purples")[8]
Purples8

Purples8A = paste(Purples8, "14", sep = "")


plot(runTime ~ jitter(age, amount = 0.5), 
     data = cbMen, 
     pch = 19,cex = 0.2, col = Purples8A,
     ylim = c(45, 165), xlim = c(15, 85),
     xlab = "Age (years)", ylab = "Run Time (minutes)")

smoothScatter(y = cbMen$runTime, x = cbMen$age,
              ylim = c(40, 165), xlim = c(15, 85),
              xlab = "Age (years)", ylab = "Run Time (minutes)")



cbMenSub = cbMen[cbMen$runTime > 30 &
                   !is.na(cbMen$age) & cbMen$age > 15, ]

ageCat = cut(cbMenSub$age, breaks = c(seq(15, 75, 10), 90))
table(ageCat)

plot(cbMenSub$runTime ~ ageCat, 
     xlab = "Age (years)", ylab = "Run Time (minutes)")

lmAge = lm(runTime ~ age, data = cbMenSub)

lmAge$coefficients

summary(lmAge)

class(lmAge)

smoothScatter(x = cbMenSub$age, y = lmAge$residuals,
              xlab = "Age (years)", ylab = "Residuals")
abline(h = 0, col = "purple", lwd = 3)

resid.lo = loess(resids ~ age, data = data.frame(resids = residuals(lmAge),age = cbMenSub$age))

age20to80 = 20:80
age20to80

resid.lo.pr = 
  predict(resid.lo, newdata = data.frame(age = age20to80))

lines(x = age20to80, y = resid.lo.pr, col = "green", lwd = 2)

menRes.lo = loess(runTime ~ age, cbMenSub)

menRes.lo.pr = predict(menRes.lo, data.frame(age = age20to80))

over50 = pmax(0, cbMenSub$age - 50)

lmOver50 = lm(runTime ~ age + over50, data = cbMenSub)

summary(lmOver50)

decades = seq(30, 60, by = 10)
overAge = lapply(decades, 
                 function(x) pmax(0, (cbMenSub$age - x)))
names(overAge) = paste("over", decades, sep = "")
overAge = as.data.frame(overAge)
tail(overAge)

lmPiecewise = lm(runTime ~ . , 
                 data = cbind(cbMenSub[, c("runTime", "age")], 
                              overAge))

summary(lmPiecewise)

overAge20 = lapply(decades, function(x) pmax(0, (age20to80 - x)))
names(overAge20) = paste("over", decades, sep = "")
overAgeDF = cbind(age = data.frame(age = age20to80), overAge20)

tail(overAgeDF)

predPiecewise = predict(lmPiecewise, overAgeDF)

plot(predPiecewise ~ age20to80,
     type = "l", col = "purple", lwd = 3,
     xlab = "Age (years)", ylab = "Run Time Prediction")

lines(x = age20to80, y = menRes.lo.pr, 
      col = "green", lty = 2, lwd = 3)
legend("topleft", col = c("purple", "green"),
       lty = c(1, 2), lwd= 3,
       legend = c("Piecewise Linear", "Loess Curve"), bty = "n")


plot(predPiecewise ~ age20to80,
     type = "l", col = "#984ea3", lwd = 3,
     #   type = "l", col = "purple", lwd = 2,
     xlab = "Age (years)", ylab = "Run Time Prediction")

lines(x = age20to80, y = menRes.lo.pr, col = "#4daf4a", lwd = 3, lty = 2)
legend("topleft", col = c("#984ea3", "#4daf4a"), lty = c(1, 2), lwd = 3,
       legend = c("Piecewise Linear", "Loess Curve"), bty = "n")


numRunners = with(cbMen, tapply(runTime, year, length))
plot(numRunners ~ names(numRunners), type="l", lwd = 2,
     xlab = "Years", ylab = "Number of Runners")

summary(cbMenSub$runTime[cbMenSub$year == 2001])

summary(cbMenSub$runTime[cbMenSub$year == 2012])

age2001 = cbMenSub[ cbMenSub$year == 2001, "age" ]
age2012 = cbMenSub[ cbMenSub$year == 2012, "age" ]

plot(density(age2001, na.rm = TRUE), 
     ylim = c(0, 0.05), col = "purple",
     lwd = 3,  xlab = "Age (years)",  main = "")
lines(density(age2012, na.rm = TRUE), 
      lwd = 3, lty = 2, col="green")
legend("topleft", col = c("purple", "green"), lty= 1:2, lwd = 3,
       legend = c("2001", "2012"), bty = "n")

qqplot(age2001, age2012, pch = 19, cex = 0.5, 
       ylim = c(10,90), xlim = c(10,90), 
       xlab = "Age in 2001 Race",
       ylab = "Age in 2012 Race", 
       main = "Quantile-quantile plot of male runner's age")
abline(a =0, b = 1, col="red", lwd = 2)

mR.lo01 = loess(runTime ~ age, cbMenSub[ cbMenSub$year == 2001,])
mR.lo.pr01 = predict(mR.lo01, data.frame(age = age20to80))

mR.lo12 = loess(runTime ~ age, cbMenSub[ cbMenSub$year == 2012,])
mR.lo.pr12 = predict(mR.lo12, data.frame(age = age20to80))

plot(mR.lo.pr01 ~ age20to80,
     type = "l", col = "purple", lwd = 3,
     xlab = "Age (years)", ylab = "Fitted Run Time (minutes)")

lines(x = age20to80, y = mR.lo.pr12,
      col = "green", lty = 2, lwd = 3)

legend("topleft", col = c("purple", "green"), lty = 1:2, lwd = 3,
       legend = c("2001", "2012"), bty = "n")


plot(mR.lo.pr01 ~ age20to80,
     type = "l", col = "#984ea3", lwd = 3,
     xlab = "Age (years)", ylab = "Prediction (minutes)")  
lines(x = age20to80, y = mR.lo.pr12, col="#4daf4a", lty = 2, lwd = 3) 
legend("topleft", col = c("#984ea3", "#4daf4a"), lty = 1:2, lwd = 3,
       legend = c("2001", "2012"), bty = "n")
gap12 = mR.lo.pr12 - mR.lo.pr01

plot(gap12 ~ age20to80, type = "l" , xlab = "Age (years)", 
     ylab = "Difference in Fitted Curves (minutes)", lwd = 2)

