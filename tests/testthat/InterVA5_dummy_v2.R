library(InterVA5)

data("probbaseV5", envir = environment())
probbaseV5 <- get("probbaseV5", envir = environment())

probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "I"  ] <- 1
probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "A+" ] <- 0.8
probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "A"  ] <- 0.5
probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "A-" ] <- 0.2
probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "B+" ] <- 0.1
probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "B"  ] <- 0.05
probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "B-" ] <- 0.02
probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "B -"] <- 0.02
probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "C+" ] <- 0.01
probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "C"  ] <- 0.005
probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "C-" ] <- 0.002
probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "D+" ] <- 0.001
probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "D"  ] <- 5e-04
probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "D-" ] <- 1e-04
probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "E"  ] <- 1e-05
probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "N"  ] <- 0
probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == ""   ] <- 0

probs <- as.data.frame(probbaseV5[1:nrow(probbaseV5),18:ncol(probbaseV5)])
probs <- apply(probs, 2, as.numeric)

# make row names symptoms
rownames(probs) <- c("Prior", probbaseV5[2:nrow(probbaseV5),1])
data("causetextV5", envir = environment())
causetextV5 <- get("causetextV5", envir = environment())

#make ages whatever top indicator is for each cause
picktop <- function(df){
  switchmax <- function(x){
    singlemax <- rep(0, length(x))
    singlemax[which.max(x==max(x))] <- 1
    singlemax
  }
  apply(df, 2, switchmax)
}
likelihood <- 0.5 # strength of connection for symptom x cod to count as yes

probs[6:12,] <- picktop(probs[6:12,])
# dummydata[,c("i022a","i022b","i022c","i022d","i022e", "i022f", "i022g")]
initialbabies <- picktop(probs[13:16,])
initialbabies[,which(probs["i022g",]==0)] <- 0
probs[13:16,] <- initialbabies

fertile <- picktop(probs[17:19,])
fertile[, which(!(probs["i019b",] >= likelihood & probs["i022c",] >= likelihood))] <- 0
probs[17:19,] <- fertile

# Actual setting up condition for y/n
#dummydata <- apply(probs,2, function(x) { ifelse(x > x["Prior"]*10 | x >= 0.8, "y", "n")}) %>% t  %>% as.data.frame

dummydata <- as.data.frame( t( apply(probs,2, function(x) { ifelse(x >= likelihood, "y", ".")}) ), stringsAsFactor=F)

dummydata$Prior <- NULL

# dummydata$causegroup <- colnames(probs) %>% substr(1,1)
# dummydata <- dummydata %>%
#         dplyr::group_by(causegroup)  %>%
#         mutate_all(function(x) x/sum(x))
# dummydata <- dummydata %>% apply(2,function(x){ifelse(x > 0, "y", ".")}) %>% as.data.frame
# dummydata <- select(dummydata, -causegroup)

dummydata <- cbind(causetextV5[,2], dummydata)
colnames(dummydata)[1] <- "ID"

#make everyone with nonspecified gender a woman
problemgender <- which(dummydata[,"i019a"] == dummydata[,"i019b"] & dummydata[,"i019b"] == ".")
dummydata[problemgender, "i019b"] = "y"

# convert to strings
rn <- rownames(dummydata)
dummydata <- apply(dummydata, 2, as.character)
rownames(dummydata) <- rn

# cherry picking problems

# Anaemia of pregnancy - take out ab pain, bleeding during delivery
# prevent confusion with Obstetric haemorrhage
dummydata["b_0907",c("i199b","i326o","i327o","i328o","i329o")] <- "."
dummydata["b_0907",c("i170o", "i174o", "i175o")] <- "y"

# Prevent confusion with digestive neoplasms
dummydata["b_0299", c("i421o", "i414a", "i354o")] <- "y"

# Tetanus is just like this
dummydata["b_0108", c("i022g", "i022j", "i310o")] <- "."
dummydata["b_0108", c( "i022f", "i287o")] <- "y"

# culture death can't be elderly even though probbase suggests otherwise
dummydata["c_cult","i022a"] <- "."
dummydata["c_cult","i022c"] <- "y"

# unspec external - for some reason, being a woman 12-19 makes it 100% indeterminate
# even though this record says they were injured, so you'd think it would be indeterminate external
dummydata["b_1299", "i022l"] <- "."

# unspec neonatal
dummydata["b_1099", c("i123o","i363o","i369o")] <- "."

# maternal
dummydata["b_0999", c("i120b","i022l")] <- "."

rm(list=setdiff(ls(), "dummydata"))

# save(dummydata,file="InterVA5_dummy_v2.RData")
# evaldummy <- function(data){
#   res <- InterVA5(data, HIV = "l", Malaria = "l", directory = getwd(), filename="TestVA")
#   causematch <- c()
#   ids <- c()
#   for (i in 1:length(res$VA5)){
#     ids <- c(ids, res$VA5[[i]]$ID)
#     causematch <- c(causematch, res$VA5[[i]]$ID %in% c(res$VA5[[i]]$PREGSTAT,
#                                                        res$VA5[[i]]$CAUSE1,
#                                                        res$VA5[[i]]$COMCAT))
#   }
#   cat(paste("Missing",nrow(dummydata) - length(res$VA5),"cod\n"))
#   if (nrow(dummydata) > length(res$VA5)) {cat(paste(dummydata[!dummydata[,"ID"] %in% ids, "ID"]))}
#   data.frame(COD = ids, Match = causematch)
# }
# 
# 
# evaldummy(dummydata)
