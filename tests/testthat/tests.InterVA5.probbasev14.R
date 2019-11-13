context("InterVA5 Algorithm with downloaded SCI")

# generate dummydata
source("InterVA5_dummy_probbasev14.R")

data(probbaseV5_14)
newProbbase <- probbaseV5_14

# A group
for (cod in rownames(dummydata)[grep("^a_", rownames(dummydata))]){
  test_that(paste0(cod, " (", dummydata[cod, "ID"], ")"), {

    out <- InterVA5(dummydata[cod,, drop=FALSE], sci = newProbbase, HIV = "l", Malaria = "l", write=F)

    expect_true(dummydata[cod, "ID"] %in% c(out$VA5[[1]]$PREGSTAT))
    rm(out)
  })
}

# B group
for (cod in rownames(dummydata)[grep("^b_", rownames(dummydata))]){
  test_that(paste0(cod, " (", dummydata[cod, "ID"], ")"), {

    out <- InterVA5(dummydata[cod,, drop=FALSE], sci = newProbbase, HIV = "l", Malaria = "l", write=F)

    expect_true(dummydata[cod, "ID"] %in% c(out$VA5[[1]]$CAUSE1))
    rm(out)
  })
}

# C group
for (cod in rownames(dummydata)[grep("^c_", rownames(dummydata))]){
  test_that(paste0(cod, " (", dummydata[cod, "ID"], ")"), {

    out <- InterVA5(dummydata[cod,, drop=FALSE], sci = newProbbase, HIV = "l", Malaria = "l", write=F)

    expect_true(dummydata[cod, "ID"] %in% c(out$VA5[[1]]$COMCAT))
    rm(out)
  })
}
