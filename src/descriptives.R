setwd("~/repos/pk/")

library("readxl")
library("data.table")

# read in data, three sheets
pk_all <- readxl::read_xlsx("dat/pk werkbestand 5000_20241111-1.xlsx", sheet = "Processed")
check1 <- readxl::read_xlsx("dat/pk werkbestand 5000_20241111-1.xlsx", sheet = "Missing")
check2 <- readxl::read_xlsx("dat/pk werkbestand 5000_20241111-1.xlsx", sheet = "Tweede scan")

setDT(pk_all)
setDT(check1)
setDT(check2)

# check overlap between the
pk_all[, row := .I]

x <- rbindlist(
    list(
        all = pk_all[, list(famid, row = row)],
        ch1 = check1[, list(famid, row = NA)],
        ch2 = check2[, list(famid, row = NA)]
    ),
    idcol = "part"
)

x[, i := .GRP, by = famid]

# lowest and highest rownum pk_all found in check1
which(pk_all$famid %in% check1$famid) |> min()
which(pk_all$famid %in% check1$famid) |> max()

# lowest and highest rownum pk_all found in check2
which(pk_all$famid %in% check2$famid) |> min()
which(pk_all$famid %in% check2$famid) |> max()

# some sumstats on the children additions
# share hh with missing children
# mean/median number of missing children
# mean/median number number children when any missing

# first, we subset on the first 1000
pk1000 <- pk_all[1:1000, ]

pk1000[, uniqueN(famid)]

# note however the duplicates in the data
pk1000[role1 == "RP" & type1 == "marriage" & type2 == "birth", sum(role2 == "child"), by = famid]
pk1000[role1 == "RP" & type1 == "marriage" & type2 == "birth", sum(role2 == "child" & !duplicated(byear.y)), by = famid]
pk1000[role1 == "RP" & type1 == "marriage" & type2 == "birth" & famid == 207078]

# so first we have to do something about the duplicates above
# this is because of remarriages,
# for now, just deduplicate on the basis of birthyear before conting

nkid <- pk1000[
    role1 == "RP" & type1 == "marriage" & type2 == "birth",
    list(nkid_original = sum(role2 == "child" & !duplicated(byear.y))),
    by = famid
]

# merge in all all families to get 0 counts
nkid <- merge(
    nkid,
    unique(pk1000[, list(famid)]),
    all.x = TRUE,
    all.y = TRUE,
    by = "famid"
)
nkid[is.na(nkid_original), nkid_original := 0]

# merge in data checks
nkid <- merge(
    nkid,
    check2[, list(addc2 = .N), by = famid],
    all.x = TRUE,
    all.y = FALSE,
    by = "famid"
)
nkid <- merge(
    nkid,
    check1[, list(addc1 = .N), by = famid],
    all.x = TRUE,
    all.y = FALSE,
    by = "famid"
)
nkid[is.na(addc2), addc2 := 0]
nkid[is.na(addc1), addc1 := 0]
# note, addc2 is the useful check for us, other one is always zero

# descriptives
nkid[, mean(addc2 > 0)]
nkid[, mean(nkid_original, na.rm = TRUE)]
nkid[, mean(nkid_original + addc2, na.rm = TRUE)]
nkid[, median(nkid_original, na.rm = TRUE)]
nkid[, median(nkid_original + addc2, na.rm = TRUE)]

t.test(nkid$nkid_original, nkid$nkid_original + nkid$addc2)
t.test(nkid$nkid_original, nkid$nkid_original + nkid$addc2, alternative = "less")

par(mfrow = c(1, 2))
hist(nkid$nkid_original)
hist(nkid$nkid_original + nkid$addc2)

# a bootstrap, but it's really not necessary here
library("boot")
tboot <- function(d, ind) {
    g1 <- d[ind, nkid_original]
    g2 <- d[ind, nkid_original + addc2]
    tstat <- t.test(g1, g2, alternative = "less")$p.value
    return(tstat)
}

sd_kids <- sd(nkid$nkid_original)
diff_kids <- 0.5

# simulate how big N should be to reject at 0.05
# n <- 5000
n <- 537
# n <- 1000
sim <- function(n) {
    replicate(
        1000,
        t.test(rnorm(n, 1.6, 3), rnorm(n, 2.1, 3))
    )
}
mean(unlist(sim(37)[3, ]) < 0.05)
mean(unlist(sim(80)[3, ]) < 0.05)
mean(unlist(sim(500)[3, ]) < 0.05)
mean(unlist(sim(1000)[3, ]) < 0.05)
