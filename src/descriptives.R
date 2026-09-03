# script that takes the PKs linked to civil certificates, and describes the biases in the PKs

rm(list = ls())

setwd("~/repos/pk/")

library("readxl")
library("data.table")
library("tinyplot")

# standard error of proportion
sep = function(x, n) sqrt((x * (1 - x)) / n)

mypar = function(...){
    par(...,
        bty = "l",
        mar = c(4, 3, 2, 1),
        mgp = c(1.7, .5, 0),
        tck=-.01,
        font.main = 1)
}

# read in data, three sheets
# pks <- readxl::read_xlsx("dat/pk werkbestand 5000_20241111-1.xlsx", sheet = "Processed")
# check1 <- readxl::read_xlsx("dat/pk werkbestand 5000_20241111-1.xlsx", sheet = "Missing")
# check2 <- readxl::read_xlsx("dat/pk werkbestand 5000_20241111-1.xlsx", sheet = "Tweede scan")

# read in data, spreak over two sheets, pks = original pk persons+roles, check is RvW's additions for missing persons+roles
pks <- readxl::read_xlsx("dat/pk werkbestand 5000_after3rdscan-2.xlsx", sheet = "Processed")
check <- readxl::read_xlsx("dat/pk werkbestand 5000_after3rdscan-2.xlsx", sheet = "MISSING_final")
# check2 <- readxl::read_xlsx("dat/pk werkbestand 5000_after3rdscan-2.xlsx", sheet = "Tweede scan")

# PK/HSN supporting files
occ = fread("./dat/R18/pkbrp.txt",
    encoding = "Latin-1")
adr = fread("./dat/R18/pkadres.txt",
    encoding = "Latin-1")
hisco = fread("~/data/hisco/HSN_HISCO_release_2020_02.csv",
    encoding = "Latin-1")
topo = fread("/Users/Rijpm101/data/hsn/HSNdocs/ToponiemenNL1812-2012Spatio-Temporeel.csv",
    encoding = "Latin-1")

setDT(pks)
setDT(check)

# drop empty rows
pks = pks[!is.na(persid.x)]

#  -1 codes should be NA
pks[byear.x <= 0, byear.x := NA]
pks[dyear.x <= 0, dyear.x := NA]

# this used to be rowid. 464 below works, not sure about rest
pks[, row_id := .I]

# data fixes
pks[famid == 28451 &  is.na(type2)]
pks[famid == 28451 & row_id == 464 & is.na(type2), type2 := "marriage"]
pks[is.na(famid), famid := 207009]
pks[famid == 0, famid := 55232]

# consistent spelling of url status
pks[cert_url_1 == "missinglink", cert_url_1 := "missing link"]
pks[cert_url_1 == "missingurl", cert_url_1 := "missing link"]
pks[cert_url_1 == "notfound", cert_url_1 := "not found"]

pks[cert_url_2 == "missinglink", cert_url_2 := "missing link"]
pks[cert_url_2 == "missingurl", cert_url_2 := "missing link"]
pks[cert_url_2 == "notfound", cert_url_2 := "not found"]
pks[cert_url_2 == "notfounf", cert_url_2 := "not found"]

# rp every married
pks[, rp_ever_married := any(role == "partner"), by = famid]
pks[, rp_ever_married := na.omit(rp_ever_married)[1], by = famid]

# person every married?
# PKs do not rell us this

# rp birth year
pks[role == "RP", byear_rp := na.omit(byear.x)[1], by = famid]
pks[, byear_rp := byear_rp[1], by = famid]

setorder(pks, byear_rp, famid)

# nkids on pk
pks[role == "child", nkids := uniqueN(persid.x), by = famid]
pks[, nkids := na.omit(nkids)[1], by = famid]
pks[is.na(nkids), nkids := 0]


# cohorts; (1855-65) cohort is thin
pks[, cohort := round(byear.x, -1)]
pks[, cohort_rp := round(byear_rp, -1)]
pks[cohort_rp <= 1870, cohort_rp := 1865]
# pks = pks[cohort_rp > 1860]

fwrite(pks, "./dat/pks_clean.csv")

# this can't actually be used because it's not made in the same structure
pka <- rbindlist(
    list(
        original = pks[, list(famid, row_id = row_id, role1 = role1, byear_rp = byear_rp, byear.x = byear.x)],
        addition = check[, list(famid, row_id = NA, role1 = role1, byear.x = byear.x)]
        # ch2 = check2[, list(famid, row_id = NA)]
    ),
    fill = TRUE,
    idcol = "part"
)

pka[, i := .GRP, by = famid]
pka[, byear_rp := na.omit(byear_rp)[1], by = famid]
# - % linked, % linked naar rol persoon op PK, en ditto over tijd

# found/not found certificates
pks[, not_found_1 := !grepl("http", cert_url_1)]
pks[, found_1 := not_found_1 == FALSE]
pks[, not_found_2 := !grepl("http", cert_url_2)]
pks[, found_2 := not_found_2 == FALSE]
pks[not_found_2 == TRUE, .N, by = cert_url_2][order(cert_url_2)]
pks[not_found_1 == TRUE, .N, by = cert_url_1][order(cert_url_1)]

# coverage
png("./out/pk_childcoverage.png", width = 700, height = 600, res = 100)
plt(
    N ~ byear.x | part,
    data = pka[, .N, by = list(byear.x = round(byear.x, -1), part)][order(byear.x, part)],
    type = "bar",
    main = "births on PKs", xlab = "year"
)
dev.off()

# infant and child mortality
# note the high number of dyear missing
pks[dyear.x > 0, age_at_death := dyear.x - byear.x]
pks[role == "child", na.omit(age_at_death)[1], by = persid.x][, .N, by = V1][order(V1)] |> knitr::kable()
pks[role == "child", na.omit(age_at_death)[1], by = persid.x][, .N, by = V1 > 0]
pks[role == "child", na.omit(age_at_death)[1], by = persid.x][, .N, by = V1 == 0]
pks[role == "child", na.omit(age_at_death)[1], by = persid.x][, .N, by = V1 == 1]
pks[role == "child", na.omit(age_at_death)[1], by = persid.x][, .N, by = V1 > 5]

toplot = unique(pks[role == "child", list(persid.x, byear.x, dyear.x, age_at_death)])
plt(~ byear.x | is.na(age_at_death), type = "hist", data = toplot)

toplot[dyear.x <= 0, dyear.x := NA]
toplot[order(byear.x), i := .I]
toplot = melt(toplot[, -"age_at_death"], id.vars = c("persid.x", "i"))

png("./out/pk_childdeaths.png", width = 700, height = 600, res = 100)
plt(i ~ value | factor(i), data = toplot, type = "b", col = "#DF536B", pch = 16, legend = FALSE)
dev.off()

# pk info where person = RP
pks_rp = pks[role == "RP"]


# found/not found over time with SE
toplot = pks_rp[,
    any(found_1),
    by = list(type1, cohort_rp, famid)][,
        list(mean(V1), sep(mean(V1), .N)),
        by = list(cohort_rp, type1)]
toplot[, upr := V1 + 2*V2]
toplot[, lwr := V1 - 2*V2]
plt(V1 ~ cohort_rp | type1, ymin = lwr, ymax = upr,
    data = toplot[order(cohort_rp)],
    pch = 19,
    type = "errorbar")
plt(V1 ~ cohort_rp | type1,data = toplot[order(cohort_rp)], type = "lines", add = TRUE)
# etc.

# # but let's simplify to w/o SE for now
# # pdf("./out/rp_found_bytype1.pdf", height = 6)
# png("./out/rp_found_bytype1.png", width = 700, height = 600, res = 100)
# mypar()
# plt(V1 ~ cohort_rp | type1, data = toplot[order(cohort_rp)],
#     ylab = "share found",
#     type = "b", pch = 19)
# dev.off()

# # type2 of rp, conditional on finding cert1
# toplot = pks_rp[found_1 == TRUE & role2 == "RP",
#     list(mean(found_2), sep(mean(found_2), .N)),
#     by = list(type2, cohort_rp)]

# # pdf("./out/rp_found_bytype2.pdf", height = 6)
# png("./out/rp_found_bytype2.png", width = 700, height = 600, res = 100)
# mypar()
# plt(V1 ~ cohort_rp | type2, data = toplot[order(cohort_rp)],
#     ylab = "share found",
#     type = "b", pch = 19)
# dev.off()

# ok, let's remake this no SE one
toplot = pks_rp[,
    list(
        birth_found = any( (found_1 & type1 == "birth") | (found_2 & type2 == "birth") ),
        marriage_found = any( (found_1 & type1 == "marriage") | (found_2 & type2 == "marriage") ),
        rp_ever_married = rp_ever_married[1],
        death_found = any( (found_1 & type1 == "death") | (found_2 & type2 == "death") )
    ),
    by = list(cohort_rp, famid)
]
toplot[rp_ever_married == FALSE, marriage_found := NA]
toplot[, rp_ever_married := NULL]
toplot = melt(toplot, id.vars = c("cohort_rp", "famid"), variable.name = "type")
toplot = toplot[, mean(value, na.rm = TRUE), by = list(cohort_rp, type)]
pdf("./out/rp_found_bytype_both.pdf", height = 6)
mypar()
plt(V1 ~ cohort_rp | type, data = toplot, type = "b", pch = 20,
    ylab = "share found", xlab = "cohort")
dev.off()

# type2 of rp's children (others not defined "from"), conditional on finding cert1
toplot = pks[found_1 == TRUE & role == "RP" & role2 == "child", list(mean(found_2), sep(mean(found_2), .N)), by = list(role2, type2, cohort_rp)]
# pdf("./out/children_found_bytype2.pdf", height = 6)
png("./out/children_found_bytype2.png", width = 700, height = 600, res = 100)
mypar()
plt(V1 ~ cohort_rp | type2, data = toplot[order(cohort_rp)],
    ylab = "share found",
    type = "b", pch = 19)
dev.off()

# ook naar rol persoon op pk
# finding any certificate by relative of RP
toplot = pks[, any(found_1), by = list(persid.x, role, cohort_rp)][,
    mean(V1), by = list(role, cohort_rp)]
# pdf("./out/relatives_found.pdf", height = 6)
png("./out/relatives_found.png", width = 700, height = 600, res = 100)
mypar()
plt(V1 ~ cohort_rp | role,
    data = toplot[order(cohort_rp)],
    ylab = "share found",
    type = "b", pch = 19)
dev.off()

npk = pks[, list(n_pk = uniqueN(persid.x)), by = list(role, famid, cohort_rp)]
nfnd = pks[, list(n_pk = uniqueN(persid.y[found_2 == TRUE])), by = list(role, famid, cohort_rp)]
toplot = merge(
    npk,
    nfnd,
    by = c("role", "famid", "cohort_rp")
)
toplot[, mean(n_pk.y / n_pk.x), by = role]
toplot = toplot[, mean(n_pk.y / n_pk.x), by = list(role, cohort_rp)]
plt(V1 ~ cohort_rp, facet = ~ role, data = toplot[role != "RP"][order(cohort_rp)], type = "b")

npk = pks[, list(n_pk = uniqueN(persid.x)), by = list(role, famid, cohort_rp)] # how many rp, partner, father, mother, child on PK
nfnd = pks[, list(n_pk = uniqueN(persid.y[found_2 == TRUE])), by = list(role = role2, famid, cohort_rp, type2)] # nb ROLE2
toplot = merge(
    npk,
    nfnd,
    by = c("role", "famid", "cohort_rp")
)
toplot[, mean(n_pk.y / n_pk.x), by = list(role, type2)]
toplot = toplot[, mean(n_pk.y / n_pk.x), by = list(role, type2, cohort_rp)]

png("./out/relatives_rp_events_found.png", width = 900, height = 500, res = 100)
mypar()
plt(V1 ~ cohort_rp | type2, facet = ~ role, data = toplot[role != "RP"][order(cohort_rp)], type = "b", pch = 19)
dev.off()

# why no births kids?
pks[, .N, by = list(role1, type1, type2, role2)][order(role1, type1, type2, role2)]
# this tells the story: from the pov of the RP, we only look for children and partner



# distribution of birthyears of child
hist(pks[role == "child", byear.x], xlim = c(1875, 1975))
# lower than expected early on, but doesn't show much


# n kids by cohort, with and without correction
nkids_pk = unique(pks[role == "child", list(famid, persid.x, byear_child = byear.x, byear_rp, cohort_rp)])
nkids_ad = unique(check[role == "child", list(famid, persid.x, byear_child = byear.x)])
nkids = merge(
    nkids_pk[, .N, by = list(famid, byear_rp, cohort_rp)],
    nkids_ad[, .N, by = famid],
    by = "famid",
    all.x = TRUE,
    suffixes = c("_pk", "_add"),
)

# merge in rp sex
nkids = rbindlist(
    list(
        nkids,
        unique(pks[nkids == 0 & role == "RP", list(famid, byear_rp, cohort_rp, N_pk = 0)])
    ),
    fill = TRUE
)
setorder(nkids, byear_rp)

nkids[is.na(N_add), N_add := 0]
nkids[, N_corrected := N_pk + N_add]

# % households with missing kids
toplot = nkids[, list(share_w_missing = mean(N_add > 0)), by = cohort_rp]
# pdf("./out/share_missing_children.pdf")
png("./out/share_missing_children.png", width = 700, height = 600, res = 100)
mypar()
plt(share_w_missing ~ cohort_rp, data = toplot,
    ylab = "Share with missing child",
    type = "b", pch = 19)
dev.off()


pka[, cohort := round(byear.x, -1)]

toplot2 = pka[order(cohort), mean(part == "addition"), by = list(cohort)]

toplot = rbindlist(list(
    cohort_rp = toplot[, list(cohort = cohort_rp, share_w_missing)],
    cohort_birth = toplot2[cohort > 1865 & cohort < 1950, list(cohort = cohort, share_w_missing = V1)]
    ),
    idcol = "persp"
)
png("./out/missing_kids_rp_kid.png", width = 900, height = 500, res = 100)
mypar()
plt(share_w_missing ~ cohort, facet = ~ persp, data  = toplot, type = "b", pch = 19)
dev.off()



nkids[, list(share_w_missing = mean(N_add > 0)), by = cohort_rp]

# share missing kids, cohort kid
nkids


plt(share_w_missing ~ cohort_rp, data = toplot,
    ylab = "Share with missing child",
    type = "b", pch = 19)


# make this two next to each other

# average children implied by PKs
toplot = nkids[,
    list(
        n_children_pk = mean(N_pk),
        n_children_corrected = mean(N_corrected)),
    by = cohort_rp]
# pdf("./out/kidcorrection.pdf", height = 6)
png("./out/kidcorrection.png", width = 700, height = 600, res = 100)
mypar()
matplot(toplot$cohort_rp, toplot[, -"cohort_rp"],
    ylab = "Average n. children",
    xlab = "Cohort RP",
    type = "b", pch = 19, lty = 1)
legend("topright", fill = 1:2, legend = c("PK only", "PK + civreg additions"))
dev.off()

check[Reason == "probably moved out parental home before introduction PK", Reason := "moved out of parental home before PK"]
check[Reason == "perhaps entries in progress", Reason := "unknown"]
totab = check[, .N, by = Reason]
out = totab[order(-N)]
knitr::kable(out, format = "html") |> writeLines("./out/reasons_lost.html")

# and finally, a regression example, for which we need locations
# which location is tricky
# could be place of birth first child, but not everyone has kids!
# could be age, say, 25, but missing for 50 RPs, and no clear pattern like "die early". Moreover, many PKers are already well over 25 when they show up on PKs
# could be pob RP, saying we try to measure a culture thing
# this in fact so tricky we park it and try the stupid occupations

no_occ_ids = setdiff(pks$famid, occ$IDNR)
no_adress_ids = setdiff(pks$famid, adr$IDNR)

setdiff(no_occ_ids, no_adress_ids)
setdiff(no_adress_ids, no_occ_ids)
# this turns out to be largely the same group, bizarrely

# start with places of birth
pobrp = unique(pks[role == "RP", list(famid, byear_rp, loc = bloc.x)])
# pobrp[, list(loc = unique(loc))][, list(.I, loc)] |> fwrite()

# standardise placenames (new file available from RS)
pkplaces = fread("./dat/pkplaces.csv")
pobrp = merge(pobrp, pkplaces, by = "loc")
pobrp[, loc := `Standardized Name`]

# add amco codes from toponyms
setnames(topo, "Gemeente", "loc")
setnames(topo, "Amsterdamse code", "amco")
setnames(topo, "Provincie", "Province")
setnames(topo, "bij gem van JJJJMMDD", "fromDate")
setnames(topo, "bij gem tot JJJJMMDD", "toDate")
topo = topo[fromDate <= 19300101 & toDate > 19300101]

pobrp = merge(pobrp,
    unique(topo[, list(loc, Province, amco)]),
    by = c("loc", "Province"),
    all.x = TRUE
)
pobrp[is.na(amco)]
setnames(topo, "loc", "gemeente")
setnames(topo, "Toponiem (stad,dorp,buurtschap)", "loc")
topo = topo[!gemeente %in% pobrp$loc]
pobrp = merge(pobrp,
    unique(topo[, list(loc, Province, amco_sec = amco)]),
    by = c("loc", "Province"),
    all.x = TRUE
)
pobrp[duplicated(famid)]
pobrp[is.na(amco), amco := amco_sec]
pobrp[, amco_sec := NULL]
pobrp[is.na(amco)]

hdng = fread("~/repos/hipnl/dat/hdngpop.csv")
pobrp = merge(pobrp, hdng[year == 1930], by = "amco", all.x = TRUE)

nkids = merge(pobrp, nkids, by = "famid")
nkids[, urban := pop_31_12_f > 10e3]

# setdiff(pks$famid, occ$IDNR)

occ = occ[IDNR %in% pks$famid]
adr = adr[IDNR %in% pks$famid]

occ[, uniqueN(IDNR)]

# let's see the occup thing
nkids = merge(
    nkids,
    occ[, list(occup = tolower(BEROEPP[1])), by = list(famid = IDNR)],
    by = "famid",
    all.x = TRUE
)
occ[is.na(BEROEPP)]
nkids[is.na(occup)]
nkids = merge(
    nkids,
    unique(hisco[, list(occup = tolower(Original), HISCO, HISCLASS, HISCAM_NL)]),
    by = "occup",
    all.x = TRUE
)
dim(nkids)

nkids[is.na(HISCO), occup]
nkids[HISCO < 0, occup]

nkids

library("fixest")
mlist = list(
    # feols(N_pk ~ log(pop_31_12), data = nkids),
    # feols(N_corrected ~ log(pop_31_12), data = nkids),
    # feols(N_pk ~ urban, data = nkids),
    # feols(N_corrected ~ urban, data = nkids),
    feols(N_pk ~ urban | cohort_rp, data = nkids),
    feols(N_corrected ~ urban | cohort_rp, data = nkids),
    fepois(N_pk ~ urban | cohort_rp, data = nkids),
    fepois(N_corrected ~ urban | cohort_rp, data = nkids)
)
etable(mlist, vcov = "hetero")
etable(mlist, vcov = "hetero") |>
    knitr::kable(format = "html") |>
    writeLines("./out/urbanreg.html")

# ideally we estimate event-history model logit(birth_it) = b0 + b1 urban + ... + e
# but since we are not right-censored for fertility it's not too bad to ols the households
nkids[, mean(N_corrected), by = urban]

# ok so basically the difference isn't that big to begin with and the diff between the diffs is even smaller and there's still the big variance in the data so no sig

nkids[HISCLASS %in% c(1, 2), skill_level := "higher_skilled"]
nkids[HISCLASS %in% c(3, 4), skill_level := "medium_skilled"]
nkids[HISCLASS %in% c(6, 7, 8), skill_level := "medium_skilled"]
nkids[HISCLASS %in% c(5, 9, 10), skill_level := "lower_skilled"]
nkids[HISCLASS %in% c(11, 12, 13), skill_level := "unskilled"]


mlist = list(
    feols(N_pk ~ i(skill_level) | cohort_rp, data = nkids[HISCO > 0]),
    feols(N_corrected ~ i(skill_level) | cohort_rp, data = nkids[HISCO > 0]),
    feols(N_pk ~ HISCAM_NL | cohort_rp, data = nkids[HISCO > 0]),
    feols(N_corrected ~ HISCAM_NL | cohort_rp, data = nkids[HISCO > 0]),

    fepois(N_pk ~ i(skill_level) | cohort_rp, data = nkids[HISCO > 0]),
    fepois(N_corrected ~ i(skill_level) | cohort_rp, data = nkids[HISCO > 0]),
    fepois(N_pk ~ HISCAM_NL | cohort_rp, data = nkids[HISCO > 0]),
    fepois(N_corrected ~ HISCAM_NL | cohort_rp, data = nkids[HISCO > 0])
)
etable(mlist, vcov = "hetero")
etable(mlist[5:8], vcov = "hetero") |>
    knitr::kable(format = "html") |>
    writeLines("./out/occreg.html")

# this needs to include those additions
# and ideally you get the urban at the time of each birth
tomod = pks[role == "child" & type1 == "birth", list(famid, byear_rp, byear.x)]


tomod = unique(tomod)
tomod[, birth := 1]
tomod = merge(tomod,
    nkids[, CJ(byear.x = seq(byear_rp.x + 16, byear_rp.x + 45, 1)), by = famid],
    by = c("famid", "byear.x"),
    all.y = TRUE
)
tomod[is.na(birth), birth := 0]
tomod = merge(tomod,
    nkids,
by = "famid", all.x = TRUE)

adr[, nadr := .N, by = IDNR]
tobind = adr[VGNRADP > 1]
tobind[, JRADRP := JRADRP - 1]
x = rbind(adr, tobind)
x[nadr > 1, seq(min(JRADRP), max(JRADRP), by = 1), by = list(IDNR, PLADRP)]
x[IDNR %in% c(135086, 75143), seq(min(JRADRP), max(JRADRP), by = 1), by = list(IDNR)]


feglm(birth ~ urban , data = tomod, family = binomial(link = "logit"))

tomod[famid == 8163]
tomod[!is.na(byear.x), CJ(famid, seq(min(byear.x), max(byear.x))), by = famid][famid == 8163]

# what is the relevant loc? parent pob? can be quite outdated. loc firstborn? let's say habits are formed then? or most frequent byear
pob1 = pks[role == "child"][order(byear.x), bloc.x[1], by = famid]

adr = merge(
    adr,
    unique(pks[, list(IDNR = famid, byear_rp)]),
    all.x = FALSE,
    all.y = TRUE
)
# drop if moved before 25
adr[is.na(JRADRP)]
setorder(adr, JRADRP)
adr = adr[JRADRP < (byear_rp + 25), list(jradr = JRADRP[1], loc = PLADRP[1]), by = IDNR]
adr[jradr > 1]

pks[, uniqueN(persid.x[role == "child"]), by = list(famid)]

plt(~ byear.x, facet = ~role, data = pks[byear.x > 0], type = "histogram")






pks_rp[, .N, by = cohort]
library("fixest")
fixest::feols(found_1 ~ i(cohort) - 1, data = pks_rp) |> iplot()
# chance of finding pks and pk's connecting

# need any step because each type1 duplicated (just two anyway)
pks_rp[, any(found_1), by = famid][, mean(V1)]
pks_rp[, any(found_1), by = list(famid, type1)][, mean(V1), by = type1]

# here we don't, type2 + type1 should be unique
pks_rp[found_1 == TRUE, mean(found_2), by = type2]
pks_rp[found_1 == TRUE, mean(found_2), by = list(famid, type2)]




pks_rp[, mean(found_1), by = type1]
pks_rp[found_1 == TRUE, mean(found_2)]
pks_rp[found_1 == TRUE, mean(found_2), by = type2]
pks_rp[found_1 == TRUE, mean(found_2), by = list(type1, type2)]


toplot = pks_rp[, mean(found_1), by = list(type1, cohort = round(byear.x, -1))]
plt(V1 ~ cohort | type1, data = toplot[order(cohort)], type = "b")

pks_rp[is.na(type2)]

pks[, mean(not_found_1)]
pks[role == "RP", any(found_1), by = list(famid, type1)][, mean(V1)]
pks[role == "RP", any(found_1), by = list(famid, type1)][, mean(V1), by = type1]

pks[, mean(not_found_2)]
pks[, mean(found_2)]

pks[, .N, by = role]
pks[, mean(found), by = role]
pks[, mean(found), by = type1]
pks[role == "RP", mean(found), by = type1]
pks[role == "RP", mean(found), by = type2]

pks[byear.x > 0][order(byear.x), mean(!not_found), by = byear.x] |> plot(type = "b")
pks[byear.x > 0 & role == "RP"][order(byear.x), mean(!not_found), by = byear.x] |> points(type = "b", add = TRUE, col = 2)
toplot = pks[byear.x > 0][order(byear.x), mean(!not_found), by = list(type1, byear.x)]
library("tinyplot")
plt(V1 ~ byear.x | type1, data = toplot)

# basisnummer om te hebben, deze zouden allen moeten bestaan, geeft attrition in online aan
pks[role == "RP" & type1 == "birth", grepl("http", cert_url_1[1]), by = famid][, mean(V1)]
pks[role == "RP" & type1 == "birth" & !grepl("http", cert_url_1), .N, by = bloc.x][order(N)]
pks[role == "RP" & type1 == "birth" & !grepl("http", cert_url_1), .N, by = byear.x][order(byear.x)]

# linkage rates to primary certificates, c. 8 breakdowns
# rp by certificate

# linkage rates to primary + secondary certificates
# rp by certificate



# - distributies die al aangeven dat er iets mis is (zie paper)


# - percentage missende kinderen
nkid <- pks[
    role1 == "RP" & type1 == "marriage" & type2 == "birth",
    list(byear_rp = byear.x, myear_rp = myear.x, byear_kid = byear.x, nkid_original = sum(role2 == "child" & !duplicated(byear.y))),
    by = famid
]

# missing means no kids
nkid <- merge(
    nkid,
    unique(pks[, list(famid)]),
    all.x = TRUE,
    all.y = TRUE,
    by = "famid"
)
nkid[is.na(nkid_original), nkid_original := 0]

# merge in data checks
nkid <- merge(
    nkid,
    check[, list(addc2 = .N), by = famid],
    all.x = TRUE,
    all.y = FALSE,
    by = "famid"
)

nkid[, sum(!is.na(addc2))]
nkid[, mean(!is.na(addc2))]
nkid[order(byear_rp), list(share_added_kids = mean(!is.na(addc2))), by = round(byear_rp, -1)] |> plot(type="b")








# lowest and highest rownum pks found in check1
which(pks$famid %in% check$famid) |> min()
which(pks$famid %in% check$famid) |> max()

# lowest and highest rownum pks found in check2
# which(pks$famid %in% check2$famid) |> min()
# which(pks$famid %in% check2$famid) |> max()

# some sumstats on the children additions
# share hh with missing children
# mean/median number of missing children
# mean/median number number children when any missing

# first, we subset on the first 1000
pk1000 <- pks[1:1000, ]



# note however the duplicates in the data
pk1000[role1 == "RP" & type1 == "marriage" & type2 == "birth", sum(role2 == "child"), by = famid]
pk1000[role1 == "RP" & type1 == "marriage" & type2 == "birth", sum(role2 == "child" & !duplicated(byear.y)), by = famid]
# pk1000[role1 == "RP" & type1 == "marriage" & type2 == "birth" & famid == 207078]

# so first we have to do something about the duplicates above
# this is because of remarriages,
# for now, just deduplicate on the basis of birthyear before conting
cat("number of families in the checked data:\n")
pk1000[, uniqueN(famid)]

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
nkid_mean_original <- nkid[, mean(nkid_original, na.rm = TRUE)]
nkid_mean_addc2 <- nkid[, mean(addc2 > 0)]
nkid_mean_total <- nkid[, mean(nkid_original + addc2, na.rm = TRUE)]
nkid_median_original <- nkid[, median(nkid_original, na.rm = TRUE)]
nkid_median_total <- nkid[, median(nkid_original + addc2, na.rm = TRUE)]

cat("Mean children original:", nkid_mean_original, "\n")
cat("Share of obserfvations with children added", nkid_mean_addc2, "\n")
cat("Mean of original + added kids:", nkid_mean_total, "\n")
cat("Median of original:", nkid_median_original, "\n")
cat("Median of original + added kids:", nkid_median_total, "\n")

# one sided t-test
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

cat("bootstrap p-value:", boot(nkid, tboot, R = 1000)$t0)


sd_kids <- sd(nkid$nkid_original)
diff_kids <- 0.5

# simulate how big N should be to reject at 0.05
sim <- function(n) {
    replicate(
        1000,
        t.test(rnorm(n, 1.6, 3), rnorm(n, 2.1, 3), alternative = "less")
    )
}

cat("Proportion of p-values < 0.05 for n = 37:", mean(unlist(sim(37)[3, ]) < 0.05), "\n")
cat("Proportion of p-values < 0.05 for n = 80:", mean(unlist(sim(80)[3, ]) < 0.05), "\n")
cat("Proportion of p-values < 0.05 for n = 300:", mean(unlist(sim(300)[3, ]) < 0.05), "\n")
cat("Proportion of p-values < 0.05 for n = 500:", mean(unlist(sim(500)[3, ]) < 0.05), "\n")
cat("Proportion of p-values < 0.05 for n = 1000:", mean(unlist(sim(1000)[3, ]) < 0.05), "\n")
