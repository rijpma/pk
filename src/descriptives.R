# Script that takes the PKs linked to civil certificates and describes the biases in the PKs.

rm(list = ls())

setwd("~/repos/pk/")

library("readxl")
library("data.table")
library("tinyplot")

# Standard error of a proportion.
sep = function(x, n) sqrt((x * (1 - x)) / n)

mypar = function(...) {
    par(...,
        bty = "l",
        mar = c(4, 3, 2, 1),
        mgp = c(1.7, .5, 0),
        tck = -.01,
        font.main = 1)
}

# Read the PK workbook spread over two sheets: pks holds the original persons and
# roles, check holds RvW's additions for persons and roles that were missing.
pks = readxl::read_xlsx("dat/pk werkbestand 5000_after3rdscan-2.xlsx", sheet = "Processed")
check = readxl::read_xlsx("dat/pk werkbestand 5000_after3rdscan-2.xlsx", sheet = "MISSING_final")

# Supporting PK/HSN files.
occ = fread("./dat/R18/pkbrp.txt", encoding = "Latin-1")
hisco = fread("~/data/hisco/HSN_HISCO_release_2020_02.csv", encoding = "Latin-1")
topo = fread("~/data/hsn/HSNdocs/ToponiemenNL1812-2012Spatio-Temporeel.csv", encoding = "Latin-1")

setDT(pks)
setDT(check)

# Drop empty rows.
pks = pks[!is.na(persid.x)]

# Non-positive year codes are missing.
pks[byear.x <= 0, byear.x := NA]
pks[dyear.x <= 0, dyear.x := NA]

# row_id records the original row position; used for the targeted fixes below.
pks[, row_id := .I]

# Data fixes.
pks[famid == 28451 & row_id == 464 & is.na(type2), type2 := "marriage"]
pks[is.na(famid), famid := 207009]
pks[famid == 0, famid := 55232]

# Standardise the spelling of the certificate-link status codes.
pks[cert_url_1 == "missinglink", cert_url_1 := "missing link"]
pks[cert_url_1 == "missingurl", cert_url_1 := "missing link"]
pks[cert_url_1 == "notfound", cert_url_1 := "not found"]

pks[cert_url_2 == "missinglink", cert_url_2 := "missing link"]
pks[cert_url_2 == "missingurl", cert_url_2 := "missing link"]
pks[cert_url_2 == "notfound", cert_url_2 := "not found"]
pks[cert_url_2 == "notfounf", cert_url_2 := "not found"]

# Collapse the free-text reasons for missingness into a small set of categories.
check[Reason == "probably moved out parental home before introduction PK", Reason := "moved out of parental home before PK"]
check[Reason == "perhaps entries in progress", Reason := "unknown"]

# Flag whether each RP ever appears with a partner.
pks[, rp_ever_married := any(role == "partner"), by = famid]
pks[, rp_ever_married := na.omit(rp_ever_married)[1], by = famid]

# PKs do not record whether any given person ever married.

# Carry the RP's birth year onto every row of the household.
pks[role == "RP", byear_rp := na.omit(byear.x)[1], by = famid]
pks[, byear_rp := byear_rp[1], by = famid]

setorder(pks, byear_rp, famid)

# Number of children listed on each PK.
pks[role == "child", nkids := uniqueN(persid.x), by = famid]
pks[, nkids := na.omit(nkids)[1], by = famid]
pks[is.na(nkids), nkids := 0]

# Ten-year birth cohorts; the 1855-65 cohort is thin, so fold it (and any earlier
# cohorts) into a single 1865 bucket.
pks[, cohort := round(byear.x, -1)]
pks[, cohort_rp := round(byear_rp, -1)]
pks[cohort_rp <= 1870, cohort_rp := 1865]


fwrite(pks, "./dat/pks_clean.csv")

# Combine the original PK rows with RvW's additions. The result is not in the same
# structure as the originals
pka = rbindlist(
    list(
        original = pks[, list(famid, persid.x, row_id = row_id, role1 = role1, type1 = type1, byear_rp = byear_rp, cohort_rp = cohort_rp, byear.x = byear.x)],
        addition = check[, list(famid, persid.x, row_id = NA, role1 = role1, type1 = type1, byear.x = byear.x)]
    ),
    fill = TRUE,
    idcol = "part"
)

pka[, i := .GRP, by = famid]
pka[, byear_rp := na.omit(byear_rp)[1], by = famid]
pka[, cohort_rp := na.omit(cohort_rp)[1], by = famid]
pka[, cohort := round(byear.x, -1)]
# - % linked, % linked naar rol persoon op PK, en ditto over tijd

# Flag which certificates were found vs not found, based on whether a URL exists.
pks[, not_found_1 := !grepl("http", cert_url_1)]
pks[, found_1 := not_found_1 == FALSE]
pks[, not_found_2 := !grepl("http", cert_url_2)]
pks[, found_2 := not_found_2 == FALSE]
pks[not_found_2 == TRUE, .N, by = cert_url_2][order(cert_url_2)]
pks[not_found_1 == TRUE, .N, by = cert_url_1][order(cert_url_1)]

# Restrict to the RP rows.
pks_rp = pks[role == "RP"]

# Figure 4: share of RPs' life events on PKs linked to civil certificates, by RP cohort.
# Marriages are only reported for RPs who ever married; standard errors are omitted.
# We can't simply take mean(found): birth and marriage must be handled separately.
toplot = pks_rp[,
    list(
        birth_found = any((found_1 & type1 == "birth") | (found_2 & type2 == "birth" & role2 == "RP")),
        marriage_found = any((found_1 & type1 == "marriage") | (found_2 & type2 == "marriage" & role2 == "RP")),
        rp_ever_married = rp_ever_married[1],
        death_found = any((found_1 & type1 == "death") | (found_2 & type2 == "death" & role2 == "RP"))
    ),
    by = list(cohort_rp, famid)
]
toplot[rp_ever_married == FALSE, marriage_found := NA]
toplot[, rp_ever_married := NULL]
toplot = melt(toplot, id.vars = c("cohort_rp", "famid"), variable.name = "type")
toplot = toplot[, mean(value, na.rm = TRUE), by = list(cohort_rp, type)]
png("./out/rp_found_bytype_both.png", width = 700, height = 600, res = 100)
mypar()
plt(V1 ~ cohort_rp | type, data = toplot, type = "b", pch = 16,
    ylab = "share found", xlab = "cohort")
dev.off()

# Figure 5: share of RP relatives' life events on PKs linked to civil certificates,
# by RP cohort, limited to children and partners.
npk = pks[, list(n_relative = uniqueN(persid.x)), by = list(role, famid, cohort_rp)] # rp, partner, father, mother, child on PK
nfnd = pks[, list(n_relative_found = uniqueN(persid.y[found_2 == TRUE])), by = list(role = role2, famid, cohort_rp, type2)] # note: uses role2
toplot = merge(npk, nfnd, by = c("role", "famid", "cohort_rp"))
toplot = toplot[, list(share_found = mean(n_relative_found / n_relative)), by = list(role, type2, cohort_rp)]

png("./out/relatives_rp_events_found.png", width = 900, height = 500, res = 100)
mypar()
plt(share_found ~ cohort_rp | type2, facet = ~ role, data = toplot[role != "RP"][order(cohort_rp)], type = "b", pch = 16)
dev.off()

# Figure 6: distribution of children born on PKs, by origin (original vs addition).
toplot = pka[role1 == "child" & type1 == "birth", list(persid.x, byear.x, part)]
toplot = unique(toplot)[, .N, by = list(byear.x = round(byear.x, -1), part)][order(byear.x, part)]

png("./out/pk_childcoverage.png", width = 700, height = 600, res = 100)
plt(
    N ~ byear.x | part,
    data = toplot,
    type = "bar",
    main = "births on PKs", xlab = "year"
)
dev.off()

# Figure 7: child missingness by child birth year and cohort.
# Take births from pka, deduplicating children who have multiple second certificates.
pka_births = pka[role1 == "child" & type1 == "birth"][!duplicated(persid.x)]

pka_births[, .N, by = cohort][order(cohort)]

toplot = rbindlist(
    list(
        RP = pka_births[, mean(part == "addition"), by = list(cohort = cohort_rp)],
        RP_shifted = pka_births[, mean(part == "addition"), by = list(cohort = cohort_rp + 30)],
        birth = pka_births[, mean(part == "addition"), by = cohort]
    ),
    idcol = "POV"
)
png("./out/kid_missingness.png", width = 700, height = 600, res = 100)
mypar()
plt(V1 ~ cohort | POV, data = toplot[cohort > 1860][order(cohort)], type = "b", pch = 16)
dev.off()

# Table 2: reasons for missingness.
totab = check[, .N, by = Reason]
out = totab[order(-N)]
knitr::kable(out)
knitr::kable(out, format = "html") |>
    writeLines("./out/reasons_lost.html")

# Figure 8: average number of children per RP, with and without civil-registry
# corrections, by RP cohort.
nkids_pk = unique(pks[role == "child", list(famid, persid.x, byear_child = byear.x, byear_rp, cohort_rp)])
nkids_ad = unique(check[role == "child", list(famid, persid.x, byear_child = byear.x)])
nkids = merge(
    nkids_pk[, .N, by = list(famid, byear_rp, cohort_rp)],
    nkids_ad[, .N, by = famid],
    by = "famid",
    all.x = TRUE,
    suffixes = c("_pk", "_add"),
)

# Add back RPs with no children on their PK so the averages include zero-child households.
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

toplot = nkids[,
    list(
        n_children_pk = mean(N_pk),
        n_children_corrected = mean(N_corrected)),
    by = cohort_rp]
png("./out/kidcorrection.png", width = 700, height = 600, res = 100)
mypar()
matplot(toplot$cohort_rp, toplot[, -"cohort_rp"],
    ylab = "Average n. children",
    xlab = "Cohort RP",
    type = "b", pch = 19, lty = 1)
legend("topright", fill = 1:2, legend = c("PK only", "PK + civreg additions"))
dev.off()

# Figure 9: child mortality. Note the large number of missing death years.
pks[dyear.x > 0, age_at_death := dyear.x - byear.x]
pks[role == "child", na.omit(age_at_death)[1], by = persid.x][, .N, by = V1][order(V1)] |> knitr::kable()
pks[role == "child", na.omit(age_at_death)[1], by = persid.x][, .N, by = V1 > 0]
pks[role == "child", na.omit(age_at_death)[1], by = persid.x][, .N, by = V1 == 0]
pks[role == "child", na.omit(age_at_death)[1], by = persid.x][, .N, by = V1 == 1]
pks[role == "child", na.omit(age_at_death)[1], by = persid.x][, .N, by = V1 == 2]
pks[role == "child", na.omit(age_at_death)[1], by = persid.x][, .N, by = V1 == 3]
pks[role == "child", na.omit(age_at_death)[1], by = persid.x][, .N, by = V1 == 4]
pks[role == "child", na.omit(age_at_death)[1], by = persid.x][, .N, by = V1 > 5]

toplot = unique(pks[role == "child", list(persid.x, byear.x, dyear.x, age_at_death)])

toplot[dyear.x <= 0, dyear.x := NA]
toplot[order(byear.x), i := .I]
toplot = melt(toplot[, -"age_at_death"], id.vars = c("persid.x", "i"))
toplot[, registered_death := variable == "dyear.x" & !is.na(value)]

png("./out/pk_childdeaths.png", width = 700, height = 600, res = 100)
mypar()
plt(i ~ value | factor(registered_death), data = toplot, pch = 16, type = "p", legend = "topleft", col = c(1, 2))
plt_add(i ~ value | factor(i), data = toplot, type = "l", col = "#DF536B", pch = 16, legend = FALSE)
dev.off()

# Regression example. Choosing a location is tricky: place of birth of the first
# child is unavailable for childless RPs, age-25 location is missing for many RPs
# with no clear pattern, and RP place of birth conflates a cultural measure. We
# therefore park location and use occupation instead.

# Start with place of birth.
pobrp = unique(pks[role == "RP", list(famid, byear_rp, loc = bloc.x)])

# Standardise place names.
pkplaces = fread("./dat/pkplaces.csv")
pobrp = merge(pobrp, pkplaces, by = "loc")
pobrp[, loc := `Standardized Name`]

# Attach AMCO codes from the toponym list.
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

occ = occ[IDNR %in% pks$famid]

occ[, uniqueN(IDNR)]

# Merge in occupation, then the HISCO classification.
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
    feols(N_pk ~ urban | cohort_rp, data = nkids),
    feols(N_corrected ~ urban | cohort_rp, data = nkids),
    fepois(N_pk ~ urban | cohort_rp, data = nkids),
    fepois(N_corrected ~ urban | cohort_rp, data = nkids)
)
etable(mlist, vcov = "hetero")
etable(mlist, vcov = "hetero") |>
    knitr::kable(format = "html") |>
    writeLines("./out/urbanreg.html")

# Ideally this would be an event-history model of births, but households are not
# right-censored for fertility, so OLS on households is acceptable.
nkids[, mean(N_corrected), by = urban]

# The urban/rural difference is small to begin with, the difference-in-differences
# is smaller still, and the variance is high, so nothing is significant.
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
