#Find me at https://github.com/MDBorthwick/AgroforestryDisplacement-.git

###############################################################################
# Woodland BROILER carrying-capacity model  --  Red Ranger
# WAP Just Transition project 2024
# There isn't a forage-only system available, so the approach is to
# think-with Brodoclea calculations, adapted here for slow-growing broilers.
#
#   N_sustainable = min( N_annual , N_lean )
#
#                 ( sum_i Y_i ) * D_t * D_p * A_hab * A_breed * eps
#   N        =   ----------------------------------------------------
#                            ( a * W^0.75 ) + ( b * G )
#
# Estimates the number of forage-fed Red Ranger broilers a hectare of
# woodland can sustain through a grow-out. Supply (forage energy) and demand
# (per-bird requirement) are BOTH expressed per day, so they divide directly.
# The sustainable figure is the leaner of the annual-average and worst-season
# results.
#
# BROILER NOTE: the demand side is built around daily liveweight GAIN, not
# egg-laying. Red Rangers are a slow-growing coloured broiler that forages
# actively, processed at roughly 11-12 weeks and ~2.5-3 kg. Their strong
# foraging is what makes a forage-orientated system viable, and is reflected
# in a relatively high A_breed.
#
###############################################################################


## ===========================================================================
## 1. EDIT THESE VALUES  --  all parameters live here
## ===========================================================================

hectares <- 5.0   # your total woodland area (ha) -- change this to your site

# ----- Forage energy supply (kcal per hectare per day) ----------------------
# GROSS edible energy birds could in principle obtain, by source.
# Give two scenarios: annual mean and the worst (lean) season.

# Annual-average daily yields:
Y_mast_annual   <- 45000   # nuts / acorns / chestnut (the energy backbone)
Y_fruit_annual  <- 18000   # fallen fruit / windfalls
Y_invert_annual <- 16000   # insects, larvae, worms (key protein source)
Y_herb_annual   <-  7000   # herbaceous ground forage (low-energy filler)

# Worst-season (lean) daily yields  -- e.g. mid-winter, temperate:
Y_mast_lean     <- 14000
Y_fruit_lean    <-     0
Y_invert_lean   <-  3000
Y_herb_lean     <-  2500

# ----- Diversity multipliers (each 0 to 1) ----------------------------------
D_t  <- 0.60   # temporal-evenness: higher when species mature across the season
D_p  <- 0.80   # pollinator/invertebrate support: higher with early+late flowering

# ----- Accessibility and utilisation (each 0 to 1) --------------------------
A_hab   <- 0.55   # fraction of forage physically reachable by birds
A_breed <- 0.80   # fraction of reachable forage a breed actually captures
                  #   Red Rangers forage actively, so this sits high
eps     <- 0.70   # digestibility / metabolizable-energy utilisation fraction

# ----- Per-bird energy requirement  --  BROILER (Red Ranger) ----------------
# Demand splits into maintenance plus GROWTH (no egg production term).
W   <- 1.8     # representative mean live weight across the grow-out (kg).
               #   Red Rangers reach ~2.5-3 kg by ~11-12 wk; 1.8 kg is a
               #   mid-to-late representative weight for maintenance scaling.
ADG <- 35      # average daily liveweight GAIN (grams/bird/day).
               #   ~2.7 kg over ~77 days ~= 35 g/day. Adjust to your records.
G   <- ADG / 1000   # daily gain in kg/bird/day (used by the model)

a <- 100       # allometric maintenance coefficient (kcal per kg^0.75 per day)
b <- 3500      # energy cost per kg of liveweight gain (kcal/kg gain).
               #   Replaces the old egg-energy coefficient. Re-derive if you
               #   change strain or diet density.


## ===========================================================================
## 2. MODEL  --  edit below only to change the model itself
## ===========================================================================

carrying_capacity <- function(Y_mast, Y_fruit, Y_invert, Y_herb,
                              D_t, D_p, A_hab, A_breed, eps,
                              a, W, b, G) {

  Y_total <- Y_mast + Y_fruit + Y_invert + Y_herb        # sum_i Y_i (gross)

  supply  <- Y_total * D_t * D_p * A_hab * A_breed * eps # captured kcal/ha/day

  R_bird  <- (a * W^0.75) + (b * G)                      # per-bird kcal/day
                                                          #   maintenance + growth

  N <- supply / R_bird                                    # birds/ha (daily basis)

  list(Y_total = Y_total, supply = supply, R_bird = R_bird, N = N)
}

annual <- carrying_capacity(Y_mast_annual, Y_fruit_annual,
                            Y_invert_annual, Y_herb_annual,
                            D_t, D_p, A_hab, A_breed, eps, a, W, b, G)

lean   <- carrying_capacity(Y_mast_lean, Y_fruit_lean,
                            Y_invert_lean, Y_herb_lean,
                            D_t, D_p, A_hab, A_breed, eps, a, W, b, G)

N_annual      <- annual$N
N_lean        <- lean$N
N_sustainable <- min(N_annual, N_lean)   # the binding constraint


## ===========================================================================
## 3. OUTPUT
## ===========================================================================

cat("=====================================================\n")
cat(" Woodland broiler carrying capacity  --  Red Ranger\n")
cat("=====================================================\n")
cat(sprintf(" Woodland area             : %7.1f ha\n",            hectares))
cat(sprintf(" Mean grow-out weight W    : %7.2f kg\n",            W))
cat(sprintf(" Daily gain (ADG)          : %7.0f g/bird/day\n",    ADG))
cat(sprintf(" Per-bird requirement R    : %7.1f kcal/bird/day\n", annual$R_bird))
cat("-----------------------------------------------------\n")
cat(" ANNUAL AVERAGE\n")
cat("-----------------------------------------------------\n")
cat(sprintf(" Gross forage              : %7.0f kcal/ha/day\n",  annual$Y_total))
cat(sprintf("   Mast (nuts/acorns)      : %7.0f kcal/ha/day\n",  Y_mast_annual))
cat(sprintf("   Fruit / windfalls       : %7.0f kcal/ha/day\n",  Y_fruit_annual))
cat(sprintf("   Invertebrates           : %7.0f kcal/ha/day\n",  Y_invert_annual))
cat(sprintf("   Herbaceous              : %7.0f kcal/ha/day\n",  Y_herb_annual))
cat(sprintf(" Captured forage           : %7.0f kcal/ha/day\n",  annual$supply))
cat(sprintf(" N (per ha)                : %7.1f birds/ha\n",     N_annual))
cat(sprintf(" N (total, %.1f ha)        : %7.1f birds\n",        hectares, N_annual * hectares))
cat("-----------------------------------------------------\n")
cat(" LEAN SEASON (binding constraint)\n")
cat("-----------------------------------------------------\n")
cat(sprintf(" Gross forage              : %7.0f kcal/ha/day\n",  lean$Y_total))
cat(sprintf("   Mast (nuts/acorns)      : %7.0f kcal/ha/day\n",  Y_mast_lean))
cat(sprintf("   Fruit / windfalls       : %7.0f kcal/ha/day\n",  Y_fruit_lean))
cat(sprintf("   Invertebrates           : %7.0f kcal/ha/day\n",  Y_invert_lean))
cat(sprintf("   Herbaceous              : %7.0f kcal/ha/day\n",  Y_herb_lean))
cat(sprintf(" Captured forage           : %7.0f kcal/ha/day\n",  lean$supply))
cat(sprintf(" N (per ha)                : %7.1f birds/ha\n",     N_lean))
cat(sprintf(" N (total, %.1f ha)        : %7.1f birds\n",        hectares, N_lean * hectares))
cat("=====================================================\n")
cat(sprintf(" SUSTAINABLE N (per ha)    : %7.1f birds/ha\n",     N_sustainable))
cat(sprintf(" SUSTAINABLE N (TOTAL)     : %7.1f birds\n",        N_sustainable * hectares))
cat("   (lean season is the binding constraint)\n")
cat("=====================================================\n")


## ===========================================================================
## 4. SENSITIVITY -- one-at-a-time on the binding (lean) season
## ===========================================================================

sensitivity <- function(pct = 0.10) {
  base <- N_lean
  args0 <- list(Y_mast = Y_mast_lean, Y_fruit = Y_fruit_lean,
                Y_invert = Y_invert_lean, Y_herb = Y_herb_lean,
                D_t = D_t, D_p = D_p, A_hab = A_hab, A_breed = A_breed,
                eps = eps, a = a, W = W, b = b, G = G)
  vars <- c("D_t", "D_p", "A_hab", "A_breed", "eps", "W", "G")
  out <- sapply(vars, function(v) {
    args <- args0
    args[[v]] <- args[[v]] * (1 + pct)
    do.call(carrying_capacity, args)$N - base
  })
  round(out, 2)
}

sens <- sensitivity(0.10)

cat("\n-----------------------------------------------------\n")
cat(" Sensitivity: delta birds/ha from a +10% bump per input\n")
cat("-----------------------------------------------------\n")
for (nm in names(sens)) {
  cat(sprintf("  %-10s : %+.2f birds/ha   (%+.1f birds on %.1f ha)\n",
              nm, sens[nm], sens[nm] * hectares, hectares))
}
cat("-----------------------------------------------------\n")

top_lever <- names(sens)[which.max(abs(sens))]


## ===========================================================================
## 6. FINAL REPORT
## ===========================================================================

cat("\n=====================================================\n")
cat(" RECOMMENDATIONS\n")
cat("=====================================================\n")

cat("\n")
cat(strwrap(paste(
  "Note: all supply-side parameters (D_t, D_p, A_hab, A_breed, eps) are mathematically equal in leverage because they multiply together in the numerator. Where one dominates in your results, it is because that parameter has the most room left for proportional improvement given its current value. This allows for a basic analysis, generated below."
), width = 53, prefix = " ", initial = " "), sep = "\n")

cat("\n")
if (top_lever %in% c("D_t", "D_p")) {
  cat(strwrap(paste(
    "Tree species diversity and planting structure are your highest-return intervention.",
    "Extending the forage season through species selection (D_t) and supporting",
    "invertebrate populations via early and late flowering plants (D_p) will unlock",
    "more carrying capacity than any other single change you can make."
  ), width = 53, prefix = " ", initial = " "), sep = "\n")
} else if (top_lever %in% c("A_hab", "A_breed")) {
  cat(strwrap(paste(
    "Accessibility is your binding constraint. Focus on making more of the woodland",
    "physically reachable by birds (A_hab). Red Rangers already forage well, so",
    "A_breed has less headroom than in a sedentary commercial strain."
  ), width = 53, prefix = " ", initial = " "), sep = "\n")
} else if (top_lever == "eps") {
  cat(strwrap(paste(
    "Digestibility is the dominant factor. The woodland may be producing adequate",
    "energy but birds are not converting it efficiently. Focus on forage quality",
    "over quantity for the grow-out diet."
  ), width = 53, prefix = " ", initial = " "), sep = "\n")
} else if (top_lever == "W") {
  cat(strwrap(paste(
    "Mean grow-out weight is the most sensitive parameter. Because heavier birds",
    "require more maintenance energy, carrying a flock to a higher finish weight",
    "directly reduces how many birds the woodland can support. Processing at a",
    "lighter weight, or a shorter grow-out, raises stocking density."
  ), width = 53, prefix = " ", initial = " "), sep = "\n")
} else if (top_lever == "G") {
  cat(strwrap(paste(
    "Daily growth rate is your dominant cost driver. Faster gain carries a large",
    "energy overhead that competes directly with stocking density. A forage-only",
    "diet will tend to slow gain anyway; budget energy realistically rather than",
    "assuming concentrate-fed growth rates."
  ), width = 53, prefix = " ", initial = " "), sep = "\n")
}

cat("\n")
cat(sprintf(" RECOMMENDED STOCKING RATE : %7.1f birds/ha\n", N_sustainable))
