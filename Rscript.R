################################ SETUP #####################################
# Load the devtools package, or install devtools if this package is not already installed
if(!require(devtools)){install.packages("devtools"); library(devtools)}

# Install and load the bwrappers package
install_github("michaelkardas/bwrappers")
library(bwrappers)

# Load the sample data set
bdata <- bwrappers::bdata

################################ SAMPLE CODE #####################################
bdata$IV1 <- factor(bdata$IV1)
bdata$IV2 <- factor(bdata$IV2)
bdata$IV3 <- factor(bdata$IV3)
bdata$DV2 <- factor(bdata$DV2)

### Functions For Data Cleaning ###
# Splitting bdata Frames
wrap.split(df = bdata, iv1 = IV1)
wrap.split(df = bdata, iv1 = IV1, iv2 = IV2)

# Merging Columns
wrap.merge(df = bdata)

# Generating Columns
wrap.generate(df = bdata, string1 = "T1", string2 = "T2", operation = "difference")

### Functions For Data Analysis ###
# Descriptive Statistics
wrap.desc(dv1 = bdata$DV5)
wrap.desc(dv1 = bdata$DV5, iv1 = bdata$IV2)

# T Tests (One Sample)
wrap.t.one(dv1 = bdata$DV5, mu = 5)

# T Tests (Independent Samples)
wrap.t.ind(dv1 = bdata$DV5, iv1 = bdata$IV1)

# T Tests (Paired Samples)
wrap.t.pair(dv1 = bdata$DV3_T1, dv2 = bdata$DV3_T2)

# ANOVA (Main Effects & Interaction Effects)
wrap.anova(dv1 = bdata[c(6, 8)]) # 1 within-subjects factor
wrap.anova(dv1 = bdata$DV5, iv1 = bdata$IV1, iv2 = bdata$IV2) # 2 between-subjects factors
wrap.anova(dv1 = bdata[c(6, 8)], iv1 = bdata$IV1, iv2 = bdata$IV2) # 1 within-subjects factor; 2 between-subjects factors

# ANOVA (Planned Contrasts)
wrap.planned(dv1 = bdata$DV5, iv1 = bdata$IV2, levels = c("PhotoA", "PhotoB"), weights = c(-1, 1))
wrap.planned(dv1 = bdata$DV5, iv1 = bdata$IV2, levels = c("PhotoA", "PhotoB", "PhotoC"), weights = c(-1, 0.5, 0.5))

# ANOVA (Simple Main Effects)
wrap.simple(dv1 = bdata$DV5, iv1 = bdata$IV1, iv2 = bdata$IV2)

# Linear Regression
wrap.lm(formula = bdata$DV7 ~ bdata$DV5 * bdata$DV6, standardized = FALSE)
wrap.lm(formula = bdata$DV7 ~ bdata$DV5 * bdata$DV6, standardized = TRUE)

# Correlation Tests
wrap.cor(dv1 = bdata$DV3_T1, dv2 = bdata$DV3_T2)

# Chi-Square Tests
wrap.chi(dv1 = bdata$DV2) # One-way goodness-of-fit test
wrap.chi(dv1 = bdata$DV2, iv1 = bdata$IV2) # Two-way contingency test

# Levene's Test for Equality of Variances
wrap.levene(bdata$DV5,bdata$IV2)

### Functions For Data Visualization ###
# Bar Plots
wrap.bar(dv1 = bdata[c(10:12)],ylim=c(0,10),ymajor=2) # 1 within-subjects factor
wrap.bar(dv1 = bdata$DV5, iv1 = bdata$IV1, iv2 = bdata$IV2,ylim=c(0,10),ymajor=2) # 2 between-subjects factors
wrap.bar(dv1 = bdata[c(10:12)], iv1 = bdata$IV1, iv2 = bdata$IV3,ylim=c(0,10),ymajor=2) # 1 within-subjects factor; 2 between-subjects factors

# Line Plots
wrap.line(dv1 = bdata[c(6,8)], iv1 = bdata$IV1, iv2 = bdata$IV2,ylim=c(0,10),ymajor=2) # 1 within-subjects factor; 2 between-subjects factors

# Histograms
wrap.hist(dv1 = bdata$DV1, likert = FALSE)
wrap.hist(dv1 = bdata$DV5, likert = TRUE)
