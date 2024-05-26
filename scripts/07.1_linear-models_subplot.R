# Created: 2024-05-26
# Last updated: 2024-05-26

# Purpose: Run generalized linear models.


# Count -------------------------------------------------------------------

unique(subplot$PlantSource)
unique(subplot$PlantSource2)

glm.all <- glm(Count ~ Perc_dev_cum + AridityIndex + Treatment + Weedy,
               data = pitseed, family = "poisson")
summary(glm.all)
#   no overdispersion according to residual deviance/degrees freedom

glm.all2 <- glm(Count ~ Perc_dev_cum + AridityIndex + Treatment + PlantSource2,
                data = pitseed, family = "poisson")
summary(glm.all2)


glm.nb.all <- manyglm(Count ~ Perc_dev_cum + AridityIndex + Treatment + Weedy,
                      data = pitseed, family = "negative.binomial")
coefficients(glm.nb.all)