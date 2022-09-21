# Simulating data
set.seed(1994)
N <- 100
simData <- data.frame(Genotype = sample(c("AA", "Aa", "aa"), size = N, replace = T),
                      Handedness = sample(c("Right", "Left"), size = N, replace = T),
                      Netflix = sample(c("Yes", "No"), size = N, replace = T),
                      Class = sample(c("Freshman", "Sophomore", "Junior", "Senior"), size = N, replace = T, prob = c(0.40, 0.20, 0.15, 0.15)),
                      nPets = rpois(n = N, lambda = 1),
                      Height = rnorm(n = N, mean = 66.5, sd = 3)) %>% mutate(Weight = 190 + scale(Height)*10 + rnorm(n = N, mean = 0, sd = 8), Age = 17 + ifelse(Class == "Freshman", 0, ifelse(Class == "Sophomore", 1, ifelse(Class == "Junior", 3, 4))) + rpois(n = N, lambda = 2))
