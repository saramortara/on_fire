# Making species inforgraphics

sp.status <- c(6, 27, 30)
names(sp.status) <- c("Critically Endangered", "Endangered", "Vulnerable")

# packages

library(waffle)

waffle(sp.status, rows=6)

parts <- sp.status

