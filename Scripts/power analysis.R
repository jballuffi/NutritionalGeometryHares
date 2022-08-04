library('pwr')

tCA <- trials[Diet == "A" | Diet == "C"]

tCD <- trials[Diet == "C" | Diet == "D"]

summary(lm(tCA$Weight_change ~ tCA$Diet))


summary(lm(tCD$Weight_change ~ tCD$Diet))


#calc cohen's d at https://www.socscistatistics.com/effectsize/default3.aspx

#power test on t-test between C and D
pwr.t.test(n = NULL, d = 0.264655, sig.level = 0.05, power = .8, type = "paired", alternative = "greater")

#power test if we remove the weight gaining bun in diet D becomes an outlier
pwr.t.test(n = NULL, d = .559617, sig.level = 0.05, power = .8, type = "two.sample", alternative = "greater")

#power test if we remove outlier and do a paired analysis
pwr.t.test(n = NULL, d = .559617, sig.level = 0.05, power = .8, type = "paired", alternative = "greater")

#power test on A vs C
pwr.t.test(n = NULL, d = 1.79988, sig.level = 0.05, power = .8, type = "two.sample", alternative = "greater")


#power analysis for anova
pwr.anova.test(k = 4, f = .4, sig.level = 0.05, power = 0.8)
