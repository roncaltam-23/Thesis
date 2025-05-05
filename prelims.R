#Read the dataset
thesis <- read.csv("~/data.csv")

# omit missing values
thesis <- na.omit(thesis)

# Load necessary libraries
library(stargazer)

#Drop Variables
thesis <- thesis[, !(names(thesis) %in% c("GDPpc2","PopD","Trade", "AgriLU", "RegQ", "RLaw"))]

# Create a clean Descriptive Statistics Table
stargazer(
  thesis[, !(names(thesis) %in% c("Country", "Year", "GDPpc2"))],  # Remove unwanted variables
  type = "text",        
  title = "Descriptive Statistics of Key Variables", 
  digits = 3,           # Number of decimals
  summary.stat = c("mean", "sd", "min", "median", "max"),  # Statistics you want
  out.header = FALSE,   # No extra header
  initial.zero = TRUE   # Show leading 0 before decimal
)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#converting the data into a panel dataset
  
library(plm)
library(stargazer)

paneldata <- pdata.frame(thesis, index = c("Country", "Year")) 
pdim(paneldata)

stargazer(paneldata, type = "text", title = "Descriptive Statistics of Panel Data", digits = 3) 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Exploratory Descriptive Analysis
library(dplyr)
library(ggplot2)
library(ggthemes)

#logging GDPpc and RDef
log <- thesis %>% mutate(log_RDef = RDef, log_GDPpc = log(GDPpc))

#A. Variables' visual presentation over time per country

#Rate of Deforestation
ggplot(log, aes(x = Year, y = log_RDef)) +
  geom_hline(yintercept = 0, lty = "dashed") + 
  geom_line() +
  facet_wrap(~ Country, scales = "free_x") +
  scale_x_continuous(breaks = seq(2008,2020,4)) +
  theme_foundation() + 
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )
  ggtitle("Rate of Deforestation Over Time by Country") +
  xlab("Year") +
  ylab("Rate of Deforestation")

#GDP per capita
ggplot(log, aes(x = Year, y = log_GDPpc, color = Country)) +
  geom_line() +
  facet_wrap(~ Country, scales = "free_y") +
  scale_x_continuous(breaks = c(2010, 2015, 2020)) +
  theme_minimal() +
  ggtitle("GDP per capita over time by Country") +
  xlab("Year") +
  ylab("GDP per capita")

#Share of Renewable Energy
ggplot(log, aes(x = Year, y = RENEW, color = Country)) +
  geom_line() +
  facet_wrap(~ Country, scales = "free_y") +
  scale_x_continuous(breaks = c(2010, 2015, 2020)) +
  theme_minimal() +
  ggtitle("Share of Renewable Energy Over Time by Country") +
  xlab("Year") +
  ylab("Share of Renewable Energy")

#Foreign Direct Investment
ggplot(log, aes(x = Year, y = FDI, color = Country)) +
  geom_line() +
  facet_wrap(~ Country, scales = "free_y") +
  scale_x_continuous(breaks = c(2010, 2015, 2020)) +
  theme_minimal() +
  ggtitle("Foreign Direct Investment Over Time by Country") +
  xlab("Year") +
  ylab("FDI")

#Wood Exports
ggplot(log, aes(x = Year, y = WoodEx, color = Country)) +
geom_line() +
  facet_wrap(~ Country, scales = "free_y") +
  scale_x_continuous(breaks = c(2010, 2015, 2020)) +
  theme_minimal() +
  ggtitle("Wood Exports Over Time by Country") +
  xlab("Year") +
  ylab("Wood Exports")

#Control of Corruption
ggplot(log, aes(x = Year, y = CCor, color = Country)) +
  geom_line() +
  facet_wrap(~ Country, scales = "free_y") +
  scale_x_continuous(breaks = c(2010, 2015, 2020)) +
  theme_minimal() +
  ggtitle("Control of Corruption Over Time by Country") +
  xlab("Year") +
  ylab("Control of Corruption")


#B. DV vs. IV Visual Presentation

library(ggplot2)
library(dplyr)
library(scales)

#RDef-GDPpc
ggplot(log, aes(x = log_GDPpc, y = log_RDef)) +
  geom_line(aes(group = Country), color = "gray60", linewidth = 0.8) +  # Connect points
  geom_point(color = "gray30", size = 2) +
  facet_wrap(~ Country, scales = "free") +
  labs(
    title = "Relationship between Deforestation Rates and GDP per Capita",
    subtitle = "Per Country (ASEAN Panel Data)",
    x = "GDP per Capita",
    y = "Deforestation Rate"
  ) +
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "grey90", color = "grey50"),
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(1, "lines")
  )

#RDef-FDI
ggplot(log, aes(x = FDI, y = log_RDef)) +
  geom_line(aes(group = Country), color = "gray60", linewidth = 0.8) +  # Connect points
  geom_point(color = "gray30", size = 2) +
  facet_wrap(~ Country, scales = "free") +
  labs(
    title = "Relationship between Deforestation Rates and FDI (% of GDP)",
    subtitle = "Per Country (ASEAN Panel Data)",
    x = "Foreign Direct Investment (% of GDP)",
    y = "Deforestation Rates"
  ) +
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "grey90", color = "grey50"),
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(1, "lines")
  )

#RDef-RENEW
ggplot(log, aes(x = RENEW, y = log_RDef)) +
  geom_line(aes(group = Country), color = "gray60", linewidth = 0.8) +  # Connect points
  geom_point(color = "gray30", size = 2) +
  facet_wrap(~ Country, scales = "free") +
  labs(
    title = "Relationship between Deforestation Rates and Share of Renewable Energy",
    subtitle = "Per Country (ASEAN Panel Data)",
    x = "Share of Renewable Energy Consumption",
    y = "Deforestation Rates"
  ) +
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "grey90", color = "grey50"),
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(1, "lines")
  )

#RDEf-WoodEx
ggplot(log, aes(x = WoodEx, y = log_RDef)) +
  geom_line(aes(group = Country), color = "gray60", linewidth = 0.8) +  # Connect points
  geom_point(color = "gray30", size = 2) +
  facet_wrap(~ Country, scales = "free") +
  labs(
    title = "Relationship between Deforestation Rates and Wood Exports",
    subtitle = "Per Country (ASEAN Panel Data)",
    x = "Wood Exports",
    y = "Deforestation Rates"
  ) +
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "grey90", color = "grey50"),
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(1, "lines")
  )

#RDef-CCor
ggplot(log, aes(x = CCor, y = log_RDef)) +
  geom_line(aes(group = Country), color = "gray60", linewidth = 0.8) +  # Connect points
  geom_point(color = "gray30", size = 2) +
  facet_wrap(~ Country, scales = "free") +
  labs(
    title = "Relationship between Deforestation Rates and Control of Corruption",
    subtitle = "Per Country (ASEAN Panel Data)",
    x = "Control of Corruption",
    y = "Deforestation Rates"
  ) +
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "grey90", color = "grey50"),
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(1, "lines")
  )

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Regression Analysis
  
#A. Pooled OLS
Pooled <- plm(RDef ~ GDPpc + FDI + WoodEx + RENEW + CCor, data = paneldata, index = c("Country", "Year"), 
                model = "pooling")
summary(Pooled)

#B. Random Effects (Wallace-Hussain Transformation)
Random <- plm(RDef ~ GDPpc + FDI + WoodEx + RENEW + CCor, data = paneldata, index = c("Country", "Year"), 
              model = "random", random.method = "walhus")
summary(Random)

#C. Fixed Effects
Fixed <- plm(RDef ~ GDPpc + FDI + CCor + WoodEx + RENEW , data = paneldata, index = c("Country", "Year"), 
             model = "within") 
summary(Fixed)

#Generate Well-Formatted Table with Proper Labels
stargazer(Pooled, Random, Fixed, type = "text",
          title = "Panel Regression Results",
          column.labels = c("Pooled", "Random", "Fixed"),
          digits = 3, align = TRUE)

# 1. Breusch-Pagan Lagrange Multiplier test (Testing: Pooled OLS vs. Random Effects)
plmtest(Random, type = "bp")  

Null Hypothesis (H₀): There are no panel (individual-specific) effects. Pooled OLS is appropriate.
Alternative Hypothesis (H₁): There are significant panel effects. Random Effects model is preferred.

Result:chisq = 83.555, p-value < 2.2e-16 

Decision: Reject H₀.
Conclusion: Random Effects model is better than Pooled OLS. 
There are significant individual effects in your data.

# 2. Pooled vs Fixed Effects (Testing: Pooled OLS vs. Fixed Effects)
pFtest(Fixed, Pooled)

Null Hypothesis (H₀):All individual effects are zero (no fixed effects needed). Pooled OLS is appropriate.
Alternative Hypothesis (H₁): At least one individual effect is non-zero (Fixed Effects model is needed).

Result: F = 19.168, p-value = 6.377e-10 

Decision: Reject H₀.
Conclusion: Fixed Effects model is more appropriate than Pooled OLS.

# 3. Fixed vs Random Effects (Hausman Test)
phtest(Fixed, Random)

Null Hypothesis (H₀):Random Effects estimator is consistent and efficient → RE is appropriate.

Alternative Hypothesis (H₁):Random Effects estimator is inconsistent → prefer Fixed Effects.

Result: chisq = 0.98819, p-value = 0.9635 → high p-value.

Decision: Fail to reject H₀.
Conclusion: Random Effects model is appropriate; it is consistent and more efficient.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Post Estimation Analysis
  
library(lmtest)
library(plm)
library(car)

#A. Variance Inflation Factor
vif(lm(RDef ~ GDPpc + RENEW + WoodEx + FDI + CCor, data = paneldata))

Purpose: Detect multicollinearity among the explanatory variables.
Rule of Thumb:
  VIF > 10: High multicollinearity (problematic)

#B. Breusch-Pagan Test for heteroscedasticity
bptest(Random)

Purpose: Test if residuals have constant variance (homoskedasticity).
Null Hypothesis (H₀): Homoskedasticity (constant variance of errors)
Alternative Hypothesis (H₁): Heteroskedasticity (variance of errors is not constant)
Results:BP = 7.4212, p-value = 0.1911
Interpretation: Since p-value > 0.05, fail to reject H₀.
Conclusion: No evidence of heteroskedasticity. 

#C. Breusch-Godfrey/Wooldridge test for serial correlation in panel models
pbgtest(Random)  

Purpose: Checks autocorrelation in the panel data errors.
Null Hypothesis (H₀): No serial correlation in the idiosyncratic errors
Alternative Hypothesis (H₁): Serial correlation exists
Results:chisq = 5.4916, df = 13, p-value = 0.9627
Interpretation: fail to reject H₀.
Conclusion: No evidence of serial correlation in your model residuals.

#D. Cross-sectional Dependence (Pesaran's CD test)
pcdtest(Random, test = "cd")

Purpose: Check if errors across cross-sectional units (countries) are correlated.
Null Hypothesis (H₀): No cross-sectional dependence (errors are independent across panels)
Alternative Hypothesis (H₁): Presence of cross-sectional dependence
Results: z = -1.5315, p-value = 0.1257
Interpretation: p-value > 0.05 → fail to reject H₀.
Conclusion: No significant cross-sectional dependence found.
