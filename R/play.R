# Set a seed for reproducibility
set.seed(123)

# Number of simulated households
n <- 100

# Define regions with approximate probabilities (example proportions)
regions <- c("Brunei-Muara", "Tutong", "Belait", "Temburong")
region_probs <- c(0.74, 0.12, 0.12, 0.02)

# Define types of internet connection
connection_types <- c("Fixed Broadband", "Mobile Broadband", "Both")
conn_probs <- c(0.62, 0.08, 0.30)

# Create the fake dataset
aiti_data <- data.frame(
  Household_ID = 1:n,
  Region = sample(regions, n, replace = TRUE, prob = region_probs),
  Household_Size = sample(1:8, n, replace = TRUE),
  Internet_Access = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.95, 0.05)),
  Type_of_Connection = sample(connection_types, n, replace = TRUE, prob = conn_probs),
  Monthly_Expenditure = round(runif(n, min = 20, max = 100), 2),  # cost in BND
  Data_Usage = round(runif(n, min = 200, max = 800), 1),  # in GB
  Satisfaction = sample(1:5, n, replace = TRUE)
)

# View the first few rows
head(aiti_data)



library(ggalluvial)
library(dplyr)

# Suppose `aiti_data` is your fake data.frame
# Add buckets:
aiti_data2 <- aiti_data %>%
  mutate(
    SizeCat = cut(Household_Size, breaks = c(0,2,5, Inf), labels = c("Small","Medium","Large")),
    SatCat  = cut(Satisfaction, breaks = c(0,2,3,5), labels = c("Low","Med","High"))
  )

ggplot(aiti_data2,
       aes(axis1 = SizeCat, axis2 = Internet_Access, axis3 = Type_of_Connection,
           y = ..count..)) +
  geom_alluvium(aes(fill = SizeCat), width = 1/12) +
  geom_stratum(width = 1/12, fill = "grey", color = "white") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Household Size","Has Internet?","Conn. Type"), expand = c(.05,.05)) +
  theme_minimal() +
  labs(title = "Household Size → Internet Access → Connection Type",
       y = "Number of Households")