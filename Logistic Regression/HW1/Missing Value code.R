##########################################################
################# Question 4 ##########################
##########################################################

#count of missing values by variable
colSums(is.na(insurance_t))

#proportional
round(colSums(is.na(insurance_t)/nrow(insurance_t)),2)

#missing values graph
missing.vals <- data.frame(unknowns = colSums(is.na(insurance_t)/nrow(insurance_t)))
missing.vals <- as.data.frame(missing.vals)
missing.vals$indx <- rownames(missing.vals)

#only looking at variables with missing values
mv.sorted <- missing.vals %>%
  filter(unknowns >0)

#bar chart --- reorder function puts bar graph in descending order
ggplot(data = mv.sorted, aes(x = reorder(indx, -unknowns), y = unknowns)) + 
  geom_bar(stat = 'identity', fill = "blue") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust =1)) +
  labs(x = "Missing Values", y = "Proportion of Missing Values", title = "Proportion of Missing Values by Variable")
