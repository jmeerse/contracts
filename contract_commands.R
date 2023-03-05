library(tidyverse)
library(nflreadr)
library(ggrepel)

all_con <- load_contracts()

twocon <- all_con %>% group_by(otc_id) %>% summarise(n = n()) %>% filter(n >= 2) %>% ungroup()

two_con <- all_con %>% filter(otc_id %in% twocon$otc_id)


#### do everything for all positions, not just qbs ####
two_con <- two_con %>% filter(year_signed > 0)

two_con <- two_con[
  with(two_con, order(otc_id, year_signed)),
] #sorts dataframe to group players together 

two_con <- two_con %>% group_by(otc_id, year_signed) %>% 
  filter(value == max(value, na.rm = T))

twocon <- two_con %>% 
  group_by(otc_id) %>% 
  summarise(n = n()) %>% 
  filter(n >= 2) %>% ungroup()

two_con <- two_con %>% filter(otc_id %in% twocon$otc_id)

two_con <- transform(two_con, contract = ave(otc_id, otc_id, FUN = seq_along))

two_con$contract <- as.numeric(two_con$contract)

two_con %>% ggplot(aes(x= year_signed, 
                       y = value, 
                       group = otc_id,
                       color = position)) + 
  geom_line()


two_con <- two_con %>%
  group_by(otc_id) %>%
  mutate(Diff = value - lag(value)) #increase/decrease in total value - should look at apy instead?

#ave increase by position
contract2 <- two_con %>% group_by(position) %>% 
  filter(contract == 2) %>% 
  summarise(mean_inc = mean(Diff, na.rm = T),
            median_inc = median(Diff, na.rm = T),
            mean_apy = mean(apy, na.rm = T),
            median_apy = median(apy, na.rm = T),
            n = n())

contract2 %>% ggplot(aes(x = mean_inc,  y = reorder(position, mean_inc))) + geom_col()

contract2 %>% ggplot(aes(x = median_inc,  y = reorder(position, median_inc))) + geom_col()

contract2 %>% ggplot(aes(x = mean_inc, y = median_inc)) + geom_point()                     

contract2 %>% ggplot(aes(x = mean_inc, y = median_inc)) + 
  geom_point() +
  geom_text_repel(aes(label = position)) 


#### apy ####
two_con %>% ggplot(aes(x = year_signed, 
                       y = apy, 
                       group = otc_id, 
                       color = position)) + 
  geom_line() +
  facet_wrap(~position)

contract2 %>% ggplot(aes(x = mean_apy,  y = reorder(position, mean_apy))) + geom_col()

contract2 %>% ggplot(aes(x = median_apy,  y = reorder(position, median_apy))) + geom_col()

contract2 %>% ggplot(aes(x = mean_apy, y = median_apy)) + 
  geom_point() +
  geom_text_repel(aes(label = position))


two_con %>% filter(position == "QB") %>% ggplot(aes(x = contract, y = apy)) + geom_point()

two_con %>% ggplot(aes(x = position, y = apy)) + geom_boxplot()

# ideas: look at draft position/round; look at % increase; separate starters; look at final year; look at length of contracts (what contract says vs time to next contract)
fitz <- two_con %>% filter(player == "Ryan Fitzpatrick")
two_con %>% ggplot(aes(x = year_signed)) + geom_histogram() #see how many contracts were signed each year

