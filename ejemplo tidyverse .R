mpg %>% 
  add_row(manufacturer ="jeep",
          displ=2,
          year=2008,
          cyl=4,
          trans ="manual(m6)",
          cty = 15,
          hwy = 24,
          fl ="p",
          class = "suv"
          )
mpg %>% 
  mutate(
    cty = cty * 1.609 / 3.785,
    hwy = hwy * 1.609 / 3.785,
    manufacturer =as_factor(manufacturer),
    model = as_factor(model)
  )
mpg_editada= mpg %>% 
  mutate_if(
    .predicate = is.character,
    .funs = as_factor
  )
mpg_editada

mpg_editada2= mpg %>% 
  mutate_if(
    .predicate = is.character,
    .funs = as_factor
  ) %>% 
  mutate(trans= fct_lump_min(trans, 20,
                             other_level ="otros"))
mpg_editada2

?fct_lump_min

plot(cars)

mpg_editada2 %>% 
  ggplot()+
  geom_point(mapping =aes(x =displ, y=hwy))


mpg_editada2 %>% 
  ggplot()+
  geom_point(mapping =aes(x=displ,
                          y = hwy,
                          color =class)
             )

ggplot(data =mpg_editada2)+
  geom_point(mapping =aes (x=displ,
                           y= hwy,
                           color = class,
                           shape = drv,
                           size = cyl),
             alpha= 0.7)
library(patchwork)

mpg_editada %>% 
  ggplot(aes(x=manufacturer, y=displ))+
  geom_boxplot()

library(tsibble)

data(world_bank_pop, package = "tidyr")
pop=world_bank_pop
head(pop)

pop_tidy = pop %>% 
  pivot_longer(cols=-c(country, indicator),
               names_to = "year", values_to = "value") %>% 
  
  pivot_wider(names_from = indicator,
              values_from = value) %>% 
  
  select (country, year, contains ("TOTL")) %>% 
  
  rename(urban_pop = SP.URB.TOTL, total_pop = SP.POP.TOTL) %>% 
  
  mutate(rural_pop_pct = (1 -urban_pop / total_pop)*100,
         country = as_factor(country),
         year = as.integer(year)
         ) %>% 
  
  filter(country %in% c("MEX","BRA","ARG")) %>% 
  
  as_tsibble(key = country, index=year)

pop_tidy

install.packages("tsibble")

install.packages("fable")
library(fable)
pop <- read_csv("https://raw.githubusercontent.com/daniel-nuno/time_series_s2024/main/Intro%20a%20R/world_bank_pop.csv")

pop_train = pop_tidy %>% 
filter(year <= 2009)
pop_query = pop_tidy %>% 
  filter(year > 2009 & year <= 2013)
pop_train_quey = pop_tidy %>% 
  filter(year <= 2013)

pop_train %>% 
  autoplot(total_pop) + ggtitle("Total population")+
            ylab("")

pop_train %>% 
  autoplot(rural_pop_pct)+ ggtitle("Rural population (%)")+
  ylab("")

pop_fit = pop_train %>% 
  model('RW w/ drift '=RW(rural_pop_pct ~ drift()),
        'TSLM w/ trend '= TSLM(rural_pop_pct ~ trend()),
        ETS= ETS (rural_pop_pct ~ error("A")+ trend("A")+ season ("N"))
        )
tidy(pop_fit)

pop_fcst = pop_fit %>% 
  forecast(h = " 4 years")

pop_fcst %>% 
  autoplot(pop_train_query)+
  facet_grid(cols= vars(.model), rows=vars(country), scales ="free_y")+
  guides(color =FALSE)+
  ylab("Rural population (%)")

pop_fit2 <- pop_train %>%
  model('RW w/ drift' = RW(rural_pop_pct  ~ drift()),
        'TSLM w/ trend' = TSLM(rural_pop_pct ~ trend()),
        ETS = ETS(rural_pop_pct ~ error('A') + trend('Ad') + season('N')))
pop_fit

pop_fcst2 <- pop_fit2 %>%
  forecast(h = "4 years")


accuracy(pop_fcst2,pop_train_query) %>% 
  arrange(country,MAPE)


pop_train %>% 
  model(ETS = ETS(rural_pop_pct ~error("A")+trend("Ad")+season("N"))
        ) %>% 
  forecast(h="12 years") %>% 
  autoplot(pop_tidy)+
  geom_vline(xintercept = 2014, linetype="dashed", color="red")+
  ylab("Rural population(%)")
