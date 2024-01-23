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
