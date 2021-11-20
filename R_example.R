install.packages("tidyverse")
library(tidyverse)
view(starwars)

#1
shipNames<-filter(starwars,starships !="character(0)")
length(shipNames$name)
(shipNames$name)

#2
eye_color_f<-starwars%>%
  group_by(eye_color)%>%
  summarize(count=n())
arrange(eye_color_f,desc(count))

#3
#N/A(NULL) VARIABLES DELETED
no_null_list_x<-filter(starwars,birth_year !="N/A")
no_null_list<-filter(no_null_list_x,species !="N/A")
mean_age<-no_null_list %>%
  group_by(species) %>%
  summarize(
    mean_birth=mean(birth_year)
  )
a1<-arrange(mean_age,desc((mean_birth)))
head(a1,3) # oldest 3 species
a1 # mean age of all species

#3.1
#N/A(NULL) VARIABLES ARE NOT DELETED
mean_age<-starwars %>%
  group_by(species) %>%
  summarize(
    mean_birth=mean(birth_year)
  )
a1<-arrange(mean_age,desc(mean_birth))
head(a1,3) # oldest 3 species
a1 # mean age of all species

#4
my_char<-rbind(starwars,c(name="Sczk",height=183,mass=81,hair_color="black",skin_color="brunette",eye_color="brown",birth_year=23.0,sex="male",gender="masculine",homeworld="Dathomir",species="Zabrak","The Phantom Menace","Sith Speeder","Scimitar"))
my_char$height<-as.integer(my_char$height)
my_char$mass<-as.double(my_char$mass)
tail(my_char,1)

#5
my_char<-my_char %>%
  mutate(name,BMI=mass/((height/100)^2))

my_char<-my_char %>%
  mutate(name,health_hype = if_else(BMI>30, "obese",if_else(BMI>25,"overweight",if_else(BMI>18.5,"healthy","underweight"))))

view(my_char)

#6
cleaned_list<-na.omit(my_char)
cleaned_list$birth_year<-as.integer(cleaned_list$birth_year)
temp_list1<-filter(cleaned_list,birth_year<100.0)
ggplot(data=temp_list1)+
  geom_point(mapping=aes(x=BMI,y=birth_year))

#7
ggplot(data=cleaned_list)+
  geom_point(mapping=aes(x=birth_year,y=BMI))+
  geom_smooth(mapping=aes(x=birth_year,y=BMI))

ggplot(data=temp_list1,mapping=aes(x=birth_year,y=BMI))+
  geom_point()+
  geom_smooth(data=filter(temp_list1,birth_year<100 & BMI<100))

