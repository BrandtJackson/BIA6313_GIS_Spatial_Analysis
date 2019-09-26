#Week 2: Functions, Loops & Spatial Data

#HOW THIS WORKS  

#I am going to code live. You will follow along. 
#If you fall behind and need to catch up, you can copy the required code snippets from here: 
#http://bit.ly/2D0bMH3 
#which is a shorter URL of this longer hyperlink: 
#https://www.dropbox.com/s/isiizuehbfn5eaq/Week_2_R_functions.R?dl=0   
#All you have to do is refresh your browser. 

#Let's get started!

#Load gapminder package. 
#If you haven't seen the gapminder video, then be sure to do so: https://youtu.be/hVimVzgtD6w


library(gapminder)

str(gapminder)

min(gapminder$lifeExp)
max(gapminder$lifeExp)
range(gapminder$lifeExp)

