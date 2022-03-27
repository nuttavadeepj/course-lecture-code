# To-do Exercise for Fun

# Write a code to simulate a random rolling of two-die for 900 times. 
# Record a sum of the two face values, analyze and plot them, 
# using a simple histogram, box plot,with statistical values (mean, median, and mode).

set.seed(24)
sum_die <- sample(1:6, 900, replace = T) + sample(1:6, 900, replace = T)
sum_die
table(sum_die)
x <- sort(table(sum_die))
x
summary(sum_die)
#Find mode
y <- names(x[length(x)])
Mode <- as.integer(y)
Mode

plot(sum_die)
hist(sum_die)
boxplot(sum_die)

# In addition, find a probability of getting a sum of 4 of those 900 rolls.
#total sum of 4 is
x["4"]

#probability of getting a sum of 4 is
x["4"]/900
