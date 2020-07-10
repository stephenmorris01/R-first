# creates a list of user inputs

print("I will create a list of integers.")

l <- list()

n <- readline(prompt="Enter number of items : ")
n <- as.integer(n)

l <- list(n)

print("Enter each of the ", n, " numbers. Enter one at a time, each time followed by the 'Enter' key.")
for i in range(0, n):
  
  item<- int(input())

list.append(item) # adding the items

print("Here is your list of ", n, " numbers!")
print(list)