print("Welcome")
rand_int = sample(1:1000,size = 1)
guess = 0
while(guess != rand_int){
  guess = readline(prompt = "Guess the number: ")
  if(guess == rand_int){
    print("You win!")
  }
  else{
    if(guess > rand_int){
      print("Guess is Too high")
    }
    else{
      print("Guess is Too low")
    }
  }
}
print("Game is Over")



