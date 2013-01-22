Aparking = c()
Bparking = c()
Cparking = c()
for (i in 1:10000) {
  
  cars = c("X","X","X","X","X","X","X","X","X","X","X","X","X","X","X","X","X","X","X","X")
  road = c(" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," ")
  #lot = c(road,cars,cars,road,cars,cars,road,cars,cars,road)
  lot = c(road,cars,cars,road)
  
  starting_lot = matrix(data = lot, nrow = 4, ncol=20, byrow = T)
  
  way_to_go = c(1,0,0,-1,0,0,1,0,0,-1)
  #way_to_turn = c(c(9,0,0,-3,0,0,-3,0,0,-3), c(3,0,0,-3,0,0,3,0,0,-3), c(9,0,0,-3,0,0,3,0,0,-3))
  way_to_turn = c(c(3,0,0,-3,0,0,-3,0,0,-3), c(3,0,0,-3,0,0,3,0,0,-3), c(3,0,0,-3,0,0,3,0,0,-3))
  
  time = 1
  cars = c("C","B","A")
  carcol = c(0,0,0)
  carrow = c(0,0,0)
  carsparked=0
  timeittooktopark = c(0,0,0)
  for (i in 1:3) {
    spot_row = sample(c(1,4),1)
    spot_col = sample(1:20,1)
    while (starting_lot[spot_row,spot_col] != " ") {
      spot_row = sample(c(1,4),1)
      spot_col = sample(1:20,1)
    }
    carcol[i] = spot_col
    carrow[i] = spot_row
  }
  
  
  
  starting_lot[carrow[1],carcol[1]] = "C"
  starting_lot[carrow[2],carcol[2]] = "B"
  starting_lot[carrow[3],carcol[3]] = "A"
  while (carsparked != 3) {
    time = time+1
    #print(time)
    if (time%%100 == 0) {
      spot_row = sample(c(2,3),1)
      spot_col = sample(1:20,1)
      while (starting_lot[spot_row,spot_col] != "X") {
        spot_row = sample(c(2,3),1)
        spot_col = sample(1:20,1)
      }
      starting_lot[spot_row,spot_col] = " "
    }
    for (i in 1:3) {     
      if (carrow[i] == 2 || carrow[i] == 3) {
      }
      else if (i != 3 && carrow[i]+1 < 5 && starting_lot[carrow[i]+1,carcol[i]] == " ") {
        #print("I'm parking1")
        carsparked = carsparked + 1
        starting_lot[carrow[i],carcol[i]] = " "
        starting_lot[carrow[i] + 1,carcol[i]] = cars[i]
        carrow[i] = carrow[i] + 1
        timeittooktopark[i] = time
      }
      else if (i != 2 && carrow[i]-1 > 0 && starting_lot[carrow[i]-1,carcol[i]] == " ") {
        #print("I'm parking2")
        carsparked = carsparked + 1
        starting_lot[carrow[i],carcol[i]] = " "
        starting_lot[carrow[i] - 1,carcol[i]] = cars[i]
        carrow[i] = carrow[i] - 1
        timeittooktopark[i] = time
      }
      else if (carrow[i] == 1 && carcol[i] == 20){
        starting_lot[carrow[i],carcol[i]] = " "
        carrow[i] = way_to_turn[(i-1)*10+carrow[i]] + carrow[i]
        starting_lot[carrow[i],carcol[i]] = cars[i]
      }
      else if (carrow[i] == 4 && carcol[i] == 1){
        starting_lot[carrow[i],carcol[i]] = " "
        carrow[i] = way_to_turn[(i-1)*10+carrow[i]] + carrow[i]
        starting_lot[carrow[i],carcol[i]] = cars[i]
      }
      else if (starting_lot[carrow[i],carcol[i]+way_to_go[carrow[i]]] == " ") {
        starting_lot[carrow[i],carcol[i]] = " "
        starting_lot[carrow[i],carcol[i]+way_to_go[carrow[i]]] = cars[i]
        carcol[i] = carcol[i]+way_to_go[carrow[i]]
      }
    }
    
    #print(starting_lot)
  }
  #print(starting_lot)
  #print(timeittooktopark)
  Aparking = append(Aparking, timeittooktopark[3])
  Bparking = append(Aparking, timeittooktopark[2])
  Cparking = append(Aparking, timeittooktopark[1])
  print(length(Aparking))
}
print(mean(Aparking))
print(mean(Bparking))
print(mean(Cparking))