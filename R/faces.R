




#' @title Here
#' @description Yay
#' @author Me
#' @export
#' @example 2X2

vector_eater = function(x, z) {
  temp_length <- length(x)
  if (temp_length > length(z)) {
    temp_length <- length(z)
  }
  for (i in 1:temp_length) {
    z[i] = x[i]
  }

  z

}



#' @export

vector_setter = function(x, y, z) {
  counter = 1

  temp_length <- length(z)
  if (temp_length > length(x)) {
    #temp_length <- length(x)
  }
  for (i in y) {
    if (i <= temp_length) {
      if (length(x) >= counter) {
        z[i] = x[counter]
        counter = counter + 1
      } else{

      }

    } else{

    }

  }

  z

}






#' @export
vector_chooser = function(x, y, z) {
  counter = 1

  temp_length <- length(x)
  if (temp_length > length(z)) {
    temp_length <- length(z)
  }
  for (i in y) {
    if (i <= temp_length) {
      z[counter] = x[i]
      counter = counter + 1
    } else{

    }

  }

  z


}



#' @export

vector_face = function(xam){

  presets <- c(1,0.2, 0.2, 0.2, 0.3, 0.35, 0.55, 0.15, 0.25, 0.4, 0.4)

  x <- xam[1] * 2.0



  left_eye_size = (20 + (xam[2]*presets[2]))/1000
  eyelv = (175 + xam[3]*presets[3])/100
  right_eye_size = (20 + (xam[4]*presets[4]))/1000
  eyerv = (205 + (xam[5]*presets[5]))/100
  eye_height  = (255 + (xam[6]*presets[6]))/100
  mouth_height  = (175 + (xam[7]*presets[7]))/100
  mouth_width = (25 + (xam[8]*presets[8]))/100
  mouth_thickness  = (5 + (xam[9]*presets[9]))/1000
  left_eye_offset = (-20 + (xam[10]*presets[10]))/100
  right_eye_offset = (-20 + (xam[11]*presets[11]))/100

  ramp <- colorRamp(c("#F5E704", "brown"))
  skin_colors <- rgb( ramp(seq(0, 1, length = 100)), max = 255)
  skin_color <- skin_colors[xam[12]]# skin_colors[sample(1:100, 1)]

  ramp <- colorRamp(c("pink", "gray"))
  back_colors <- rgb( ramp(seq(0, 1, length = 100)), max = 255)
  background_color <- back_colors[xam[13]]# back_colors[sample(1:100, 1)]

  ramp <- colorRamp(c("purple", "green"))
  shirt_colors <- rgb( ramp(seq(0, 1, length = 100)), max = 255)
  shirt_color <-  shirt_colors[xam[14]]# shirt_colors[sample(1:100, 1)]

  library(shape)
  # background
  emptyplot(c(0, 2*x), c(0, 2*x), asp = 1, col = background_color)#emptyplot(c(0, 2*x), c(0, 2*x), main = name_maker() , asp = 1, col = background_color)
  # body
  plotcircle(mid = c(x, -0.175 * x), r =  x, from = 0, to = 6*pi/2,col = shirt_color)
  # head
  plotcircle(mid = c(x, 1.2 * x), r = 0.5 * x, from = 0, to = 6*pi/2, col = skin_color)
  # left eye
  plotcircle(mid = c(eyelv, eye_height + left_eye_offset), r = left_eye_size, from = 0, to = 6*pi/2, col = "black")
  # right eye
  plotcircle(mid = c(eyerv, eye_height + right_eye_offset), r = right_eye_size, from = 0, to = 6*pi/2, col = "black")
  # mouth
  plotellipse(mid = c(x, mouth_height), rx = mouth_width, ry = mouth_thickness, col = "black")

}


random_vector = function(){
  rand_vec <- vector(mode = "integer", length = 14)
  rand_vec[1] = 1
  for(i in 2:14){
    rand_vec[i] = as.integer(runif(1,1,100))
  }
  rand_vec
}


#############################################################

convert_csv = function(x){
  # 1) get a data frame from a csv file

  file_name <- x # "iris.csv" # for now, must be a .csv file
  target_file_name <- "altered_dnd.csv" # for now, must be a .csv file
  #datasets <- read.table("dataset.csv", header=F)
  #datasets <- as.matrix(read.csv(file_name, header=F))  # NO!!!
  datasets <- as.data.frame(read.csv(file_name, header=F))  # YES!!!!
  head(datasets)

  #head(datasets)
  #datasets[2,4]
  #datasets[3,3]

  # 2) since the last column will be the classifier names, let's grab those
  classifier_names <- datasets[,ncol(datasets)]

  # 3) determine the length
  #cat("this here is the last column we need to choppy-choppy",ncol(datasets))
  #head(datasets)
  datasets <- datasets[,-ncol(datasets)]
  #head(datasets)
  #cat("for(i in 1:",ncol(datasets),")")

  range_matrix <- matrix(NA,ncol(datasets),2)
  #print("big mav")
  #range_matrix


  get_max_and_mins = function(x,r_x){
    rx <- r_x
    for(s in 1:ncol(x)){
      min1 <- x[which.min(x[,s]),s]
      max1 <- x[which.max(x[,s]),s]
      rx[s,1] <- min1
      rx[s,2] <- max1
    }
    rx
  }


  range_matrix <- get_max_and_mins(datasets,range_matrix)
  #print("range matrix")
  #range_matrix

  increments <- vector(mode = "numeric", length = ncol(datasets))

  for(v in 1:length(increments)){
    increments[v] <- (range_matrix[v,2] - range_matrix[v,1])/100
  }


  reference_matrix <- matrix(NA,nrow(datasets),ncol(datasets))

  for(x in 1:nrow(datasets)){
    for(y in 1:ncol(datasets)){
      myron <- (datasets[x,y] - range_matrix[y,1]) /increments[y]
      if(myron == 0){
        myron = 1
      }
      reference_matrix[x,y] <- round(myron)

    }
  }


  altered_dataset <- as.data.frame(reference_matrix)
  altered_dataset[,ncol(altered_dataset)+1]  <- classifier_names


  return(altered_dataset)
}

# a_d <- convert_csv("iris.csv")
#######################################################



###########################################################

#' @export
four_face = function(v, name = ""){

  black_and_white <- FALSE
  xam <- c(1,50,50,50,50,50,50,50,50,50,50)

  presets <- c(1,0.2, 0.2, 0.2, 0.2, 0.3, 0.5, 0.2, 0.2, 0.4, 0.4)
  x <- xam[1] * 2.0

  #20  175   20  215  260  180   20    5  -20   -20
  #20  175   20  205  255  175   25    5  -20   -20
  #20  175   20  205  255  175   25    5  -20   -20
  left_eye_size = (20 + (xam[2]*presets[2]))/1000
  eyelv = (175 + xam[3]*presets[3])/100
  right_eye_size = (20 + (xam[4]*presets[4]))/1000
  eyerv = (205 + (xam[5]*presets[5]))/100
  eye_height  = (255 + (xam[6]*presets[6]))/100
  mouth_height  = (175 + (xam[7]*presets[7]))/100
  mouth_width = (25 + (xam[8]*presets[8]))/100
  mouth_thickness  = (5 + (xam[9]*presets[9]))/1000
  left_eye_offset = (-20 + (xam[10]*presets[10]))/100
  right_eye_offset = (-20 + (xam[11]*presets[11]))/100

  ############## show stats ###################
  #cat(left_eye_size,"\n")
  #cat(eyelv,"\n")
  #cat(right_eye_size,"\n")
  #cat(eyerv,"\n")
  #cat(eye_height,"\n")
  #cat(mouth_height,"\n")
  #cat(mouth_width,"\n")
  #cat(mouth_thickness,"\n")
  #cat(left_eye_offset,"\n")
  #cat(right_eye_offset,"\n")
  ############## show stats ###################
  #print("ideal:")
  ############## show ideal stats ###################
  #cat(.03,"\n")
  #cat(1.85,"\n")
  #cat(.03,"\n")
  #cat(2.15,"\n")
  #cat(2.7,"\n")
  #cat(2.0,"\n")
  #cat(0.35,"\n")
  #cat(0.015,"\n")
  #cat(0,"\n")
  #cat(0,"\n")
  ############## show ideal stats ###################
  head_size <-  0.5 + (v[1] * 0.01)
  #cat("head size",head_size)
  skin_index <- v[2]
  shirt_index <- v[3]
  background_index <- v[4]

  ramp <- colorRamp(c("blue", "yellow"))
  if(black_and_white){
    ramp <- colorRamp(c("black", "white"))
  }
  skin_colors <- rgb( ramp(seq(0, 1, length = 100)), max = 255)
  skin_color <- skin_colors[skin_index] # <- skin_colors[sample(1:100, 1)]

  ramp <- colorRamp(c("blue", "yellow"))
  if(black_and_white){
    ramp <- colorRamp(c("black", "white"))
  }
  back_colors <- rgb( ramp(seq(0, 1, length = 100)), max = 255)
  background_color <- back_colors[background_index]  # <- back_colors[sample(1:100, 1)]

  ramp <- colorRamp(c("blue", "yellow"))
  if(black_and_white){
    ramp <- colorRamp(c("black", "white"))
  }
  shirt_colors <- rgb( ramp(seq(0, 1, length = 100)), max = 255)
  shirt_color <- shirt_colors[shirt_index]

  library(shape)
  # background
  emptyplot(c(0, 2*x), c(0, 2*x), main = name, asp = 1, col = background_color)#emptyplot(c(0, 2*x), c(0, 2*x), main = name_maker() , asp = 1, col = background_color)
  # body
  plotcircle(mid = c(x, -0.175 * x), r =  x, from = 0, to = 6*pi/2,col = shirt_color)
  # head
  plotcircle(mid = c(x, 1.2 * x), r = head_size, from = 0, to = 6*pi/2, col = skin_color)
  # left eye
  plotcircle(mid = c(eyelv, eye_height + left_eye_offset), r = left_eye_size, from = 0, to = 6*pi/2, col = "blue")
  # right eye
  plotcircle(mid = c(eyerv, eye_height + right_eye_offset), r = right_eye_size, from = 0, to = 6*pi/2, col = "blue")
  # mouth
  plotellipse(mid = c(x, mouth_height), rx = mouth_width, ry = mouth_thickness, col = "blue")

}

###########################################################

###########################################################
#' @export
read_table = function(x){


  file_name <- x     # for now, must be a .csv file
  # print(x)
  #dataset <- as.matrix(read.csv(file_name, header=F))  # NO!!!
  dataset <- as.data.frame(read.csv(file_name, header=F))  # YES!!!!
  #head(dataset)
  dataset
}

############################################################


