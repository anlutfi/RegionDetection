###GLOBAL CONSTANTS###
X <- 1
Y <- 2
LABEL <- 3
LEFT <- (-1)
RIGHT <- (-1) * LEFT
UP <- (-1)
DOWN <- (-1) * UP
###

color_diff<-function(thiscolor, othercolor, gs) #gives the inverse of the distance between thiscolor and othercolor
{
	if(!gs)
	{
		r1<-floor(thiscolor/(16 ^ 4))
		b1<-thiscolor %% (16 ^ 2)
		g1<-floor((thiscolor %% (16 ^ 4))/(16 ^ 2))

		r2<-floor(othercolor/(16 ^ 4))
		b2<-othercolor %% (16 ^ 2)
		g2<-floor((othercolor %% (16 ^ 4))/(16 ^ 2))
		difference <- ((((r2-r1)^2) + ((g2-g1)^2) + ((b2-b1)^2)) ^ 0.5)
	}
	else
	{
		difference <- abs((thiscolor %% (16 ^ 2)) - (othercolor %% (16 ^ 2)))
	}
	if(difference == 0) #prevents division by zero
	{
		result <- 1
	}
	else
	{
		result <- 1 / difference
	}
	result
}

random_walk <- function(x, y, prob, imglabel, walksize, relax=FALSE)
{ 
	walk <- list()
	label <- 0
	for (i in 1:(walksize))
	{
		walk[[i]] <- array(0, 2)
		walk[[i]][X] <- x
		walk[[i]][Y] <- y
		a <- runif(1)##
		j <- 1##
		while(a>prob[x,y,j]) ##determines which of the current pixel's neighbours is to be visited next
		{
			j <- j + 1
		}
		if(j==1)
		{
			x <- x + UP
			y <- y + LEFT
		}
		else if(j==2)
		{
			x <- x + UP
		}
		else if(j==3)
		{
			x <- x + UP
			y <- y + RIGHT
		}
		else if(j==4)
		{
			y <- y + RIGHT
		}
		else if(j==5)
		{
			x <- x + DOWN
			y <- y + RIGHT
		}
		else if(j==6)
		{
			x <- x + DOWN
		}
		else if(j==7)
		{
			x <- x + DOWN
			y <- y + LEFT
		}
		else #j==8
		{
			y <- y + LEFT
		}
		if( (imglabel[x,y] < 0 && !relax) || (imglabel[x,y] != 0 && relax) ) ## if the next pixel is prelabelled, returns the absolute of that label
		{
			label <- imglabel[x,y]
			break
		}
	}
	list(((-1) * label), walk) #no prelabelled pixel was reached
}

simulate <- function(filename, nwalks, fileprelabels="", maxwalksize=0, gs, relax=0)
{
	imgstr <- readPNG(system.file("img", filename, package="png"))
	imgstr <- as.raster(imgstr[,,1:3])
	width<-length(imgstr[1,])
	height<-length(imgstr[,1])
	img <- matrix(0, height, width)
	for(i in 1:height)
	{
		for(j in 1:width)
		{
			img[i,j] <- as.integer(paste("0x", strsplit(imgstr[i,j], "#")[[1]][2], sep = ""))
		}
	}
	
	prelabels <- load_prelabels(fileprelabels)
	nlabels <- abs(prelabels[length(prelabels[,1]), LABEL])
	imglabel <- matrix(0, height, width)
	for(i in 1:length(prelabels[,1]))
	{
		imglabel[prelabels[i,X], prelabels[i,Y]] <- (-1) * abs(prelabels[i, LABEL])
	}
	
	if(!maxwalksize)
	{
		maxwalksize <- width * height
	}
	
	prob <- array(-9, dim=c(height, width, 8))
	
	calculate_probability <- function(begin, finish, ispositive) #calculate the accumulated probability array for all neighbours of a pixel
	{
		d <- array(-9, dim=c(8))
		total <- 0
		
		calculate_pair_dist <- function(i) #given a pixel, its 8 neighbours are numbered clockwise, starting with 1 for its upper left neighbour
		{
			switch(i,
				color_diff(img[x,y], img[x+UP,y+LEFT], gs),
				color_diff(img[x,y], img[x+UP,y], gs),
				color_diff(img[x,y], img[x+UP,y+RIGHT], gs),
				color_diff(img[x,y], img[x,y+RIGHT], gs),
				color_diff(img[x,y], img[x+DOWN,y+RIGHT], gs),
				color_diff(img[x,y], img[x+DOWN,y], gs),
				color_diff(img[x,y], img[x+DOWN,y+LEFT], gs),
				color_diff(img[x,y], img[x,y+LEFT], gs)
				)
		}
		
		if(!ispositive)
		{
			
			for(i in 1:8)
			{
				if(i>=begin && i<=finish)
				{
					d[i] <- -1 * (finish + 1)
				}
				else
				{
					d[i] <- calculate_pair_dist(i)
					total <- total + d[i]
				}
			}
		}
		else
		{
			for(i in 1:finish)
			{
				if(i<begin)
				{
					d[i] <- -1 * (begin +1)
				}
				else
				{
					d[i] <- calculate_pair_dist(i)
					total <- total + d[i]
				}
			}
		}
		acumulated_prob <- 0
		for(i in 1:8)
		{
			if(d[i] >= 0)
			{
				acumulated_prob <- acumulated_prob + (d[i]/total)
				d[i] <- acumulated_prob
			}
		}
		d
	}
	
	for(x in 1:height) ##
	{
		for(y in 1:width) ## calculate probabilities for all pixel transitions of the picture
		{
			if(x==1) #pixel on top edge
			{
				if(y==1) #pixel on top left corner
				{
					prob[x,y,] <- calculate_probability(4, 6, TRUE)
				}
				else if(y == width) #pixel on top right corner
				{
					prob[x,y,] <- calculate_probability(6, 8, TRUE)
				}
				else
				{
					prob[x,y,] <- calculate_probability(4, 8, TRUE)
				}
			}
			else if(x == height) #pixel on bottom edge
			{
				if(y == 1) #pixel on bottom left corner
				{
					prob[x,y,] <- calculate_probability(2, 4, TRUE)
				}
				else if(y == width) #pixel on bottom right corner
				{
					prob[x,y,] <- calculate_probability(3, 7, FALSE)
				}
				else
				{
					prob[x,y,] <- calculate_probability(5, 7, FALSE)
				}
			}
			else if(y == 1) #pixel on left edge
			{
				prob[x,y,] <- calculate_probability(2, 6, TRUE)
			}
			else if(y == width) #pixel on right edge
			{
				prob[x,y,] <- calculate_probability(3, 5, FALSE)
			}
			else #normal pixel with all 8 neighbours
			{
				prob[x,y,] <- calculate_probability(1, 8, TRUE)
			}
		}
	}
	times <- relax
	repeat
	{
		change <- FALSE
		for(x in 1:height) ##
		{
			for(y in 1:width) ##
			{
				if(imglabel[x,y] == 0) ## for each yet unlabelled pixel in the image
				{
					change <- TRUE
					lbs <- array(0, nlabels)
					mx <- 0
					mxlabel <- 0
					for(i in 1:nwalks) # do nwalks random walks
					{
						rw <- random_walk(x, y, prob, imglabel, maxwalksize, relax=!times)
						lb <- rw[[1]]
						walk <- rw[[2]]
						if(lb)
						{
							lbs[lb] <- lbs[lb] + 1
							if(mx < lbs[lb])
							{
								mx <- lbs[lb]
								mxlabel <- lb
							}
						}
					}
					imglabel[x,y] <- mxlabel #label the pixel with the mode of the labels obtained on all the walks
				}
			}
		}
		print("=============")
		times<-max((times - 1), 0)
		if((!relax) || (!change))
		{
			break
		}
	}
	labelplot(imglabel, height, width, nlabels)
}

labelplot <- function(imglabel, height, width, nlabels)
{
	for(x in 1:height)
	{
		for(y in 1:width)
		{
			imglabel[x,y] <- abs(imglabel[x,y])/nlabels
		}
	}
	plot(1:2, type='n')
	rasterImage(imglabel, 1.2, 1.27, 1.8, 1.73, interpolate=FALSE)
}

load_prelabels <- function(filename="")
{
	if(filename != "")
	{
		d <- read.table(filename, sep=" ")
		prelabels<-matrix(0, length(d[,1,1]), 3)
		for(i in 1:length(d[,1,1]))
		{
			prelabels[i,X] <- d[i,Y]
			prelabels[i,Y] <- d[i,X]
			prelabels[i,LABEL] <- d[i,LABEL]
		}
	}
	prelabels
}

simulate("checkers.png", 5, fileprelabels="U:\\simestocastica\\labels.txt", maxwalksize=150, FALSE, relax=3)