telescope_schematics <- function(lens_table_data,obj)
{
	library("ggplot2")
	source("lens_matrix.R")

	
	lens_table <- lens_table_data[order(lens_table_data[,3]),]
	telescope_matrix <- list()
	image_matrix <- list()
	images <- list()
	inter_image_pos <- list()

	previous_lens_position <- lens_table[1,3]
	previous_lens_width <- lens_table[1,4]*0.15
	
	
	
	#Position Correction
	
	for(i in 1:nrow(lens_table))
	{
	  lens_position <- lens_table[i,3]
	  lens_width <- lens_table[i,4]*0.15
	  
	  lens_distance <- lens_position - previous_lens_position
	  width_sum <- lens_width + previous_lens_width
	  
	  #Position Correction
	  if(lens_distance>0 && lens_distance < width_sum)
	  {
	    
	    lens_table[i,3] <- previous_lens_position + width_sum
	    #plot_graph <- plot_graph + annotate("segment",x=previous_lens_position,xend=lens_position,y=previous_ray_end,yend=previous_ray_end, color="dodgerblue")
	    #plot_graph <- plot_graph + annotate("segment",x=previous_lens_position,xend=lens_position,y=-1*previous_ray_end,yend=-1*previous_ray_end, color="dodgerblue")
	  }
	  previous_lens_position <- lens_table[i,3]
	  previous_lens_width <- lens_table[i,4]*0.15
	}
	
	#print(lens_table)

	obj_matrix <- matrix(c(1/obj,1),nrow=2,ncol=1,byrow=TRUE)
	matrix_data <- lens_matrix(lens_table)
	for(i in 1:length(matrix_data))
	{
		telescope_matrix[[i]] <- matrix_data[[i]]
		image_matrix[[i]] <- telescope_matrix[[i]] %*% obj_matrix
		images[[i]] <- image_matrix[[i]][1,1]/image_matrix[[i]][2,1]
		images[[i]] <- 1/images[[i]]
		image<-images[[i]]
	}


	telescope_length <- lens_table[nrow(lens_table),3] - lens_table[1,3]

	#print("Telescope Length:")
	#print(telescope_length)

	if(obj>2*telescope_length)
	{
		start_pos <- lens_table[1,3]-(telescope_length/10)
		infinite_start <- TRUE
	}
	else
	{
		start_pos <- lens_table[1,3]-obj
		infinite_start <- FALSE
	}

	#print("Starting Position:")
	#print(start_pos)

	if(image>lens_table[nrow(lens_table),2]*3)
	{
		end_pos <- lens_table[nrow(lens_table),3]+(telescope_length/10)
		infinite_end <- TRUE
	}
	else
	{
		end_pos <- lens_table[nrow(lens_table),3]+image
		infinite_end <- FALSE
	}

	#print("Ending Position:")
	#print(end_pos)

	#print("Image Distance:")
	#print(image)

	plot_height <- 3*max(lens_table[,4])
	tel_height <- max(lens_table[,4])*0.5

	plot_graph <- ggplot() + geom_point() + xlim(start_pos, end_pos) + ylim(-plot_height, plot_height)

	previous_lens_position <- lens_table[1,3]
	previous_lens_width <- lens_table[1,4]*0.15
	previous_ray_end <- lens_table[1,4]*0.5


	for(i in 1:nrow(lens_table))
	{
		lens_position <- lens_table[i,3]
		lens_height <- lens_table[i,4]*0.5
		lens_width <- lens_height*0.3

		lens_distance <- lens_position - previous_lens_position
		width_sum <- lens_width + previous_lens_width


#Convex Lens
		if(lens_table[i,2]>0)
		{
			plot_graph <- plot_graph + annotate("path",
 			  x=lens_position + lens_width*cos(seq(0,2*pi,length.out=100)),
			   y= 0 + lens_height*sin(seq(0,2*pi,length.out=100)))
		}

#Concave Lens
		else if (lens_table[i,2]<0)
		{
			plot_graph <- plot_graph + annotate("path",
 			  x=lens_position + lens_width*cos(seq(-1/2*pi,1/2*pi,length.out=100))-2*lens_width,
			   y= 0 + lens_height*sin(seq(-1/2*pi,1/2*pi,length.out=100)))

			plot_graph <- plot_graph + annotate("path",
 			  x=lens_position + lens_width*cos(seq(1/2*pi,3/2*pi,length.out=100))+2*lens_width,
			   y= 0 + lens_height*sin(seq(1/2*pi,3/2*pi,length.out=100)))

			plot_graph <- plot_graph + annotate("segment",x=lens_position-2*lens_width,xend=lens_position+2*lens_width,y=lens_height,yend=lens_height)
			plot_graph <- plot_graph + annotate("segment",x=lens_position-2*lens_width,xend=lens_position+2*lens_width,y=-lens_height,yend=-lens_height)

		}

#Name Plotter
		if(i%%2!=0)
		{
			plot_graph <- plot_graph + annotate("text",x=lens_position,y=plot_height*0.25 + (plot_height*0.05*(floor(i/2))%%2),label=lens_table[i,1])
		}
		else
		{
			plot_graph <- plot_graph + annotate("text",x=lens_position,y=-1*plot_height*0.25 - (plot_height*0.05*(floor(i/2))%%2),label=lens_table[i,1])
		}

#Rays Tracer

		inter_image_pos[[i]] <- lens_position + images[[i]]
		#print(inter_image_pos[[i]])
	#Real Inverted Image
		if (i < nrow(lens_table) && inter_image_pos[[i]] > lens_table[i,3] && inter_image_pos[[i]] < lens_table[i+1,3] && inter_image_pos[[i]] > start_pos && inter_image_pos[[i]] < end_pos)
		{
			plot_graph <- plot_graph + annotate("point",x=inter_image_pos[[i]],y=0)
			slope <- -1*previous_ray_end/(inter_image_pos[[i]]-lens_position)
			ray_end <- slope*(lens_table[i+1,3]-lens_position) + previous_ray_end
			if(abs(ray_end) < tel_height)
			{
				plot_graph <- plot_graph + annotate("segment",x=lens_position,xend=lens_table[i+1,3],y=previous_ray_end,yend=ray_end, color="dodgerblue")
				plot_graph <- plot_graph + annotate("segment",x=lens_position,xend=lens_table[i+1,3],y=-1*previous_ray_end,yend=-1*ray_end, color="dodgerblue")
				previous_ray_end <- ray_end
			}
			else
			{
				mid_point <- ((-1*tel_height)-previous_ray_end)/slope + lens_position
				plot_graph <- plot_graph + annotate("segment",x=lens_position,xend=mid_point,y=previous_ray_end,yend=-1*tel_height, color="dodgerblue")
				plot_graph <- plot_graph + annotate("segment",x=lens_position,xend=mid_point,y=-1*previous_ray_end,yend=tel_height, color="dodgerblue")
				previous_ray_end <- tel_height
				plot_graph <- plot_graph + annotate("segment",x=mid_point,xend=lens_table[i+1,3],y=previous_ray_end,yend=lens_table[i+1,4]*0.5, color="dodgerblue")
				plot_graph <- plot_graph + annotate("segment",x=mid_point,xend=lens_table[i+1,3],y=-1*previous_ray_end,yend=-1*lens_table[i+1,4]*0.5, color="dodgerblue")
			}

		}
	#Real Not Inverted Image
		else if(i < nrow(lens_table) && inter_image_pos[[i]] > lens_table[i,3] && inter_image_pos[[i]] > lens_table[i+1,3] && inter_image_pos[[i]] > start_pos && inter_image_pos[[i]] < end_pos)
		{
			slope <- -1*previous_ray_end/(inter_image_pos[[i]]-lens_position)
			ray_end <- slope*(lens_table[i+1,3]-lens_position) + previous_ray_end
			if(ray_end < tel_height)
			{
				plot_graph <- plot_graph + annotate("segment",x=lens_position,xend=lens_table[i+1,3],y=previous_ray_end,yend=ray_end, color="dodgerblue")
				plot_graph <- plot_graph + annotate("segment",x=lens_position,xend=lens_table[i+1,3],y=-1*previous_ray_end,yend=-1*ray_end, color="dodgerblue")
				previous_ray_end <- ray_end
			}
			else
			{
				mid_point <- ((-1*tel_height)-previous_ray_end)/slope + lens_position
				plot_graph <- plot_graph + annotate("segment",x=lens_position,xend=mid_point,y=previous_ray_end,yend=-1*tel_height, color="dodgerblue")
				plot_graph <- plot_graph + annotate("segment",x=lens_position,xend=mid_point,y=-1*previous_ray_end,yend=tel_height, color="dodgerblue")
				previous_ray_end <- tel_height
				plot_graph <- plot_graph + annotate("segment",x=mid_point,xend=lens_table[i+1,3],y=previous_ray_end,yend=lens_table[i+1,4]*0.5, color="dodgerblue")
				plot_graph <- plot_graph + annotate("segment",x=mid_point,xend=lens_table[i+1,3],y=-1*previous_ray_end,yend=-1*lens_table[i+1,4]*0.5, color="dodgerblue")
			}
			
		}
	#Virtual Image (STILL NEEDS TESTING!!!!!!)
		else if(i < nrow(lens_table) && inter_image_pos[[i]] < lens_table[i,3] && inter_image_pos[[i]] > start_pos && inter_image_pos[[i]] < end_pos)
		{
			slope <- -1*previous_ray_end/(inter_image_pos[[i]]-lens_position)
			ray_end <- slope*(lens_table[i+1,3]-lens_position) + previous_ray_end
			if(ray_end < tel_height)
			{
				plot_graph <- plot_graph + annotate("segment",x=lens_position,xend=lens_table[i+1,3],y=previous_ray_end,yend=ray_end, color="dodgerblue")
				plot_graph <- plot_graph + annotate("segment",x=lens_position,xend=lens_table[i+1,3],y=-1*previous_ray_end,yend=-1*ray_end, color="dodgerblue")
				previous_ray_end <- ray_end
			}
			else
			{
				mid_point <- ((-1*tel_height)-previous_ray_end)/slope + lens_position
				plot_graph <- plot_graph + annotate("segment",x=lens_position,xend=mid_point,y=previous_ray_end,yend=-1*tel_height, color="dodgerblue")
				plot_graph <- plot_graph + annotate("segment",x=lens_position,xend=mid_point,y=-1*previous_ray_end,yend=tel_height, color="dodgerblue")
				previous_ray_end <- tel_height
				plot_graph <- plot_graph + annotate("segment",x=mid_point,xend=lens_table[i+1,3],y=previous_ray_end,yend=lens_table[i+1,4]*0.5, color="dodgerblue")
				plot_graph <- plot_graph + annotate("segment",x=mid_point,xend=lens_table[i+1,3],y=-1*previous_ray_end,yend=-1*lens_table[i+1,4]*0.5, color="dodgerblue")
			}
		}
	#Image on Infinity
		else if(i < nrow(lens_table) && (inter_image_pos[[i]] < start_pos || inter_image_pos[[i]] > end_pos))
		{
		  #print(inter_image_pos[[i]])
			plot_graph <- plot_graph + annotate("segment",x=lens_position,xend=lens_table[i+1,3],y=previous_ray_end,yend=previous_ray_end, color="dodgerblue")
			plot_graph <- plot_graph + annotate("segment",x=lens_position,xend=lens_table[i+1,3],y=-1*previous_ray_end,yend=-1*previous_ray_end, color="dodgerblue")
		}


		previous_lens_position <- lens_position
		previous_lens_width <- lens_width

	}
	
#Object Plotter

	if(infinite_start)
	{
		plot_graph <- plot_graph + annotate("segment",x=start_pos,xend=lens_table[1,3],y=lens_table[1,4]*0.5,yend=lens_table[1,4]*0.5, color="dodgerblue")
		plot_graph <- plot_graph + annotate("segment",x=start_pos,xend=lens_table[1,3],y=-1*lens_table[1,4]*0.5,yend=-1*lens_table[1,4]*0.5, color="dodgerblue")
	}
	else
	{
		plot_graph <- plot_graph + annotate("segment",x=start_pos,xend=lens_table[1,3],y=0,yend=lens_table[1,4]*0.5, color="dodgerblue")
		plot_graph <- plot_graph + annotate("segment",x=start_pos,xend=lens_table[1,3],y=0,yend=-1*lens_table[1,4]*0.5, color="dodgerblue")
	}

#Image Plotter

	if(infinite_end)
	{
		plot_graph <- plot_graph + annotate("segment",x=lens_table[nrow(lens_table),3],xend=end_pos,y=previous_ray_end,yend=previous_ray_end, color="dodgerblue")
		plot_graph <- plot_graph + annotate("segment",x=lens_table[nrow(lens_table),3],xend=end_pos,y=-1*previous_ray_end,yend=-1*previous_ray_end, color="dodgerblue")
	}
	else
	{
		plot_graph <- plot_graph + annotate("segment",x=lens_table[nrow(lens_table),3],xend=end_pos,y=previous_ray_end,yend=0, color="dodgerblue")
		plot_graph <- plot_graph + annotate("segment",x=lens_table[nrow(lens_table),3],xend=end_pos,y=-1*previous_ray_end,yend=0, color="dodgerblue")	
	}
  
	plot_graph<- plot_graph + annotate("rect", xmin = end_pos*0.6, xmax = end_pos, ymin = plot_height*0.7, ymax = plot_height,alpha = 1,color="white",fill="white")
	plot_graph <- plot_graph + annotate("text",x=end_pos*0.61,y=plot_height*0.95,label=paste0("Telescope Length: ",round(telescope_length,1)," cm"),size=3.3,hjust=0)
	if(infinite_start)
	{
	  plot_graph <- plot_graph + annotate("text",x=end_pos*0.61,y=plot_height*0.85,label=paste0("Object Position: -Infinite"),size=3.3,hjust=0)
	}
	else
	{
	  plot_graph <- plot_graph + annotate("text",x=end_pos*0.61,y=plot_height*0.85,label=paste0("Object Position: ",round(obj,1)," cm"),size=3.3,hjust=0)
	}
	if(infinite_end)
	{
	  plot_graph <- plot_graph + annotate("text",x=end_pos*0.61,y=plot_height*0.75,label=paste0("Image Position: +Infinite"),size=3.3,hjust=0)
	}
	else
	{
	  plot_graph <- plot_graph + annotate("text",x=end_pos*0.61,y=plot_height*0.75,label=paste0("Image Position: ",round(image,1)," cm"),size=3.3,hjust=0)
	}
	
	plot_graph
}