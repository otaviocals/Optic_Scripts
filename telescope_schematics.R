telescope_schematics <- function(lens_table,obj)
{
	source("lens_matrix.R")

	lens_table <- lens_table[order(lens_table[,3]),]

	obj_matrix <- matrix(c(1/obj,1),nrow=2,ncol=1,byrow=TRUE)
	telescope_matrix <- lens_matrix(lens_table)
	image_matrix <- telescope_matrix %*% obj_matrix
	image <- image_matrix[1,1]/image_matrix[2,1]
	image <- 1/image

	telescope_length <- lens_table[nrow(lens_table),3] - lens_table[1,3]

	print("Telescope Length:")
	print(telescope_length)

	if(obj>2*telescope_length)
	{
		start_pos <- lens_table[1,3]-(telescope_length/10)
	}
	else
	{
		start_pos <- lens_table[1,3]-obj
	}

	print("Starting Position:")
	print(start_pos)

	if(image>2*telescope_length)
	{
		end_pos <- lens_table[nrow(lens_table),3]+(telescope_length/10)
	}
	else
	{
		end_pos <- lens_table[nrow(lens_table),3]+image
	}

	print("Ending Position:")
	print(end_pos)

	print("Image Distance:")
	image
}