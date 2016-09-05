lens_matrix <- function(lens_table)
{
	refract_num<-nrow(lens_table)

	refract_matrices <- list()
	trans_matrices <- list()

	final_matrix <- matrix(c(1,0,0,1),nrow=2,ncol=2, byrow=TRUE)

	for(i in 1:refract_num)
	{
		p <- 1/lens_table[i,2]
		refract_matrices[[i]]<- matrix(c(1,p,0,1),nrow=2,ncol=2, byrow=TRUE)
		if(i<refract_num)
		{
			d <- -1*(lens_table[i+1,3] - lens_table[i,3])
			trans_matrices[[i]]<-matrix(c(1,0,d,1),nrow=2,ncol=2, byrow=TRUE)

			final_matrix <- refract_matrices[[i]] %*% final_matrix
			final_matrix <- trans_matrices[[i]] %*% final_matrix
		}
		else
		{
			final_matrix <- refract_matrices[[i]] %*% final_matrix
		}
	}
	final_matrix

}