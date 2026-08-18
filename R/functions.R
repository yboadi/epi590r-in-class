# start out with a number to test
x <- 3
# you'll want your function to return this number
x^2
square <- function(x) {
	squared_val <- x * x
	return(squared_val)
	}
# test it out
square(x)
square(53)
53^2 # does this match?

raise <- function(x, power) {
	higher <- x ^ power
	return (higher)
}

# test with
raise(x = 2, power = 4)
# should give you
2^4

raise <- function(x, power = 2 ) {
	higher <- x ^ power
	return (higher)
}

# test
raise(x = 5)
# should give you
5^2


vec <- c(34, 68, 93, 32)
vec

sd <- function(x) {
	n <- length (x)
	sd_val <- sum (x - (mean(x)) ^ 2) / n -1
	if ((vec) <= 1 ) {
		print ("NA")
	}
	}

new_sd <- function(x) {
	demeaned_x <- x - mean(x)
  squared_demeaned_x <- demeaned_x^2
  sum_of_squares <- sum(squared_demeaned_x)
  n_minus_1 <- length(x) - 1
  std_dev <- sqrt (sum_of_squares / n_minus_1)
  return(std_dev)
}
new_sd(2)

new_sd <- function(x, na.rm = TRUE) {
	if (na.rm) {clean_x <- na.omit(x)
		# remove NAs
	} else {
		#don't remove NAs
		new_x <- x
	} #will have a new value of x if NAs are removed, same value if not

	If (length(new_x) <= 1) {
		return_val <- NA
	} else {
		#calculate the standard deviation using new x and save as return_val
		demeaned_x <- new_x - mean(new_x)
		squared_demeaned_x <- demeaned_x^2
		sum_of_squares <- sum(squared_demeaned_x)
		n_minus_1 <- length(new_x) - 1
		std_dev <- sqrt (sum_of_squares / n_minus_1)
}
new_sd <- function(x, na.rm = T) {
	demeaned_x <- x - mean(x)
	squared_demeaned_x <- demeaned_x^2
	sum_of_squares <- sum(squared_demeaned_x)
	n_minus_1 <- length(x) - 1
	std_dev <- sqrt (sum_of_squares / n_minus_1)
	return(std_dev)
}
new_sd(2)

if (na.rm = TRUE) {

}
