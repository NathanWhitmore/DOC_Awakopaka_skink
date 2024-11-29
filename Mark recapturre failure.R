library(tidyverse)

p <- seq(from = 0, to = 0.1, by=0.001)
N <- seq(from = 5, to = 30, by=1)

# Define the number of rows and columns
n_rows <- p
n_cols <- N

# Create a matrix where each element is the product of its row and column indices
matrix_product <- outer(n_rows, n_cols, FUN = function(row, col) (1 - row)^col) %>%
  round(3)

rownames(matrix_product) <- p
colnames(matrix_product) <- N


matrix_product

mat.df <- as.data.frame(matrix_product)
colnames(mat.df) <- paste("N =", colnames(mat.df))

mat.df <- mat.df %>% rownames_to_column()

# Convert to long format
mat.longer <- mat.df %>% 
  pivot_longer(cols = `N = 5`:`N = 30`, # Dynamically selects all numeric columns
               names_to = "N",
               values_to = "Prob (Fail)")

mat.longer$N <- str_replace_all(mat.longer$N, "N = ", "")

mat.longer <- mat.longer %>% rename(p = rowname)

# Ensure N is numeric (optional)
mat.longer <- mat.longer %>%
  mutate(p = as.numeric(p),
         N = as.numeric(N))

ggplot()+
  geom_tile(data = mat.longer,
            aes(x=p, y=N, fill= `Prob (Fail)`))+
  scale_fill_gradient2(low = "orange", 
                       mid = "white", 
                       high = "black",
                       midpoint = 0.5)
  
mat.longer$survey1 <- (mat.longer$`Prob (Fail)`)
mat.longer$survey2 <- (mat.longer$`Prob (Fail)`)^2
mat.longer$survey3 <- (mat.longer$`Prob (Fail)`)^3
mat.longer$survey4 <- (mat.longer$`Prob (Fail)`)^4
mat.longer$survey5 <- (mat.longer$`Prob (Fail)`)^5
mat.longer$survey6 <- (mat.longer$`Prob (Fail)`)^6
mat.longer$survey7 <- (mat.longer$`Prob (Fail)`)^7
mat.longer$survey8 <- (mat.longer$`Prob (Fail)`)^8
mat.longer$survey9 <- (mat.longer$`Prob (Fail)`)^9
mat.longer$survey10 <- (mat.longer$`Prob (Fail)`)^10

much.longer <- mat.longer %>%
  pivot_longer(cols = survey1:survey10,
               names_to = "Survey",
               values_to = "Failure")

much.longer$Survey <- str_replace_all(much.longer$Survey, "survey", "")
much.longer$Survey <- as.numeric(much.longer$Survey)


ggplot()+
  geom_tile(data = much.longer,
            aes(x=p, y=N, fill= Failure))+
  scale_fill_gradient2(low = "red", 
                       mid = "white", 
                       high = "black",
                       midpoint = 0.5)+
  facet_wrap(~Survey)


much.longer %>% group_by(Survey) %>% summarise(mean(Failure))
