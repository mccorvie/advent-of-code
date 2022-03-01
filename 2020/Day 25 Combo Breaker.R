require( tidyverse)
public1 <- 14205034L
public2 <- 18047856L

# public1 <- 5764801L
# public2 <- 17807724L

subject <- 7L
modulus <-   20201227L
max_loops <- modulus-1
# 20201226 = 2 x 3 x 29 x 116099

ss <- rep( subject, max_loops )

by_loopnum <- accumulate( ss, ~ (.x * .y )%% modulus ) 

secret1 = which( by_loopnum == public1)
secret2 = which( by_loopnum == public2)

# overflow
decrypt <- (secret1*secret2) %% (modulus-1)
by_loopnum[decrypt]

# multiply in parts
aa <- (secret1*10000) %% (modulus-1)
rr <- (secret2 %%  10000)
qq <- secret2 %/% 10000

decrypt <-(qq*aa) %% (modulus-1) + (rr*secret1)%%(modulus-1)
decrypt <- decrypt %% (modulus-1)
by_loopnum[decrypt]
