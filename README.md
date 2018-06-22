# A Rubik's Cube Solver

Copyright (C) 2018 Hideyuki Kawabata

Here is a solver based on the CFOP method without F2L.

Usage:

  - with GHCi:

   (a) input: a scramble

   ```
    *Cubing> solve_check "R  L'  U'  D2  F2  U'  F'  R  D'  U'  B2"
    ...
   ```

   (b) input: a scrambled pattern

   ```
    *Cubing> solve_check_pat "OBOWBWYW, BRWYBOWW, ROROWRYG, BBYBGORY, GROBGGOY, RRYGGYWG"
    ...
   ```
   
    Note: the above input string corresponds to the following pattern:

   ```
           YWB                            765         
           WWW                            8W4         
           OBO                            123         
       OGG WOB YRW ROG                765 765 765 765 
       YGB WRY GBO YOB       <--->    8G4 8R4 8B4 8O4 
       GRO BRW ROR BBY                123 123 123 123 
           WYG                            765         
           GYG                            8Y4         
           RRY                            123         
   ```

  - stand alone execution: 

   (a)

   ```
    $ ./Cubing                               
    Input a scramble:                        
    R  L'  U'  D2  F2  U'  F'  R  D'  U'  B2 
    ...                                      
   ```

   or 

   (b) 

   ```
    $ ./Cubing                                                   
    Input a scrambled pattern:                                 
    OBOWBWYW, BRWYBOWW, ROROWRYG, BBYBGORY, GROBGGOY, RRYGGYWG 
    ...                                                        
   ```
