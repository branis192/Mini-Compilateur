
SETI R0, #0        
SETI R1, #0        
SETI R2, #0        
SETI R3, #0        

INVOKE 1, 0, 1   

INVOKE 3, 2, 3    
SETI R4, #1        
INVOKE 4, 4, 0   

SUB R5, R0, R4     
INVOKE 3, 5, 3   
INVOKE 4, 4, 0   

SUB R6, R1, R4     
INVOKE 3, 2, 6   
INVOKE 4, 4, 0

INVOKE 3, 5, 6   
INVOKE 4, 4, 0   

STOP

