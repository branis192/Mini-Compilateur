
SETI R0, #0      
SETI R1, #0      
SETI R2, #0      
SETI R3, #0      
SETI R4, #1      
SETI R5, #0      
SETI R6, #0      
SETI R7, #0      

INVOKE 1, 2, 3   

L1:              
    GOTO_GE L2, R0, R2  

L3:              
    GOTO_GE L4, R1, R3  

    INVOKE 3, 0, 1    
    INVOKE 5, 5, 0      

    GOTO_NE L5, R5, R4  

    SETI R5, #0         
    INVOKE 4, 5, 0      
    GOTO L7             

L5:                    
    INVOKE 5, 6, 1      
    GOTO_EQ L6, R6, R4  

    INVOKE 5, 6, 2      
    GOTO_EQ L6, R6, R4  

    INVOKE 5, 6, 3      
    GOTO_EQ L6, R6, R4  

    INVOKE 5, 6, 4      
    GOTO_EQ L6, R6, R4  

    INVOKE 5, 6, 5      
    GOTO_EQ L6, R6, R4  

    INVOKE 5, 6, 6      
    GOTO_EQ L6, R6, R4  

    INVOKE 5, 6, 7      
    GOTO_EQ L6, R6, R4  

    INVOKE 5, 6, 8      
    GOTO_EQ L6, R6, R4  

    GOTO L7             

L6:                    
    SETI R5, #1         
    INVOKE 4, 5, 0      

L7:                    
    ADD R1, R1, R4      
    GOTO L3             

L4:                    
    ADD R0, R0, R4      
    SETI R1, #0         
    GOTO L1             

L2:                    
    STOP                
