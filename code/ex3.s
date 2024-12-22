
SETI R0 , #0      
SETI R1 , #0      
SETI R2 , #0      
SETI R3 , #0      
SETI R4 , #1
      

INVOKE 1, 2, 3    

L1:               
GOTO_GE L2 , R0 , R2  
L3:                
GOTO_GE L4 , R1 , R3 

INVOKE 3, 0, 1
INVOKE 4, 4, 0  

ADD R1 , R1 , R4  
GOTO L3            
L4:                
ADD R0 , R0 , R4  
SETI R1 , #0       
GOTO L1            

L2:                
STOP               

