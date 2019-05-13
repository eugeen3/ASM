.model	small
.stack	100h
.data
            
MaxArrayLength              equ 30            
MaxNumber                   equ 7FFFh
MinNumber                   equ 8000h
            
ArrayLength                 db  ? 
ErrorCode                   db  ?
Sign                        db  ? 
DirectionFlag               db  ?  
EqualCounter                db  0

Array                       dw  MaxArrayLength dup (0)                               
Buffer                      dw  ?
                                                              
MaxNumLength                db  7 
RealLength                  db  ?                          
buff                        db  7 dup (0)                                         

InputArrayLengthMsg         db  0Dh,'Input array length: $' 
InputNumbers                db  0Dh,'Input numbers:', '$'
                                
InputError                  db  0Dh,'Incorrect value!', 0Ah, 0Dh, '$' 
LengthError                 db  0Dh,'Array length should be geater than 0 and not grater than 30!', 0Ah, '$'
 
IncreasingSequence          db  0Dh,'Increasing sequence', 0Ah, '$' 
DecreasingSequence          db  0Dh,'Decreasing sequence', 0Ah, '$'
RandomSequence              db  0Dh,'Random sequence', 0Ah, '$'     
EqualSequence               db  0Dh,'All numbers are equal', 0Ah, '$'
newLineStr                  db  0Ah, 0Dh, '$'        
                      
.code 


inputArrayLength proc   
    mov cx, 1           
    inputLoop:
       printString InputArrayLengthMsg                  
       call inputValue          
       
       cmp ErrorCode, 0
       jnz inputLoop 
       
       cmp Buffer, MaxArrayLength
       jg inputLoopError   
       
       cmp Buffer, 0
       jg success   
       
       inputLoopError:
       
       printString LengthError
       jmp inputLoop
       
       success:
       
       mov bx, Buffer 
       mov ArrayLength, bl
       printString InputNumbers   
       printString newLineStr              
    loop inputLoop     
    ret      
endp 

printString macro string
    push ax
    push dx
    
    mov dx, offset string
    mov ah, 9
    int 21h

    pop dx
    pop ax
endm

inputArray proc
    xor di,di                     
                                               
    mov cl,ArrayLength            
    inputArrayLoop:              
       call inputValue      
       
       cmp ErrorCode, 0
       jnz inputArrayLoop 
       
       push bx  
       xor bx, bx
       mov bx, Buffer
       jo inputArrayLoop
       
       mov Array[di], bx 
       add di, 2         
       pop bx          
    loop inputArrayLoop           
    ret      
endp     

inputValue proc                 
    push cx                       
    inputElMain:                  
        mov Buffer, 0          
        
        mov ah,0Ah               
        lea dx, MaxNumLength         
        int 21h                   
                                  
        printString newLineStr                  
                                
        cmp RealLength,0                 
        je inputValueError             
                                  
        mov Sign, 0              
        xor bx,bx                 
                                  
        mov bl,RealLength                
        lea si,RealLength                
                                  
        add si,bx                 
        mov bx,1                  
                                  
                                  
        xor cx,cx                 
        mov cl,RealLength                
        inputElLoop:
            std                               
            lodsb                 
                                  
            call checkSymbol        
            mov ah, 0
            cmp ErrorCode, 1             
            je inputValueError         

            cmp ErrorCode,2                
            je nextSymbol            
                                  
            sub al,'0'            
            mul bx                

            cmp ErrorCode, 0          
                                  
            jnz inputValueError        
         
            add Buffer,ax  
                       
            jo checkIfMin         
            js checkIfMin       
            jmp checkScope         
            
            checkIfMin:
                cmp Buffer, MinNumber
                je checkNumLength  
                jne inputValueError
            
            checkNumLength:
                cmp RealLength, 6
                je continue
                jne inputValueError
              
            checkScope:            
                push dx
                mov dx, Buffer 
                cmp dx, MaxNumber
                jg inputValueError
                cmp dx, MinNumber
                jl inputValueError
                pop dx  
           
            continue:           
                mov ax,bx             
                mov bx,10           
                mul bx               
            
            cmp ErrorCode, 0                                
            jz toNextSymbol       
          
                              
            toNextSymbol:          
                mov bx,ax         
                jmp nextSymbol                          
                                  
            inputValueError:           
                printString InputError 
                jmp inputArrayLoop  
                jmp exitInputEl          
                                  
            nextSymbol:
                mov ErrorCode, 0           
        loop inputElLoop          
                                  
    cmp Sign, 0                   
    je exitInputEl                
    neg Buffer                    
                                  
    exitInputEl:                  
        pop cx                        
        ret                           
endp 
                                  
checkSymbol proc                     
    cmp al,'-'                    
    je SignSymbol                   
                                  
    cmp al,'9'                    
    ja checkSymbolError                
                                  
    cmp al,'0'                    
    jb checkSymbolError                
                                  
    jmp exitCheckGood            
                                  
    SignSymbol:                     
        cmp si,offset RealLength         
        je exitWithMinus          
                                  
    checkSymbolError:
        mov ErrorCode, 1                  
        jmp checkSymbolExit          
                                          
    exitWithMinus: 
        mov ErrorCode, 2                 
        mov Sign, 1              
        cmp RealLength, 1                
        je checkSymbolError           
                                  
        jmp checkSymbolExit          
                                  
    exitCheckGood:
        mov ErrorCode, 0                 
                                  
    checkSymbolExit:                 
        ret                       
endp                                                               

                         

CheckArray proc 
	push cx
	push ax   
	
	xor cx, cx 
	xor di, di  
	mov cl, ArrayLength
	sub cl, 2
	
	mov ax, Array[di]
	cmp ax, Array[di + 2]
	jg set_decreasing
	jl set_increasing
	je set_equal

	set_increasing:
	    mov DirectionFlag, 1
	    jmp checkAllArray:
	
	set_decreasing:
	    mov DirectionFlag, 0
	    jmp checkAllArray
	
	set_equal:
	    mov DirectionFlag, 2
	    mov EqualCounter, 1
	
	checkAllArray:
	
	    add di, 2 
	    mov ax, Array[di]
	    cmp ax, Array[di + 2]
	    jg checkIfDec
	    jl checkIfInc
	    je incrECntr
	
	    checkIfInc:
	        cmp DirectionFlag, 2 
	        je prevEqInc  
	        jne prevNotEqInc 
	
	    prevEqInc:
	        mov DirectionFlag, 1
	        jmp toNext
	
	    prevNotEqInc:
	        cmp DirectionFlag, 1
	        jne randomSeq 
	        jmp toNext
	
	    checkIfDec:
	        cmp DirectionFlag, 2 
	        je prevEqDec  
	        jne prevNotEqDec 
	
	    prevEqDec:
	        mov DirectionFlag, 0
	        jmp toNext
	
	    prevNotEqDec:
	        cmp DirectionFlag, 0
	        jne randomSeq   
	        jmp toNext
	        
        incrECntr: 
            inc EqualCounter   
        
        toNext:    
    loop checkAllArray 
	    
	    mov al, EqualCounter 
	    inc al
	    cmp al, ArrayLength
	    je equalSeq
	    cmp DirectionFlag, 1
	    je increasingSeq
	    jne decreasingSeq
	
	    increasingSeq:
	        printString IncreasingSequence
	        jmp endProgram
	
	    decreasingSeq:
	        printString DecreasingSequence 
	        jmp endProgram
	
	    randomSeq:
	        printString RandomSequence
	        jmp endProgram
	    equalSeq:
	        printString EqualSequence
	        
	    endProgram: 
	
	        xor ax, ax                             
            mov	ah,4ch                    
            int	21h
	        ret
endp               
       
start:                            
mov	ax,@data                      
mov	ds,ax                         
                                  
xor	ax,ax                         
                                  
call inputArrayLength         
call inputArray                   
call CheckArray                                          
                               
end	start                         