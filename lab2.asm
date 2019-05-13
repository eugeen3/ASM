.model small                   
.stack 100h                   
 
.data
msgEnterString       db "Enter string: $"
msgEnterSubstring    db 0Ah, 0Dh, "Enter substring: $"
msgResult            db 0Ah, 0Ah, 0Ah, 0Ah, 0Dh, "Result string: $" 
msgError             db 0Ah, 0Dh, "Incorrect input! Substring is empty or contains 1+ words or spaces!$"
msgErrorStr          db 0Ah, 0Dh, "Incorrect input! String is empty or contains only spaces!$"  
enterStr             db 0Ah, 0Dh, '$'
max_length           equ 200 
                               
String db 202,max_length dup('$')                        
StringLength db ?                
        
SubString  db 202,max_length dup('$')     
SubStringLength db ?  
                                                                                     
.code                          
start:                         
                              
    mov ax, @data              
    mov ds, ax                 
                                      
    lea dx, msgEnterString     
    call outputString          
    lea dx, String             
    call inputString
    xor ax, ax
    mov al, String[1]           
    cmp al, 200   
    ja overflow
    jmp not_overflow 

    overflow:                
        mov String[201], 0Dh
        mov String[202], '$'
        mov StringLength, 200
        jmp inp
        
    not_overflow:
    mov StringLength, al
     
    inp: 
    lea dx, msgEnterSubstring  
    call outputString          
    lea dx, SubString           
    call inputString              
    xor ax, ax
    mov al, SubString[1]           
    mov SubStringLength, al
                      
    call exitIfEmpty         
    call exitIfEmptySub             
    call exitIfNotSingleWord   
      
    xor cx, cx                
    lea si, String           
    inc si                    
    jmp skip_spaces            

    find:
        push bx 
        mov bl, [si]           
        cmp bl, 9h          
        pop bx
        je skip_spaces
        
        mov bl, [si]                      
        cmp bl, ' '          
        pop bx
        je skip_spaces       
        
        mov bl, [si] 
        cmp bl, 0Dh              
        pop bx
        je exit 
        
        mov bl, [si]
        cmp bl, 1h              
        pop bx
        je exit 
        
        mov bl, [si]
        cmp bl, '$'              
        pop bx
        je exit                 
        
        jmp find               
        
        skip_spaces:           
            inc si   
            push bx 
            mov bl, [si]
            cmp bl, ' '
            pop bx
           je skip_spaces      
              
        call searchSubString   
        jmp find               
     
    error_exit:
        call outputErrorResultStr
              
    error_exit_sub:
        call outputErrorResult       
          
    exit:  
        call outputResult         
                 
inputString proc               
    push ax                      
    mov ah, 0Ah                
    int 21h                    
    pop ax                     
    ret                       
inputString endp               
 
outputString proc              
    push ax                    
    mov ah, 09h                
    int 21h                    
    pop ax                    
    ret                      
outputString endp                            

outputResult proc              
    lea dx, msgResult          
    call outputString          
    lea dx, String
    add dx, 2            
    call outputString          
           
    lea dx, enterStr
    call outputString       
           
    xor ax, ax    
    mov ah,4ch               
    int 21h  
    ret                  
outputResult endp   

outputErrorResultStr proc              
    lea dx, msgErrorStr           
    call outputString                    
    mov ax, 4ch                
    int 21h  
    ret                  
outputErrorResultStr endp  

outputErrorResult proc              
    lea dx, msgError           
    call outputString                    
    mov ax, 4ch                
    int 21h  
    ret                  
outputErrorResult endp         
 
exitIfEmpty proc   
    push si
    push cx
                   
    lea si, String  
    inc si
    
    skip_spaces_str:         
            inc si             
            push bx 
            mov bl, [si]
            cmp bl,' '        
            pop bx
           je skip_spaces_str  
          
    push bx 
    mov bl, [si]                           
    cmp bl, 0Dh                  
    pop bx
    error_exit_marker:
    je error_exit              
       
    pop cx
    pop si
    ret                        
exitIfEmpty endp 

exitIfEmptySub proc   
    push di
    push cx
                   
    lea di, SubString  
    inc di
    
    skip_spaces_sub:         
            inc di             
            push bx 
            mov bl, [di]
            cmp bl,' '        
            pop bx
           je skip_spaces_sub  
          
    push bx 
    mov bl, [di]                           
    cmp bl, 0Dh                  
    pop bx
    error_exit_marker_sub:
    je error_exit_sub               
       
    pop cx
    pop di
    ret                        
exitIfEmptySub endp      

exitIfNotSingleWord proc   
    push di
    push cx
                  
    lea di, SubString  
    inc di
    
    xor cx, cx        
    mov cl, [SubStringLength]
    check_substr:             
            inc di            
            push bx 
            mov bl, [di]
            cmp bl,' '        
            pop bx      
           je error_exit_marker 
    loop check_substr         
                              
    pop cx                    
    pop di                   
    ret                       
exitIfNotSingleWord endp      
                                              
                                              
                                              
searchSubString proc            
    push ax
    push bx                     
    push cx
    push dx                     
    push di                                       
                                 
    xor cx, cx 
    xor dx, dx
    xor bx, bx                  
    mov cl, [SubStringLength]            
    mov dx, si   
    lea di, SubString
    add di, 2 

    wordEnd:
        mov ah,[si]                 
        cmp ah, 09h                                  
        je complete 
                        
        cmp ah, ' '                                  
        je complete
                                    
        cmp ah, 0Dh
        je complete
                                     
        cmp ah, '$'                  
        je complete
                          
        inc si  
        jne wordEnd
           
    complete:                         
        sub si,dx                    
        mov bx,si
    
    compareWord:
        mov si, dx
                               
        comparestr:                
            mov ah,[si]              
            cmp ah,[di]             
            je  to_next             
            jne increase             
                             
        to_next:                 
            dec cx                  
            cmp cx,0             
            je equal 
            inc di
            inc si               
            jmp compareWord           
                                 
        increase:                                                 
            cmp ah, ' '                       
            je checkContain 
            cmp ah, '$'
            je checkContain
            inc si          
            jmp comparestr       
        
        checkContain:
            cmp cx, 0  
            jne endOfWord
                                 
    equal:
        skipTillTheEnd:
            mov ah,[si]
            inc si
            cmp ah, 30h
            jae skipTillTheEnd 
            dec si
                              
        call shift   
        jmp endOfWord
                                 
    endOfWord:                                   
    pop di                       
    pop dx                       
    pop cx
    pop bx 
    pop ax                      
    ret                            
searchSubString endp            
                                                    
shift proc  
    push dx                     
    push cx                      
    push bx               
                                
    lea ax, String  
    mov dh, 0h
    mov dl, [StringLength]
    add ax, dx                
    sub ax, si                    
    mov cx, ax                    
    add cx, 4                   
    push si
                                  
    shift_left:                  
        mov ah,[si]             
        sub si, bx               
        mov [si], ah             
        add si, bx               
        inc si                   
    loop shift_left             
    
    pop si                                                                           
    sub si,bx                    
    pop bx                       
    pop cx 
    pop dx                      
    ret                          
shift endp                       
               
end start                        