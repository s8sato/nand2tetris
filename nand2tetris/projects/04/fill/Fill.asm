// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

    @8192   // 256 * 512 / 16
    D=A
    @number_of_words
    M=D

(MAIN)

    @i
    M=0

    // goto BLACKEN if key is pressed
    @KBD
    D=M
    @BLACKEN
    D;JNE

    // goto WHITEN otherwise

(WHITEN)

    // whiten the ith word
    @i
    D=M
    @SCREEN
    A=A+D
    M=0

    // i++
    @i
    M=M+1

    // goto WHITEN if i < number_of_words
    @i
    D=M
    @number_of_words
    D=D-M
    @WHITEN
    D;JLT

    // goto MAIN otherwise
    @MAIN
    0;JMP

(BLACKEN)

    // blacken the ith word
    @i
    D=M
    @SCREEN
    A=A+D
    M=-1

    // i++
    @i
    M=M+1

    // goto BLACKEN if i < number_of_words
    @i
    D=M
    @number_of_words
    D=D-M
    @BLACKEN
    D;JLT

    // goto MAIN otherwise
    @MAIN
    0;JMP
