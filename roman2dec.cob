IDENTIFICATION DIVISION.
PROGRAM-ID. ROMAN-NUMERAL-CONVERTER.
*> PROGRAM TO CONVERT ROMAN NUMERALS TO THEIR DECIMAL EQUIVALENT

ENVIRONMENT DIVISION.

INPUT-OUTPUT SECTION.
FILE-CONTROL.
select INPUT-FILE assign to dynamic input_file_name organization is line sequential.
  
      

DATA DIVISION.
FILE SECTION.
   FD INPUT-FILE.
   01 INPUT-DATA.
      02 IN-R      PIC X(15).
    
      



WORKING-STORAGE SECTION.
01 file-info.
       05 file-size        pic x(8) comp-x.
       05 file-date.
          10 f-day         pic x comp-x.
          10 f-month       pic x comp-x.
          10 f-year        pic xx comp-x.
       05 file-time.
          10 f-hours       pic x comp-x.
          10 f-minutes     pic x comp-x.
          10 f-seconds     pic x comp-x.
          10 f-hundredths  pic x comp-x.
77 input_file_name  pic x(30).
77 EOF-SWITCH   PIC 9    VALUE 1.
77 SWITCH       PIC 9.
77 N 	        PIC S9(2)  COMP.
77 SUM1         PIC S9(8)  COMP.
77 I            PIC S9(2)  COMP.
77 PREV         PIC S9(4)  COMP.
77 D            PIC S9(4)  COMP.

01 ARRAY-AREA.
   02 R         PIC X(1)   OCCURS 16 TIMES.
01 INPUT-DATA-RECORD.
   02 IN-R      PIC X(15).
   *> get rid of filler
01 OUTPUT-TITLE-LINE.
   02 FILLER    PIC X(28)  VALUE
                    "  ROMAN NUMBER EQUIVALENTS  ". 
01 OUTPUT-UNDERLINE-1.
   02 FILLER    PIC X(30)  VALUE
                    "------------------------------". 
01 OUTPUT-COLUMN-HEADINGS.
   02 FILLER    PIC X(14)  VALUE
                    "  ROMAN NUMBER". 
   02 FILLER    PIC X(16)  VALUE
                    "     DEC. EQUIV.". 
01 OUTPUT-UNDERLINE-2.
   02 FILLER    PIC X(30)  VALUE
                    " ---------------------------- ". 
01 OUTPUT-TABLE-RECORD.
   02 FILLER    PIC X      VALUE SPACE.
   02 OUT-R     PIC X(15).
   02 FILLER    PIC X(3)   VALUE SPACES.
   02 V         PIC Z(9).
01 OUTPUT-ERROR-MESS.
   02 FILLER    PIC X      VALUE SPACE.
   02 OUT-ER-R  PIC X(15).
   02 FILLER    PIC X(24)  VALUE
                    "   ILLEGAL ROMAN NUMERAL". 
  
PROCEDURE DIVISION.


PERFORM get_file_name.
display " "
OPEN INPUT INPUT-FILE.
display "------------------------------"
display " ROMAN NUMBER CONVERSION."
display " ---------------------------- "
display " conversion table value roman to decimal"
display " I = 1 "
display " V = 5"
display " X = 10"
display " L = 50"
display " C = 100"
display " D = 500"
display " M = 1000"

READ INPUT-FILE INTO INPUT-DATA-RECORD
   AT END MOVE ZERO TO EOF-SWITCH.
PERFORM PROC-BODY
   UNTIL EOF-SWITCH IS EQUAL TO ZERO.
CLOSE INPUT-FILE.
display "  ---------------------------- "
STOP RUN.
PROC-BODY.
   MOVE IN-R IN INPUT-DATA-RECORD TO ARRAY-AREA.
   MOVE 1 TO N.
   PERFORM SEARCH-LOOP
      UNTIL R(N) IS EQUAL TO SPACE.
         compute N = N - 1.
   PERFORM CONV.
   IF SWITCH IS EQUAL TO 1
      MOVE SUM1 TO V
      MOVE ARRAY-AREA TO OUT-R
      display OUTPUT-TABLE-RECORD
   end-if.
   READ INPUT-FILE INTO INPUT-DATA-RECORD
     AT END MOVE ZERO TO EOF-SWITCH.

SEARCH-LOOP.
   compute N = N + 1.
  
   
CONV.
   MOVE ZERO TO SUM1.
   MOVE 1001 TO PREV.
   MOVE 1 TO SWITCH.
   PERFORM CONVERSION-LOOP
      VARYING I FROM 1 BY 1
      UNTIL I IS GREATER THAN N OR
         SWITCH IS EQUAL TO 2.

   

CONVERSION-LOOP.
   IF R(I) IS EQUAL TO "I"
      MOVE 1 TO D
      add d to sum1
      if d > prev
         compute sum1 = sum1 - 2 * prev
      end-if
      move d to prev
   ELSE IF R(I) IS EQUAL TO "V"
      MOVE 5 TO D
      add d to sum1
      if d > prev
         compute sum1 = sum1 - 2 * prev
      end-if
      move d to prev
   ELSE IF R(I) IS EQUAL TO "X"
      MOVE 10 TO D
      add d to sum1
      if d > prev
         compute sum1 = sum1 - 2 * prev
      end-if
      move d to prev
   ELSE IF R(I) IS EQUAL TO "L"
      MOVE 50 TO D
      add d to sum1
      if d > prev
         compute sum1 = sum1 - 2 * prev
      end-if
      move d to prev
   ELSE IF R(I) IS EQUAL TO "C"
      MOVE 100 TO D
      add d to sum1
      if d > prev
         compute sum1 = sum1 - 2 * prev
      end-if
      move d to prev
   ELSE IF R(I) IS EQUAL TO "D"
      MOVE 500 TO D
      add d to sum1
      if d > prev
         compute sum1 = sum1 - 2 * prev
      end-if
      move d to prev
   ELSE IF R(I) IS EQUAL TO "M"
      MOVE 1000 TO D
      add d to sum1
      if d > prev
         compute sum1 = sum1 - 2 * prev
      end-if
      move d to prev
   ELSE MOVE 2 TO SWITCH
        MOVE ARRAY-AREA TO OUT-ER-R
        display OUTPUT-ERROR-MESS 
   end-if.
 

get_file_name.
 display "Welcome to the Roman Numeral Conversion:"
 display " Enter Filename to Convert "
    Accept input_file_name from Console.
    call "CBL_CHECK_FILE_EXIST" using input_file_name file-info.
    if return-code not equal zero
        display "Error: File " input_file_name(1:20) " does not exist"
         PERFORM get_file_name.

