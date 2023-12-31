IDENTIFICATION DIVISION.
PROGRAM-ID. ROMAN-NUMERAL.
*> PROGRAM TO CONVERT ROMAN NUMERALS TO THEIR DECIMAL EQUIVALENT

ENVIRONMENT DIVISION.

INPUT-OUTPUT DIVISION.

DATA DIVISION.

WORKING-STORAGE SECTION.
77 EOF-SWITCH   PIC 9    VALUE 1.
77 SWITCH       PIC 9.
77 N 	        PIC S9(2)  COMP.
77 SUM1         PIC S9(8)  COMP.
77 I            PIC S9(2)  COMP.
77 PREV         PIC S9(4)  COMP.
77 D            PIC S9(4)  COMP.
01 ARRAY-AREA.
   02 R         PIC X(1)   OCCURS 15 TIMES.
01 INPUT-DATA-RECORD.
   02 IN-R      PIC X(15).
   02 FILLER    PIC X(65).
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
   OPEN INPUT INPUT-FILE, OUTPUT OUTPUT-FILE.
   WRITE OUTPUT-LINE FROM OUTPUT-TITLE-LINE
      AFTER ADVANCING 0 LINES.
   WRITE OUTPUT-LINE FROM OUTPUT-UNDERLINE-1
      AFTER ADVANCING 1 LINE.
   WRITE OUTPUT-LINE FROM OUTPUT-COLUMN-HEADINGS
      AFTER ADVANCING 1 LINE.
   WRITE OUTPUT-LINE FROM OUTPUT-UNDERLINE-2
      AFTER ADVANCING 1 LINE.
   READ INPUT-FILE INTO INPUT-DATA-RECORD
      AT END MOVE ZERO TO EOF-SWITCH.
   PERFORM PROC-BODY
      UNTIL EOF-SWITCH IS EQUAL TO ZERO.
   CLOSE INPUT-FILE, OUTPUT-FILE.
   STOP RUN.

PROC-BODY.
   MOVE IN-R TO ARRAY-AREA.
   MOVE 1 TO N.
   PERFORM SEARCH-LOOP
      UNTIL R(N) IS EQUAL TO SPACE.
   SUBTRACT 1 FROM N.
   PERFORM CONV.
   IF SWITCH IS EQUAL TO 1
      MOVE SUM1 TO V
      MOVE ARRAY-AREA TO OUT-R
      WRITE OUTPUT-LINE FROM OUTPUT-TABLE-RECORD
         AFTER ADVANCING 1 LINE
   ELSE NEXT SENTENCE.
   READ INPUT-FILE INTO INPUT-DATA-RECORD
     AT END MOVE ZERO TO EOF-SWITCH.

SEARCH-LOOP.
   ADD 1 TO N.
   
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
   ELSE IF R(I) IS EQUAL TO "V"
      MOVE 5 TO D
   ELSE IF R(I) IS EQUAL TO "X"
      MOVE 10 TO D
   ELSE IF R(I) IS EQUAL TO "L"
      MOVE 50 TO D
   ELSE IF R(I) IS EQUAL TO "C"
      MOVE 100 TO D
   ELSE IF R(I) IS EQUAL TO "D"
      MOVE 500 TO D
   ELSE IF R(I) IS EQUAL TO "M"
      MOVE 1000 TO D
   ELSE MOVE 2 TO SWITCH
        MOVE ARRAY-AREA TO OUT-ER-R
        WRITE OUTPUT-LINE FROM OUTPUT-ERROR-MESS 
           AFTER ADVANCING 1 LINE.
   ADD D TO SUM1.
   IF D IS GREATER THAN PREV
      COMPUTE SUM1 = SUM1 - 2 * PREV
   ELSE NEXT SENTENCE.
   MOVE D TO PREV.



