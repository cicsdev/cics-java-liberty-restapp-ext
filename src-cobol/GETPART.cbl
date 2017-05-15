       PROCESS NODYNAM,RENT,APOST,CICS,TRUNC(OPT)
      
      *****************************************************************
      * Licensed Materials - Property of IBM
      *
      * SAMPLE
      *
      * Copyright IBM Corp. 2017 All Rights Reserved
      *
      * Government Users Restricted Rights - Use, duplication or
      * disclosure restricted by GSA ADP Schedule Contract with
      * IBM Corp.
      *
      *****************************************************************
      *
      * Simple program used to receive a part ID in the commarea, and
      * return a fully-populated STOKPART copybook. This emulates a
      * COBOL program performing a lookup, for example, in a VSAM file.
      *
      * To simplify the code, this program has minimal error-handling
      * logic.
      *
      *****************************************************************
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID.              GETPART.
       DATE-WRITTEN.            May 2017.
      *
       ENVIRONMENT DIVISION.
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
      *
      *    STOCK-PART copybook
           COPY STOKPART REPLACING STOCK-PART BY WS-STOCK-PART.
      *
       01  WS-TASK-NUMBER       PIC 9(7) DISPLAY.
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA.
           03 CA-DATA-IN.
                05 PART-ID          PIC 9(8) DISPLAY.
                05 FILLER           PIC X(72).
           03 CA-DATA-OUT REDEFINES CA-DATA-IN.
                05 STOCK-PART       PIC X(80).
      *
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
      *
       MAIN-PROCESSING SECTION.
      *
      *    Assume the commarea is well-formed and use as our record
           MOVE DFHCOMMAREA TO WS-STOCK-PART.
      *
      *    Copy the supplied part ID
           MOVE PART-ID IN CA-DATA-IN TO PART-ID IN WS-STOCK-PART.
      *
      *    Make up some sample data in working storage
           MOVE 123456 TO SUPPLIER IN WS-STOCK-PART.
           MOVE 12.99 TO UNIT-PRICE IN WS-STOCK-PART.
           MOVE '17-05-15' TO LAST-ORDER-DATE IN WS-STOCK-PART.
           MOVE 85 TO STOCK-QUANTITY IN WS-STOCK-PART.
           MOVE '17-11-15' TO NEXT-ORDER-DATE IN WS-STOCK-PART.
      *
      *    Generate a varying name
           MOVE SPACES TO DESCRIPTION IN WS-STOCK-PART
           MOVE EIBTASKN TO WS-TASK-NUMBER.
           STRING 'Generated part #' DELIMITED BY SIZE
                WS-TASK-NUMBER DELIMITED BY SIZE
                INTO DESCRIPTION OF WS-STOCK-PART.
      *
      *    Copy the generated part information to the commarea
           MOVE WS-STOCK-PART TO CA-DATA-OUT IN DFHCOMMAREA.
      *
      *    Back to CICS
           EXEC CICS RETURN END-EXEC.
      *
           GOBACK.
