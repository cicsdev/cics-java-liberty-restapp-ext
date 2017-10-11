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
      * Simple CICS terminal program to receive a supplier ID from the
      * terminal (if specified, otherwise use Task number), LINK to
      * specified Liberty program, and send results to the terminal.
      *
      * To simplify the code, this program has minimal error-handling
      * logic, except on the LINK to Liberty call itself.
      *
      *****************************************************************
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID.              SUPPLINK.
       DATE-WRITTEN.            October 2017.
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
      *    SUPPLIER copybook
           COPY SUPPLIER REPLACING SUPPLIER BY WS-SUPPLIER.
      *
       01  WS-TERMINAL-INPUT    PIC X(80).
       01  WS-SUPPLIER-NO       PIC 9(8) DISPLAY.
       01  WS-LIBERTY-CHANNEL   PIC X(16) VALUE 'LIBERTY-CHANNEL'.
       01  WS-LIBERTY-PROGRAM   PIC X(8)  VALUE 'GETSUPPI'.
       01  WS-STORAGE.
           03 WS-RECEIVE-LENGTH             PIC 9(4) COMP.
           03 FILLER                        PIC 9(4) COMP.
           03 LINK-RESP                     PIC 9(8) COMP.
           03 LINK-RESP2                    PIC 9(8) COMP.
           03 SUPPID-START                  PIC 9(8) COMP.
           03 SUPPID-END                    PIC 9(8) COMP.

      * Message to display for normal completion.
      * Display Link to Liberty USERID, Supplier ID and name.
       01 RESPONSE-MESSAGE.
          03 FILLER PIC X(13) VALUE 'CICS USERID: '.
          03 RESP-CICS-USERID PIC X(8) DISPLAY. 
          03 FILLER PIC X(14) VALUE ' SUPPLIER ID: '. 
          03 RESP-SUPPLIER-ID PIC 9(8) DISPLAY. 
          03 FILLER PIC X(16) VALUE ' SUPPLIER NAME: '. 
          03 RESP-SUPPLIER-NAME PIC X(40). 

      * Error message to display if Link to Liberty fails.
      * Include slots for target PROGRAM, RESP and RESP2.
       01 ERROR-MESSAGE.
          03 FILLER PIC X(17) VALUE 'ERROR LINKING TO '.
          03 ERROR-PROG PIC X(8) DISPLAY. 
          03 FILLER PIC X(7) VALUE '. RESP:'. 
          03 ERROR-RESP PIC 9(8) DISPLAY. 
          03 FILLER PIC X(7) VALUE ' RESP2:'.
          03 ERROR-RESP2 PIC 9(8) DISPLAY. 
      *
      *
       PROCEDURE DIVISION USING DFHEIBLK.
      *
       MAIN-PROCESSING SECTION.
      *
           MOVE LENGTH OF WS-TERMINAL-INPUT TO WS-RECEIVE-LENGTH.
           EXEC CICS RECEIVE INTO(WS-TERMINAL-INPUT) 
                     LENGTH(WS-RECEIVE-LENGTH) END-EXEC.

      * Perform very basic "parsing" of terminal input data.
      * Starting after TRANID, skip blanks and get numeric field.

      *    Start at first character after TRANID (assuming 4 char tranid)
           MOVE 8 TO SUPPID-START.
      *    Scan forward until end of input or non-blank character found.     
           PERFORM UNTIL SUPPID-START GREATER THAN WS-RECEIVE-LENGTH OR
                         WS-TERMINAL-INPUT(SUPPID-START:1) NOT EQUAL ' '
             ADD 1 TO SUPPID-START
           END-PERFORM.

      *    Start at previously determined point.
           MOVE SUPPID-START TO SUPPID-END.
      *    Scan forward until end of input or *blank* character found.     
           PERFORM UNTIL SUPPID-END GREATER THAN WS-RECEIVE-LENGTH OR
                         WS-TERMINAL-INPUT(SUPPID-END:1) = ' '
             ADD 1 TO SUPPID-END
           END-PERFORM.

      *    If a supplier ID was provided on the command, use it.
           IF SUPPID-END GREATER THAN SUPPID-START THEN
              MOVE WS-TERMINAL-INPUT(SUPPID-START: 
                   SUPPID-END - SUPPID-START) TO WS-SUPPLIER-NO
           ELSE
      *    Otherwise use the CICS Task number as a default ID.
              MOVE EIBTASKN TO WS-SUPPLIER-NO
           END-IF.

      *    Store Supplier ID in StockPart structure and 
      *    write it to 'STOKPART' container.
           MOVE WS-SUPPLIER-NO TO SUPPLIER.
           EXEC CICS PUT CONTAINER('STOKPART') 
                     CHANNEL(WS-LIBERTY-CHANNEL) 
                     FROM(WS-STOCK-PART) END-EXEC.

      * Link to Liberty J2EE program passing channel.
           EXEC CICS LINK PROGRAM(WS-LIBERTY-PROGRAM) 
                     CHANNEL(WS-LIBERTY-CHANNEL) 
                     RESP(LINK-RESP) RESP2(LINK-RESP2) END-EXEC.

      * Perform basic response checking from LINK, report error.
           IF LINK-RESP NOT EQUAL DFHRESP(NORMAL) THEN
              MOVE WS-LIBERTY-PROGRAM TO ERROR-PROG
              MOVE LINK-RESP TO ERROR-RESP
              MOVE LINK-RESP2 TO ERROR-RESP2
      *    Send the response data to the terminal.
              EXEC CICS SEND TEXT FROM(ERROR-MESSAGE) 
                     ERASE FREEKB END-EXEC    
      *
      *    Return control to CICS (end transaction).
              EXEC CICS RETURN END-EXEC
           END-IF.

      * Normal response from LINK so continue...
      * Get Liberty output container from the channel
           EXEC CICS GET CONTAINER('SUPPLIER') 
                     CHANNEL(WS-LIBERTY-CHANNEL) 
                     INTO(WS-SUPPLIER) END-EXEC.

      * Copy fields from container structure to output message.
           MOVE SUPPLIER-ID TO RESP-SUPPLIER-ID.
           MOVE SUPPLIER-NAME TO RESP-SUPPLIER-NAME.

      * Copy Liberty USERID from container direct to output message.
           EXEC CICS GET CONTAINER('USERID') 
                     CHANNEL(WS-LIBERTY-CHANNEL) 
                     INTO(RESP-CICS-USERID) END-EXEC.

      *    Send the complete response message to the terminal.
           EXEC CICS SEND TEXT FROM(RESPONSE-MESSAGE) 
                     ERASE FREEKB END-EXEC.    
      *
      *    Return control to CICS (end transaction).
           EXEC CICS RETURN END-EXEC.
      *
           GOBACK.
