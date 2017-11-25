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
       PROGRAM-ID.              LINK2SUP.
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
      *    Working storage definitions
       01  WS-STORAGE.
           03 WS-TERMINAL-INPUT     PIC X(80)         VALUE SPACES.
           03 WS-RECEIVE-LENGTH     PIC 9(4)  COMP    VALUE ZERO.
           03 WS-TRANID             PIC X(4)          VALUE SPACES.
           03 WS-TRANID-LEN         PIC S9(8) COMP-4  VALUE ZERO.
           03 WS-TRANID-POS         PIC S9(8) COMP-4  VALUE ZERO.
           03 WS-SUPPLIER-TXT       PIC 9(8)  DISPLAY VALUE ZERO.
           03 WS-SUPPLIER-LEN       PIC S9(8) COMP-4  VALUE ZERO.
           03 WS-SUPPLIER-NO        PIC 9(8)  COMP-4  VALUE ZERO.
           03 LINK-RESP             PIC 9(8)  COMP    VALUE ZERO.
           03 LINK-RESP2            PIC 9(8)  COMP    VALUE ZERO.

      *    Message to display for normal completion.
      *    Display Supplier ID and name.
       01 RESPONSE-MESSAGE.
          03 FILLER PIC X(14) VALUE ' SUPPLIER ID: '.
          03 RESP-SUPPLIER-ID PIC 9(8) DISPLAY.
          03 FILLER PIC X(16) VALUE ' SUPPLIER NAME: '.
          03 RESP-SUPPLIER-NAME PIC X(40).

      *   Error message to display if Link to Liberty fails.
      *   Include slots for target PROGRAM, RESP and RESP2.
       01 ERROR-MESSAGE.
          03 FILLER PIC X(17) VALUE 'ERROR LINKING TO '.
          03 ERROR-PROG PIC X(8) DISPLAY.
          03 FILLER PIC X(7) VALUE '. RESP:'.
          03 ERROR-RESP PIC 9(8) DISPLAY.
          03 FILLER PIC X(7) VALUE ' RESP2:'.
          03 ERROR-RESP2 PIC 9(8) DISPLAY.

      *   Names of various CICS constructs
       77 LIBERTY-CHANNEL PIC X(16) VALUE 'L2LCHANNEL'.
       77 LIBERTY-PROGRAM PIC X(8)  VALUE 'GETSUPPI'.
       77 CONT-STOCK-PART PIC X(16) VALUE 'STOCK-PART'.
       77 CONT-SUPPLIER   PIC X(16) VALUE 'SUPPLIER'.
       77 CONT-USERID     PIC X(16) VALUE 'USERID'.
      *
      *
       PROCEDURE DIVISION USING DFHEIBLK.
      *
       MAIN-PROCESSING SECTION.

      *    Receive data from terminal
           MOVE LENGTH OF WS-TERMINAL-INPUT TO WS-RECEIVE-LENGTH.
           EXEC CICS RECEIVE INTO(WS-TERMINAL-INPUT)
                     LENGTH(WS-RECEIVE-LENGTH) END-EXEC.
      *    Fold input to uppercase if not already done by CICS
           MOVE FUNCTION UPPER-CASE(WS-TERMINAL-INPUT) 
                TO WS-TERMINAL-INPUT.

      *    Perform very basic parsing of terminal input data:
      *    1) Find length of tranid in case it's less than 4.
           INSPECT EIBTRNID TALLYING WS-TRANID-LEN 
                   FOR CHARACTERS BEFORE INITIAL SPACE.
      *    2) Find tranid in terminal input
           INSPECT WS-TERMINAL-INPUT(1:WS-RECEIVE-LENGTH) 
                   TALLYING WS-TRANID-POS FOR CHARACTERS 
                   BEFORE INITIAL EIBTRNID(1:WS-TRANID-LEN)
      *    3) Find action in remainder of string after tranid
           UNSTRING WS-TERMINAL-INPUT(1 + WS-TRANID-POS:
                    WS-RECEIVE-LENGTH - WS-TRANID-POS)
                DELIMITED BY ALL SPACE INTO
                WS-TRANID 
                WS-SUPPLIER-TXT COUNT IN WS-SUPPLIER-LEN
           END-UNSTRING.

      *    This example only needs us to populate the supplier ID
      *    field. Initialize the structure and store our fake supplier
      *    in the STOCK-PART structure.
           MOVE LOW-VALUES TO WS-STOCK-PART.

      *    Check if a valid supplier ID was provided, else use
      *    the CICS task number
           IF WS-SUPPLIER-LEN > 0 AND WS-SUPPLIER-LEN < 9 AND
                WS-SUPPLIER-TXT NUMERIC THEN
              MOVE WS-SUPPLIER-TXT TO WS-SUPPLIER-NO
           ELSE
              MOVE EIBTASKN TO WS-SUPPLIER-NO
           END-IF.

      *    Update the stock part supplier ID
           MOVE WS-SUPPLIER-NO TO SUPPLIER IN WS-STOCK-PART.

      *    Write the stock part to the correct container.
           EXEC CICS PUT CONTAINER(CONT-STOCK-PART)
                     CHANNEL(LIBERTY-CHANNEL)
                     FROM(WS-STOCK-PART) END-EXEC.

      *    Link to Liberty J2EE program passing channel.
           EXEC CICS LINK PROGRAM(LIBERTY-PROGRAM)
                     CHANNEL(LIBERTY-CHANNEL)
                     RESP(LINK-RESP) RESP2(LINK-RESP2) END-EXEC.

      *    Perform basic response checking from LINK, report error.
           IF LINK-RESP NOT EQUAL DFHRESP(NORMAL) THEN

              MOVE LIBERTY-PROGRAM TO ERROR-PROG
              MOVE LINK-RESP TO ERROR-RESP
              MOVE LINK-RESP2 TO ERROR-RESP2

      *       Send the response data to the terminal.
              EXEC CICS SEND TEXT FROM(ERROR-MESSAGE)
                     ERASE FREEKB END-EXEC
      *
      *       Return control to CICS (end transaction).
              EXEC CICS RETURN END-EXEC
           END-IF.

      *    Normal response from LINK so continue...
      *    Get Liberty output container from the channel
           EXEC CICS GET CONTAINER(CONT-SUPPLIER)
                     CHANNEL(LIBERTY-CHANNEL)
                     INTO(WS-SUPPLIER) END-EXEC.

      *    Copy fields from container structure to output message.
           MOVE SUPPLIER-ID IN WS-SUPPLIER TO RESP-SUPPLIER-ID.
           MOVE SUPPLIER-NAME IN WS-SUPPLIER TO RESP-SUPPLIER-NAME.

      *    Send the complete response message to the terminal.
           EXEC CICS SEND TEXT FROM(RESPONSE-MESSAGE)
                     ERASE FREEKB END-EXEC.
      *
      *    Return control to CICS (end transaction).
           EXEC CICS RETURN END-EXEC.
      *
           GOBACK.

