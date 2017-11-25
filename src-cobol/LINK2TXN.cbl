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
      * Simple terminal program to investigate 
      * behaviour of Link to Liberty.
      * Write to a TSQ, call L2L program passing action from terminal,
      *  then also rollback or commit depending on action.
      * Transaction may also be abended in Linked Java method.
      *
      * To simplify the code, this program has minimal error-handling
      * logic, except on the LINK to Liberty call itself.
      *
      *****************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID.              LINK2TXN.
       DATE-WRITTEN.            November 2017.
      *
       ENVIRONMENT DIVISION.
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
      *
      *    Working storage definitions
       01  WS-STORAGE.
           03 WS-TERMINAL-INPUT     PIC X(80)         VALUE SPACES.
           03 WS-PARAMETER-INPUT    PIC X(80)         VALUE SPACES.
           03 WS-RECEIVE-LENGTH     PIC 9(4)  COMP    VALUE ZERO.
           03 WS-TRANID-LEN         PIC S9(8) COMP-4  VALUE ZERO.
           03 WS-TRANID-POS         PIC S9(8) COMP-4  VALUE ZERO.
           03 WS-TRANID             PIC X(4)          VALUE SPACES.
           03 WS-ACTION             PIC X(10)         VALUE SPACES.
           03 WS-ACTION-LEN         PIC S9(8) COMP-4  VALUE ZERO.
           03 LINK-RESP             PIC 9(8)  COMP    VALUE ZERO.
           03 LINK-RESP2            PIC 9(8)  COMP    VALUE ZERO.

      *    Message to write to TSQ
       01 TSQ-MESSAGE.
          03 FILLER PIC X(14) VALUE 'WRITTEN FROM: '.
          03 TSQ-PROGRAM PIC X(8) VALUE 'LINK2TXN'.
          03 FILLER PIC X(10) VALUE ' BY TASK: '.
          03 TSQ-TASK-ID PIC 9(8) DISPLAY.
          03 FILLER PIC X(16) VALUE ' FOR ACTION: '.
          03 TSQ-ACTION PIC X(10) VALUE SPACES.

      *    Message to display on normal completion.
       01 RESPONSE-MESSAGE.
          03 FILLER PIC X(12) VALUE 'TRANSACTION '.
          03 RESP-TRAN PIC X(4).
          03 FILLER PIC X(6) VALUE ' TASK '.
          03 RESP-TASK PIC 9(8) DISPLAY.
          03 FILLER PIC X(8) VALUE ' ACTION '.
          03 RESP-ACTION PIC X(10).
          03 FILLER PIC X(8) VALUE 'COMPLETE'.

      *   Error message to display if Link to Liberty fails.
       01 ERROR-MESSAGE.
          03 FILLER PIC X(17) VALUE 'ERROR LINKING TO '.
          03 ERROR-PROG PIC X(8) DISPLAY.
          03 FILLER PIC X(7) VALUE '. RESP:'.
          03 ERROR-RESP PIC 9(8) DISPLAY.
          03 FILLER PIC X(7) VALUE ' RESP2:'.
          03 ERROR-RESP2 PIC 9(8) DISPLAY.

      *   Various constants, eg CICS resource names.
       77 LIBERTY-CHANNEL PIC X(16) VALUE 'L2LCHANNEL'.
       77 LIBERTY-PROGRAM PIC X(8)  VALUE 'L2LTRAN'.
       77 CONT-ACTION     PIC X(16) VALUE 'ACTION'.
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
                    DELIMITED BY ALL SPACE 
                    INTO WS-TRANID WS-ACTION.

      *    Write an item to a TSQ to demonstrate recoverability.
      *    If the transaction abends, the write will be rolled back.
           MOVE EIBTASKN TO TSQ-TASK-ID.
           MOVE WS-ACTION TO TSQ-ACTION.
           EXEC CICS WRITEQ TS QUEUE(LIBERTY-PROGRAM) 
                FROM(TSQ-MESSAGE) END-EXEC.

      *    Write the action container, may have trailing blanks.
           EXEC CICS PUT CONTAINER(CONT-ACTION) CHAR
                     CHANNEL(LIBERTY-CHANNEL)
                     FROM(WS-ACTION) END-EXEC.

      *    Link to Liberty J2EE program passing channel.
           EXEC CICS LINK PROGRAM(LIBERTY-PROGRAM)
                     CHANNEL(LIBERTY-CHANNEL)
                     RESP(LINK-RESP) RESP2(LINK-RESP2) END-EXEC.

      *    Perform basic response checking from LINK, report error.
           IF LINK-RESP NOT EQUAL DFHRESP(NORMAL) THEN

      *       Roll back the transaction if an error occurred.
              EXEC CICS SYNCPOINT ROLLBACK END-EXEC

      *       Send error message to terminal and return.
              MOVE LIBERTY-PROGRAM TO ERROR-PROG
              MOVE LINK-RESP TO ERROR-RESP
              MOVE LINK-RESP2 TO ERROR-RESP2

      *       Send the response data to the terminal.
              EXEC CICS SEND TEXT FROM(ERROR-MESSAGE)
                     ERASE FREEKB END-EXEC
           ELSE 

      *       Fill in response message
              MOVE EIBTRNID TO RESP-TRAN
              MOVE EIBTASKN TO RESP-TASK
              MOVE WS-ACTION TO RESP-ACTION

      *       Send the complete response message to the terminal.
              EXEC CICS SEND TEXT FROM(RESPONSE-MESSAGE)
                   ERASE FREEKB END-EXEC
           END-IF.

      *    Return control to CICS, this will commit.
           EXEC CICS RETURN END-EXEC.
      *
           GOBACK.

