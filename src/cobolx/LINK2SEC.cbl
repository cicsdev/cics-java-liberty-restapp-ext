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
      * Simple CICS terminal program to LINK to a Liberty Java program		
      * to retrieve information about the security context for the
      * Linked-to Java task.
      *
      * To simplify the code, this program has minimal error-handling
      * logic, except on the LINK to Liberty call itself.
      *
      *****************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID.              LINK2SEC.
       DATE-WRITTEN.            November 2017.
      *
       ENVIRONMENT DIVISION.
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.

      *    Working storage definitions
       01  WS-STORAGE.
           03 LINK-RESP             PIC 9(8)  COMP    VALUE ZERO.
           03 LINK-RESP2            PIC 9(8)  COMP    VALUE ZERO.

      *    Message to display for normal completion.
      *    Display Link to Liberty USERID and Java Principal name.
       01 RESPONSE-MESSAGE.
          03 FILLER PIC X(13)            VALUE 'CICS USERID: '.
          03 RESP-USERID    PIC X(10)    VALUE SPACES.
          03 FILLER PIC X(17)            VALUE ' Java Principal: '.
          03 RESP-PRINCIPAL PIC X(10)    VALUE SPACES.

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
       77 LIBERTY-CHANNEL PIC X(16)   VALUE 'L2LCHANNEL'.
       77 LIBERTY-PROGRAM PIC X(8)    VALUE 'L2LSEC'.
       77 CONT-USERID     PIC X(16)   VALUE 'USERID'.
       77 CONT-PRINCIPAL  PIC X(16)   VALUE 'PRINCIPAL'.
      *
      *
       PROCEDURE DIVISION USING DFHEIBLK.
      *
       MAIN-PROCESSING SECTION.

      *    Link to Liberty J2EE program creating channel.
           EXEC CICS LINK PROGRAM(LIBERTY-PROGRAM)
                     CHANNEL(LIBERTY-CHANNEL)
                     RESP(LINK-RESP) RESP2(LINK-RESP2) END-EXEC.

      *    Perform basic response checking from LINK, report error.
           IF LINK-RESP NOT EQUAL DFHRESP(NORMAL) THEN

              MOVE LIBERTY-PROGRAM TO ERROR-PROG
              MOVE LINK-RESP TO ERROR-RESP
              MOVE LINK-RESP2 TO ERROR-RESP2

      *       Send the error response to the terminal.
              EXEC CICS SEND TEXT FROM(ERROR-MESSAGE)
                     ERASE FREEKB END-EXEC
      * 
           ELSE

      *       Normal response from LINK so continue...
      *       Get USERID output container from the channel
              EXEC CICS GET CONTAINER(CONT-USERID)
                        CHANNEL(LIBERTY-CHANNEL)
                        INTO(RESP-USERID) END-EXEC

      *       Get PRINCIPAL output container from the channel
              EXEC CICS GET CONTAINER(CONT-PRINCIPAL)
                        CHANNEL(LIBERTY-CHANNEL)
                        INTO(RESP-PRINCIPAL) END-EXEC

      *       Send the complete response message to the terminal.
              EXEC CICS SEND TEXT FROM(RESPONSE-MESSAGE)
                        ERASE FREEKB END-EXEC
      *
           END-IF.

      *    Return control to CICS (end transaction).
           EXEC CICS RETURN END-EXEC.
      *
           GOBACK.

