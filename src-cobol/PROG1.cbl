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
      * Very simple program to take no commarea and write a message
      * using the COBOL DISPLAY command.
      *
      *****************************************************************
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID.              PROG1.
       DATE-WRITTEN.            May 2017.
      *
       ENVIRONMENT DIVISION.
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
      *
       LINKAGE SECTION.
      *
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
      *
       MAIN-PROCESSING SECTION.
      *
      *    No data in, no data out. Use DISPLAY to indicate success
           DISPLAY 'Task ' EIBTASKN ' in PROG1'.
      *
      *    Back to CICS
           EXEC CICS RETURN END-EXEC.
      *
           GOBACK.
