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
      * Simple program used to receive a record in the commarea and
      * write it out to a VSAM file. This program is the non-Java
      * part of the LinkCommareaResource example.
      *
      * To simplify the code, this program has minimal error-handling
      * logic.
      *
      *****************************************************************
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID.              ADDPART.
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
      *    Name of the CICS file to use
       77  FILE-NAME            PIC X(8) VALUE 'SMPLXMPL'.
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA          PIC X(80).
      *
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
      *
       MAIN-PROCESSING SECTION.
      *
      *    Assume the commarea is well-formed and use as our record
           MOVE DFHCOMMAREA TO WS-STOCK-PART.
      *
      *    Write to the file using CICS file control services
           EXEC CICS WRITE FILE(FILE-NAME)
                     FROM(WS-STOCK-PART)
                     RIDFLD(PART-ID of WS-STOCK-PART)
                     END-EXEC.
      *
      *    Update the description to prove we can pass data back in
      *    a commarea to Java
           MOVE '<ADDED>' TO DESCRIPTION OF WS-STOCK-PART(1:7).
      *
      *    Copy the updated record back to the commarea
           MOVE WS-STOCK-PART TO DFHCOMMAREA.
      *
      *    Back to CICS
           EXEC CICS RETURN END-EXEC.
      *
           GOBACK.
