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
      * extract the supplier ID. This is copied into a return record,
      * along with a generated supplier name.
      *
      * To simplify the code, this program has minimal error-handling
      * logic.
      *
      *****************************************************************
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID.              GETSUPPL.
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
      *    SUPPLIER copybook
           COPY SUPPLIER REPLACING SUPPLIER BY WS-SUPPLIER.
      *
       01  WS-TASK-NUMBER       PIC 9(7) DISPLAY.
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
      *    Normally, we would lookup the supplier in another VSAM file
      *    Instead, just return the supplier as our task number
           MOVE SUPPLIER OF WS-STOCK-PART TO SUPPLIER-ID OF WS-SUPPLIER.
           MOVE SPACES TO SUPPLIER-NAME OF WS-SUPPLIER.
           MOVE EIBTASKN TO WS-TASK-NUMBER.
      *
      *    Convert to a formatted string
           STRING 'Supplier #' DELIMITED BY SIZE
                WS-TASK-NUMBER DELIMITED BY SIZE
                INTO SUPPLIER-NAME OF WS-SUPPLIER.
      *
      *    Copy the data into the supplied commarea
           MOVE WS-SUPPLIER TO DFHCOMMAREA.
      *
      *    Back to CICS
           EXEC CICS RETURN END-EXEC.
      *
           GOBACK.
