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
       01  STOCK-PART.
           03   PART-ID                 PIC 9(8) DISPLAY.
           03   SUPPLIER                PIC 9(8) DISPLAY.
           03   UNIT-PRICE              PIC 99999V99 PACKED-DECIMAL.
           03   LAST-ORDER-DATE.
                05  LAST-ORDER-DATE-YY  PIC X(2).
                05  FILLER              PIC X(1) VALUE '-'.
                05  LAST-ORDER-DATE-MM  PIC X(2).
                05  FILLER              PIC X(1) VALUE '-'.
                05  LAST-ORDER-DATE-DD  PIC X(2).
           03   STOCK-QUANTITY          PIC 9(8) BINARY.
           03   NEXT-ORDER-DATE.
                05  NEXT-ORDER-DATE-YY  PIC X(2).
                05  FILLER              PIC X(1) VALUE '-'.
                05  NEXT-ORDER-DATE-MM  PIC X(2).
                05  FILLER              PIC X(1) VALUE '-'.
                05  NEXT-ORDER-DATE-DD  PIC X(2).
           03   DESCRIPTION             PIC X(40).
