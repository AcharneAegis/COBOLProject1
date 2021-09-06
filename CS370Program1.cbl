       IDENTIFICATION DIVISION.
       PROGRAM-ID. CS370PROGRAM1.
       AUTHOR. P W ASKEW.
      ******************************************************************
      * This program serves to give practive with the basics of COBOL
      * The problem given is a CEO needs us to write a report on the 
      * employees that have not recieved a raise in the last year
      * ******
      * INPUT:
      *    The PR1F21-Knox.txt file contains the following
      *        1.  Store ID
      *        2.  Employee ID
      *        3.  Employee Position
      *        4.  Employee Last Name
      *        5.  Employee First Name
      *        6.  Employee Middle Initial
      *        7.  Hire Date
      *        8.  Employee Status
      *        9.  Separation Date
      *        10.  Starting Yearly Salary
      *        11.  Date of Last Pay Increase
      *        12.  Current Yearly Salary
      * *******
      * OUTPUT:
      *    The SALARY REPORT file contains the following
      *    *************
      *    DETAIL LINE:
      *        1.  Employee ID
      *        2.  Employee Position
      *        3.  Employee First Name
      *        4.  Employee Last Name
      *        5.  Employee Status
      *        6.  Date of Last Pay Increase
      *        7.  Current Salary
      *    **************
      *    FINAL TOTALS
      *        1.  Salary Total
      *    *************
      * CALCULATIONS
      *    TO BE FILLED IN LATER
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE
               ASSIGN TO 'Knox.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT EMP-REPORT-FILE
               ASSIGN TO PRINTER 'Knox-Salary-Report'.

       DATA DIVISION.
       FILE SECTION.

       FD EMPLOYEE-FILE
           RECORD CONTAINS 75 CHARACTERS.

       01  EMPLOYEE-RECORD.
           05  EMP-STORE-ID            PIC A(4).
           05  EMP-ID                  PIC X(5).
           05  EMP-POSITION            PIC A(2).
           05  EMP-LAST-NAME           PIC X(10).
           05  EMP-FIRST-NAME          PIC X(10).
           05  EMP-STATUS              PIC X(1).
           05  FILLER                  PIC 9(8).
           05  FILLER                  PIC 9(8).
           05  EMP-LAST-RAISE-DATE     PIC 9(8).
           05  EMP-CURRENT-SALARY      PIC 9(8).

       FD EMP-REPORT-FILE
           RECORD CONTAINS 80 CHARACTERS.

       01  REPORT-RECORD               PIC X(80).

       WORKING-STORAGE SECTION.
       
       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG                PIC X           VALUE ' '.
               88 NO-MORE-DATA                         VALUE 'N'.

       01  DETAIL-FIELDS.
           05  DF
           