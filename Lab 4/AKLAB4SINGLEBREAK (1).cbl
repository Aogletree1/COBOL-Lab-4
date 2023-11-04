       IDENTIFICATION DIVISION.
       PROGRAM-ID. LAB5SINGLEBREAK.
       AUTHOR. YOURNAMEHERE.
      *************************
      *              LAB 5 - SINGLE LEVEL CONTROL BREAK AND
      *                      EVALUATE STATEMENTS
      *
      *    THIS LAB EXERCISE WILL GIVE YOU PRACTICE WITH A SINGLE 
      *    LEVEL CONTROL BREAK AND EVALUATE STATEMENTS.  
      *    A REPORT IS TO BE WRITTEN FROM A FILE CONTAINING 
      *    STUDENT INFORMATION EACH STUDENT HAS 4 GRADES. GET 
      *    AN AVERAGE FOR EACH STUDENT,ASSIGN A LETTER GRADE FOR 
      *    EACH STUDENT. STUDENTS ARE TO BE GROUPED TOGETHER BY CLASS
      *    AND THEN CALCULATE AN OVERALL AVERAGE FOR EACH CLASS 
      *******
      *    INPUT: THE DATA FILE IS A DISK FILE CONTAINING THE
      *        COURSE NUMBER, STUDENT NAME AND FOUR EXAM GRADES 
      *******
      *    OUTPUT:  
      *       A PRINTED REPORT CONTAINING A DETAIL LINE FOR EACH 
      *       STUDENT WITH COURSE NUMBER, STUDENT NAME,GRADE AVERAGE,
      *       A LETTER GRADE. IT WILL ALSO HAVE A TOTAL LINE WITH 
      *       THE CLASS AVERAGE FOR EACH COURSE
      *******
      *    CALCULATIONS:
      *        SUM OF THE FOUR EXAMS FOR EACH STUDENT
      *        AVERAGE IS THE SUM OF THE FOUR EXAMS / NUM OF TESTS
      *        SUM OF THE STUDENT AVERAGES
      *        COUNT OF THE STUDENTS IN CLASS
      *        CLASS AVERAGE IS THE SUM OF THE AVERAGES / COUNT OF
      *            STUDENTS
      ******
      *   INSTRUCTIONS
      *
      *   1. Change the Author Name to your TEAM name.
      *   2. Change the XXX in the Header-1 to your TEAM Number
      *   2. Check for a control break use an EVALUATE
      *   3. Add the four incoming grades together
      *   4. Get an individual average score
      *   5. Evaluate the average score and assign a letter grade
      *      use an EVALUATE statement
      *   4. When the break occurs print the Total Line for each 
      *      CLASS Group
      *************************
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLASS-FILE
               ASSIGN TO 'L5CLASSLIST.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT REPORT-FILE
               ASSIGN TO 'L5REPORT.TXT'.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  CLASS-FILE
           RECORD CONTAINS 49 CHARACTERS.
      *
       01  CF-RECORD.
      *
           05  CF-COURSE-NUM           PIC X(5).
           05  CF-NAME.
               10  CF-LASTNAME         PIC X(9).
               10  CF-FIRSTNAME        PIC X(11).
           05  FILLER                  PIC X(12).
           05  CF-GRADE1			   PIC 999.
           05  CF-GRADE2               PIC 999.
           05  CF-GRADE3               PIC 999.
           05  CF-GRADE4               PIC 999.

      *
       FD  REPORT-FILE
           RECORD CONTAINS 80 CHARACTERS.
      *
       01  REPORT-REC               PIC X(80).
      *

       WORKING-STORAGE SECTION.
      *
       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG            PIC X         VALUE 'Y'.
           05  FIRST-RECORD        PIC X         VALUE 'Y'.
     *
       01  CURRENT-DATE.
           05  CD-YEAR             PIC XXXX.
           05  CD-MONTH            PIC XX.
           05  CD-DAY              PIC XX.
      *
       01 HOLD-FIELD.
           05  CLASS-HOLD          PIC X(5)      VALUE SPACES.
      *
       01  DETAIL-FIELDS.
           05  DF-AVG              PIC S999V9    VALUE +0.
           05  DF-SUM-GRADES       PIC S9(4)     VALUE +0.
      *
       01  TOTAL-FIELDS.
           05  TF-CLASS-AVG        PIC S999V9    VALUE +0.
           05  TF-SUM-AVERAGES     PIC S9(4)V9   VALUE +0.
           05  TF-NUM-STUDENTS     PIC S99       VALUE +0.
      *
       01  REPORT-FIELDS.
           05  PROPER-SPACING      PIC 9        VALUE 1.
      *
       01  CONSTANT-FIELDS.
           05  CF-NUM-TESTS        PIC S9        VALUE +4.
      **************************OUTPUT AREA***************************
       01  HEADER-1.
           05                      PIC X(5)      VALUE SPACES.
           05                      PIC X(22)     VALUE 'XXX'.
           05                      PIC X(28)     VALUE 'CLASS GRADES'.
           05  H1-DATE.
               10  H1-MONTH        PIC XX.
               10  FILLER          PIC X         VALUE '/'.
               10  H1-DAY          PIC XX.
               10  FILLER          PIC X         VALUE '/'.
               10  H1-YEAR         PIC XXXX.
      *
       01  HEADER-2.
           05                      PIC X(6)      VALUE SPACES.
           05                      PIC X(6)      VALUE 'COURSE'.
           05                      PIC X(13)     VALUE SPACES.
           05                      PIC X(4)      VALUE 'NAME'.
           05                      PIC X(13)     VALUE SPACES.
           05                      PIC X(7)      VALUE 'AVERAGE'.
           05                      PIC X(5)      VALUE SPACES.
           05                      PIC X(5)      VALUE 'GRADE'.
      *
       01  DETAIL-LINE.
           05                      PIC X(8)     VALUE SPACES.
           05  DL-COURSE-NUM       PIC X(5).
           05                      PIC X(5)     VALUE SPACES.
           05  DL-NAME             PIC X(20).
           05                      PIC X(5)     VALUE SPACES.
           05  DL-AVERAGE          PIC ZZ9.9.
           05                      PIC X(8).
           05  DL-LETTER-GRADE     PIC X.
      *
       01  TOTAL-LINE.
           05                      PIC X(37)     VALUE SPACES.
           05                      PIC X(16)     VALUE 'CLASS AVERAGE'.
           05  TL-CLASS-AVG        PIC ZZ9.9.


       PROCEDURE DIVISION.
      *                        CGJ
       10-CONTROL-MODULE.

           PERFORM 15-HOUSEKEEPING
           PERFORM 20-READ-A-REC
           PERFORM 50-EOF-ROUTINE
           .

       15-HOUSEKEEPING.

           OPEN INPUT CLASS-FILE
               OUTPUT REPORT-FILE

           ACCEPT CURRENT-DATE FROM DATE YYYYMMDD
           MOVE CD-MONTH TO H1-MONTH
           MOVE CD-DAY TO H1-DAY
           MOVE CD-YEAR TO H1-YEAR
           PERFORM 25-HEADER-ROUTINE
           .

       20-READ-A-REC.

          PERFORM UNTIL EOF-FLAG = 'N'
           READ CLASS-FILE
               AT END
                    MOVE 'N' TO EOF-FLAG
               NOT AT END
                   PERFORM 30-FIND-INDIVIDUAL-AVG
           END-READ
          END-PERFORM

           .

       25-HEADER-ROUTINE.

           WRITE REPORT-REC FROM HEADER-1
               AFTER ADVANCING PROPER-SPACING

           MOVE 3 TO PROPER-SPACING

           MOVE HEADER-2 TO REPORT-REC
           PERFORM 40-WRITE-A-LINE

           MOVE 2 TO PROPER-SPACING
           .

       30-FIND-INDIVIDUAL-AVG.

      * PUT EVALUATE STATEMENT TO CHECK FOR CONTROL BREAK
          EVALUATE TRUE
            WHEN FIRST-RECORD = 'Y'
                MOVE 'N' TO FIRST-RECORD
                MOVE CF-COURSE-NUM TO CLASS-HOLD
            WHEN CF-COURSE-NUM NOT EQUAL TO CLASS-HOLD
                PERFORM 45-CLASS-BREAK
            END-EVALUATE


           MOVE CF-COURSE-NUM TO DL-COURSE-NUM
           MOVE CF-NAME TO DL-NAME
 
      *  ADD THE 4 GRADES TO GET AN AVERAGE

           ADD CF-GRADE1, CF-GRADE2, CF-GRADE3, CF-GRADE4 
                    GIVING DF-SUM-GRADES

      *  GET THE AVERAGE

               DIVIDE CF-NUM-TESTS INTO DF-SUM-GRADES
                    GIVING DF-AVG

           MOVE DF-AVG TO DL-AVERAGE

           PERFORM 35-EVALUATE-GRADE

           MOVE DETAIL-LINE TO REPORT-REC
           PERFORM 40-WRITE-A-LINE

           MOVE 1 TO PROPER-SPACING

           ADD 1 TO TF-NUM-STUDENTS
           ADD DF-AVG TO TF-SUM-AVERAGES
           MOVE ZEROS TO DF-SUM-GRADES
           .

       35-EVALUATE-GRADE.
      * WRITE THE EVALUATE STATEMENT TO TEST DF-AVG AND 
      * ASSIGN A LETTER GRADE

         EVALUATE TRUE
            WHEN DF-AVG >= 90
               MOVE 'A' TO DL-LETTER-GRADE

            WHEN DF-AVG >= 80 AND DF-AVG <90
               MOVE 'B' TO DL-LETTER-GRADE

            WHEN DF-AVG >= 70 AND DF-AVG <80
               MOVE 'C' TO DL-LETTER-GRADE

            WHEN DF-AVG >= 60 AND DF-AVG <70
               MOVE 'D' TO DL-LETTER-GRADE

            WHEN OTHER
               MOVE 'F' TO DL-LETTER-GRADE
         END-EVALUATE

           .

       40-WRITE-A-LINE.

           WRITE REPORT-REC
               AFTER ADVANCING PROPER-SPACING
           .

        45-CLASS-BREAK.
      *  GET THE OVERALL AVERAGE FOR THE STUDENTS

          DIVIDE TF-SUM-AVERAGES BY TF-NUM-STUDENTS
               GIVING TF-CLASS-AVG

           MOVE TF-CLASS-AVG TO TL-CLASS-AVG
           MOVE TOTAL-LINE TO REPORT-REC
           MOVE 2 TO PROPER-SPACING
           PERFORM 40-WRITE-A-LINE

      * MOVE ZEROS TO REINITIALIZE THE TOTAL FIELDS

           MOVE 0 TO TF-SUM-AVERAGES
           MOVE ZERO TO TF-NUM-STUDENTS
           MOVE ZEROES TO TF-CLASS-AVG
           MOVE 0 TO TL-CLASS-AVG

      * MOVE THE INCOMING CLASS CODE TO THE CLASS HOLD

           MOVE CF-COURSE-NUM TO CLASS-HOLD
                     
           .

       50-EOF-ROUTINE.

      * FORCE LAST TOTAL LINE TO PRINT

          PERFORM 45-CLASS-BREAK

           CLOSE CLASS-FILE
                 REPORT-FILE

           STOP RUN
           .
