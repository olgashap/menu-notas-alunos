      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. ProgramaMenuCompleto.
      *--------------------------------
      * Add, Change, Inquire, and Delete
      * for the Notas File.
      *--------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT NOTAS-FILE
           ASSIGN TO "notasindex.dat"
           ORGANIZATION IS INDEXED
           RECORD KEY IS ALUNO-NUMBER
           ACCESS MODE IS DYNAMIC.


       DATA DIVISION.
           FILE SECTION.

           FD NOTAS-FILE.
           01 NOTAS-RECORD.
           05  ALUNO-NUMBER                  PIC 999 VALUE 1.
           05  NOMEALUNO               PIC X(20).
           05  NOTAINGLES              PIC 99V99 VALUE 21.
                88 NOTA-INGLES VALUE 0.00 THRU 20.00.
           05  NOTAPORTUGUES           PIC 99V99 VALUE 21.
                88 NOTA-PORTUGUES VALUE 0.00 THRU 20.00.
           05  NOTAMATEMATICA          PIC 99V99 VALUE 21.
                88 NOTA-MATEMATICA VALUE 0.00 THRU 20.00.
           05  NOTAPROGRAMACAO        PIC 99V99 VALUE 21.
                88 NOTA-PROGRAMACAO VALUE 0.00 THRU 20.00.



       WORKING-STORAGE SECTION.
           77  MEDIA    PIC 99V99.

           77  APROVEITAMENTO  PIC X(18).
           77  NOTA-INVALIDA     PIC X(50)
             VALUE "NOTA INVALIDA. INTRODUZA NOTA ENTRE 0.00 E 20.00!".
           77  MENU-PICK PIC 9.
               88 MENU-PICK-IS-VALID VALUES 0 THRU 8.

           77  THE-MODE PIC X(7).
           77  WHICH-FIELD PIC 9.
           77  OK-TO-DELETE PIC X.
           77  RECORD-FOUND PIC X.
           77  ALUNO-NUMBER-FIELD PIC Z(10).
           77  FILE-AT-END PIC X.

           01  A-DUMMY PIC X.
           77  LINE-COUNT PIC 999 VALUE ZERO.
           77  PAGE-NUMBER PIC 99999 VALUE ZERO.
           77  MAXIMUM-LINES PIC 999 VALUE 15.

           77 DISPLAY-RECORD PIC X(150).

      *-----------------------------
      * VARIAVEIS PARA A LIST5-MODE
      *-----------------------------
       01 DETAIL-LINE.

       05 DISPLAY-NUMBER PIC 9(4).
       05 FILLER PIC X(4) VALUE SPACE.
       05 DISPLAY-NAME PIC X(30).
       05 FILLER PIC X(8) VALUE SPACE.
       05 DISPLAY-NOTA-INGLES PIC 9(2).
       05 FILLER PIC X(13) VALUE SPACE.
       05 DISPLAY-NOTA-PORTUGUES PIC 9(2).
       05 FILLER PIC X(13) VALUE SPACE.
       05 DISPLAY-NOTA-MATEMATICA PIC 9(2).
       05 FILLER PIC X(13) VALUE SPACE.
       05 DISPLAY-NOTA-PROGRAMACAO PIC 9(2).
       05 FILLER PIC X(18) VALUE SPACE.
       05 DISPLAY-CALCULO-MEDIA PIC 9(2).
       05 FILLER PIC X(13) VALUE SPACE.
       05 DISPLAY-AVALIACAO PIC X(18).



       01 COLUMN-LINE.
       05 FILLER PIC X(2) VALUE "NO".
       05 FILLER PIC X(4) VALUE SPACE.
       05 FILLER PIC X(10) VALUE "NAME ALUNO".
       05 FILLER PIC X(25) VALUE SPACE.
       05 FILLER PIC X(11) VALUE "NOTA INGLES".
       05 FILLER PIC X(3) VALUE SPACE.
       05 FILLER PIC X(14) VALUE "NOTA PORTUGUES".
       05 FILLER PIC X(3) VALUE SPACE.
       05 FILLER PIC X(15) VALUE "NOTA MATEMATICA".
       05 FILLER PIC X(3) VALUE SPACE.
       05 FILLER PIC X(16) VALUE "NOTA PROGRAMACAO".
       05 FILLER PIC X(3) VALUE SPACE.
       05 FILLER PIC X(5) VALUE "MEDIA".
       05 FILLER PIC X(3) VALUE SPACE.
       05 FILLER PIC X(9) VALUE "AVALIACAO".


       01 TITLE-LINE.
       05 FILLER PIC X(15) VALUE SPACE.
       05 FILLER PIC X(11)
            VALUE "ALUNO LIST".
       05 FILLER PIC X(15) VALUE SPACE.
       05 FILLER PIC X(5) VALUE "PAGE:".
       05 FILLER PIC X(1) VALUE SPACE.
       05 DISPLAY-PAGE-NUMBER PIC ZZZZ9.


       PROCEDURE DIVISION.
           PROGRAM-BEGIN.
           PERFORM MAIN-PROCESS.


       PROGRAM-DONE.
            STOP RUN.

           OPENING-PROCEDURE.
           OPEN I-O NOTAS-FILE.

           CLOSING-PROCEDURE.
           CLOSE NOTAS-FILE.


       MAIN-PROCESS.
           PERFORM GET-MENU-PICK.
           PERFORM MAINTAIN-THE-FILE
                    UNTIL MENU-PICK = 0.

      *--------------------------------
      * MENU
      *--------------------------------
       GET-MENU-PICK.
           IF THE-MODE="LISTING"
               PERFORM PRESS-ENTER.
           PERFORM DISPLAY-THE-MENU.
           PERFORM GET-THE-PICK.
           PERFORM MENU-RETRY
           UNTIL MENU-PICK-IS-VALID.

       DISPLAY-THE-MENU.
           PERFORM CLEAR-SCREEN.
           DISPLAY " PLEASE SELECT:".
           DISPLAY " ".
           DISPLAY " 1. ADD RECORDS".
           DISPLAY " 2. CHANGE A RECORD".
           DISPLAY " 3. LOOK UP A RECORD".
           DISPLAY " 4. DELETE A RECORD".
           DISPLAY " 5. LIST ALL (pages)".
           DISPLAY " 6. LIST NUMBER/NAME".
           DISPLAY " 7. LIST ALL".
           DISPLAY " 8. CREATE FILE".
           DISPLAY " ".
           DISPLAY " 0. EXIT".
           PERFORM SCROLL-LINE 2 TIMES.

       GET-THE-PICK.
           DISPLAY "YOUR CHOICE (0-8)?".
           ACCEPT MENU-PICK.

       MENU-RETRY.
           DISPLAY "INVALID SELECTION - PLEASE RETRY.".
           PERFORM GET-THE-PICK.
           CLEAR-SCREEN.
           PERFORM SCROLL-LINE 25 TIMES.

       SCROLL-LINE.
           DISPLAY " ".

       MAINTAIN-THE-FILE.

           PERFORM DO-THE-PICK.
           PERFORM GET-MENU-PICK.


       DO-THE-PICK.
           PERFORM OPENING-PROCEDURE.
           IF MENU-PICK = 1
           PERFORM ADD-MODE
           ELSE
           IF MENU-PICK = 2
           PERFORM CHANGE-MODE
           ELSE
           IF MENU-PICK = 3
           PERFORM INQUIRE-MODE
           ELSE
           IF MENU-PICK = 4
           PERFORM DELETE-MODE
           ELSE
           IF MENU-PICK = 5
               PERFORM LIST5-MODE
           ELSE
           IF MENU-PICK = 6
               PERFORM LIST6-MODE
           ELSE
           IF MENU-PICK = 7
               PERFORM LIST7-MODE
            ELSE
           IF MENU-PICK = 8
               PERFORM CREATE8-MODE.
           PERFORM CLOSING-PROCEDURE.



      *--------------------------------
      * LIST5
      *--------------------------------
       LIST5-MODE.
           MOVE "LISTING" TO THE-MODE.

           MOVE ZEROES TO LINE-COUNT
       PAGE-NUMBER.
           MOVE "N" TO FILE-AT-END.

           PERFORM START-NEW-PAGE.

           PERFORM READ-NEXT-RECORD.
           IF FILE-AT-END = "Y"
           MOVE "NO RECORDS FOUND" TO DISPLAY-RECORD
           PERFORM WRITE-DISPLAY-RECORD
           ELSE
           PERFORM DISPLAY-ALUNO-FIELDS
           UNTIL FILE-AT-END = "Y".

       DISPLAY-ALUNO-FIELDS.
           IF LINE-COUNT > MAXIMUM-LINES
           PERFORM START-NEXT-PAGE.
           PERFORM DISPLAY-THE-RECORD.
           PERFORM READ-NEXT-RECORD.

       DISPLAY-THE-RECORD.
           PERFORM DISPLAY-LINE-1.



           PERFORM LINE-FEED.

       DISPLAY-LINE-1.
           MOVE SPACE TO DETAIL-LINE.
           MOVE ALUNO-NUMBER TO DISPLAY-NUMBER.
           MOVE NOMEALUNO TO DISPLAY-NAME.
           MOVE NOTAINGLES TO DISPLAY-NOTA-INGLES.
           MOVE NOTAPORTUGUES TO DISPLAY-NOTA-PORTUGUES.
           MOVE NOTAMATEMATICA TO DISPLAY-NOTA-MATEMATICA.
           MOVE NOTAPROGRAMACAO TO DISPLAY-NOTA-PROGRAMACAO.
           MOVE MEDIA TO DISPLAY-CALCULO-MEDIA.
           MOVE APROVEITAMENTO TO DISPLAY-AVALIACAO.
           MOVE DETAIL-LINE TO DISPLAY-RECORD.
           PERFORM WRITE-DISPLAY-RECORD.

       WRITE-DISPLAY-RECORD.
           DISPLAY DISPLAY-RECORD.
           ADD 1 TO LINE-COUNT.

       LINE-FEED.
           MOVE SPACE TO DISPLAY-RECORD.
           PERFORM WRITE-DISPLAY-RECORD.

       START-NEXT-PAGE.

           PERFORM END-LAST-PAGE.
           PERFORM START-NEW-PAGE.

       START-NEW-PAGE.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO DISPLAY-PAGE-NUMBER.
           MOVE TITLE-LINE TO DISPLAY-RECORD.
           PERFORM WRITE-DISPLAY-RECORD.
           PERFORM LINE-FEED.
           MOVE COLUMN-LINE TO DISPLAY-RECORD.
           PERFORM WRITE-DISPLAY-RECORD.
           PERFORM LINE-FEED.

       END-LAST-PAGE.
           PERFORM PRESS-ENTER.
           MOVE ZERO TO LINE-COUNT.

       PRESS-ENTER.
           DISPLAY "PRESS ENTER TO CONTINUE. . .".
           ACCEPT A-DUMMY.
      *--------------------------------
      * LIST6 - NO-NOME
      *--------------------------------
       LIST6-MODE.

           MOVE "LISTING" TO THE-MODE.
           MOVE "N" TO FILE-AT-END.
           PERFORM READ-NEXT-RECORD.
           IF FILE-AT-END = "Y"
               DISPLAY "NO RECORDS FOUND"
           ELSE
               PERFORM DISPLAY-ALUNO-DATA
                   UNTIL FILE-AT-END = "Y".

       DISPLAY-ALUNO-DATA.
           DISPLAY "NO: " ALUNO-NUMBER
           " NAME: " NOMEALUNO.
           PERFORM READ-NEXT-RECORD.


      *--------------------------------------------------
      * LIST7-MODE.
      *--------------------------------------------------
       LIST7-MODE.

           MOVE "LISTING" TO THE-MODE.
           MOVE "N" TO FILE-AT-END.
           PERFORM READ-NEXT-RECORD.
           PERFORM READ-AND-DISPLAY
               UNTIL FILE-AT-END = "Y".

           READ-AND-DISPLAY.
               DISPLAY NOTAS-RECORD.
               PERFORM READ-NEXT-RECORD.


      *------------------------------------------------
      * Create an Empty Vendor File.
      *------------------------------------------------

       CREATE8-MODE.
           MOVE "LISTING" TO THE-MODE.
      *    Pedir confirmacao antes de DESTRUIR o ficheiro
           PERFORM CLOSING-PROCEDURE.
           OPEN OUTPUT NOTAS-FILE.
           DISPLAY "O ficheiro foi criado".
           PERFORM CLOSING-PROCEDURE.

       READ-NEXT-RECORD.
           READ NOTAS-FILE NEXT RECORD
           AT END MOVE "Y" TO FILE-AT-END.


      *--------------------------------
      * ADD
      *--------------------------------
       ADD-MODE.
           MOVE "ADD" TO THE-MODE.
           PERFORM GET-NEW-ALUNO-NUMBER.
           PERFORM ADD-RECORDS
           UNTIL ALUNO-NUMBER = ZEROES.

       GET-NEW-ALUNO-NUMBER.
           PERFORM INIT-NOTAS-RECORD.
           PERFORM ENTER-ALUNO-NUMBER.
           MOVE "Y" TO RECORD-FOUND.
           PERFORM FIND-NEW-NOTAS-RECORD
           UNTIL RECORD-FOUND = "N" OR
       ALUNO-NUMBER = ZEROES.

       FIND-NEW-NOTAS-RECORD.
           PERFORM READ-NOTAS-RECORD.
            IF RECORD-FOUND = "Y"
           DISPLAY "RECORD ALREADY ON FILE"
           PERFORM ENTER-ALUNO-NUMBER.

       ADD-RECORDS.
           PERFORM ENTER-REMAINING-FIELDS.
           PERFORM WRITE-NOTAS-RECORD.
           PERFORM GET-NEW-ALUNO-NUMBER.

       ENTER-REMAINING-FIELDS.
           PERFORM ENTER-NOMEALUNO.
           PERFORM ENTER-NOTAINGLES.
           PERFORM ENTER-NOTAPORTUGUES.
           PERFORM ENTER-NOTAMATEMATICA.
           PERFORM ENTER-NOTAPROGRAMACAO.
           PERFORM CALCULO-MEDIA.
           PERFORM AVALIACAO.

      *--------------------------------
      * CHANGE
      *--------------------------------
       CHANGE-MODE.

           MOVE "CHANGE" TO THE-MODE.
           PERFORM GET-NOTAS-RECORD.
           PERFORM CHANGE-RECORDS
           UNTIL ALUNO-NUMBER = ZEROES.

       CHANGE-RECORDS.
           PERFORM GET-FIELD-TO-CHANGE.
           PERFORM CHANGE-ONE-FIELD
           UNTIL WHICH-FIELD = ZERO.
           PERFORM GET-NOTAS-RECORD.
       GET-FIELD-TO-CHANGE.
           PERFORM DISPLAY-ALL-FIELDS.
           PERFORM ASK-WHICH-FIELD.

       ASK-WHICH-FIELD.
           DISPLAY "ENTER THE NUMBER OF THE FIELD".
           DISPLAY "TO CHANGE (1-7) OR 0 TO EXIT".
           ACCEPT WHICH-FIELD.
           IF WHICH-FIELD > 7
               DISPLAY "INVALID ENTRY".

       CHANGE-ONE-FIELD.
           PERFORM CHANGE-THIS-FIELD.
           PERFORM GET-FIELD-TO-CHANGE.

           CHANGE-THIS-FIELD.
           IF WHICH-FIELD = 1
           PERFORM ENTER-NOMEALUNO.
           IF WHICH-FIELD = 2
           PERFORM  ENTER-NOTAINGLES.
           IF WHICH-FIELD = 3
           PERFORM ENTER-NOTAPORTUGUES.
           IF WHICH-FIELD = 4
           PERFORM ENTER-NOTAMATEMATICA.
           IF WHICH-FIELD = 5
           PERFORM ENTER-NOTAPROGRAMACAO.
           IF WHICH-FIELD = 6
           PERFORM CALCULO-MEDIA.
           IF WHICH-FIELD = 7
           PERFORM AVALIACAO.

           PERFORM REWRITE-NOTAS-RECORD.

      *--------------------------------
      * INQUIRE
      *--------------------------------
       INQUIRE-MODE.
           MOVE "DISPLAY" TO THE-MODE.
           PERFORM GET-NOTAS-RECORD.
           PERFORM INQUIRE-RECORDS
           UNTIL ALUNO-NUMBER = ZEROES.

       INQUIRE-RECORDS.
           PERFORM DISPLAY-ALL-FIELDS.
           PERFORM GET-NOTAS-RECORD.

      *--------------------------------
      * DELETE
      *--------------------------------
       DELETE-MODE.
           MOVE "DELETE" TO THE-MODE.
           PERFORM GET-NOTAS-RECORD.
           PERFORM DELETE-RECORDS
           UNTIL ALUNO-NUMBER = ZEROES.

           DELETE-RECORDS.
           PERFORM DISPLAY-ALL-FIELDS.
           MOVE "X" TO OK-TO-DELETE.

           PERFORM ASK-TO-DELETE
           UNTIL OK-TO-DELETE = "Y" OR "N".

           IF OK-TO-DELETE = "Y"
           PERFORM DELETE-NOTAS-RECORD.

           PERFORM GET-NOTAS-RECORD.

       ASK-TO-DELETE.
           DISPLAY "DELETE THIS RECORD (Y/N)?".
           ACCEPT OK-TO-DELETE.
           IF OK-TO-DELETE = "y"
           MOVE "Y" TO OK-TO-DELETE.
           IF OK-TO-DELETE = "n"
           MOVE "N" TO OK-TO-DELETE.
           IF OK-TO-DELETE NOT = "Y" AND
                 OK-TO-DELETE NOT = "N"
           DISPLAY "YOU MUST ENTER YES OR NO".

      *--------------------------------
      * Routines shared by all modes
      *--------------------------------

       INIT-NOTAS-RECORD.
           MOVE SPACE TO NOTAS-RECORD.
           MOVE ZEROES TO ALUNO-NUMBER.

       ENTER-ALUNO-NUMBER.
           DISPLAY " ".
           DISPLAY "ENTER ALUNO NUMBER " .
           DISPLAY "TO " THE-MODE " (1-99999)".
           DISPLAY "ENTER 0 TO STOP ENTRY".
           ACCEPT ALUNO-NUMBER-FIELD.

           MOVE ALUNO-NUMBER-FIELD TO ALUNO-NUMBER.

       GET-NOTAS-RECORD.
           PERFORM INIT-NOTAS-RECORD.
           PERFORM ENTER-ALUNO-NUMBER.
           MOVE "N" TO RECORD-FOUND.
           PERFORM FIND-NOTAS-RECORD
                      UNTIL RECORD-FOUND = "Y" OR
           ALUNO-NUMBER = ZEROES.

      *--------------------------------
      * Routines shared Add and Change
      *--------------------------------
       FIND-NOTAS-RECORD.
           PERFORM READ-NOTAS-RECORD.
           IF RECORD-FOUND = "N"
           DISPLAY "RECORD NOT FOUND"
           PERFORM ENTER-ALUNO-NUMBER.

           ENTER-NOMEALUNO.
           DISPLAY "ENTER ALUNO NAME".
           ACCEPT NOMEALUNO.

           ENTER-NOTAINGLES.
           DISPLAY "Nota Ingles: "
           ACCEPT NOTAINGLES.

           ENTER-NOTAPORTUGUES.
           DISPLAY "Nota Portugues: "
           ACCEPT NOTAPORTUGUES.

           ENTER-NOTAMATEMATICA.
           DISPLAY "Nota Matematica: "
           ACCEPT NOTAMATEMATICA.

           ENTER-NOTAPROGRAMACAO.
           DISPLAY "Nota Programacao: "
           ACCEPT NOTAPROGRAMACAO.

       CALCULO-MEDIA.
                COMPUTE MEDIA = (NOTAINGLES + NOTAPORTUGUES
                + NOTAMATEMATICA + NOTAPROGRAMACAO)/4.
                DISPLAY "Media: " MEDIA.
                IF MEDIA >= 10
                    DISPLAY "APROVADO"
                ELSE
                    DISPLAY "REPROVADO".
       AVALIACAO.
                IF MEDIA <5 THEN
                    MOVE "MUITO INSUFICIENTE" TO APROVEITAMENTO
                ELSE IF MEDIA <10 THEN
                    MOVE "INSUFICIENTE" TO APROVEITAMENTO
                ELSE IF MEDIA <15 THEN
                    MOVE "SUFICIENTE" TO APROVEITAMENTO
                ELSE IF MEDIA <18 THEN
                    MOVE "BOM" TO APROVEITAMENTO
                ELSE
                    MOVE "BOM" TO APROVEITAMENTO.
                DISPLAY "Avaliacao: " APROVEITAMENTO.
      *--------------------------------
      * Routines shared by Change,
      * Inquire and Delete
      *--------------------------------
       DISPLAY-ALL-FIELDS.
           DISPLAY " ".
           PERFORM DISPLAY-ALUNO-NUMBER.
           PERFORM DISPLAY-NOMEALUNO.
           PERFORM DISPLAY-NOTAINGLES.
           PERFORM DISPLAY-NOTAPORTUGUES.
           PERFORM DISPLAY-NOTAMATEMATICA.
           PERFORM DISPLAY-NOTAPROGRAMACAO.
           PERFORM DISPLAY-MEDIA.
           PERFORM DISPLAY-APROVEITAMENTO.

           DISPLAY " ".

           DISPLAY-ALUNO-NUMBER.
           DISPLAY " ALUNO NUMBER: " ALUNO-NUMBER.

           DISPLAY-NOMEALUNO.
           DISPLAY "1. ALUNO NAME: " NOMEALUNO.

           DISPLAY-NOTAINGLES.
           DISPLAY "2. Nota ingles: " NOTAINGLES.

           DISPLAY-NOTAPORTUGUES.
           DISPLAY "3. Nota portugues: " NOTAPORTUGUES.

           DISPLAY-NOTAMATEMATICA.
           DISPLAY "4. Nota matematica: " NOTAMATEMATICA.

           DISPLAY-NOTAPROGRAMACAO.
           DISPLAY "5. Nota programacao: " NOTAPROGRAMACAO.

           DISPLAY-MEDIA.
           DISPLAY "6. Media: " MEDIA.

           DISPLAY-APROVEITAMENTO.
           DISPLAY "7. Aproveitamento: "APROVEITAMENTO.


      *--------------------------------
      * File I-O Routines
      *--------------------------------
       READ-NOTAS-RECORD.
           MOVE "Y" TO RECORD-FOUND.
           READ NOTAS-FILE RECORD
           INVALID KEY
           MOVE "N" TO RECORD-FOUND.

       WRITE-NOTAS-RECORD.
           WRITE NOTAS-RECORD
           INVALID KEY
           DISPLAY "RECORD ALREADY ON FILE".

       REWRITE-NOTAS-RECORD.
           REWRITE NOTAS-RECORD
           INVALID KEY
           DISPLAY "ERROR REWRITING NOTAS RECORD".

       DELETE-NOTAS-RECORD.
           DELETE NOTAS-FILE RECORD
           INVALID KEY
           DISPLAY "ERROR DELETING NOTAS RECORD".
