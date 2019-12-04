      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Exo15.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT NOM ASSIGN TO 'C:\Users\***\Desktop\EXO15.txt'.
           SELECT SORTIE ASSIGN TO 'C:\Users\***\Desktop\EXO15-S.txt'.
       DATA DIVISION.
       FILE SECTION.
       FD  NOM.
       01  ENREG.
           02 F-ID PIC XX.
           02 F-NOM PIC X(7).
       FD  SORTIE.
       01  F-SORTIE.
           02 S-ID PIC XX.
           02 S-NOM PIC X(7).
       WORKING-STORAGE SECTION.
       77  BOOL PIC 9 VALUE 0.
       77  EOF PIC 9.
       77  SORTIR PIC 9.
       77  COMPTEUR-PASSAGE PIC 999.
       01  TEMP-NOM.
           02 T-ID PIC XX.
           02 T-NOM PIC X(7).
       01  TAMPON.
           02 Z-ID PIC XX.
           02 Z-NOM PIC X(7).
       01  ZONE-G.
           02 G-ID PIC XX.
           02 G-NOM PIC X(7).
       PROCEDURE DIVISION.


       DEBUT.
           MOVE 0 TO BOOL
           MOVE 0 TO SORTIR
           COMPUTE COMPTEUR-PASSAGE = 0
           PERFORM MAIN UNTIL SORTIR = 1
           PERFORM FIN2
           STOP RUN.
       FIN2.
           DISPLAY 'Le compteur a effectué ' COMPTEUR-PASSAGE ' passages
      -     ''.
       MAIN.
           PERFORM INIT
           PERFORM TRI-BULLE UNTIL EOF = 1
           PERFORM FIN.

       INIT.
           DISPLAY 'INIT'
           MOVE 0 TO EOF
           IF BOOL = 1 THEN
               MOVE 0 TO BOOL
           ELSE
               MOVE 1 TO BOOL
           END-IF
           DISPLAY BOOL
           IF BOOL = 1 THEN
               OPEN INPUT NOM OUTPUT SORTIE
           ELSE
               OPEN INPUT SORTIE OUTPUT NOM
           END-IF
      *    PREMIERE LECTURE POUR METTRE DANS LE TEMP-NON, LA 2EME LECTUR
      *    PERMET DE STOCKER L'AUTRE VALEUR DANS TAMPON
           PERFORM LECTURE
           MOVE TAMPON TO TEMP-NOM
           MOVE 1 TO SORTIR
           PERFORM LECTURE.

       TRI-BULLE.

           DISPLAY BOOL
           IF Z-ID < T-ID THEN
               MOVE TAMPON TO ZONE-G
               PERFORM ECRITURE
               MOVE 0 TO SORTIR
           ELSE
               MOVE TEMP-NOM TO ZONE-G
               PERFORM ECRITURE
               MOVE TAMPON TO TEMP-NOM

           END-IF

           PERFORM LECTURE.
       FIN.
           MOVE TEMP-NOM TO ZONE-G
           PERFORM ECRITURE
           ADD 1 TO COMPTEUR-PASSAGE

           CLOSE NOM SORTIE.
       ECRITURE.

           IF BOOL = 1 THEN
               WRITE F-SORTIE FROM ZONE-G
               END-WRITE

           ELSE
               WRITE ENREG FROM ZONE-G
               END-WRITE

           END-IF.

       LECTURE.

           IF BOOL = 1 THEN
               READ NOM INTO TAMPON
                   AT END MOVE 1 TO EOF
               END-READ

           ELSE
               READ SORTIE INTO TAMPON
                   AT END MOVE 1 TO EOF
               END-READ

           END-IF.
       END PROGRAM Exo15.
