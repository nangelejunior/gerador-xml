      ******************************************************************
       IDENTIFICATION                  DIVISION.
      ******************************************************************
       PROGRAM-ID. GXML01.
       AUTHOR. NEUCLAIR J ANGELE JUNIOR.
       DATE-WRITTEN. 31 MAI 2011.
       DATE-COMPILED.
      *REMARKS. *******************************************************
      *         *#NOME:# GXML                                         *
      *         *******************************************************
      *         *#FUNC:# GERADOR XML                                  *
      *         *******************************************************
      *         *#ANALISTA:# NEUCLAIR J ANGELE JUNIOR                 *
      *         *******************************************************
      *
      ******************************************************************
       ENVIRONMENT                     DIVISION.
      ******************************************************************
      *
      ******************************************************************
       CONFIGURATION                   SECTION.
      ******************************************************************
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
      ******************************************************************
       INPUT-OUTPUT                    SECTION.
      ******************************************************************
       FILE-CONTROL.
           SELECT ARQS  ASSIGN TO 'SYS020.XML'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE  STATUS IS WS-FS-SYS020.
      *
      ******************************************************************
       DATA                            DIVISION.
      ******************************************************************
      *
      ******************************************************************
       FILE                            SECTION.
      ******************************************************************
       FD  ARQS
           RECORDING MODE IS F
           LABEL   RECORD IS STANDARD
           BLOCK CONTAINS    0 RECORDS.
      *
       01  FD-ARQS                 PIC     X(150).
      *
      ******************************************************************
       WORKING-STORAGE                 SECTION.
      ******************************************************************
       77          FILLER          PIC     X(32)         VALUE
                                   'III WORKING-STORAGE SECTION III'.
      *
       01          WS-WORKING.
      ******************************************************************
      *                           FILE STATUS                          *
      ******************************************************************
           03      WS-AUXILIARES.
             05    FILLER          PIC     X(12)         VALUE
                                   'AUXILIARES'.
             05    WS-FS-SYS020    PIC     X(02)         VALUE ZEROS.
      *
      ******************************************************************
      *                          ACUMULADORES                          *
      ******************************************************************
           03      WS-ACUMULADORES.
             05    FILLER          PIC     X(12)         VALUE
                                   'ACUMULADORES'.
             05    WS-IND          PIC    S9(05) COMP    VALUE ZEROS.
             05    WS-CONT         PIC    S9(07) COMP-3  VALUE ZEROS.
      *
      ******************************************************************
      *                              XML                               *
      ******************************************************************
       01          FILLER          PIC     X(05)         VALUE '*XML*'.
      *
       01          WS-XML.
      *
      *------------<?XML>----------------------------------------------*
           03      XML-WXML.
             05    FILLER          PIC     X(43)         VALUE
                   '<?xml version="1.0" encoding="iso-8859-1"?>'.
      *
      *------------<ROOT>----------------------------------------------*
           03      ROOT-WXML.
             05    FILLER          PIC     X(43)         VALUE
                   '<ROOT C1="ARQUIVO XML RETORNO DA TRANSACAO '.
             05    CAB-CDTRAN      PIC     X(08)         VALUE
                   'V70I100A'.
             05    FILLER          PIC     X(03)         VALUE '" >'.
           03      ROOT-WXML-R     REDEFINES             ROOT-WXML
                                   PIC     X(54).
      *
      *------------<CAB>-----------------------------------------------*
           03      CAB-WXML.
             05    FILLER          PIC     X(05)         VALUE '<CAB '.
      *
             05    FILLER          PIC     X(09)         VALUE
                   'TAMRESP="'.
             05    TAMRESP-WXML    PIC     9(07)         VALUE ZEROS.
             05    FILLER          PIC     X(02)         VALUE '" '.
      *
             05    FILLER          PIC     X(09)         VALUE
                   'RETORNO="'.
             05    RETORNO-WXML    PIC     9(03)         VALUE ZEROS.
             05    FILLER          PIC     X(02)         VALUE '" '.
      *
             05    FILLER          PIC     X(09)         VALUE
                   'SQLCODE="'.
             05    SQLCODE-WXML    PIC    -9(03)         VALUE ZEROS.
             05    FILLER          PIC     X(02)         VALUE '" '.
      *
             05    FILLER          PIC     X(10)         VALUE
                   'MENSAGEM="'.
             05    MENSAGEM-WXML   PIC     X(80)         VALUE SPACES.
             05    FILLER          PIC     X(02)         VALUE '" '.
      *
             05    FILLER          PIC     X(02)         VALUE '/>'.
      *
      *------------<DADOS>---------------------------------------------*
       01          DADOS-WXML.
             03    FILLER          PIC     X(07)         VALUE
                   '<DADOS '.
      *
             03    FILLER          PIC     X(11)         VALUE
                   'CDMOTICAN="'.
             03    CDMOTICAN-WXML  PIC     9(05)         VALUE ZEROS.
             03    FILLER          PIC     X(02)         VALUE '" '.
      *
             03    FILLER          PIC     X(11)         VALUE
                   'DSMOTICAN="'.
             03    DSMOTICAN-WXML  PIC     X(60)         VALUE SPACES.
             03    FILLER          PIC     X(02)         VALUE '" '.
      *
             03    FILLER          PIC     X(09)         VALUE
                   'QTDECAN="'.
             03    QTDECAN-WXML    PIC     9(04)         VALUE ZEROS.
             03    FILLER          PIC     X(02)         VALUE '" '.
      *
             03    FILLER          PIC     X(02)         VALUE '/>'.
      *
      *------------<LISTA 1>-------------------------------------------*
       01          LISTA-WXML.
             03    FILLER          PIC     X(08)         VALUE
                   '<LISTA1>'.
      *
       01          OCORRENCIAS-WXML OCCURS 1000 TIMES
                   DEPENDING        ON QTDECAN-WXML.
      *
             03    MOTCAN-WXML.
               05  FILLER          PIC     X(09)         VALUE
                   '<MOTICAN '.
      *
               05  FILLER          PIC     X(12)         VALUE
                   'CDMOTICAN1="'.
               05  CDMOTICAN1-WXML PIC     9(05)         VALUE ZEROS.
               05  FILLER          PIC     X(02)         VALUE '" '.
      *
               05  FILLER          PIC     X(12)         VALUE
                   'DSMOTICAN1="'.
               05  DSMOTICAN1-WXML PIC     X(60)         VALUE SPACES.
               05  FILLER          PIC     X(02)         VALUE '" '.
      *
               05  FILLER          PIC     X(02)         VALUE '/>'.
      *
      *------------<BARRALISTA 1>--------------------------------------*
       01          BARRALISTA-WXML PIC     X(09)         VALUE
                   '</LISTA1>'.
      *
      *------------<BARRAROOT>-----------------------------------------*
       01          BARRAROOT-WXML  PIC     X(07)         VALUE
                   '</ROOT>'.
      *
      ******************************************************************
      *                          AREA DE DB2                           *
      ******************************************************************
      *
      ******************************************************************
      *                           COPYBOOKS                            *
      ******************************************************************
      *
      *
      ******************************************************************
      *
       01          FILLER          PIC     X(32)         VALUE
                                   'FFF FIM DA WORKING-STORAGE FFF'.
      *
      ******************************************************************
       LINKAGE                         SECTION.
      ******************************************************************
      *
      *
      ******************************************************************
       PROCEDURE                       DIVISION.
      ******************************************************************
      ******************************************************************
      *                        ROTINA PRINCIPAL                        *
      ******************************************************************
       RTPRINCIPAL                     SECTION.
      ******************************************************************
      *
           PERFORM RTINICIAR.
      *
           PERFORM RTPROCESSAR.
      *
           PERFORM RTFINALIZAR.
      *
      ******************************************************************
       RTPRINCIPAL-EXIT.               EXIT.
      ******************************************************************
      ******************************************************************
      *                         ROTINA INICIAL                         *
      ******************************************************************
       RTINICIAR                       SECTION.
      ******************************************************************
      *
           OPEN OUTPUT ARQS.
      *
           MOVE 1                      TO WS-IND.
      *
      ******************************************************************
       RTINICAR-EXIT.                  EXIT.
      ******************************************************************
      ******************************************************************
      *                         ROTINA PROCESSA                        *
      ******************************************************************
       RTPROCESSAR                     SECTION.
      ******************************************************************
      *
           MOVE ZEROS                  TO RETORNO-WXML
                                          SQLCODE-WXML.
           MOVE 'TRANSACAO EXECUTADA COM SUCESSO'
                                       TO MENSAGEM-WXML.
      *
           INSPECT MENSAGEM-WXML       TALLYING WS-CONT
                                       FOR CHARACTERS BEFORE '   '.
      *
           MOVE WS-CONT                TO TAMRESP-WXML.
           MOVE 00001                  TO CDMOTICAN-WXML.
           MOVE 'TESTE: 0001'          TO DSMOTICAN-WXML.
           MOVE 1000                   TO QTDECAN-WXML.
      *
           PERFORM                     UNTIL WS-IND GREATER 1000
               MOVE WS-IND             TO   CDMOTICAN1-WXML(WS-IND)
               STRING 'TESTE: ' WS-IND DELIMITED BY SIZE
                                       INTO DSMOTICAN1-WXML(WS-IND)
               ADD  1                  TO   WS-IND
           END-PERFORM.
      *
           WRITE FD-ARQS               FROM XML-WXML.
           WRITE FD-ARQS               FROM ROOT-WXML.
           WRITE FD-ARQS               FROM CAB-WXML.
           WRITE FD-ARQS               FROM DADOS-WXML.
           WRITE FD-ARQS               FROM LISTA-WXML.
      *
           MOVE  1                     TO    WS-IND.
      *
           PERFORM                     UNTIL WS-IND GREATER QTDECAN-WXML
               WRITE FD-ARQS           FROM  MOTCAN-WXML(WS-IND)
               ADD   1                 TO    WS-IND
           END-PERFORM.
      *
           WRITE FD-ARQS               FROM BARRALISTA-WXML.
           WRITE FD-ARQS               FROM BARRAROOT-WXML.
      *
      ******************************************************************
       RTPROCESSAR-EXIT.               EXIT.
      ******************************************************************
      ******************************************************************
      *                          ROTINA FINAL                          *
      ******************************************************************
       RTFINALIZAR                     SECTION.
      ******************************************************************
      *
           CLOSE ARQS.
      *
           MOVE ZEROS                  TO RETURN-CODE.
      *
           GOBACK.
      *
      ******************************************************************
       RTFINALIZAR-EXIT.               EXIT.
      ******************************************************************
      ******************************************************************
      *                        FIM DO PROGRAMA                         *
      ******************************************************************
