grammar lolcodeMarkdown;

// PARSER RULES  

program
  : HAI
    comment*            
    head?               
    comment*            
    element*            
    KTHXBYE
  ;

// ---- head / title ----
head
  : MAEK HEAD title? OIC
  ;

title
  : GIMMEH TITLE content MKAY
  ;

// ---- body elements ----
element
  : comment
  | paragraph
  | listBlock
  | bold
  | italics
  | sound
  | video
  | newlineAnn
  ;

// ---- comment ----
comment
  : OBTW content TLDR
  ;

// ---- paragraph:----
paragraph
  : MAEK PARAGRAF variableDef? paragraphItem* OIC
  ;

paragraphItem
  : comment
  | bold
  | italics
  | listBlock
  | sound
  | video
  | newlineAnn
  | variableUse
  | content
  ;

// ---- bold / italics: ----
bold
  : GIMMEH BOLD content MKAY
  ;

italics
  : GIMMEH ITALICS content MKAY
  ;

// ---- list ----
listBlock
  : MAEK LIST variableDef? listItem+ OIC
  ;

listItem
  : GIMMEH ITEM variableDef? itemPiece* MKAY
  ;

itemPiece
  : comment
  | bold
  | italics
  | newlineAnn
  | variableUse
  | content
  ;

// ---- inline annotations ----
newlineAnn
  : GIMMEH NEWLINEKW
  ;

sound
  : GIMMEH SOUNDZ content MKAY
  ;

video
  : GIMMEH VIDZ content MKAY
  ;

// ---- variables ----
variableDef
  : IKW HAZ WORD ITKW IZ content MKAY
  ;

variableUse
  : LEMME SEE WORD MKAY
  ;

// ---- plain text (single alternative; accepts both WORD and TEXT tokens) ----
content
  : (WORD | TEXT)+
  ;

// LEXER RULES

HAI        : '#HAI' | '#hai';
KTHXBYE    : '#KTHXBYE' | '#kthxbye';

OBTW       : '#OBTW' | '#obtw';
TLDR       : '#TLDR' | '#tldr';

MAEK       : '#MAEK' | '#maek';
HEAD       : 'HEAD' | 'head';
PARAGRAF   : 'PARAGRAF' | 'paragraf';
LIST       : 'LIST' | 'list';
ITEM       : 'ITEM' | 'item';
OIC        : '#OIC' | '#oic';

GIMMEH     : '#GIMMEH' | '#gimmeh';
TITLE      : 'TITLE' | 'title';
BOLD       : 'BOLD' | 'bold';
ITALICS    : 'ITALICS' | 'italics';
NEWLINEKW  : 'NEWLINE' | 'newline';
SOUNDZ     : 'SOUNDZ' | 'soundz';
VIDZ       : 'VIDZ' | 'vidz';
MKAY       : '#MKAY' | '#mkay';

IKW        : '#I' | '#i';
HAZ        : 'HAZ' | 'haz';
ITKW       : '#IT' | '#it';
IZ         : 'IZ' | 'iz';
LEMME      : '#LEMME' | '#lemme';
SEE        : 'SEE' | 'see';

// Identifier for variable names 
WORD
  : ('A'..'Z' | 'a'..'z') ('A'..'Z' | 'a'..'z' | '0'..'9')*
  ;

// Text

TEXT
  : ( 'A'..'Z' | 'a'..'z'
    | '0'..'9'
    | ',' | '.' | ':' | '?' | '!' | '%' | '/'
    | '"'
    )+
  ;

// Skip whitespace so blank lines don't produce tokens
WS : ( ' ' | '\t' | '\r' | '\n' )+ {skip();};
