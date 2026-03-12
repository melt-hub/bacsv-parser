
A simple Common Lisp implementation of a recursive descent parser for CSV (comma separated values) formatted files. 

## Why Basic?

We refer to this implementation as basic or simple because it's based on a simplified version of the original context-free grammar for CSV files, formalized in [ABNF](https://www.rfc-editor.org/rfc/rfc2234) (Augmented Backus-Naur Form) in [RFC 4180](https://www.rfc-editor.org/rfc/rfc4180). For reference, the original grammar from RFC 4180:

```abnf
   file = [header CRLF] record *(CRLF record) [CRLF]

   header = name *(COMMA name)

   record = field *(COMMA field)

   name = field

   field = (escaped / non-escaped)

   escaped = DQUOTE *(TEXTDATA / COMMA / CR / LF / 2DQUOTE) DQUOTE

   non-escaped = *TEXTDATA

   COMMA = %x2C

   CR = %x0D
   
   DQUOTE =  %x22 

   LF = %x0A 
   
   CRLF = CR LF 
   
   TEXTDATA =  %x20-21 / %x23-2B / %x2D-7E
```

And the simplified grammar we designed:

```abnf
file = [header CLRF] record *(CLRF record) [CLRF]

header = name *(COMMA name)

record = field *(COMMA field) / enclosed_field *(COMMA enclosed_fielid)

name = field

enclosed_field = DQUOTE *(TEXTDATA / COMMA) DQUOTE 

field = *(TEXTDATA) 

COMMA = %x2C

CR = %x0D

DQUOTE = %x22

LF = %x0A 
   
CRLF = CR LF 

TEXTDATA =  %x20-21 / %x23-2B / %x2D-7E
```

in a form where RegEx(s) are simulated by right-recursive production rules:

```abnf
file = header records

header = names

names = name COMMA names / name CLRF

name = field / enclosed_field


records = CLRF record records / CLRF record CLRF

record = fields / enclosed_fields

fields = field COMMA fields 

enclosed_fields = enclosed_field COMMA enclosed_fields

field = word

enclosed_field = DQUOTE word DQUOTE 

word = TEXTDATA word / COMMA word


COMMA = %x2C

CR = %x0D

DQUOTE = %x22

LF = %x0A 
   
CRLF = CR LF 

TEXTDATA =  %x20-21 / %x23-2B / %x2D-7E
```

Despite our context-free grammar being quite similar to the original, it differs in a slight detail, about the use of double quotes (`%x22`), from RFC 4180:

*"5. Each field may or may not be enclosed in double quotes (however some programs, such as Microsoft Excel, do not use double quotes at all).  If fields are not enclosed with double quotes, then double quotes may not appear inside the fields"*

*"6. Fields containing line breaks (CRLF), double quotes, and commas should be enclosed in double-quotes."*

*"7. If double-quotes are used to enclose fields, then a double-quote appearing inside a field must be escaped by preceding it with another double quote."*

Our grammar deals with none of those ambiguities; it just assumes that:

1. There may be an optional `header` line appearing as the first line of the file with the same format as normal record lines. This headers will contain `name`(s) corresponding to the `field` (s) in the `file`.
2. Each `record` is located on a separate line, delimite by a line break (`CLRF`).
3. The last record in the file may or may not have an ending line break.
4. Each `record` consints a number of `field`(s) that should be equal to the number of the `names`(s) in the `header`.
5. Each `field` consists of a sequence of any ASCII character but the double quote, enclosed between double quotes.

Imposing such limitations to the original grammar, thus to the language it generates, largely eases the complexity of our parser, nonetheless preserving usability in real world applications, being this CSV "dialect" quite common, and given the possibility to slightly modify our grammar to allow both double quote enclosed fields and non-double quot enclosed fields.
