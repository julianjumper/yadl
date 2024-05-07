grammar test;

fragment WS: (' ')*;
fragment WSplus: ' ' WS;

test: ((Statment)? WS Newline)* EOF;

Statment: Declaration | Format | Save;

Declaration: Identifier WS '=' WS Value;

Format: 'data' WSplus Identifier
    WSplus 'derived' WSplus 'from' WSplus Identifier WS
    '{' (Newline WSplus DataEntries)* Newline '}';

DataEntries: Identifier ':' WS (DataAccess | '_') WSplus 'as' WSplus Type;


Save: 'save' WSplus Identifier WSplus 'as' WSplus Identifier;

Value: Querry | Load | '('? WS ValueExpr ( WS ValueOps WS ValueExpr)* ')'?;
ValueOps: '+' | '-' | '/' | '*' | ',';
ValueExpr: Identifier | DataAccess | Integer | Identifier '(' WS Identifier WS ')';
DataAccess: ('data' | 'acc') ( '.' Identifier | '[' Integer ']' );

Load: 'load' WSplus Filename (WSplus 'as' WSplus Filetype)?;
Filetype: 'json' | 'csv';
Filename: [a-zA-Z]*'.' Filetype;

Querry: 'with' WSplus Identifier WSplus 'do' (QuerryOps)+;
QuerryOps: Newline WSplus (Filter | Map | Reduce | Join | Slice);

Filter: 'filter' WSplus Condition;
Condition: ValueExpr WS ConditionOps WS (ValueExpr | ('||' | '&&') ValueExpr)*;
ConditionOps: '=' | '<' | '>' | '<=' | '>=';

Map: 'map' WSplus 'to' WSplus Type (Mapping)*;
Mapping: Newline WSplus (Identifier | Self) WS '=' WS Value;
Self: 'self' ('[' Integer ']')?;

Reduce: 'reduce' WSplus 'to' WSplus ReduceExpr;
ReduceExpr: Value;

Join: 'join' WSplus Identifier WSplus 'with' WSplus Condition;

Slice: 'slice' WSplus SliceSize;
SliceSize: Integer (TimeUnit)?;
TimeUnit: 'd';

Identifier: IdentifierStart [a-zA-Z_0-9.]*;
IdentifierStart: [a-zA-Z_];

Integer: [0-9]+;

Newline: [\n\r] -> skip;

Type: Identifier | '(' WS Identifier WS (',' WS Identifier)* WS ')';
